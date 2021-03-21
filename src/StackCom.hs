{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}

module StackCom
  ( projectName,
    comp,
    exec,
    run,
    fac,
  )
where

import Control.Monad.State
    ( gets,
      evalState,
      execState,
      MonadState(put, get),
      State,
      StateT(StateT) )
import Control.Monad.Writer
  ( MonadWriter (tell),
    WriterT (WriterT),
    execWriterT,
  )
import Data.Maybe (fromMaybe)

projectName :: String
projectName = "StackCom"

-- Imperative language

data Prog
  = Assign Name Expr
  | If Expr Prog Prog
  | While Expr Prog
  | Seq [Prog]
  deriving stock (Show)

data Expr = Val Int | Var Name | App Op Expr Expr
  deriving stock (Show)

type Name = Char

data Op = Add | Sub | Mul | Div
  deriving stock (Show)

-- Virtual machine

type Stack = [Int]

type Mem = [(Name, Int)]

type Code = [Inst]

data Inst
  = PUSH Int
  | PUSHV Name
  | POP Name
  | DO Op
  | JUMP Label
  | JUMPZ Label
  | LABEL Label
  deriving stock (Show)

type Label = Int

--  Compiler

-- possible optimizationsi to reduce number of instructions to run:

-- * write a value straight to memory rather than via stack

-- * apply ops with one or both operands in memory

-- * get rid of the labels (they are not instructions)


type VM a = WriterT Code (State Label) a

newtype Compiled a = Compiled {unCompiled :: VM a}
  deriving newtype (MonadWriter Code, Monad, Applicative, Functor, MonadState Label)

comp :: Prog -> Code
comp = rewriteJumps . flip evalState 0 . execWriterT . unCompiled . comp'

comp' :: Prog -> Compiled ()
comp' (Assign name expr) = do
  tell (compExpr expr)
  tell [POP name]
comp' (If expr thenProg elseProg) = do
  label <- get
  let label' = succ label
  put $ succ label'
  tell $ compExpr expr
  tell [JUMPZ label]
  comp' thenProg
  tell [JUMP label', LABEL label]
  comp' elseProg
  tell [LABEL label']
comp' (While expr prog) = do
  label <- get
  let label' = succ label
  put $ succ label'
  tell [LABEL label]
  tell $ compExpr expr
  tell [JUMPZ label']
  comp' prog
  tell [JUMP label, LABEL label']
comp' (Seq progs) = mapM_ comp' progs

--comp' (Seq progs) = case progs of
--  [] -> return ()
--  (p : ps) -> comp' p >> comp' (Seq ps)

compExpr :: Expr -> Code
compExpr (Val i) = [PUSH i]
compExpr (Var name) = [PUSHV name]
compExpr (App op l r) = compExpr l ++ compExpr r ++ [DO op]

type LabelTable = [(Label, Int)]

labels :: Code -> LabelTable
labels code = [(l, i) | (LABEL l, i) <- zip code [0 ..]]

rewriteJumps :: Code -> Code
rewriteJumps code = fmap rewriteJumps' code
  where
    lt = labels code
    rewriteJumps' = \case
      JUMP label ->
        JUMP $
          fromMaybe
            (error $ "JUMP: label " ++ show label ++ " is not defined")
            (lookup label lt)
      JUMPZ label ->
        JUMPZ $
          fromMaybe
            (error $ "JUMPZ: label " ++ show label ++ " is not defined")
            (lookup label lt)
      inst -> inst

-- Machine

data Machine = Machine
  { pc :: Int,
    stack :: Stack,
    mem :: Mem
  }
  deriving stock (Show)

type StateMachine = State Machine ()

updateMem :: Mem -> (Name, Int) -> Mem
updateMem memory a@(name, _value) = a : filter ((name /=) . fst) memory

-- Execution

run :: Prog -> Mem
run = exec . comp

exec :: Code -> Mem
exec code = mem $ execState (eval code) (Machine 0 [] [])

eval :: Code -> StateMachine
eval code = do
  pcm <- gets pc
  if pcm == length code then return () else eval' code >> eval code

eval' :: Code -> StateMachine
eval' code = do
  pcm <- gets pc
  stackm <- gets stack
  memm <- gets mem
  let pcm' = succ pcm

  case code !! pcm of
    PUSH int -> put $ Machine pcm' (int : stackm) memm
    PUSHV name ->
      put $
        Machine
          pcm'
          ( fromMaybe
              (error $ "PUSHV: " ++ show name ++ " is not in memory")
              (lookup name memm) :
            stackm
          )
          memm
    POP name ->
      put $
        Machine pcm' (tail stackm) (updateMem memm (name, head stackm))
    DO op -> do
      let x = head stackm
          y = head $ tail stackm
          stack' = tail $ tail stackm
      case op of
        Add -> put $ Machine pcm' (y + x : stack') memm
        Sub -> put $ Machine pcm' (y - x : stack') memm
        Mul -> put $ Machine pcm' (y * x : stack') memm
        Div -> put $ Machine pcm' (y `div` x : stack') memm
    JUMP label -> put $ Machine (succ label) stackm memm
    JUMPZ label ->
      put $
        Machine
          (succ $ if head stackm == 0 then label else pcm)
          (tail stackm)
          memm
    LABEL _label -> put $ Machine pcm' stackm memm

-- Factorial example

fac :: Int -> Prog
fac n =
  Seq
    [ Assign 'A' (Val 1),
      Assign 'B' (Val n),
      While
        (Var 'B')
        ( Seq
            [ Assign 'A' (App Mul (Var 'A') (Var 'B')),
              Assign 'B' (App Sub (Var 'B') (Val (1)))
            ]
        )
    ]
