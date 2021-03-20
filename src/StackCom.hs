{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}

module StackCom
  ( projectName,
  comp,
  exec,
  run,
  fac
  )
where

import Control.Monad.State
    ( evalState, execState, MonadState(put, get), State )
import Data.Maybe ( fromMaybe )

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

comp :: Prog -> Code
comp prog = rewriteLabels $ evalState (comp' prog) 0

comp' :: Prog -> State Label Code
comp' (Assign name expr) = return $ compExpr expr ++ [POP name]
comp' (If expr thenProg elseProg) = do
  label <- get
  let label' = succ label
  put $ succ label'
  cthen <- comp' thenProg
  celse <- comp' elseProg
  return $
    compExpr expr
      ++ [JUMPZ label]
      ++ cthen
      ++ [JUMP label', LABEL label]
      ++ celse
      ++ [LABEL label']
comp' (While expr prog) = do
  label <- get
  let label' = succ label
  put $ succ label'
  cprog <- comp' prog
  return $
    [LABEL label]
      ++ compExpr expr
      ++ [JUMPZ label']
      ++ cprog
      ++ [JUMP label, LABEL label']
comp' (Seq progs) = do
  code <- mapM comp' progs
  return $ concat code

compExpr :: Expr -> Code
compExpr (Val i) = [PUSH i]
compExpr (Var name) = [PUSHV name]
compExpr (App op l r) = compExpr l ++ compExpr r ++ [DO op]

type LabelTable = [(Label, Int)]

labels :: Code -> LabelTable
labels code = [(l, i) | (LABEL l, i) <- zip code [0 ..]]

rewriteLabels :: Code -> Code
rewriteLabels code = fmap rewriteJumps code
  where
    lt = labels code
    rewriteJumps = \case
      JUMP label ->
        JUMP $
          fromMaybe
            (error $ "JUMP: label " ++ show label ++ " is not in defined")
            (lookup label lt)
      JUMPZ label ->
        JUMPZ $
          fromMaybe
            (error $ "JUMPZ: label " ++ show label ++ " is not in defined")
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
  m <- get
  if pc m == length code then return () else eval' code >> eval code

eval' :: Code -> StateMachine
eval' code = do
  m <- get
  let pcm = pc m
      stackm = stack m
      memm = mem m
      labelt = labels code

  case code !! pcm of
    (PUSH int) -> put $ Machine (succ pcm) (int : stackm) memm
    (PUSHV name) ->
      put $
        Machine
          (succ pcm)
          ( fromMaybe
              (error $ "PUSHV: " ++ show name ++ " is not in memory")
              (lookup name memm) :
            stackm
          )
          memm
    (POP name) ->
      put $ Machine (succ pcm) (tail stackm) (updateMem memm (name, head stackm))
    (DO op) -> do
      let x = head stackm
          y = head $ tail stackm
          stack' = tail $ tail stackm
      case op of
        Add -> put $ Machine (succ pcm) (y + x : stack') memm
        Sub -> put $ Machine (succ pcm) (y - x : stack') memm
        Mul -> put $ Machine (succ pcm) (y * x : stack') memm
        Div -> put $ Machine (succ pcm) (y `div` x : stack') memm
    (JUMP label) ->
      put $ Machine (succ label) stackm memm
    (JUMPZ label) ->
      put $ Machine (succ $ if head stackm == 0 then label else pcm) (tail stackm) memm
    (LABEL _label) -> put $ Machine (succ pcm) stackm memm

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
