{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecursiveDo                #-}


-- See http://wall.org/~lewis/2013/10/15/asm-monad.html

module StackCom
  ( projectName
  , comp
  , exec
  , run
  , fac
  ) where

import           Control.Monad.Fix    (MonadFix)
import           Control.Monad.State  (MonadState (get, put), State,
                                       StateT (StateT), evalState, execState,
                                       gets, modify)
import           Control.Monad.Writer (MonadWriter (tell), WriterT (WriterT),
                                       execWriterT)
import           Data.Maybe           (fromMaybe)

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
  -- --| LABEL Label -- Not needed; this version calculates them
  deriving stock (Show)

type Label = Int -- an index into the program counter

--  Compiler

-- possible optimizationsi to reduce number of instructions to run:

-- * write a value straight to memory rather than via stack

-- * apply ops with one or both operands in memory

type VM a = WriterT Code (State Label) a

newtype Compiled a = Compiled {unCompiled :: VM a}
  deriving newtype (MonadFix, MonadWriter Code, Monad, Applicative, Functor, MonadState Label)


-- | Increment the instruction counter (Label) and write the instruction
genInstr :: Inst -> Compiled ()
genInstr inst = do
  modify (+ 1)
  tell [inst]

-- | Instruction generation, for readabilty
pushInstr :: Int -> Compiled ()
pushInstr int = genInstr $ PUSH int

pushvInstr :: Name -> Compiled ()
pushvInstr int = genInstr $ PUSHV int

popInstr :: Name -> Compiled ()
popInstr name = genInstr $ POP name

doInstr :: Op -> Compiled ()
doInstr op = genInstr $ DO op

jumpInstr :: Label -> Compiled ()
jumpInstr label = genInstr $ JUMP label

jumpzInstr :: Label -> Compiled ()
jumpzInstr label = genInstr $ JUMPZ label

-- | generate a new label
label :: Compiled Label
label = do
  get

comp :: Prog -> Code
comp = flip evalState 0 . execWriterT . unCompiled . comp'

-- | The recursive mdo allows forward labels to be calculated
comp' :: Prog -> Compiled ()
comp' (Assign name expr) = do
  compExpr expr
  popInstr name
comp' (If expr thenProg elseProg) = mdo
  compExpr expr
  jumpzInstr elze
  comp' thenProg
  jumpInstr end
  elze <- label
  comp' elseProg
  end <- label
  return ()
comp' (While expr prog) = mdo
  begin <- label
  compExpr expr
  jumpzInstr end
  comp' prog
  jumpInstr begin
  end <- label
  return ()
comp' (Seq progs) = mapM_ comp' progs

--comp' (Seq progs) = case progs of
--  [] -> return ()
--  (p : ps) -> comp' p >> comp' (Seq ps)

compExpr :: Expr -> Compiled ()
compExpr (Val i     ) = pushInstr i
compExpr (Var name  ) = pushvInstr name
compExpr (App op l r) = do
  compExpr l
  compExpr r
  doInstr op

-- Machine

data Machine = Machine
  { pc    :: Int
  , stack :: Stack
  , mem   :: Mem
  }
  deriving stock Show

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
  pcm    <- gets pc
  stackm <- gets stack
  memm   <- gets mem
  let pcm' = succ pcm

  case code !! pcm of
    PUSH  int  -> put $ Machine pcm' (int : stackm) memm
    PUSHV name -> put $ Machine
      pcm'
      ( fromMaybe (error $ "PUSHV: " ++ show name ++ " is not in memory")
                  (lookup name memm)
      : stackm
      )
      memm
    POP name ->
      put $ Machine pcm' (tail stackm) (updateMem memm (name, head stackm))
    DO op -> do
      let x      = head stackm
          y      = head $ tail stackm
          stack' = tail $ tail stackm
      case op of
        Add -> put $ Machine pcm' (y + x : stack') memm
        Sub -> put $ Machine pcm' (y - x : stack') memm
        Mul -> put $ Machine pcm' (y * x : stack') memm
        Div -> put $ Machine pcm' (y `div` x : stack') memm
    JUMP  label -> put $ Machine label stackm memm
    JUMPZ label -> put
      $ Machine (if head stackm == 0 then label else pcm') stackm memm

-- Factorial example

fac :: Int -> Prog
fac n = Seq
  [ Assign 'A' (Val 1)
  , Assign 'B' (Val n)
  , While
    (Var 'B')
    (Seq
      [ Assign 'A' (App Mul (Var 'A') (Var 'B'))
      , Assign 'B' (App Sub (Var 'B') (Val (1)))
      ]
    )
  ]
