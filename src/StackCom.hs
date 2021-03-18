{-# LANGUAGE DerivingStrategies #-}

module StackCom
  ( projectName,
  )
where

import Control.Monad.State
import Data.List
import Data.Maybe

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
  deriving (Show)

type Name = Char

data Op = Add | Sub | Mul | Div
  deriving (Show)

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
  deriving (Show)

type Label = Int

-- Machine

data Machine = Machine
  { stack :: Stack,
    mem :: Mem
  }
  deriving stock (Show)

type StateMachine = State Machine

-- The initial machine
m0 :: Machine
m0 = Machine [] []

--  Compiler

comp :: Prog -> Code
comp prog = evalState (comp' prog) 0

comp' :: Prog -> State Label Code
comp' (Assign name expr) = return $ compExpr expr ++ [POP name]
comp' (If expr thenProg elseProg) = undefined
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

-- Execution
exec :: Code -> Mem
exec = undefined

run :: Prog -> Mem
run = exec . comp

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
