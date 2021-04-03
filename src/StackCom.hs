{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TupleSections #-}

module StackCom
  ( projectName,
    comp,
    exec,
    run,
    fac,
  )
where

import Control.Monad.Fix
import Control.Monad.State
  ( MonadState (get, put),
    State,
    StateT (StateT),
    evalState,
    execState,
    gets,
    modify,
  )
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

--  -- Assembler

newtype ASM a = ASM {unASM :: Label -> (Code, Label, a)} --  deriving (Functor)

asm :: Label -> ASM a -> Code
asm start (ASM f) = code where (code, _, _) = f start

instance Functor ASM where
  fmap f (ASM g) = ASM (\label -> let (c, l, a) = g label in (c, l, f a))

instance Applicative ASM where
  pure ret = ASM ([],,ret) -- \label -> ([], label, ret)
  pf <*> px = ASM $ \start ->
    let (code, next, f) = unASM pf start
        (code', end, x) = unASM px next
     in (code ++ code', end, f x)

instance Monad ASM where
  f >>= g = ASM $ \start ->
    let (code, next, val) = unASM f start
        (code', end, ret) = unASM (g val) next
     in (code ++ code', end, ret)

instance MonadFix ASM where
  mfix f = ASM $ \start ->
    let (code, end, ret) = unASM (f ret) start
     in (code, end, ret)

-- | assemble a single instruction
assemble :: Inst -> ASM ()
assemble inst = ASM $ \label -> ([inst], label + 1, ())

here :: ASM Label
here = ASM $ \label -> ([], label, label)
{-
compiler :: Prog -> ASM ()
compiler (Assign name expr) = do
  mapM_ assemble $ compExpr expr
  assemble $ POP name
compiler (If expr thenProg elseProg) = mdo
  mapM_ assemble $  compExpr expr
  assemble $ JUMPZ elze
  compiler thenProg
  assemble $ JUMPZ exit
  elze <- here
  compiler elseProg
  exit <- here
  return ()
compiler (While expr prog) = mdo
  begin <- here
  mapM_ assemble $ compExpr expr
  assemble $ JUMPZ exit
  compiler prog
  assemble $ JUMP begin
  exit <- here
  return ()
compiler (Seq progs) = mapM_ compiler progs

test = (unASM $ compiler (fac 10) ) 0
-}

--
--  Compiler

-- possible optimizationsi to reduce number of instructions to run:

-- * write a value straight to memory rather than via stack

-- * apply ops with one or both operands in memory

-- * get rid of the labels (they are not instructions)

type VM a = WriterT Code (State Label) a

newtype Compiled a = Compiled {unCompiled :: VM a}
  deriving newtype (MonadFix, MonadWriter Code, Monad, Applicative, Functor, MonadState Label)


emitInstr :: Inst -> Compiled ()
emitInstr inst = do
  modify (+1)
  tell [inst]

-- | Instruction generation
pushInstr :: Int -> Compiled ()
pushInstr int = emitInstr $ PUSH int

pushvInstr :: Name -> Compiled ()
pushvInstr int = emitInstr $ PUSHV int

popInstr :: Name -> Compiled ()
popInstr name = emitInstr $ POP name

doInstr :: Op -> Compiled ()
doInstr op = emitInstr $ DO op

jumpInstr :: Label -> Compiled ()
jumpInstr label = emitInstr $ JUMP label

jumpzInstr :: Label -> Compiled ()
jumpzInstr label = emitInstr $ JUMPZ label

-- | generate a new label
label :: Compiled Label
label = do get

comp :: Prog -> Code
comp = flip evalState 0 . execWriterT . unCompiled . comp'

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
compExpr (Val i) = pushInstr i
compExpr (Var name) = pushvInstr name
compExpr (App op l r) = do
   compExpr l
   compExpr r
   doInstr op

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
    JUMP label -> put $ Machine label stackm memm
    JUMPZ label ->
      put $
        Machine
          (if head stackm == 0 then label else pcm')
          (tail stackm)
          memm
    LABEL _label -> error "LABEL found, but should not appear in generated code"

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
