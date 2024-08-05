{-# LANGUAGE InstanceSigs #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Tacky where

import AST
import Control.Monad.State
import GHC.IO.Handle.Types (Handle__ (Handle__))

data TACProgram = TACProgram [TACFunction] deriving (Show, Eq)

data TACFunction = TACFunction String [TACInstruction] deriving (Show, Eq)

data TACInstruction = TACReturn TACVal | TACUnary TACUnaryOp TACVal TACVal deriving (Eq)

instance Show TACInstruction where
  show :: TACInstruction -> String
  show (TACReturn val) = "Return(" ++ show val ++ ")"
  show (TACUnary op src dst) = "Unary(" ++ show op ++ ", " ++ show src ++ ", " ++ show dst ++ ")"

data TACVal = TACConstant Int | TACVar String deriving (Eq)

instance Show TACVal where
  show :: TACVal -> String
  show (TACConstant i) = "Constant(" ++ show i ++ ")"
  show (TACVar name) = "Var(" ++ name ++ ")"

data TACUnaryOp = TACComplement | TACNegate deriving (Eq)

instance Show TACUnaryOp where
  show :: TACUnaryOp -> String
  show TACComplement = "Complement"
  show TACNegate = "Negate"

type NameGenerator = State Int

class Emittable a where
  emitTacky :: a -> [TACInstruction] -> NameGenerator [TACInstruction]

tackyOp :: UnaryOperator -> TACUnaryOp
tackyOp Complement = TACComplement
tackyOp Negate = TACNegate

nameIncrement :: String -> NameGenerator String
nameIncrement prefix = do
  num <- get
  put (num + 1)
  return $ prefix ++ "." ++ show num

instance Emittable Expression where
  emitTacky :: Expression -> [TACInstruction] -> NameGenerator [TACInstruction]
  emitTacky (Unary op (ConstantExpression (IntLiteral i))) instr = do
    let src_val = TACConstant i
    dst_name <- nameIncrement "tmp"
    return $ instr ++ [TACUnary (tackyOp op) src_val (TACVar dst_name)]
  emitTacky (Unary op e) instr = do
    src <- emitTacky e instr
    let src_val = case last src of
          TACReturn val -> val
          TACUnary _ _ val -> val
    dst_name <- nameIncrement "tmp"
    return $ src ++ [TACUnary (tackyOp op) src_val (TACVar dst_name)]
  emitTacky _ _ = error "Not emittable"

instance Emittable Statement where
  emitTacky :: Statement -> [TACInstruction] -> NameGenerator [TACInstruction]
  emitTacky (ReturnStatement (ConstantExpression (IntLiteral i))) _ = return [TACReturn (TACConstant i)]
  emitTacky (ReturnStatement e) instr = do
    newinstrs <- emitTacky e instr
    let src_val = case last newinstrs of
          TACReturn val -> val
          TACUnary _ _ val -> val
    return $ newinstrs ++ [TACReturn src_val]

emitTackyList :: (Emittable a) => [a] -> [TACInstruction] -> NameGenerator [TACInstruction]
emitTackyList xs instr = foldM (flip emitTacky) instr xs

toTACFunc :: Function -> TACFunction
toTACFunc (Function _ name _ body) = TACFunction name instrs
  where
    instrs = evalState (emitTackyList body []) 0

toTACProg :: Program -> TACProgram
toTACProg (Program functions) = TACProgram (map toTACFunc functions)
