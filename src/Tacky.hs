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

data TACInstruction
  = TACReturn TACVal
  | TACUnary TACUnaryOp TACVal TACVal
  | TACBinary TACBinaryOp TACVal TACVal TACVal
  deriving (Eq)

instance Show TACInstruction where
  show :: TACInstruction -> String
  show (TACReturn val) = "Return(" ++ show val ++ ")"
  show (TACUnary op src dst) = "Unary(" ++ show op ++ ", " ++ show src ++ ", " ++ show dst ++ ")"
  show (TACBinary op src1 src2 dst) = "Binary(" ++ show op ++ ", " ++ show src1 ++ ", " ++ show src2 ++ ", " ++ show dst ++ ")"

data TACVal
  = TACConstant Int
  | TACVar String
  deriving (Eq)

instance Show TACVal where
  show :: TACVal -> String
  show (TACConstant i) = "Constant(" ++ show i ++ ")"
  show (TACVar name) = "Var(" ++ name ++ ")"

data TACUnaryOp
  = TACComplement
  | TACNegate
  deriving (Eq)

instance Show TACUnaryOp where
  show :: TACUnaryOp -> String
  show TACComplement = "Complement"
  show TACNegate = "Negate"

data TACBinaryOp
  = TACAdd
  | TACSubtract
  | TACMultiply
  | TACDivide
  | TACRemainder
  | TACBitwiseAnd
  | TACBitwiseOr
  | TACBitwiseXor
  | TACLeftShift
  | TACRightShift
  deriving (Eq)

instance Show TACBinaryOp where
  show :: TACBinaryOp -> String
  show TACAdd = "Add"
  show TACSubtract = "Subtract"
  show TACMultiply = "Multiply"
  show TACDivide = "Divide"
  show TACRemainder = "Remainder"
  show TACBitwiseAnd = "BitwiseAnd"
  show TACBitwiseOr = "BitwiseOr"
  show TACBitwiseXor = "BitwiseXor"
  show TACLeftShift = "LeftShift"
  show TACRightShift = "RightShift"

tackyOp :: UnaryOperator -> TACUnaryOp
tackyOp Complement = TACComplement
tackyOp Negate = TACNegate

tackyBinOp :: BinaryOperator -> TACBinaryOp
tackyBinOp Add = TACAdd
tackyBinOp Subtract = TACSubtract
tackyBinOp Multiply = TACMultiply
tackyBinOp Divide = TACDivide
tackyBinOp Remainder = TACRemainder
tackyBinOp BitwiseAnd = TACBitwiseAnd
tackyBinOp BitwiseOr = TACBitwiseOr
tackyBinOp BitwiseXor = TACBitwiseXor
tackyBinOp LeftShift = TACLeftShift
tackyBinOp RightShift = TACRightShift

type InstrSt = State (Int, [TACInstruction])

class Emittable a where
  emitTacky :: a -> InstrSt TACVal

nameIncrement :: String -> InstrSt String
nameIncrement prefix = do
  (num, instr) <- get
  put (num + 1, instr)
  return $ prefix ++ "." ++ show num

instance Emittable Expression where
  emitTacky :: Expression -> InstrSt TACVal
  emitTacky (ConstantExpression (IntLiteral i)) = return $ TACConstant i
  emitTacky (Unary op e) = do
    src <- emitTacky e
    dst_name <- nameIncrement "tmp"
    (num, instr) <- get
    put (num, instr ++ [TACUnary (tackyOp op) src (TACVar dst_name)])
    return $ TACVar dst_name
  emitTacky (Binary op left right) = do
    left_val <- emitTacky left
    right_val <- emitTacky right
    dst_name <- nameIncrement "tmp"
    (num, instr) <- get
    put (num, instr ++ [TACBinary (tackyBinOp op) left_val right_val (TACVar dst_name)])
    return $ TACVar dst_name

instance Emittable Statement where
  emitTacky :: Statement -> InstrSt TACVal
  emitTacky (ReturnStatement e) = do
    src <- emitTacky e
    (num, instr) <- get
    put (num, instr ++ [TACReturn src])
    return src

emitTackyList :: (Emittable a) => [a] -> [TACInstruction]
emitTackyList [] = []
emitTackyList instrs = snd $ execState (foldM (\_ instr -> emitTacky instr) (TACConstant 0) instrs) (0, [])

toTACFunc :: Function -> TACFunction
toTACFunc (Function name body) = TACFunction name instrs
  where
    instrs = emitTackyList body

toTACProg :: Program -> TACProgram
toTACProg (Program functions) = TACProgram (map toTACFunc functions)
