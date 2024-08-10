{-# LANGUAGE InstanceSigs #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Tacky where

import AST
import Control.Monad.State
import qualified Data.Map as M

type TACIdentifier = String

data TACProgram = TACProgram [TACFunction] deriving (Show, Eq)

data TACFunction = TACFunction TACIdentifier [TACInstruction] deriving (Show, Eq)

data TACInstruction
  = TACReturn TACVal
  | TACUnary TACUnaryOp TACVal TACVal
  | TACBinary TACBinaryOp TACVal TACVal TACVal
  | TACCopy TACVal TACVal
  | TACJump TACIdentifier
  | TACJumpIfZero TACVal TACIdentifier
  | TACJumpIfNotZero TACVal TACIdentifier
  | TACLabel TACIdentifier
  deriving (Eq)

instance Show TACInstruction where
  show :: TACInstruction -> String
  show (TACReturn val) = "Return(" ++ show val ++ ")"
  show (TACUnary op src dst) = "Unary(" ++ show op ++ ", " ++ show src ++ ", " ++ show dst ++ ")"
  show (TACBinary op src1 src2 dst) = "Binary(" ++ show op ++ ", " ++ show src1 ++ ", " ++ show src2 ++ ", " ++ show dst ++ ")"
  show (TACCopy src dst) = "Copy(" ++ show src ++ ", " ++ show dst ++ ")"
  show (TACJump label) = "Jump(" ++ label ++ ")"
  show (TACJumpIfZero val label) = "JumpIfZero(" ++ show val ++ ", " ++ label ++ ")"
  show (TACJumpIfNotZero val label) = "JumpIfNotZero(" ++ show val ++ ", " ++ label ++ ")"
  show (TACLabel label) = "Label(" ++ label ++ ")"

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
  | TACNot
  deriving (Eq)

instance Show TACUnaryOp where
  show :: TACUnaryOp -> String
  show TACComplement = "Complement"
  show TACNegate = "Negate"
  show TACNot = "Not"

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
  | TACEqual
  | TACNotEqual
  | TACLessThan
  | TACLessThanOrEqual
  | TACGreaterThan
  | TACGreaterThanOrEqual
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
  show TACEqual = "Equal"
  show TACNotEqual = "NotEqual"
  show TACLessThan = "LessThan"
  show TACLessThanOrEqual = "LessThanOrEqual"
  show TACGreaterThan = "GreaterThan"
  show TACGreaterThanOrEqual = "GreaterThanOrEqual"

tackyOp :: UnaryOp -> TACUnaryOp
tackyOp Complement = TACComplement
tackyOp Negate = TACNegate
tackyOp Not = TACNot
tackyOp _ = error "TackyOp: This shouldnt happen"

tackyPostOp :: PostOp -> TACBinaryOp
tackyPostOp PostIncrement = TACAdd
tackyPostOp PostDecrement = TACSubtract

tackyPreOp :: UnaryOp -> TACBinaryOp
tackyPreOp PreIncrement = TACAdd
tackyPreOp PreDecrement = TACSubtract
tackyPreOp _ = error "TackyPreOp: This shouldnt happen"

tackyBinOp :: BinaryOp -> TACBinaryOp
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
tackyBinOp EqualTo = TACEqual
tackyBinOp NotEqualTo = TACNotEqual
tackyBinOp LessThan = TACLessThan
tackyBinOp LessThanOrEqualTo = TACLessThanOrEqual
tackyBinOp GreaterThan = TACGreaterThan
tackyBinOp GreaterThanOrEqualTo = TACGreaterThanOrEqual
tackyBinOp _ = error "Tackybinop shouldnt happen"

tackyAssignOp :: AssignmentOp -> TACBinaryOp
tackyAssignOp PlusAssign = TACAdd
tackyAssignOp MinusAssign = TACSubtract
tackyAssignOp MultiplyAssign = TACMultiply
tackyAssignOp DivideAssign = TACDivide
tackyAssignOp RemainderAssign = TACRemainder
tackyAssignOp BitwiseAndAssign = TACBitwiseAnd
tackyAssignOp BitwiseOrAssign = TACBitwiseOr
tackyAssignOp BitwiseXorAssign = TACBitwiseXor
tackyAssignOp LeftShiftAssign = TACLeftShift
tackyAssignOp RightShiftAssign = TACRightShift
tackyAssignOp _ = error "TackyAssignOp: This shouldnt happen"

type GlobalNameMap = M.Map String Int

type InstrSt = State (GlobalNameMap, [TACInstruction])

class Emittable a where
  emitTacky :: a -> InstrSt TACVal

globalNameGenerator :: String -> InstrSt String
globalNameGenerator prefix = do
  (globMap, instr) <- get
  let num = M.findWithDefault 0 prefix globMap
  let newNum = num + 1
  put (M.insert prefix newNum globMap, instr)
  return $ prefix ++ show newNum

instance Emittable Expr where
  emitTacky :: Expr -> InstrSt TACVal
  emitTacky (ConstantExpr (IntLiteral i)) = return $ TACConstant i
  emitTacky (Var (Identifier name)) = return $ TACVar name
  emitTacky (Binary And left right) = do
    false_label <- globalNameGenerator "false"
    end_label <- globalNameGenerator "end"
    result <- globalNameGenerator "result."
    v1 <- emitTacky left
    (num, instr) <- get
    put (num, instr ++ [TACJumpIfZero v1 false_label])
    v2 <- emitTacky right
    (num', instr') <- get
    put
      ( num',
        instr'
          ++ [ TACJumpIfZero v2 false_label,
               TACCopy (TACConstant 1) (TACVar result),
               TACJump end_label,
               TACLabel false_label,
               TACCopy (TACConstant 0) (TACVar result),
               TACLabel end_label
             ]
      )
    return $ TACVar result
  emitTacky (Binary Or left right) = do
    true_label <- globalNameGenerator "true"
    end_label <- globalNameGenerator "end"
    result <- globalNameGenerator "result."
    v1 <- emitTacky left
    (num, instr) <- get
    put (num, instr ++ [TACJumpIfNotZero v1 true_label])
    v2 <- emitTacky right
    (num', instr') <- get
    put
      ( num',
        instr'
          ++ [ TACJumpIfNotZero v2 true_label,
               TACCopy (TACConstant 0) (TACVar result),
               TACJump end_label,
               TACLabel true_label,
               TACCopy (TACConstant 1) (TACVar result),
               TACLabel end_label
             ]
      )
    return $ TACVar result
  emitTacky (Unary op e)
    | op == PreDecrement || op == PreIncrement = do
        src <- emitTacky e
        (num, instr) <- get
        put (num, instr ++ [TACBinary (tackyPreOp op) src (TACConstant 1) src])
        return src
    | otherwise = do
        src <- emitTacky e
        dst_name <- globalNameGenerator "tmp."
        (num, instr) <- get
        put (num, instr ++ [TACUnary (tackyOp op) src (TACVar dst_name)])
        return $ TACVar dst_name
  emitTacky (Binary op left right) = do
    left_val <- emitTacky left
    right_val <- emitTacky right
    dst_name <- globalNameGenerator "tmp."
    (num, instr) <- get
    put (num, instr ++ [TACBinary (tackyBinOp op) left_val right_val (TACVar dst_name)])
    return $ TACVar dst_name
  emitTacky (PostFix e op) = do
    src <- emitTacky e
    dst_name <- globalNameGenerator "tmp."
    (num, instr) <- get
    put (num, instr ++ [TACCopy src (TACVar dst_name), TACBinary (tackyPostOp op) src (TACConstant 1) src])
    return $ TACVar dst_name
  emitTacky (Assignment Assign l r) = do
    lhs <- emitTacky l
    rhs <- emitTacky r
    (num, instr) <- get
    put (num, instr ++ [TACCopy rhs lhs])
    return lhs
  emitTacky (Assignment op l r) = do
    lhs <- emitTacky l
    rhs <- emitTacky r
    dst_name <- globalNameGenerator "tmp."
    (num, instr) <- get
    put (num, instr ++ [TACBinary (tackyAssignOp op) lhs rhs (TACVar dst_name), TACCopy (TACVar dst_name) lhs])
    return lhs

-- emitTacky (CompoundAssignment op v e) = do
--   lhs <- emitTacky v
--   rhs <- emitTacky e
--   dst_name <- globalNameGenerator "tmp."
--   (num, instr) <- get
--   put (num, instr ++ [TACBinary (tackyBinOp op) lhs rhs (TACVar dst_name)])
--   return $ TACVar dst_name

instance Emittable Declaration where
  emitTacky :: Declaration -> InstrSt TACVal
  emitTacky (Declaration (Identifier name) (Just e)) = do
    val <- emitTacky e
    (num, instr) <- get
    put (num, instr ++ [TACCopy val (TACVar name)])
    return val
  emitTacky (Declaration (Identifier name) Nothing) = return $ TACVar name

instance Emittable Stmt where
  emitTacky :: Stmt -> InstrSt TACVal
  emitTacky (ReturnStmt e) = do
    src <- emitTacky e
    (num, instr) <- get
    put (num, instr ++ [TACReturn src])
    return src
  emitTacky (ExprStmt e) = emitTacky e
  emitTacky NullStmt = return $ TACConstant 0

instance Emittable BlockItem where
  emitTacky :: BlockItem -> InstrSt TACVal
  emitTacky (BlockStmt stmt) = emitTacky stmt
  emitTacky (BlockDecl decl) = emitTacky decl

emitTackyList :: (Emittable a) => [a] -> [TACInstruction]
emitTackyList [] = []
emitTackyList instrs = snd $ execState (foldM (\_ instr -> emitTacky instr) (TACConstant 0) instrs) (M.empty, [])

toTACFunc :: Function -> TACFunction
toTACFunc (Function (Identifier name) body) = TACFunction name (instrs ++ [TACReturn (TACConstant 0)])
  where
    instrs = emitTackyList body

toTACProg :: Program -> TACProgram
toTACProg (Program functions) = TACProgram (fmap toTACFunc functions)

isRelationalOp :: TACBinaryOp -> Bool
isRelationalOp TACEqual = True
isRelationalOp TACNotEqual = True
isRelationalOp TACLessThan = True
isRelationalOp TACLessThanOrEqual = True
isRelationalOp TACGreaterThan = True
isRelationalOp TACGreaterThanOrEqual = True
isRelationalOp _ = False