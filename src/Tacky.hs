{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Tacky where

import AST
import Control.Applicative (liftA2)
import Control.Monad.State
import Data.Foldable
import qualified Data.Map as M
import Data.Maybe (fromJust, mapMaybe)
import Util

type TACIdentifier = String

newtype TACProgram = TACProgram [TACTopLevel] deriving (Show, Eq)

data TACTopLevel
  = TACFunction TACIdentifier Bool [TACIdentifier] [TACInstruction]
  | TACStaticVariable TACIdentifier Bool CType StaticInit
  | TACStaticConstant TACIdentifier CType StaticInit
  deriving (Show, Eq)

breakLabel :: Maybe Identifier -> String
breakLabel (Just name) = name ++ ".break"
breakLabel Nothing = error "BreakLabel: This shouldnt happen"

continueLabel :: Maybe Identifier -> String
continueLabel (Just name) = name ++ ".continue"
continueLabel Nothing = error "ContinueLabel: This shouldnt happen"

startLabel :: Maybe Identifier -> String
startLabel (Just name) = name ++ ".start"
startLabel Nothing = error "StartLabel: This shouldnt happen"

data TACInstruction
  = TACReturn TACVal
  | TACSignExtend TACVal TACVal
  | TACTruncate TACVal TACVal
  | TACZeroExtend TACVal TACVal
  | TACDoubleToInt TACVal TACVal
  | TACDoubleToUInt TACVal TACVal
  | TACIntToDouble TACVal TACVal
  | TACUIntToDouble TACVal TACVal
  | TACUnary TACUnaryOp TACVal TACVal
  | TACBinary TACBinaryOp TACVal TACVal TACVal
  | TACCopy TACVal TACVal
  | TACJump TACIdentifier
  | TACJumpIfZero TACVal TACIdentifier
  | TACJumpIfNotZero TACVal TACIdentifier
  | TACLabel TACIdentifier
  | TACFuncCall TACIdentifier [TACVal] TACVal
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
  show (TACFuncCall name args dst) = "FuncCall(" ++ name ++ ", " ++ show args ++ ", " ++ show dst ++ ")"
  show (TACSignExtend src dst) = "SignExtend(" ++ show src ++ ", " ++ show dst ++ ")"
  show (TACTruncate src dst) = "Truncate(" ++ show src ++ ", " ++ show dst ++ ")"
  show (TACZeroExtend src dst) = "ZeroExtend(" ++ show src ++ ", " ++ show dst ++ ")"
  show (TACDoubleToInt src dst) = "DoubleToInt(" ++ show src ++ ", " ++ show dst ++ ")"
  show (TACDoubleToUInt src dst) = "DoubleToUInt(" ++ show src ++ ", " ++ show dst ++ ")"
  show (TACIntToDouble src dst) = "IntToDouble(" ++ show src ++ ", " ++ show dst ++ ")"
  show (TACUIntToDouble src dst) = "UIntToDouble(" ++ show src ++ ", " ++ show dst ++ ")"

data TACVal
  = TACConstant Const
  | TACVar TACIdentifier
  | TACData TACIdentifier Const
  deriving (Eq)

instance Show TACVal where
  show :: TACVal -> String
  show (TACConstant i) = "Constant(" ++ show i ++ ")"
  show (TACVar name) = "Var(" ++ name ++ ")"
  show (TACData name i) = "Data(" ++ name ++ ", " ++ show i ++ ")"

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

type StaticConstMap = M.Map Const TACIdentifier

type InstrSt = State (GlobalNameMap, [TACInstruction], SymbolTable, StaticConstMap)

class Emittable a where
  emitTacky :: a -> InstrSt TACVal

globalNameGenerator :: String -> InstrSt String
globalNameGenerator prefix = do
  (globMap, instr, st, dm) <- get
  let num = M.findWithDefault 0 prefix globMap
      newNum = num + 1
  put (M.insert prefix newNum globMap, instr, st, dm)
  return $ prefix ++ show newNum

makeTemporaryVariable :: String -> Maybe CType -> InstrSt String
makeTemporaryVariable prefix (Just ty) = do
  name <- globalNameGenerator prefix
  (num, instr, st, dm) <- get
  let st' = M.insert name (ty, LocalAttr) st
  put (num, instr, st', dm)
  return name
makeTemporaryVariable _ Nothing = error "Untyped temporary variable, this shouldnt happen"

makeStaticConstant :: String -> Const -> InstrSt TACVal
makeStaticConstant prefix c = do
  (_, _, _, dm) <- get
  case M.lookup c dm of
    Just name -> return (TACData name c)
    Nothing -> do
      name <- globalNameGenerator prefix
      (num', instr', st', dm') <- get
      let st'' = M.insert name (constType c, StaticConst c) st'
          dm'' = M.insert c name dm'
      put (num', instr', st'', dm'')
      return (TACData name c)

instance Emittable TypedExpr where
  emitTacky :: TypedExpr -> InstrSt TACVal
  emitTacky (TypedExpr (Constant c@(DoubleConst _ _)) _) = makeStaticConstant "double_const." c
  emitTacky (TypedExpr (Constant i) _) = return $ TACConstant i
  emitTacky (TypedExpr (Var name) _) = return $ TACVar name
  emitTacky (TypedExpr (Binary And left right) ty) = do
    false_label <- globalNameGenerator "false."
    end_label <- globalNameGenerator "end."
    result <- makeTemporaryVariable "result." ty
    v1 <- emitTacky left
    (num, instr, st, dm) <- get
    put (num, instr ++ [TACJumpIfZero v1 false_label], st, dm)
    v2 <- emitTacky right
    (num', instr', st', dm') <- get
    put
      ( num',
        instr'
          ++ [ TACJumpIfZero v2 false_label,
               TACCopy (TACConstant (IntConst CInt 1)) (TACVar result),
               TACJump end_label,
               TACLabel false_label,
               TACCopy (TACConstant (IntConst CInt 0)) (TACVar result),
               TACLabel end_label
             ],
        st',
        dm'
      )
    return $ TACVar result
  emitTacky (TypedExpr (Binary Or left right) ty) = do
    true_label <- globalNameGenerator "true."
    end_label <- globalNameGenerator "end."
    result <- makeTemporaryVariable "result." ty
    v1 <- emitTacky left
    (num, instr, st, dm) <- get
    put (num, instr ++ [TACJumpIfNotZero v1 true_label], st, dm)
    v2 <- emitTacky right
    (num', instr', st', dm') <- get
    put
      ( num',
        instr'
          ++ [ TACJumpIfNotZero v2 true_label,
               TACCopy (TACConstant (IntConst CInt 0)) (TACVar result),
               TACJump end_label,
               TACLabel true_label,
               TACCopy (TACConstant (IntConst CInt 1)) (TACVar result),
               TACLabel end_label
             ],
        st',
        dm'
      )
    return $ TACVar result
  emitTacky (TypedExpr (Unary op e) ty)
    | op == PreDecrement || op == PreIncrement = do
        src <- emitTacky e
        constVal <- case ty of
          Just CDouble -> makeStaticConstant "inc1." (DoubleConst CDouble 1.0)
          Just ty' -> return $ TACConstant $ IntConst ty' 1
          Nothing -> error "Untyped unary: This shouldn't happen"
        (num, instr, st, dm) <- get
        put (num, instr ++ [TACBinary (tackyPreOp op) src constVal src], st, dm)
        return src
    | otherwise = do
        src <- emitTacky e
        dst_name <- makeTemporaryVariable "tmp." ty
        (num, instr, st, dm) <- get
        put (num, instr ++ [TACUnary (tackyOp op) src (TACVar dst_name)], st, dm)
        return $ TACVar dst_name
  emitTacky (TypedExpr (Binary op left right) ty) = do
    left_val <- emitTacky left
    right_val <- emitTacky right
    dst_name <- makeTemporaryVariable "tmp." ty
    (num, instr, st, dm) <- get
    put (num, instr ++ [TACBinary (tackyBinOp op) left_val right_val (TACVar dst_name)], st, dm)
    return $ TACVar dst_name
  emitTacky (TypedExpr (PostFix e op) ty) = do
    src <- emitTacky e
    dst_name <- makeTemporaryVariable "tmp." ty
    (num, instr, st, dm) <- get
    constVal <- case ty of
      Just CDouble -> makeStaticConstant "inc1." (DoubleConst CDouble 1.0)
      Just ty' -> return $ TACConstant $ IntConst ty' 1
      Nothing -> error "Untyped unary: This shouldn't happen"
    put (num, instr ++ [TACCopy src (TACVar dst_name), TACBinary (tackyPostOp op) src constVal src], st, dm)
    return $ TACVar dst_name
  emitTacky (TypedExpr (Assignment Assign l r) _) = do
    lhs <- emitTacky l
    rhs <- emitTacky r
    (num, instr, st, dm) <- get
    put (num, instr ++ [TACCopy rhs lhs], st, dm)
    return lhs
  emitTacky e@(TypedExpr (Assignment op l r) ty)
    | isCompoundArithmeticOp op
        || isCompoundBitwiseOp op =
        emitTackyCompoundWidening e
    | otherwise = do
        lhs <- emitTacky l
        rhs <- emitTacky r
        dst_name <- makeTemporaryVariable "tmp." ty
        (num, instr, st, dm) <- get
        put (num, instr ++ [TACBinary (tackyAssignOp op) lhs rhs (TACVar dst_name), TACCopy (TACVar dst_name) lhs], st, dm)
        return lhs
  emitTacky (TypedExpr (Conditional cond thenExpr elseExpr) ty) = do
    cond_val <- emitTacky cond
    else_label <- globalNameGenerator "else."
    end_label <- globalNameGenerator "end."
    dst_name <- makeTemporaryVariable "tmp." ty
    (num, instr, st, dm) <- get
    put (num, instr ++ [TACJumpIfZero cond_val else_label], st, dm)
    then_val <- emitTacky thenExpr
    (num', instr', st', dm') <- get
    put (num', instr' ++ [TACCopy then_val (TACVar dst_name), TACJump end_label, TACLabel else_label], st', dm')
    else_val <- emitTacky elseExpr
    (num'', instr'', st'', dm'') <- get
    put (num'', instr'' ++ [TACCopy else_val (TACVar dst_name), TACLabel end_label], st'', dm'')
    return $ TACVar dst_name
  emitTacky (TypedExpr (FunctionCall name args) ty) = do
    arg_vals <- traverse emitTacky args
    dst_name <- makeTemporaryVariable "tmp." ty
    (num, instr, st, dm) <- get
    put (num, instr ++ [TACFuncCall name arg_vals (TACVar dst_name)], st, dm)
    return $ TACVar dst_name
  emitTacky c@(TypedExpr (Cast _ _) _) = do
    (_, dst) <- emitTackyCastWithSrc c
    return dst

instance Emittable VarDecl where
  emitTacky :: VarDecl -> InstrSt TACVal
  emitTacky (VarDecl name _ _ (Just Static)) = return $ TACVar name
  emitTacky (VarDecl name (Just e) _ _) = do
    val <- emitTacky e
    (num, instr, st, dm) <- get
    put (num, instr ++ [TACCopy val (TACVar name)], st, dm)
    return val
  emitTacky (VarDecl name Nothing _ _) = return $ TACVar name

instance Emittable Stmt where
  emitTacky :: Stmt -> InstrSt TACVal
  emitTacky (ForStmt Nothing _ _ _ _) = error "ForStmt without label, this shouldnt happen"
  emitTacky (WhileStmt Nothing _ _) = error "WhileStmt without label, this shouldnt happen"
  emitTacky (DoWhileStmt Nothing _ _) = error "DoWhileStmt without label, this shouldnt happen"
  emitTacky (ContinueStmt Nothing) = error "ContinueStmt without label, this shouldnt happen"
  emitTacky (BreakStmt Nothing) = error "BreakStmt without label, this shouldnt happen"
  emitTacky (CaseStmt Nothing _ _) = error "CaseStmt without label, this shouldnt happen"
  emitTacky (DefaultStmt Nothing) = error "DefaultStmt without label, this shouldnt happen"
  emitTacky (SwitchStmt Nothing _ _ _ _) = error "Switch without label, this shouldnt happen"
  emitTacky (ReturnStmt e) = do
    src <- emitTacky e
    (num, instr, st, dm) <- get
    put (num, instr ++ [TACReturn src], st, dm)
    return src
  emitTacky (ExprStmt e) = emitTacky e
  emitTacky NullStmt = return $ TACConstant (IntConst CInt 0)
  emitTacky (IfStmt cond thenStmt (Just elseStmt)) = do
    cond_val <- emitTacky cond
    else_label <- globalNameGenerator "else."
    end_label <- globalNameGenerator "end."
    (num, instr, st, dm) <- get
    put (num, instr ++ [TACJumpIfZero cond_val else_label], st, dm)
    _ <- emitTacky thenStmt
    (num', instr', st', dm') <- get
    put (num', instr' ++ [TACJump end_label, TACLabel else_label], st', dm')
    _ <- emitTacky elseStmt
    (num'', instr'', st'', dm'') <- get
    put (num'', instr'' ++ [TACLabel end_label], st'', dm'')
    return $ TACConstant (IntConst CInt 0)
  emitTacky (IfStmt cond thenStmt Nothing) = do
    cond_val <- emitTacky cond
    end_label <- globalNameGenerator "end."
    (num, instr, st, dm) <- get
    put (num, instr ++ [TACJumpIfZero cond_val end_label], st, dm)
    _ <- emitTacky thenStmt
    (num', instr', st', dm') <- get
    put (num', instr' ++ [TACLabel end_label], st', dm')
    return $ TACConstant (IntConst CInt 0)
  emitTacky (LabeledStmt name stmt) = do
    (num, instr, st, dm) <- get
    put (num, instr ++ [TACLabel $ name ++ ".label"], st, dm)
    _ <- emitTacky stmt
    return $ TACConstant (IntConst CInt 0)
  emitTacky (GotoStmt name) = do
    (num, instr, st, dm) <- get
    put (num, instr ++ [TACJump $ name ++ ".label"], st, dm)
    return $ TACConstant (IntConst CInt 0)
  emitTacky (CompoundStmt block) = emitTacky block
  emitTacky (WhileStmt label cond block) = do
    let continue_label = continueLabel label
    let break_label = breakLabel label
    (num, instr, st, dm) <- get
    put (num, instr ++ [TACLabel continue_label], st, dm)
    cond_val <- emitTacky cond
    (num', instr', st', dm') <- get
    put (num', instr' ++ [TACJumpIfZero cond_val break_label], st', dm')
    _ <- emitTacky block
    (num'', instr'', st'', dm'') <- get
    put (num'', instr'' ++ [TACJump continue_label, TACLabel break_label], st'', dm'')
    return $ TACConstant (IntConst CInt 0)
  emitTacky (DoWhileStmt label block cond) = do
    let start_label = startLabel label
    let break_label = breakLabel label
    let continue_label = continueLabel label
    (num, instr, st, dm) <- get
    put (num, instr ++ [TACLabel start_label], st, dm)
    _ <- emitTacky block
    (num', instr', st', dm') <- get
    put (num', instr' ++ [TACLabel continue_label], st', dm')
    cond_val <- emitTacky cond
    (num'', instr'', st'', dm'') <- get
    put (num'', instr'' ++ [TACJumpIfNotZero cond_val start_label, TACLabel break_label], st'', dm'')
    return $ TACConstant (IntConst CInt 0)
  emitTacky (ForStmt label forinit cond update block) = do
    let start_label = startLabel label
    let continue_label = continueLabel label
    let break_label = breakLabel label
    _ <- emitTacky forinit
    (num, instr, st, dm) <- get
    put (num, instr ++ [TACLabel start_label], st, dm)
    case cond of
      Just c -> do
        cond_val <- emitTacky c
        (num', instr', st', dm') <- get
        put (num', instr' ++ [TACJumpIfZero cond_val break_label], st', dm')
      Nothing -> return ()
    _ <- emitTacky block
    (num', instr', st', dm') <- get
    put (num', instr' ++ [TACLabel continue_label], st', dm')
    case update of
      Just u -> do
        _ <- emitTacky u
        return ()
      Nothing -> return ()
    (num'', instr'', st'', dm'') <- get
    put (num'', instr'' ++ [TACJump start_label, TACLabel break_label], st'', dm'')
    return $ TACConstant (IntConst CInt 0)
  emitTacky (BreakStmt label) = do
    (num, instr, st, dm) <- get
    put (num, instr ++ [TACJump $ breakLabel label], st, dm)
    return $ TACConstant (IntConst CInt 0)
  emitTacky (ContinueStmt label) = do
    (num, instr, st, dm) <- get
    put (num, instr ++ [TACJump $ continueLabel label], st, dm)
    return $ TACConstant (IntConst CInt 0)
  emitTacky (SwitchStmt label'@(Just label) caseSet hasDefault expr block) = do
    v <- emitTacky expr
    if not hasDefault && null caseSet
      then do
        return $ TACConstant (IntConst CInt 0)
      else do
        let break_label = breakLabel label'
        traverse_ (caseToIf label v) caseSet
        when hasDefault $ do
          (num, instr, st, dm) <- get
          put (num, instr ++ [TACJump $ switchToDefaultTACIdentifier label], st, dm)
          return ()
        (num, instr, st, dm) <- get
        put (num, instr ++ [TACJump break_label], st, dm)
        _ <- emitTacky block
        (num', instr', st', dm') <- get
        put (num', instr' ++ [TACLabel break_label], st', dm')
        return $ TACConstant (IntConst CInt 0)
  emitTacky (CaseStmt (Just label) (TypedExpr (Constant i) _) stmt) = do
    (num, instr, st, dm) <- get
    put (num, instr ++ [TACLabel $ caseToTACIdentifier label i], st, dm)
    _ <- emitTacky stmt
    return $ TACConstant (IntConst CInt 0)
  emitTacky (DefaultStmt (Just label)) = do
    (num, instr, st, dm) <- get
    put (num, instr ++ [TACLabel $ switchToDefaultTACIdentifier label], st, dm)
    return $ TACConstant (IntConst CInt 0)
  emitTacky (CaseStmt {}) = error "CaseStmt with non-constant, this shouldnt happen"

showNeg :: (Integral a, Show a) => a -> String
showNeg i = if i < 0 then "neg" ++ show (abs i) else show i

caseToTACIdentifier :: Identifier -> Const -> TACIdentifier
caseToTACIdentifier name (IntConst _ c) = name ++ ".case." ++ showNeg c
caseToTACIdentifier _ _ = error "Non integer case, this shouldnt happen"

switchToDefaultTACIdentifier :: Identifier -> TACIdentifier
switchToDefaultTACIdentifier name = name ++ ".default"

caseToIf :: Identifier -> TACVal -> Const -> InstrSt ()
caseToIf identifier v i = do
  tmp <- makeTemporaryVariable "tmp." (Just CInt)
  (num, instr, st, dm) <- get
  put (num, instr ++ [TACBinary TACEqual v (TACConstant i) (TACVar tmp), TACJumpIfNotZero (TACVar tmp) (caseToTACIdentifier identifier i)], st, dm)
  return ()

instance Emittable ForInit where
  emitTacky :: ForInit -> InstrSt TACVal
  emitTacky (InitDecl decl) = emitTacky decl
  emitTacky (InitExpr Nothing) = return $ TACConstant (IntConst CInt 0)
  emitTacky (InitExpr (Just e)) = emitTacky e

instance Emittable BlockItem where
  emitTacky :: BlockItem -> InstrSt TACVal
  emitTacky (BlockStmt stmt) = emitTacky stmt
  emitTacky (BlockDecl decl) = emitTacky decl

instance Emittable Block where
  emitTacky :: Block -> InstrSt TACVal
  emitTacky (Block items) = traverse_ emitTacky items >> return (TACConstant (IntConst CInt 0))

instance Emittable Decl where
  emitTacky :: Decl -> InstrSt TACVal
  emitTacky (VDecl decl) = emitTacky decl
  emitTacky (FDecl decl) = emitTacky decl

instance Emittable FuncDecl where
  emitTacky :: FuncDecl -> InstrSt TACVal
  emitTacky _ = return $ TACConstant (IntConst CInt 0)

toTACFunc :: SymbolTable -> StaticConstMap -> GlobalNameMap -> FuncDecl -> (GlobalNameMap, TACTopLevel, SymbolTable, StaticConstMap)
toTACFunc st dm globalMap (FuncDecl name args (Just block) _ _) = (newMap, TACFunction name functionGlobal args (instrs ++ [TACReturn $ TACConstant (IntConst CInt 0)]), newSt, newDm)
  where
    (newMap, instrs, newSt, newDm) = execState (emitTacky block) (globalMap, [], st, dm)
    functionSymbol = M.lookup name st
    functionGlobal = case functionSymbol of
      Just (_, FuncAttr _ global) -> global
      _ -> error "Function without symbol, this shouldnt happen"
toTACFunc _ _ _ _ = error "FuncDecl without body, this shouldnt happen"

toTACProg :: (Program, SymbolTable) -> (TACProgram, SymbolTable, GlobalNameMap, StaticConstMap)
toTACProg (Program fs, st) = (TACProgram funcs, newSt, newGm, newDm)
  where
    (newSt, newDm, funcs, newGm) = processFunctions M.empty st M.empty functionDecls staticVars
    staticVars = symbolsToTacky st
    functionDecls = mapMaybe getFunctionDecl fs

    getFunctionDecl :: Decl -> Maybe FuncDecl
    getFunctionDecl (FDecl decl@(FuncDecl _ _ (Just _) _ _)) = Just decl
    getFunctionDecl _ = Nothing

    processFunctions globalMap symTable dm [] acc = (symTable, dm, acc, globalMap)
    processFunctions globalMap symTable dm (f : fs') acc =
      let (newGlobalMap, tacFunc, newSymTable, newDm') = toTACFunc symTable dm globalMap f
       in processFunctions newGlobalMap newSymTable newDm' fs' (acc ++ [tacFunc])

isRelationalOp :: TACBinaryOp -> Bool
isRelationalOp TACEqual = True
isRelationalOp TACNotEqual = True
isRelationalOp TACLessThan = True
isRelationalOp TACLessThanOrEqual = True
isRelationalOp TACGreaterThan = True
isRelationalOp TACGreaterThanOrEqual = True
isRelationalOp _ = False

symbolsToTacky :: SymbolTable -> [TACTopLevel]
symbolsToTacky st = fmap convert stList
  where
    stList = filter filterStatic (M.toList st)
    filterStatic (_, (_, StaticAttr (Initial _) _)) = True
    filterStatic (_, (_, StaticAttr Tentative _)) = True
    filterStatic _ = False
    convert (name, (ty, StaticAttr (Initial (StaticInit c)) global)) = TACStaticVariable name global ty (StaticInit (convertConst ty c))
    convert (name, (ty, StaticAttr Tentative global)) = TACStaticVariable name global ty (StaticInit $ IntConst ty 0)
    convert _ = error "This shouldnt happen"

emitTackyCastWithSrc :: TypedExpr -> InstrSt (TACVal, TACVal)
emitTackyCastWithSrc (TypedExpr (Cast targetType e@(TypedExpr (Constant c) _)) _) = do
  src <- emitTacky e
  if targetType == constType c
    then return (src, src)
    else do
      let c' = convertConst targetType c
      if targetType == CDouble
        then do
          name <- makeStaticConstant "double_const." c'
          return (src, name)
        else
          return (src, TACConstant c')
emitTackyCastWithSrc (TypedExpr (Cast targetType e) _) = do
  src <- emitTacky e
  let srcType = fromJust $ tyType e
  if targetType == srcType
    then return (src, src)
    else do
      dst_name <- makeTemporaryVariable "tmp." (Just targetType)
      (num, instr, st, dm) <- get
      let castInstr = castInstruction targetType srcType
      put (num, instr ++ [castInstr src (TACVar dst_name)], st, dm)
      return (src, TACVar dst_name)
emitTackyCastWithSrc e = do
  src <- emitTacky e
  return (src, src)

castInstruction :: CType -> CType -> TACVal -> TACVal -> TACInstruction
castInstruction targetType srcType
  | targetType == CDouble && signed srcType = TACIntToDouble
  | targetType == CDouble && not (signed srcType) = TACUIntToDouble
  | srcType == CDouble && signed targetType = TACDoubleToInt
  | srcType == CDouble && not (signed targetType) = TACDoubleToUInt
  | size targetType == size srcType = TACCopy
  | size targetType < size srcType = TACTruncate
  | signed srcType = TACSignExtend
  | otherwise = TACZeroExtend

emitTackyCompoundWidening :: TypedExpr -> InstrSt TACVal
emitTackyCompoundWidening (TypedExpr (Assignment op l r) t) = do
  let leftType = tyType l
      rightType = tyType r
      commonType = liftA2 getCommonType leftType rightType
      castLeft = convertTo l commonType
      castRight = convertTo r commonType
  (lhs_src, lhs_converted) <- emitTackyCastWithSrc castLeft
  rhs <- emitTacky castRight
  dst_name <- makeTemporaryVariable "tmp." commonType
  let initialInstr = [TACBinary (tackyAssignOp op) lhs_converted rhs (TACVar dst_name)]
  if t == commonType
    then do
      (num, instr, st, dm) <- get
      put (num, instr ++ initialInstr ++ [TACCopy (TACVar dst_name) lhs_src], st, dm)
      return lhs_src
    else do
      castTemp <- makeTemporaryVariable "tmp." t
      let castInstr = castInstruction (fromJust t) (fromJust commonType)
      (num, instr, st, dm) <- get
      put (num, instr ++ initialInstr ++ [castInstr (TACVar dst_name) (TACVar castTemp), TACCopy (TACVar castTemp) lhs_src], st, dm)
      return lhs_src
emitTackyCompoundWidening _ = error "Not an assignment, this shouldnt happen"
