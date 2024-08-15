{-# LANGUAGE InstanceSigs #-}

module Tacky
  ( TACProgram (..),
    TACFunction (..),
    TACInstruction (..),
    TACVal (..),
    TACUnaryOp (..),
    TACBinaryOp (..),
    TACIdentifier,
    isRelationalOp,
    toTACProg,
  )
where

import AST
import Control.Monad.State
import Data.Foldable
import qualified Data.Map as M

type TACIdentifier = String

newtype TACProgram = TACProgram [TACFunction] deriving (Show, Eq)

data TACFunction = TACFunction TACIdentifier [TACInstruction] deriving (Show, Eq)

breakLabel :: Maybe Identifier -> String
breakLabel (Just (Identifier name)) = name ++ ".break"
breakLabel Nothing = error "BreakLabel: This shouldnt happen"

continueLabel :: Maybe Identifier -> String
continueLabel (Just (Identifier name)) = name ++ ".continue"
continueLabel Nothing = error "ContinueLabel: This shouldnt happen"

startLabel :: Maybe Identifier -> String
startLabel (Just (Identifier name)) = name ++ ".start"
startLabel Nothing = error "StartLabel: This shouldnt happen"

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
  emitTacky (Constant (IntLiteral i)) = return $ TACConstant i
  emitTacky (Var (Identifier name)) = return $ TACVar name
  emitTacky (Binary And left right) = do
    false_label <- globalNameGenerator "false."
    end_label <- globalNameGenerator "end."
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
    true_label <- globalNameGenerator "true."
    end_label <- globalNameGenerator "end."
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
  emitTacky (Conditional cond thenExpr elseExpr) = do
    cond_val <- emitTacky cond
    else_label <- globalNameGenerator "else."
    end_label <- globalNameGenerator "end."
    dst_name <- globalNameGenerator "tmp."
    (num, instr) <- get
    put (num, instr ++ [TACJumpIfZero cond_val else_label])
    then_val <- emitTacky thenExpr
    (num', instr') <- get
    put (num', instr' ++ [TACCopy then_val (TACVar dst_name), TACJump end_label, TACLabel else_label])
    else_val <- emitTacky elseExpr
    (num'', instr'') <- get
    put (num'', instr'' ++ [TACCopy else_val (TACVar dst_name), TACLabel end_label])
    return $ TACVar dst_name

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
    (num, instr) <- get
    put (num, instr ++ [TACReturn src])
    return src
  emitTacky (ExprStmt e) = emitTacky e
  emitTacky NullStmt = return $ TACConstant 0
  emitTacky (IfStmt cond thenStmt (Just elseStmt)) = do
    cond_val <- emitTacky cond
    else_label <- globalNameGenerator "else."
    end_label <- globalNameGenerator "end."
    (num, instr) <- get
    put (num, instr ++ [TACJumpIfZero cond_val else_label])
    _ <- emitTacky thenStmt
    (num', instr') <- get
    put (num', instr' ++ [TACJump end_label, TACLabel else_label])
    _ <- emitTacky elseStmt
    (num'', instr'') <- get
    put (num'', instr'' ++ [TACLabel end_label])
    return $ TACConstant 0
  emitTacky (IfStmt cond thenStmt Nothing) = do
    cond_val <- emitTacky cond
    end_label <- globalNameGenerator "end."
    (num, instr) <- get
    put (num, instr ++ [TACJumpIfZero cond_val end_label])
    _ <- emitTacky thenStmt
    (num', instr') <- get
    put (num', instr' ++ [TACLabel end_label])
    return $ TACConstant 0
  emitTacky (LabeledStmt (Identifier name) stmt) = do
    (num, instr) <- get
    put (num, instr ++ [TACLabel $ name ++ ".label"])
    _ <- emitTacky stmt
    return $ TACConstant 0
  emitTacky (GotoStmt (Identifier name)) = do
    (num, instr) <- get
    put (num, instr ++ [TACJump $ name ++ ".label"])
    return $ TACConstant 0
  emitTacky (CompoundStmt block) = emitTacky block
  emitTacky (WhileStmt label cond block) = do
    let continue_label = continueLabel label
    let break_label = breakLabel label
    (num, instr) <- get
    put (num, instr ++ [TACLabel continue_label])
    cond_val <- emitTacky cond
    (num', instr') <- get
    put (num', instr' ++ [TACJumpIfZero cond_val break_label])
    _ <- emitTacky block
    (num'', instr'') <- get
    put (num'', instr'' ++ [TACJump continue_label, TACLabel break_label])
    return $ TACConstant 0
  emitTacky (DoWhileStmt label block cond) = do
    let start_label = startLabel label
    let break_label = breakLabel label
    let continue_label = continueLabel label
    (num, instr) <- get
    put (num, instr ++ [TACLabel start_label])
    _ <- emitTacky block
    (num', instr') <- get
    put (num', instr' ++ [TACLabel continue_label])
    cond_val <- emitTacky cond
    (num'', instr'') <- get
    put (num'', instr'' ++ [TACJumpIfNotZero cond_val start_label, TACLabel break_label])
    return $ TACConstant 0
  emitTacky (ForStmt label forinit cond update block) = do
    let start_label = startLabel label
    let continue_label = continueLabel label
    let break_label = breakLabel label
    _ <- emitTacky forinit
    (num, instr) <- get
    put (num, instr ++ [TACLabel start_label])
    case cond of
      Just c -> do
        cond_val <- emitTacky c
        (num', instr') <- get
        put (num', instr' ++ [TACJumpIfZero cond_val break_label])
      Nothing -> return ()
    _ <- emitTacky block
    (num', instr') <- get
    put (num', instr' ++ [TACLabel continue_label])
    case update of
      Just u -> do
        _ <- emitTacky u
        return ()
      Nothing -> return ()
    (num'', instr'') <- get
    put (num'', instr'' ++ [TACJump start_label, TACLabel break_label])
    return $ TACConstant 0
  emitTacky (BreakStmt label) = do
    (num, instr) <- get
    put (num, instr ++ [TACJump $ breakLabel label])
    return $ TACConstant 0
  emitTacky (ContinueStmt label) = do
    (num, instr) <- get
    put (num, instr ++ [TACJump $ continueLabel label])
    return $ TACConstant 0
  emitTacky (SwitchStmt label'@(Just label) caseSet hasDefault expr block) = do
    v <- emitTacky expr
    if not hasDefault && null caseSet
      then do
        return $ TACConstant 0
      else do
        let break_label = breakLabel label'
        traverse_ (caseToIf label v) caseSet
        when hasDefault $ do
          (num, instr) <- get
          put (num, instr ++ [TACJump $ switchToDefaultTACIdentifier label])
          return ()
        (num, instr) <- get
        put (num, instr ++ [TACJump break_label])
        _ <- emitTacky block
        (num', instr') <- get
        put (num', instr' ++ [TACLabel break_label])
        return $ TACConstant 0
  emitTacky (CaseStmt (Just label) (Constant i) stmt) = do
    (num, instr) <- get
    put (num, instr ++ [TACLabel $ caseToTACIdentifier label i])
    _ <- emitTacky stmt
    return $ TACConstant 0
  emitTacky (DefaultStmt (Just label)) = do
    (num, instr) <- get
    put (num, instr ++ [TACLabel $ switchToDefaultTACIdentifier label])
    return $ TACConstant 0
  emitTacky (CaseStmt {}) = error "CaseStmt with non-constant, this shouldnt happen"

caseToTACIdentifier :: Identifier -> IntLiteral -> TACIdentifier
caseToTACIdentifier (Identifier name) (IntLiteral i) = name ++ ".case." ++ show i

switchToDefaultTACIdentifier :: Identifier -> TACIdentifier
switchToDefaultTACIdentifier (Identifier name) = name ++ ".default"

caseToIf :: Identifier -> TACVal -> IntLiteral -> InstrSt ()
caseToIf identifier v i@(IntLiteral i') = do
  tmp <- globalNameGenerator "tmp."
  (num, instr) <- get
  put (num, instr ++ [TACBinary TACEqual v (TACConstant i') (TACVar tmp), TACJumpIfNotZero (TACVar tmp) (caseToTACIdentifier identifier i)])
  return ()

instance Emittable ForInit where
  emitTacky :: ForInit -> InstrSt TACVal
  emitTacky (InitDecl decl) = emitTacky decl
  emitTacky (InitExpr Nothing) = return $ TACConstant 0
  emitTacky (InitExpr (Just e)) = emitTacky e

instance Emittable BlockItem where
  emitTacky :: BlockItem -> InstrSt TACVal
  emitTacky (BlockStmt stmt) = emitTacky stmt
  emitTacky (BlockDecl decl) = emitTacky decl

instance Emittable Block where
  emitTacky :: Block -> InstrSt TACVal
  emitTacky (Block items) = traverse_ emitTacky items >> return (TACConstant 0)

toTACFunc :: Function -> TACFunction
toTACFunc (Function (Identifier name) block) = TACFunction name (snd (execState (emitTacky block) (M.empty, [])) ++ [TACReturn (TACConstant 0)])

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