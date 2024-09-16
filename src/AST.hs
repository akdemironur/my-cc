{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module AST where

import qualified Data.Set as S

newtype Program = Program [Decl] deriving (Eq)

newtype IntLiteral = IntLiteral Int deriving (Eq, Ord)

data Const = ConstInt Int | ConstLong Int deriving (Eq, Show, Ord)

type Identifier = String

data ForInit
  = InitDecl VarDecl
  | InitExpr (Maybe TypedExpr)
  deriving (Eq)

data Stmt
  = ReturnStmt TypedExpr
  | ExprStmt TypedExpr
  | IfStmt TypedExpr Stmt (Maybe Stmt)
  | NullStmt
  | LabeledStmt Identifier Stmt
  | GotoStmt Identifier
  | CompoundStmt Block
  | ForStmt (Maybe Identifier) ForInit (Maybe TypedExpr) (Maybe TypedExpr) Stmt
  | WhileStmt (Maybe Identifier) TypedExpr Stmt
  | DoWhileStmt (Maybe Identifier) Stmt TypedExpr
  | BreakStmt (Maybe Identifier)
  | ContinueStmt (Maybe Identifier)
  | SwitchStmt (Maybe Identifier) (S.Set Const) Bool TypedExpr Stmt
  | CaseStmt (Maybe Identifier) TypedExpr Stmt
  | DefaultStmt (Maybe Identifier)
  deriving (Eq)

data TypedExpr = TypedExpr
  { tyExpr :: Expr,
    tyType :: Maybe CType
  }
  deriving (Eq, Show)

data Expr
  = Constant Const
  | Var Identifier
  | Cast CType TypedExpr
  | Unary UnaryOp TypedExpr
  | PostFix TypedExpr PostOp
  | Binary BinaryOp TypedExpr TypedExpr
  | Assignment AssignmentOp TypedExpr TypedExpr
  | Conditional TypedExpr TypedExpr TypedExpr
  | FunctionCall Identifier [TypedExpr]
  deriving (Eq)

data AssignmentOp
  = Assign
  | PlusAssign
  | MinusAssign
  | MultiplyAssign
  | DivideAssign
  | RemainderAssign
  | BitwiseAndAssign
  | BitwiseOrAssign
  | BitwiseXorAssign
  | LeftShiftAssign
  | RightShiftAssign
  deriving (Eq)

isCompoundArithmeticOp :: AssignmentOp -> Bool
isCompoundArithmeticOp = (`elem` [PlusAssign, MinusAssign, MultiplyAssign, DivideAssign, RemainderAssign])

isCompoundBitshiftOp :: AssignmentOp -> Bool
isCompoundBitshiftOp = (`elem` [LeftShiftAssign, RightShiftAssign])

isCompoundBitwiseOp :: AssignmentOp -> Bool
isCompoundBitwiseOp = (`elem` [BitwiseAndAssign, BitwiseOrAssign, BitwiseXorAssign])

data UnaryOp
  = Complement
  | Negate
  | Not
  | PreIncrement
  | PreDecrement
  deriving (Eq)

data PostOp
  = PostIncrement
  | PostDecrement
  deriving (Eq)

data Operator
  = U UnaryOp
  | B BinaryOp
  | P PostOp
  | A AssignmentOp
  | C ConditionalOp
  deriving (Eq)

data CType
  = CInt
  | CLong
  | CFunc [CType] CType
  deriving (Show, Eq)

data VarDecl = VarDecl
  { varName :: Identifier,
    varExpr :: Maybe TypedExpr,
    varType :: CType,
    varStorage :: Maybe StorageClass
  }
  deriving (Eq)

data FuncDecl = FuncDecl
  { funcName :: Identifier,
    funcParams :: [Identifier],
    funcBlock :: Maybe Block,
    funcType :: CType,
    funcStorage :: Maybe StorageClass
  }
  deriving (Eq)

data Decl = VDecl VarDecl | FDecl FuncDecl deriving (Eq)

data StorageClass = Static | Extern deriving (Show, Eq)

newtype Block = Block [BlockItem] deriving (Eq)

data BlockItem
  = BlockStmt Stmt
  | BlockDecl Decl
  deriving (Eq)

data ConditionalOp
  = ConditionalOp
  deriving (Eq)

data BinaryOp
  = Add
  | Subtract
  | Multiply
  | Divide
  | Remainder
  | BitwiseAnd
  | BitwiseOr
  | BitwiseXor
  | LeftShift
  | RightShift
  | And
  | Or
  | EqualTo
  | NotEqualTo
  | LessThan
  | LessThanOrEqualTo
  | GreaterThan
  | GreaterThanOrEqualTo
  deriving (Eq)

isLogicalOp :: BinaryOp -> Bool
isLogicalOp = (`elem` [And, Or])

isComparisonOp :: BinaryOp -> Bool
isComparisonOp = (`elem` [EqualTo, NotEqualTo, LessThan, LessThanOrEqualTo, GreaterThan, GreaterThanOrEqualTo])

isBitshiftOp :: BinaryOp -> Bool
isBitshiftOp = (`elem` [LeftShift, RightShift])

isArithmeticOp :: BinaryOp -> Bool
isArithmeticOp op = not $ isLogicalOp op || isComparisonOp op || isBitshiftOp op

class OpPrecedence a where
  opPrecedence :: a -> Int

instance OpPrecedence ConditionalOp where
  opPrecedence :: ConditionalOp -> Int
  opPrecedence ConditionalOp = 3

instance OpPrecedence UnaryOp where
  opPrecedence :: UnaryOp -> Int
  opPrecedence Complement = 55
  opPrecedence Negate = 55
  opPrecedence Not = 55
  opPrecedence PreIncrement = 55
  opPrecedence PreDecrement = 55

instance OpPrecedence BinaryOp where
  opPrecedence :: BinaryOp -> Int
  opPrecedence Add = 45
  opPrecedence Subtract = 45
  opPrecedence Multiply = 50
  opPrecedence Divide = 50
  opPrecedence Remainder = 50
  opPrecedence BitwiseAnd = 20
  opPrecedence BitwiseOr = 10
  opPrecedence BitwiseXor = 15
  opPrecedence LeftShift = 40
  opPrecedence RightShift = 40
  opPrecedence And = 10
  opPrecedence Or = 5
  opPrecedence EqualTo = 30
  opPrecedence NotEqualTo = 30
  opPrecedence LessThan = 35
  opPrecedence LessThanOrEqualTo = 35
  opPrecedence GreaterThan = 35
  opPrecedence GreaterThanOrEqualTo = 35

instance OpPrecedence PostOp where
  opPrecedence :: PostOp -> Int
  opPrecedence PostIncrement = 60
  opPrecedence PostDecrement = 60

instance OpPrecedence AssignmentOp where
  opPrecedence :: AssignmentOp -> Int
  opPrecedence Assign = 1
  opPrecedence PlusAssign = 1
  opPrecedence MinusAssign = 1
  opPrecedence MultiplyAssign = 1
  opPrecedence DivideAssign = 1
  opPrecedence RemainderAssign = 1
  opPrecedence BitwiseAndAssign = 1
  opPrecedence BitwiseOrAssign = 1
  opPrecedence BitwiseXorAssign = 1
  opPrecedence LeftShiftAssign = 1
  opPrecedence RightShiftAssign = 1

instance Show Program where
  show :: Program -> String
  show (Program functions) = "Program(\n" ++ indentAST (unlines (map show functions)) ++ ")"

instance Show BlockItem where
  show :: BlockItem -> String
  show (BlockStmt stmt) = show stmt
  show (BlockDecl decl) = show decl

instance Show Decl where
  show :: Decl -> String
  show (VDecl decl) = show decl
  show (FDecl decl) = show decl

instance Show FuncDecl where
  show :: FuncDecl -> String
  show (FuncDecl name params block ftype storage) =
    "FuncDecl(\n"
      ++ "  name: "
      ++ show name
      ++ ",\n"
      ++ "  params: "
      ++ show params
      ++ ",\n"
      ++ "  block: "
      ++ indentAST (maybe "None" show block)
      ++ ",\n"
      ++ "  type: "
      ++ show ftype
      ++ ",\n"
      ++ "  storage: "
      ++ show storage
      ++ "\n"
      ++ ")"

instance Show VarDecl where
  show :: VarDecl -> String
  show (VarDecl name expr vtype storage) =
    "VarDecl(\n"
      ++ "  name: "
      ++ show name
      ++ ",\n"
      ++ "  expr: "
      ++ indentAST (maybe "None" show expr)
      ++ ",\n"
      ++ "  type: "
      ++ show vtype
      ++ ",\n"
      ++ "  storage: "
      ++ show storage
      ++ "\n"
      ++ ")"

instance Show Block where
  show :: Block -> String
  show (Block items) = "Block(\n" ++ indentAST (unlines (map show items)) ++ ")"

instance Show IntLiteral where
  show :: IntLiteral -> String
  show (IntLiteral i) = show i

instance Show ForInit where
  show :: ForInit -> String
  show (InitDecl decl) = "InitDecl(" ++ show decl ++ ")"
  show (InitExpr e) = "InitExpr(" ++ maybe "None" show e ++ ")"

instance Show Stmt where
  show :: Stmt -> String
  show (ReturnStmt e) =
    "Return(\n"
      ++ indentAST (show e)
      ++ "\n)"
  show (ExprStmt e) =
    "Expr(\n"
      ++ indentAST (show e)
      ++ "\n)"
  show (IfStmt e s1 s2) =
    "If(\n"
      ++ indentAST ("Condition: " ++ show e)
      ++ ",\n"
      ++ indentAST ("Then: " ++ show s1)
      ++ ",\n"
      ++ indentAST ("Else: " ++ maybe "None" show s2)
  show NullStmt = "Null"
  show (LabeledStmt i s) = "Label(" ++ show i ++ ", " ++ show s ++ ")"
  show (GotoStmt i) = "Goto(" ++ show i ++ ")"
  show (CompoundStmt b) = "Compound(\n" ++ indentAST (show b) ++ ")"
  show (ForStmt label forinit cond iter stmt) =
    "For(\n"
      ++ indentAST ("Label: " ++ maybe "None" show label)
      ++ indentAST ("Init: " ++ show forinit)
      ++ ",\n"
      ++ indentAST ("Condition: " ++ maybe "None" show cond)
      ++ ",\n"
      ++ indentAST ("Iter: " ++ maybe "None" show iter)
      ++ ",\n"
      ++ indentAST ("Body: " ++ show stmt)
      ++ "\n)"
  show (WhileStmt l e s) =
    "While(\n"
      ++ indentAST ("Label: " ++ maybe "None" show l)
      ++ indentAST ("Condition: " ++ show e)
      ++ ",\n"
      ++ indentAST ("Body: " ++ show s)
      ++ "\n)"
  show (DoWhileStmt l s e) =
    "DoWhile(\n"
      ++ indentAST ("Label: " ++ maybe "None" show l)
      ++ indentAST ("Body: " ++ show s)
      ++ ",\n"
      ++ indentAST ("Condition: " ++ show e)
      ++ "\n)"
  show (BreakStmt l) = "Break(" ++ maybe "None" show l ++ ")"
  show (ContinueStmt l) = "Continue(" ++ maybe "None" show l ++ ")"
  show (SwitchStmt l _ _ e s) =
    "Switch(\n"
      ++ indentAST ("Label: " ++ maybe "None" show l)
      ++ indentAST ("Condition: " ++ show e)
      ++ ",\n"
      ++ indentAST ("Body: " ++ show s)
      ++ "\n)"
  show (CaseStmt l e s) = "Case(" ++ maybe "None" show l ++ ",  " ++ show e ++ ", " ++ show s ++ ")"
  show (DefaultStmt l) = "Default(" ++ maybe "None" show l ++ ")"

instance Show Expr where
  show :: Expr -> String
  show (Constant i) = "Constant(" ++ show i ++ ")"
  show (Unary op e) =
    "Unary(\n"
      ++ "  operator: "
      ++ show op
      ++ ",\n"
      ++ "  expression: "
      ++ indentAST (show e)
      ++ ")"
  show (Binary op e1 e2) =
    "Binary(\n"
      ++ "  operator: "
      ++ show op
      ++ ",\n"
      ++ "  left: "
      ++ indentAST (show e1)
      ++ ",\n"
      ++ "  right: "
      ++ indentAST (show e2)
      ++ "\n"
      ++ ")"
  show (Var s) = "Var(" ++ show s ++ ")"
  show (PostFix e op) =
    "PostFix(\n"
      ++ indentAST (show e)
      ++ ",\n"
      ++ indentAST (show op)
      ++ ")"
  show (Assignment op e1 e2) =
    "Assignment("
      ++ show op
      ++ ", "
      ++ show e1
      ++ ", "
      ++ show e2
      ++ ")"
  show (Conditional e1 e2 e3) =
    "Conditional(\n"
      ++ "  condition: "
      ++ indentAST (show e1)
      ++ ",\n"
      ++ "  then: "
      ++ indentAST (show e2)
      ++ ",\n"
      ++ "  else: "
      ++ indentAST (show e3)
      ++ "\n"
      ++ ")"
  show (FunctionCall i es) = "FunctionCall(" ++ show i ++ ", " ++ show es ++ ")"
  show (Cast t e) = "Cast(" ++ show t ++ ", " ++ show e ++ ")"

instance Show UnaryOp where
  show :: UnaryOp -> String
  show Complement = "~"
  show Negate = "-"
  show Not = "!"
  show PreIncrement = "++"
  show PreDecrement = "--"

instance Show PostOp where
  show :: PostOp -> String
  show PostIncrement = "++"
  show PostDecrement = "--"

instance Show BinaryOp where
  show :: BinaryOp -> String
  show Add = "+"
  show Subtract = "-"
  show Multiply = "*"
  show Divide = "/"
  show Remainder = "%"
  show BitwiseAnd = "&"
  show BitwiseOr = "|"
  show BitwiseXor = "^"
  show LeftShift = "<<"
  show RightShift = ">>"
  show And = "&&"
  show Or = "||"
  show EqualTo = "=="
  show NotEqualTo = "!="
  show LessThan = "<"
  show LessThanOrEqualTo = "<="
  show GreaterThan = ">"
  show GreaterThanOrEqualTo = ">="

instance Show AssignmentOp where
  show :: AssignmentOp -> String
  show Assign = "="
  show PlusAssign = "+="
  show MinusAssign = "-="
  show MultiplyAssign = "*="
  show DivideAssign = "/="
  show RemainderAssign = "%="
  show BitwiseAndAssign = "&="
  show BitwiseOrAssign = "|="
  show BitwiseXorAssign = "^="
  show LeftShiftAssign = "<<="
  show RightShiftAssign = ">>="

indentAST :: String -> String
indentAST = unlines . map ("  " ++) . lines
