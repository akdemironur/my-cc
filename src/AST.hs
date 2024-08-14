{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module AST where

import Lexer ()

newtype Program = Program [Function] deriving (Eq)

data Function = Function Identifier Block deriving (Eq)

data Type
  = IntType
  | VoidType
  deriving (Eq)

data Parameter = Parameter Type Identifier deriving (Eq)

newtype IntLiteral = IntLiteral Int deriving (Eq)

newtype Identifier = Identifier String deriving (Eq, Ord)

data Stmt
  = ReturnStmt Expr
  | ExprStmt Expr
  | IfStmt Expr Stmt (Maybe Stmt)
  | NullStmt
  | LabelStmt Identifier
  | GotoStmt Identifier
  | CompoundStmt Block
  deriving (Eq)

data Expr
  = Constant IntLiteral
  | Var Identifier
  | Unary UnaryOp Expr
  | PostFix Expr PostOp
  | Binary BinaryOp Expr Expr
  | Assignment AssignmentOp Expr Expr
  | Conditional Expr Expr Expr
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

data Declaration = Declaration Identifier (Maybe Expr) deriving (Eq)

newtype Block = Block [BlockItem] deriving (Eq)

data BlockItem
  = BlockStmt Stmt
  | BlockDecl Declaration
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
  show (Program functions) = "Program(\n" ++ indent (unlines (map show functions)) ++ ")"

instance Show BlockItem where
  show :: BlockItem -> String
  show (BlockStmt stmt) = show stmt
  show (BlockDecl decl) = show decl

instance Show Declaration where
  show :: Declaration -> String
  show (Declaration name Nothing) = "Declaration(\n" ++ indent ("name: " ++ show name) ++ "\n)"
  show (Declaration name (Just expr)) =
    "Declaration(\n"
      ++ "  name: "
      ++ show name
      ++ ",\n"
      ++ "  expression: "
      ++ indent (show expr)
      ++ "\n"
      ++ ")"

instance Show Function where
  show :: Function -> String
  show (Function name block) =
    "Function(\n"
      ++ "  name: "
      ++ show name
      ++ ",\n"
      ++ "  body: "
      ++ indent (show block)
      ++ "\n"
      ++ ")"

instance Show Block where
  show :: Block -> String
  show (Block items) = "Block(\n" ++ indent (unlines (map show items)) ++ ")"

instance Show Type where
  show :: Type -> String
  show IntType = "Int"
  show VoidType = "Void"

instance Show Parameter where
  show :: Parameter -> String
  show (Parameter t name) =
    "Parameter(\n"
      ++ "  type: "
      ++ show t
      ++ ",\n"
      ++ "  name: "
      ++ show name
      ++ "\n"
      ++ ")"

instance Show IntLiteral where
  show :: IntLiteral -> String
  show (IntLiteral i) = show i

instance Show Identifier where
  show :: Identifier -> String
  show (Identifier s) = show s

instance Show Stmt where
  show :: Stmt -> String
  show (ReturnStmt e) =
    "Return(\n"
      ++ indent (show e)
      ++ "\n)"
  show (ExprStmt e) =
    "Expr(\n"
      ++ indent (show e)
      ++ "\n)"
  show (IfStmt e s1 s2) =
    "If(\n"
      ++ indent ("Condition: " ++ show e)
      ++ ",\n"
      ++ indent ("Then: " ++ show s1)
      ++ ",\n"
      ++ indent ("Else: " ++ maybe "None" show s2)
  show NullStmt = "Null"
  show (LabelStmt i) = "Label(" ++ show i ++ ")"
  show (GotoStmt i) = "Goto(" ++ show i ++ ")"
  show (CompoundStmt b) = "Compound(\n" ++ indent (show b) ++ ")"

instance Show Expr where
  show :: Expr -> String
  show (Constant i) = "Constant(" ++ show i ++ ")"
  show (Unary op e) =
    "Unary(\n"
      ++ "  operator: "
      ++ show op
      ++ ",\n"
      ++ "  expression: "
      ++ indent (show e)
      ++ ")"
  show (Binary op e1 e2) =
    "Binary(\n"
      ++ "  operator: "
      ++ show op
      ++ ",\n"
      ++ "  left: "
      ++ indent (show e1)
      ++ ",\n"
      ++ "  right: "
      ++ indent (show e2)
      ++ "\n"
      ++ ")"
  show (Var s) = "Var(" ++ show s ++ ")"
  show (PostFix e op) =
    "PostFix(\n"
      ++ indent (show e)
      ++ ",\n"
      ++ indent (show op)
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
      ++ indent (show e1)
      ++ ",\n"
      ++ "  then: "
      ++ indent (show e2)
      ++ ",\n"
      ++ "  else: "
      ++ indent (show e3)
      ++ "\n"
      ++ ")"

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

indent :: String -> String
indent = unlines . map ("  " ++) . lines
