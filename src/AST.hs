{-# LANGUAGE InstanceSigs #-}

module AST
  ( Program (..),
    IntLiteral (..),
    Identifier (..),
    ForInit (..),
    Stmt (..),
    Expr (..),
    AssignmentOp (..),
    UnaryOp (..),
    PostOp (..),
    Operator (..),
    VarDecl (..),
    Block (..),
    BlockItem (..),
    ConditionalOp (..),
    BinaryOp (..),
    OpPrecedence (..),
    FuncDecl (..),
    Decl (..),
  )
where

import qualified Data.Set as S
import Lexer ()

newtype Program = Program [FuncDecl] deriving (Eq)

newtype IntLiteral = IntLiteral Int deriving (Eq, Ord)

newtype Identifier = Identifier String deriving (Eq, Ord)

data ForInit
  = InitDecl VarDecl
  | InitExpr (Maybe Expr)
  deriving (Eq)

data Stmt
  = ReturnStmt Expr
  | ExprStmt Expr
  | IfStmt Expr Stmt (Maybe Stmt)
  | NullStmt
  | LabeledStmt Identifier Stmt
  | GotoStmt Identifier
  | CompoundStmt Block
  | ForStmt (Maybe Identifier) ForInit (Maybe Expr) (Maybe Expr) Stmt
  | WhileStmt (Maybe Identifier) Expr Stmt
  | DoWhileStmt (Maybe Identifier) Stmt Expr
  | BreakStmt (Maybe Identifier)
  | ContinueStmt (Maybe Identifier)
  | SwitchStmt (Maybe Identifier) (S.Set IntLiteral) Bool Expr Stmt
  | CaseStmt (Maybe Identifier) Expr Stmt
  | DefaultStmt (Maybe Identifier)
  deriving (Eq)

data Expr
  = Constant IntLiteral
  | Var Identifier
  | Unary UnaryOp Expr
  | PostFix Expr PostOp
  | Binary BinaryOp Expr Expr
  | Assignment AssignmentOp Expr Expr
  | Conditional Expr Expr Expr
  | FunctionCall Identifier [Expr]
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

data VarDecl = VarDecl Identifier (Maybe Expr) deriving (Eq)

data FuncDecl = FuncDecl Identifier [Identifier] (Maybe Block) deriving (Eq)

data Decl = VDecl VarDecl | FDecl FuncDecl deriving (Eq)

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

instance Show Decl where
  show :: Decl -> String
  show (VDecl decl) = show decl
  show (FDecl decl) = show decl

instance Show FuncDecl where
  show :: FuncDecl -> String
  show (FuncDecl name params block) =
    "FuncDecl(\n"
      ++ "  name: "
      ++ show name
      ++ ",\n"
      ++ "  parameters: "
      ++ show params
      ++ ",\n"
      ++ "  body: "
      ++ indent (maybe "None" show block)
      ++ "\n"
      ++ ")"

instance Show VarDecl where
  show :: VarDecl -> String
  show (VarDecl name Nothing) = "VarDecl(\n" ++ indent ("name: " ++ show name) ++ "\n)"
  show (VarDecl name (Just expr)) =
    "VarDecl(\n"
      ++ "  name: "
      ++ show name
      ++ ",\n"
      ++ "  expression: "
      ++ indent (show expr)
      ++ "\n"
      ++ ")"

instance Show Block where
  show :: Block -> String
  show (Block items) = "Block(\n" ++ indent (unlines (map show items)) ++ ")"

instance Show IntLiteral where
  show :: IntLiteral -> String
  show (IntLiteral i) = show i

instance Show Identifier where
  show :: Identifier -> String
  show (Identifier s) = show s

instance Show ForInit where
  show :: ForInit -> String
  show (InitDecl decl) = "InitDecl(" ++ show decl ++ ")"
  show (InitExpr e) = "InitExpr(" ++ maybe "None" show e ++ ")"

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
  show (LabeledStmt i s) = "Label(" ++ show i ++ ", " ++ show s ++ ")"
  show (GotoStmt i) = "Goto(" ++ show i ++ ")"
  show (CompoundStmt b) = "Compound(\n" ++ indent (show b) ++ ")"
  show (ForStmt label forinit cond iter stmt) =
    "For(\n"
      ++ indent ("Label: " ++ maybe "None" show label)
      ++ indent ("Init: " ++ show forinit)
      ++ ",\n"
      ++ indent ("Condition: " ++ maybe "None" show cond)
      ++ ",\n"
      ++ indent ("Iter: " ++ maybe "None" show iter)
      ++ ",\n"
      ++ indent ("Body: " ++ show stmt)
      ++ "\n)"
  show (WhileStmt l e s) =
    "While(\n"
      ++ indent ("Label: " ++ maybe "None" show l)
      ++ indent ("Condition: " ++ show e)
      ++ ",\n"
      ++ indent ("Body: " ++ show s)
      ++ "\n)"
  show (DoWhileStmt l s e) =
    "DoWhile(\n"
      ++ indent ("Label: " ++ maybe "None" show l)
      ++ indent ("Body: " ++ show s)
      ++ ",\n"
      ++ indent ("Condition: " ++ show e)
      ++ "\n)"
  show (BreakStmt l) = "Break(" ++ maybe "None" show l ++ ")"
  show (ContinueStmt l) = "Continue(" ++ maybe "None" show l ++ ")"
  show (SwitchStmt l _ _ e s) =
    "Switch(\n"
      ++ indent ("Label: " ++ maybe "None" show l)
      ++ indent ("Condition: " ++ show e)
      ++ ",\n"
      ++ indent ("Body: " ++ show s)
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
  show (FunctionCall i es) = "FunctionCall(" ++ show i ++ ", " ++ show es ++ ")"

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
