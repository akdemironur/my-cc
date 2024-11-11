{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module AST where

import qualified Data.Map as M
import qualified Data.Set as S

data IdentifierAttrs
  = FuncAttr Bool Bool -- Defined, Global
  | StaticAttr InitialValue Bool -- InitialValue, Global
  | StaticConst Const
  | LocalAttr
  deriving (Show, Eq)

newtype StaticInit = StaticInit Const deriving (Show, Eq)

data InitialValue
  = Tentative
  | Initial StaticInit
  | NoInitializer
  deriving (Show, Eq)

type SymbolTableEntry = (CType, IdentifierAttrs)

type SymbolTable = M.Map Identifier SymbolTableEntry

newtype Program = Program [Decl] deriving (Eq)

data CType
  = CInt
  | CLong
  | CUInt
  | CULong
  | CDouble
  | CFunc [CType] CType
  deriving (Show, Eq, Ord)

data Const
  = IntConst CType Integer
  | DoubleConst CType Double
  deriving (Show)

instance Eq Const where
  (IntConst c1 i1) == (IntConst c2 i2) = c1 == c2 && i1 == i2
  (DoubleConst t1 d1) == (DoubleConst t2 d2) =
    t1 == t2
      && ( (isNaN d1 && isNaN d2)
             || ( case (isNegativeZero d1, isNegativeZero d2) of
                    (True, True) -> True
                    (True, False) -> False
                    (False, True) -> False
                    (False, False) -> d1 == d2
                )
         )
  _ == _ = False

instance Ord Const where
  compare (IntConst c1 i1) (IntConst c2 i2) =
    compare (c1, i1) (c2, i2)
  compare (DoubleConst t1 d1) (DoubleConst t2 d2) =
    case compare t1 t2 of
      EQ ->
        if isNaN d1 || isNaN d2
          then EQ -- This makes NaN equal to itself
          else case (isNegativeZero d1, isNegativeZero d2) of
            (True, True) -> EQ
            (True, False) -> LT -- -0.0 < 0.0
            (False, True) -> GT -- 0.0 > -0.0
            (False, False) -> compare d1 d2
      other -> other
  compare (DoubleConst _ _) (IntConst _ _) = GT
  compare (IntConst _ _) (DoubleConst _ _) = LT

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
  deriving (Eq)

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
  deriving (Eq, Ord)

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
  deriving (Eq, Ord)

data PostOp
  = PostIncrement
  | PostDecrement
  deriving (Eq, Ord)

data Operator
  = U UnaryOp
  | B BinaryOp
  | P PostOp
  | A AssignmentOp
  | C ConditionalOp
  deriving (Eq, Ord)

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
  deriving (Eq, Ord)

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
  deriving (Eq, Ord)

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

indentAST :: String -> String
indentAST = unlines . map ("  " ++) . lines

showMaybe :: (Show a) => Maybe a -> String
showMaybe = maybe "None" show

showKeyValue :: String -> String -> String
showKeyValue key value = key ++ ": " ++ value

showNode :: String -> [String] -> String
showNode name fields = name ++ "(\n" ++ indentAST (unlines fields) ++ ")"

instance Show Program where
  show (Program functions) = showNode "Program" (map show functions)

instance Show BlockItem where
  show (BlockStmt stmt) = show stmt
  show (BlockDecl decl) = show decl

instance Show Decl where
  show (VDecl decl) = show decl
  show (FDecl decl) = show decl

instance Show FuncDecl where
  show (FuncDecl name params block ftype storage) =
    showNode
      "FuncDecl"
      [ showKeyValue "name" (show name),
        showKeyValue "params" (show params),
        showKeyValue "block" (showMaybe block),
        showKeyValue "type" (show ftype),
        showKeyValue "storage" (show storage)
      ]

instance Show VarDecl where
  show (VarDecl name expr vtype storage) =
    showNode
      "VarDecl"
      [ showKeyValue "name" (show name),
        showKeyValue "expr" (showMaybe expr),
        showKeyValue "type" (show vtype),
        showKeyValue "storage" (show storage)
      ]

instance Show Block where
  show (Block items) = showNode "Block" (map show items)

instance Show ForInit where
  show (InitDecl decl) = "InitDecl(" ++ show decl ++ ")"
  show (InitExpr e) = "InitExpr(" ++ showMaybe e ++ ")"

instance Show TypedExpr where
  show (TypedExpr e t) = "TypedExpr(" ++ show e ++ ", " ++ showMaybe t ++ ")"

instance Show Stmt where
  show (ReturnStmt e) = showNode "Return" [show e]
  show (ExprStmt e) = showNode "Expr" [show e]
  show (IfStmt e s1 s2) =
    showNode
      "If"
      [ showKeyValue "Condition" (show e),
        showKeyValue "Then" (show s1),
        showKeyValue "Else" (showMaybe s2)
      ]
  show NullStmt = "Null"
  show (LabeledStmt i s) = "Label(" ++ show i ++ ", " ++ show s ++ ")"
  show (GotoStmt i) = "Goto(" ++ show i ++ ")"
  show (CompoundStmt b) = showNode "Compound" [show b]
  show (ForStmt label forinit cond iter stmt) =
    showNode
      "For"
      [ showKeyValue "Label" (showMaybe label),
        showKeyValue "Init" (show forinit),
        showKeyValue "Condition" (showMaybe cond),
        showKeyValue "Iter" (showMaybe iter),
        showKeyValue "Body" (show stmt)
      ]
  show (WhileStmt l e s) =
    showNode
      "While"
      [ showKeyValue "Label" (showMaybe l),
        showKeyValue "Condition" (show e),
        showKeyValue "Body" (show s)
      ]
  show (DoWhileStmt l s e) =
    showNode
      "DoWhile"
      [ showKeyValue "Label" (showMaybe l),
        showKeyValue "Body" (show s),
        showKeyValue "Condition" (show e)
      ]
  show (BreakStmt l) = "Break(" ++ showMaybe l ++ ")"
  show (ContinueStmt l) = "Continue(" ++ showMaybe l ++ ")"
  show (SwitchStmt l _ _ e s) =
    showNode
      "Switch"
      [ showKeyValue "Label" (showMaybe l),
        showKeyValue "Condition" (show e),
        showKeyValue "Body" (show s)
      ]
  show (CaseStmt l e s) = "Case(" ++ showMaybe l ++ ", " ++ show e ++ ", " ++ show s ++ ")"
  show (DefaultStmt l) = "Default(" ++ showMaybe l ++ ")"

instance Show Expr where
  show (Constant i) = "Constant(" ++ show i ++ ")"
  show (Unary op e) =
    showNode
      "Unary"
      [ showKeyValue "operator" (show op),
        showKeyValue "expression" (show e)
      ]
  show (Binary op e1 e2) =
    showNode
      "Binary"
      [ showKeyValue "operator" (show op),
        showKeyValue "left" (show e1),
        showKeyValue "right" (show e2)
      ]
  show (Var s) = "Var(" ++ show s ++ ")"
  show (PostFix e op) =
    showNode
      "PostFix"
      [ show e,
        show op
      ]
  show (Assignment op e1 e2) =
    showNode
      "Assignment"
      [ showKeyValue "operator" (show op),
        showKeyValue "left" (show e1),
        showKeyValue "right" (show e2)
      ]
  show (Conditional e1 e2 e3) =
    showNode
      "Conditional"
      [ showKeyValue "condition" (show e1),
        showKeyValue "then" (show e2),
        showKeyValue "else" (show e3)
      ]
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
