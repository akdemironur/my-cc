{-# HLINT ignore "Use newtype instead of data" #-}
{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module AST where

import Lexer

data Program = Program [Function] deriving (Eq)

data Function = Function Type String [Parameter] [Statement] deriving (Eq)

data Type = IntType | VoidType deriving (Eq)

data Parameter = Parameter Type String deriving (Eq)

data IntLiteral = IntLiteral Int deriving (Eq)

data Identifier = Identifier String deriving (Eq)

data Statement = ReturnStatement Expression deriving (Eq)

data Expression = ConstantExpression IntLiteral | Unary UnaryOperator Expression deriving (Eq)

data UnaryOperator = Complement | Negate deriving (Eq)

instance Show Program where
  show :: Program -> String
  show (Program functions) = "Program(\n" ++ indent (unlines (map show functions)) ++ ")"

instance Show Function where
  show :: Function -> String
  show (Function _ name _ statements) =
    "Function(\n"
      ++ "  name: "
      ++ show name
      ++ ",\n"
      ++ "  body: "
      ++ indent (unlines (map show statements))
      ++ "\n"
      ++ ")"

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

instance Show Statement where
  show :: Statement -> String
  show (ReturnStatement e) = "Return(\n" ++ indent (show e) ++ "\n)"

instance Show Expression where
  show :: Expression -> String
  show (ConstantExpression i) = "Constant(" ++ show i ++ ")"
  show (Unary op e) = "Unary(\n" ++ "  operator: " ++ show op ++ ",\n" ++ "  expression: " ++ indent (show e) ++ ")"

instance Show UnaryOperator where
  show :: UnaryOperator -> String
  show Complement = "~"
  show Negate = "-"

indent :: String -> String
indent = unlines . map ("  " ++) . lines
