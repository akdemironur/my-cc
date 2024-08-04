{-# HLINT ignore "Use newtype instead of data" #-}
{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module AST where

import Lexer

data Program = Program [Function] deriving (Eq)

instance Show Program where
  show :: Program -> String
  show (Program functions) = "Program(\n" ++ indent (unlines (map show functions)) ++ ")"

data Function = Function Type String [Parameter] [Statement] deriving (Eq)

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

data Type = IntType | VoidType deriving (Eq)

instance Show Type where
  show :: Type -> String
  show IntType = "Int"
  show VoidType = "Void"

data Parameter = Parameter Type String deriving (Eq)

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

data IntLiteral = IntLiteral Int deriving (Eq)

instance Show IntLiteral where
  show :: IntLiteral -> String
  show (IntLiteral i) = show i

data Identifier = Identifier String deriving (Eq)

instance Show Identifier where
  show :: Identifier -> String
  show (Identifier s) = show s

data Statement = ReturnStatement Expression deriving (Eq)

instance Show Statement where
  show :: Statement -> String
  show (ReturnStatement e) = "Return(\n" ++ indent (show e) ++ "\n)"

data Expression = ConstantExpression IntLiteral deriving (Eq)

instance Show Expression where
  show :: Expression -> String
  show (ConstantExpression i) = "Constant(" ++ show i ++ ")"

indent :: String -> String
indent = unlines . map ("  " ++) . lines
