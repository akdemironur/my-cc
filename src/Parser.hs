{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Parser where

import AST
  ( Expression (..),
    Function (..),
    Identifier (..),
    IntLiteral (..),
    Parameter (..),
    Program (..),
    Statement (..),
    Type (..),
  )
import Lexer (Token (..))

class Parseable a where
  parse :: [Token] -> (a, [Token])

expectAndConsume :: (Eq a, Show a) => [a] -> [a] -> [a]
expectAndConsume [] xs = xs
expectAndConsume _ [] = error "Unexpected end of input"
expectAndConsume (e : es) (x : xs)
  | e == x = expectAndConsume es xs
  | otherwise = error $ "Expected " ++ show e ++ ", got " ++ show x

instance Parseable Statement where
  parse :: [Token] -> (Statement, [Token])
  parse (TReturnKeyword : ts) =
    let (expression, ts') = parse ts
     in (ReturnStatement expression, expectAndConsume [TSemicolon] ts')
  parse _ = error "Invalid statement"

instance Parseable Expression where
  parse :: [Token] -> (Expression, [Token])
  parse (TConstant c : ts) = (ConstantExpression (IntLiteral c), ts)
  parse _ = error "Invalid expression"

instance Parseable Identifier where
  parse :: [Token] -> (Identifier, [Token])
  parse (TIdentifier s : ts) = (Identifier s, ts)
  parse _ = error "Invalid identifier"

instance Parseable IntLiteral where
  parse :: [Token] -> (IntLiteral, [Token])
  parse (TConstant c : ts) = (IntLiteral c, ts)
  parse _ = error "Invalid integer literal"

instance Parseable Type where
  parse :: [Token] -> (Type, [Token])
  parse (TIntKeyword : ts) = (IntType, ts)
  parse (TVoidKeyword : ts) = (VoidType, ts)
  parse _ = error "Invalid type"

instance Parseable Function where
  parse :: [Token] -> (Function, [Token])
  parse ts =
    let (t, ts') = parse ts
        (Identifier name, ts'') = parse ts'
        ts''' = expectAndConsume [TOpenParen, TVoidKeyword, TCloseParen, TOpenBrace] ts''
        (statement, ts'''') = parse ts'''
     in (Function t name [] [statement], expectAndConsume [TCloseBrace] ts'''')

instance Parseable Program where
  parse :: [Token] -> (Program, [Token])
  parse ts = if null ts' then (Program [function], []) else error "Invalid program"
    where
      (function, ts') = parse ts

instance Parseable Parameter where
  parse :: [Token] -> (Parameter, [Token])
  parse ts =
    let (t, ts') = parse ts
        (Identifier name, ts'') = parse ts'
     in (Parameter t name, ts'')

parseProgram :: [Token] -> Program
parseProgram = fst . parse