{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Parser where

import AST
import Lexer

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
  parse ts = parseExprHelper leftExpr ts' 0
    where
      (leftExpr, ts') = parseFactor ts

parseExpr :: [Token] -> Int -> (Expression, [Token])
parseExpr ts = parseExprHelper leftExpr ts'
  where
    (leftExpr, ts') = parseFactor ts

parseExprHelper :: Expression -> [Token] -> Int -> (Expression, [Token])
parseExprHelper leftExpr [] _ = (leftExpr, [])
parseExprHelper leftExpr ts@(t : _) minPrec
  | isBinOp t && precedence t >= minPrec = parseExprHelper leftExpr' ts'' minPrec
  | otherwise = (leftExpr, ts)
  where
    (operator, ts') = parse ts
    (rightExpr, ts'') = parseExpr ts' (precedence t + 1)
    leftExpr' = Binary operator leftExpr rightExpr

parseFactor :: [Token] -> (Expression, [Token])
parseFactor (TConstant c : ts) = (ConstantExpression (IntLiteral c), ts)
parseFactor (TOpenParen : ts) =
  let (expression, ts') = parse ts
   in (expression, expectAndConsume [TCloseParen] ts')
parseFactor ts =
  let (operator, ts') = parse ts
      (expression, ts'') = parseFactor ts'
   in (Unary operator expression, ts'')

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
    let ts' = expectAndConsume [TIntKeyword] ts
        (Identifier name, ts'') = parse ts'
        ts''' = expectAndConsume [TOpenParen, TVoidKeyword, TCloseParen, TOpenBrace] ts''
        (statement, ts'''') = parse ts'''
     in (Function name [statement], expectAndConsume [TCloseBrace] ts'''')

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

instance Parseable BinaryOperator where
  parse :: [Token] -> (BinaryOperator, [Token])
  parse (TPlus : ts) = (Add, ts)
  parse (THyphen : ts) = (Subtract, ts)
  parse (TAsterisk : ts) = (Multiply, ts)
  parse (TSlash : ts) = (Divide, ts)
  parse (TPercent : ts) = (Remainder, ts)
  parse (TBitwiseAnd : ts) = (BitwiseAnd, ts)
  parse (TBitwiseOr : ts) = (BitwiseOr, ts)
  parse (TBitwiseXor : ts) = (BitwiseXor, ts)
  parse (TLeftShift : ts) = (LeftShift, ts)
  parse (TRightShift : ts) = (RightShift, ts)
  parse (TLogicalAnd : ts) = (And, ts)
  parse (TLogicalOr : ts) = (Or, ts)
  parse (TEqualTo : ts) = (EqualTo, ts)
  parse (TNotEqualTo : ts) = (NotEqualTo, ts)
  parse (TLessThan : ts) = (LessThan, ts)
  parse (TLessThanOrEqualTo : ts) = (LessThanOrEqualTo, ts)
  parse (TGreaterThan : ts) = (GreaterThan, ts)
  parse (TGreaterThanOrEqualTo : ts) = (GreaterThanOrEqualTo, ts)
  parse _ = error "Invalid binary operator"

instance Parseable UnaryOperator where
  parse :: [Token] -> (UnaryOperator, [Token])
  parse (TTilde : ts) = (Complement, ts)
  parse (THyphen : ts) = (Negate, ts)
  parse (TExclamation : ts) = (Not, ts)
  parse _ = error "Invalid unary operator"

parseProgram :: [Token] -> Program
parseProgram = fst . parse