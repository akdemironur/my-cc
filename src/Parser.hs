{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use lambda-case" #-}

module Parser where

import AST
import Control.Applicative (Alternative (empty, many), many, optional, (<|>))
import Control.Monad (MonadPlus, mzero, void)
import Data.Functor (($>), (<&>))
import Lexer

data Error t s
  = UnexpedtedEndOfInput
  | UnexpectedToken t
  | CustomError s
  | EmptyError
  deriving (Show, Eq)

newtype Parser a = Parser {runParser :: [Token] -> Either String (a, [Token])}

instance Functor Parser where
  fmap :: (a -> b) -> Parser a -> Parser b
  fmap f (Parser p) = Parser $ \tokens -> do
    (x, rest) <- p tokens
    return (f x, rest)

instance Applicative Parser where
  pure :: a -> Parser a
  pure x = Parser $ \tokens -> Right (x, tokens)
  (<*>) :: Parser (a -> b) -> Parser a -> Parser b
  (Parser p1) <*> (Parser p2) = Parser $ \tokens -> do
    (f, rest1) <- p1 tokens
    (x, rest2) <- p2 rest1
    return (f x, rest2)

instance Monad Parser where
  return :: a -> Parser a
  return = pure
  (>>=) :: Parser a -> (a -> Parser b) -> Parser b
  (Parser p) >>= f = Parser $ \tokens -> do
    (x, rest) <- p tokens
    runParser (f x) rest

instance Alternative Parser where
  empty :: Parser a
  empty = Parser $ \_ -> Left "Empty parser"
  (<|>) :: Parser a -> Parser a -> Parser a
  (Parser p1) <|> (Parser p2) = Parser $ \tokens -> case p1 tokens of
    Left _ -> p2 tokens
    Right x -> Right x

instance MonadPlus Parser where
  mzero :: Parser a
  mzero = empty

try :: Parser a -> Parser ()
try (Parser p) = Parser $ \tokens -> case p tokens of
  Left err -> Left err
  Right (_, _) -> Right ((), tokens)

satisfy :: (Token -> Bool) -> Parser Token
satisfy p = Parser $ \tokens -> case tokens of
  [] -> Left "Empty token list"
  (t : ts) ->
    if p t
      then Right (t, ts)
      else Left (unwords ["Unexpected: ", show t, "in", show (take 20 tokens)])

parseToken :: Token -> Parser ()
parseToken t = void (satisfy (== t))

tokenToBinaryOp :: Token -> BinaryOp
tokenToBinaryOp TPlus = Add
tokenToBinaryOp THyphen = Subtract
tokenToBinaryOp TAsterisk = Multiply
tokenToBinaryOp TSlash = Divide
tokenToBinaryOp TPercent = Remainder
tokenToBinaryOp TBitwiseAnd = BitwiseAnd
tokenToBinaryOp TBitwiseOr = BitwiseOr
tokenToBinaryOp TBitwiseXor = BitwiseXor
tokenToBinaryOp TLeftShift = LeftShift
tokenToBinaryOp TRightShift = RightShift
tokenToBinaryOp TLogicalAnd = And
tokenToBinaryOp TLogicalOr = Or
tokenToBinaryOp TEqualTo = EqualTo
tokenToBinaryOp TNotEqualTo = NotEqualTo
tokenToBinaryOp TLessThan = LessThan
tokenToBinaryOp TLessThanOrEqualTo = LessThanOrEqualTo
tokenToBinaryOp TGreaterThan = GreaterThan
tokenToBinaryOp TGreaterThanOrEqualTo = GreaterThanOrEqualTo
tokenToBinaryOp t = error $ "Invalid binary operator: " ++ show t

tokenToAssignmentOp :: Token -> AssignmentOp
tokenToAssignmentOp TAssignment = Assign
tokenToAssignmentOp TPlusAssignment = PlusAssign
tokenToAssignmentOp THyphenAssignment = MinusAssign
tokenToAssignmentOp TAsteriskAssignment = MultiplyAssign
tokenToAssignmentOp TSlashAssignment = DivideAssign
tokenToAssignmentOp TPercentAssignment = RemainderAssign
tokenToAssignmentOp TBitwiseAndAssignment = BitwiseAndAssign
tokenToAssignmentOp TBitwiseOrAssignment = BitwiseOrAssign
tokenToAssignmentOp TBitwiseXorAssignment = BitwiseXorAssign
tokenToAssignmentOp TLeftShiftAssignment = LeftShiftAssign
tokenToAssignmentOp TRightShiftAssignment = RightShiftAssign
tokenToAssignmentOp t = error $ "Invalid assignment operator: " ++ show t

parseBinaryOp :: Int -> Parser BinaryOp
parseBinaryOp minPrec = satisfy (\t -> isBinOp t && opPrecedence (tokenToBinaryOp t) >= minPrec) <&> tokenToBinaryOp

parseAssignmentOp :: Int -> Parser AssignmentOp
parseAssignmentOp minPrec = satisfy (\t -> isAssignmentOp t && opPrecedence (tokenToAssignmentOp t) >= minPrec) <&> tokenToAssignmentOp

parsePostOp :: Parser PostOp
parsePostOp = satisfy isPostOp <&> tokenToPostOp

parseUnaryOp :: Parser UnaryOp
parseUnaryOp = satisfy isUnOp <&> tokenToUnaryOp

parseConditionalOp :: Int -> Parser ConditionalOp
parseConditionalOp minPrec = satisfy (\t -> t == TQuestionMark && opPrecedence ConditionalOp >= minPrec) $> ConditionalOp

tokenToUnaryOp :: Token -> UnaryOp
tokenToUnaryOp TTilde = Complement
tokenToUnaryOp THyphen = Negate
tokenToUnaryOp TExclamation = Not
tokenToUnaryOp TTwoPlus = PreIncrement
tokenToUnaryOp TTwoHyphens = PreDecrement
tokenToUnaryOp t = error $ "Invalid unary operator: " ++ show t

tokenToPostOp :: Token -> PostOp
tokenToPostOp TTwoPlus = PostIncrement
tokenToPostOp TTwoHyphens = PostDecrement
tokenToPostOp t = error $ "Invalid post operator: " ++ show t

parseIdentifier :: Parser Identifier
parseIdentifier = Parser $ \tokens -> case tokens of
  (TIdentifier s : ts) -> Right (Identifier s, ts)
  _ -> Left "Invalid identifier"

parseIntLiteral :: Parser IntLiteral
parseIntLiteral = Parser $ \tokens -> case tokens of
  (TConstant i : ts) -> Right (IntLiteral i, ts)
  _ -> Left "Invalid integer literal"

parseType :: Parser Type
parseType = Parser $ \tokens -> case tokens of
  (TIntKeyword : ts) -> Right (IntType, ts)
  (TVoidKeyword : ts) -> Right (VoidType, ts)
  _ -> Left "Invalid type"

parseParameter :: Parser Parameter
parseParameter = Parameter <$> parseType <*> parseIdentifier

parseDeclaration :: Parser Declaration
parseDeclaration = do
  parseToken TIntKeyword
  name <- parseIdentifier
  expr <- optional (parseToken TAssignment *> parseExpr)
  parseToken TSemicolon
  return (Declaration name expr)

parseUExpr :: Parser Expr
parseUExpr = Unary <$> parseUnaryOp <*> parseUExpr <|> parsePExpr

parsePExpr :: Parser Expr
parsePExpr = do
  pe <- parsePrimaryExpr
  ops <- many parsePostOp
  case ops of
    [] -> return pe
    _ -> return $ foldl PostFix pe ops

parsePrimaryExpr :: Parser Expr
parsePrimaryExpr =
  Constant <$> parseIntLiteral
    <|> Var <$> parseIdentifier
    <|> (parseToken TOpenParen *> parseExpr <* parseToken TCloseParen)

parseExpr :: Parser Expr
parseExpr = parseExprPrec 0

parseExprPrec :: Int -> Parser Expr
parseExprPrec minPrec = parseBAExpr minPrec <|> parseUExpr

parseBAExpr :: Int -> Parser Expr
parseBAExpr mp = do
  l <- parseUExpr <|> parsePrimaryExpr
  parseBAExprHelper l mp
  where
    parseBAExprHelper :: Expr -> Int -> Parser Expr
    parseBAExprHelper leftExpr minPrec = do
      next_op <-
        B <$> parseBinaryOp minPrec
          <|> A <$> parseAssignmentOp minPrec
          <|> C <$> parseConditionalOp minPrec
      case next_op of
        B op -> do
          rightExpr <- parseExprPrec (opPrecedence op + 1)
          let newLeft = Binary op leftExpr rightExpr
          parseBAExprHelper newLeft minPrec <|> return newLeft
        A op -> do
          rightExpr <- parseExprPrec (opPrecedence op)
          let newLeft = Assignment op leftExpr rightExpr
          parseBAExprHelper newLeft minPrec <|> return newLeft
        C op -> do
          middle <- parseExpr <* parseToken TColon
          right <- parseExprPrec (opPrecedence op)
          let newLeft = Conditional leftExpr middle right
          parseBAExprHelper newLeft minPrec <|> return newLeft
        _ -> empty

parseStmt :: Parser Stmt
parseStmt =
  ReturnStmt <$> (parseToken TReturnKeyword *> parseExpr <* parseToken TSemicolon)
    <|> NullStmt <$ parseToken TSemicolon
    <|> ExprStmt <$> (parseExpr <* parseToken TSemicolon)
    <|> parseIfStmt
    <|> parseLabelStmt
    <|> parseGotoStmt
    <|> parseBlockStmt

parseLabelStmt :: Parser Stmt
parseLabelStmt = do
  identifier <- parseIdentifier
  parseToken TColon
  _ <- try parseStmt
  return (LabelStmt identifier)

parseBlockStmt :: Parser Stmt
parseBlockStmt = CompoundStmt <$> parseBlock

parseGotoStmt :: Parser Stmt
parseGotoStmt = do
  parseToken TGotoKeyword
  identifier <- parseIdentifier
  parseToken TSemicolon
  return (GotoStmt identifier)

parseIfStmt :: Parser Stmt
parseIfStmt = do
  parseToken TIfKeyword
  parseToken TOpenParen
  condition <- parseExpr
  parseToken TCloseParen
  thenBlock <- parseStmt
  elseBlock <- optional (parseToken TElseKeyword *> parseStmt)
  return (IfStmt condition thenBlock elseBlock)

parseBlockItem :: Parser BlockItem
parseBlockItem =
  BlockStmt <$> parseStmt
    <|> BlockDecl <$> parseDeclaration

parseManyBlockItem :: Parser [BlockItem]
parseManyBlockItem = many parseBlockItem

parseBlock :: Parser Block
parseBlock = do
  parseToken TOpenBrace
  items <- many parseBlockItem
  parseToken TCloseBrace
  return (Block items)

parseFunction :: Parser Function
parseFunction = do
  parseToken TIntKeyword
  name <- parseIdentifier
  parseToken TOpenParen
  parseToken TVoidKeyword
  parseToken TCloseParen
  Function name <$> parseBlock

parseProgram :: Parser Program
parseProgram = do
  function <- parseFunction
  return (Program [function])

parseAll :: [Token] -> Program
parseAll tokens = case runParser parseProgram tokens of
  Right (program, []) -> program
  Right (_, rest) -> error $ "Unparsed tokens: " ++ show rest
  Left err -> error err
