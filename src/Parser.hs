{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use lambda-case" #-}

module Parser where

import AST
import Control.Applicative (Alternative (empty, many), many, optional, some, (<|>))
import Control.Monad (MonadPlus, guard, mzero, unless, void, when)
import Data.Functor (($>), (<&>))
import Data.List (partition)
import Data.Maybe (listToMaybe)
import qualified Data.Set as S
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

instance MonadFail Parser where
  fail :: String -> Parser a
  fail = Parser . const . Left

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
  (TIdentifier s : ts) -> Right (s, ts)
  _ -> Left $ "Expected identifier, got: " ++ show (take 10 tokens)

parseIntLiteral :: Parser IntLiteral
parseIntLiteral = Parser $ \tokens -> case tokens of
  (TConstant i : ts) -> Right (IntLiteral i, ts)
  _ -> Left $ "Expected integer literal, got: " ++ show (take 10 tokens)

parseVarDecl :: Parser VarDecl
parseVarDecl = do
  (variableType, storageClass) <- parseTypeAndStorageClass
  name <- parseIdentifier
  expr <- optional (parseToken TAssignment *> parseExpr)
  parseToken TSemicolon
  return (VarDecl name (toTypedExpr <$> expr) variableType storageClass)

toTypedExpr :: Expr -> TypedExpr
toTypedExpr e = TypedExpr e Nothing

parseUExpr :: Parser Expr
parseUExpr = Unary <$> parseUnaryOp <*> (toTypedExpr <$> parseUExpr) <|> parsePExpr <|> parseCastExpr

parsePExpr :: Parser Expr
parsePExpr = do
  pe <- parsePrimaryExpr
  ops <- many parsePostOp
  case ops of
    [] -> return pe
    _ -> do
      let tpe = toTypedExpr pe
      let fpo op ty = toTypedExpr $ PostFix ty op
      let pf = foldr fpo tpe ops
      return $ tyExpr pf

parsePrimaryExpr :: Parser Expr
parsePrimaryExpr =
  Constant <$> parseConst
    <|> parseFunctionCall
    <|> Var <$> parseIdentifier
    <|> (parseToken TOpenParen *> parseExpr <* parseToken TCloseParen)

parseExpr :: Parser Expr
parseExpr = parseExprPrec 0

parseFunctionCall :: Parser Expr
parseFunctionCall = FunctionCall <$> parseIdentifier <*> fmap (toTypedExpr <$>) parseArguments

parseExprPrec :: Int -> Parser Expr
parseExprPrec minPrec = parseBAExpr minPrec <|> parseUExpr

parseCastExpr :: Parser Expr
parseCastExpr = do
  parseToken TOpenParen
  t <- parseType
  parseToken TCloseParen
  Cast t . toTypedExpr <$> parseUExpr

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
          let newLeft = Binary op (TypedExpr leftExpr Nothing) (TypedExpr rightExpr Nothing)
          parseBAExprHelper newLeft minPrec <|> return newLeft
        A op -> do
          rightExpr <- parseExprPrec (opPrecedence op)
          let newLeft = Assignment op (TypedExpr leftExpr Nothing) (TypedExpr rightExpr Nothing)
          parseBAExprHelper newLeft minPrec <|> return newLeft
        C op -> do
          middle <- parseExpr <* parseToken TColon
          right <- parseExprPrec (opPrecedence op)
          let newLeft = Conditional (TypedExpr leftExpr Nothing) (TypedExpr middle Nothing) (TypedExpr right Nothing)
          parseBAExprHelper newLeft minPrec <|> return newLeft
        _ -> empty

parseTypedExpr :: Parser TypedExpr
parseTypedExpr = toTypedExpr <$> parseExpr

parseStmt :: Parser Stmt
parseStmt =
  ReturnStmt <$> (parseToken TReturnKeyword *> parseTypedExpr <* parseToken TSemicolon)
    <|> NullStmt <$ parseToken TSemicolon
    <|> ExprStmt <$> (parseTypedExpr <* parseToken TSemicolon)
    <|> parseIfStmt
    <|> parseLabeledStmt
    <|> parseGotoStmt
    <|> parseBlockStmt
    <|> parseBreakStmt
    <|> parseContinueStmt
    <|> parseWhileStmt
    <|> parseDoWhileStmt
    <|> parseForStmt
    <|> parseSwitchStmt
    <|> parseCaseStmt
    <|> parseDefaultStmt

parseForInit :: Parser ForInit
parseForInit = parseForInitDecl <|> parseForInitExpr
  where
    parseForInitDecl :: Parser ForInit
    parseForInitDecl =
      InitDecl
        <$> parseVarDecl
    parseForInitExpr :: Parser ForInit
    parseForInitExpr =
      InitExpr
        <$> optional parseTypedExpr
        <* parseToken TSemicolon

parseLabeledStmt :: Parser Stmt
parseLabeledStmt = do
  identifier <- parseIdentifier
  parseToken TColon
  LabeledStmt identifier <$> parseStmt

parseBlockStmt :: Parser Stmt
parseBlockStmt = CompoundStmt <$> parseBlock

parseBreakStmt :: Parser Stmt
parseBreakStmt =
  BreakStmt Nothing
    <$ parseToken TBreakKeyword
    <* parseToken TSemicolon

parseContinueStmt :: Parser Stmt
parseContinueStmt =
  ContinueStmt Nothing
    <$ parseToken TContinueKeyword
    <* parseToken TSemicolon

parseWhileStmt :: Parser Stmt
parseWhileStmt = do
  parseToken TWhileKeyword
  parseToken TOpenParen
  condition <- parseTypedExpr
  parseToken TCloseParen
  WhileStmt Nothing condition <$> parseStmt

parseDoWhileStmt :: Parser Stmt
parseDoWhileStmt = do
  parseToken TDoKeyword
  block <- parseStmt
  parseToken TWhileKeyword
  parseToken TOpenParen
  condition <- parseTypedExpr
  parseToken TCloseParen
  parseToken TSemicolon
  return (DoWhileStmt Nothing block condition)

parseForStmt :: Parser Stmt
parseForStmt = do
  parseToken TForKeyword
  parseToken TOpenParen
  forinit <- parseForInit
  condition <- optional parseTypedExpr
  parseToken TSemicolon
  iter <- optional parseTypedExpr
  parseToken TCloseParen
  ForStmt Nothing forinit condition iter <$> parseStmt

parseCaseStmt :: Parser Stmt
parseCaseStmt = do
  parseToken TCaseKeyword
  expr <- parseTypedExpr
  parseToken TColon
  CaseStmt Nothing expr <$> parseStmt

parseDefaultStmt :: Parser Stmt
parseDefaultStmt = do
  parseToken TDefaultKeyword
  parseToken TColon
  _ <- try parseStmt
  return $ DefaultStmt Nothing

parseSwitchStmt :: Parser Stmt
parseSwitchStmt = do
  parseToken TSwitchKeyword
  parseToken TOpenParen
  expr <- parseTypedExpr
  parseToken TCloseParen
  SwitchStmt Nothing S.empty False expr <$> parseStmt

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
  condition <- parseTypedExpr
  parseToken TCloseParen
  thenBlock <- parseStmt
  elseBlock <- optional (parseToken TElseKeyword *> parseStmt)
  return (IfStmt condition thenBlock elseBlock)

parseBlockItem :: Parser BlockItem
parseBlockItem =
  BlockStmt <$> parseStmt
    <|> BlockDecl <$> parseDecl

parseDecl :: Parser Decl
parseDecl = FDecl <$> parseFunction <|> VDecl <$> parseVarDecl

parseBlock :: Parser Block
parseBlock = do
  parseToken TOpenBrace
  items <- many parseBlockItem
  parseToken TCloseBrace
  return (Block items)

parseFunctionJustDecl :: Parser FuncDecl
parseFunctionJustDecl = do
  (functionReturnType, storageClass) <- parseTypeAndStorageClass
  name <- parseIdentifier
  (params, paramTypes) <- parseParameters
  let functionType = CFunc paramTypes functionReturnType
  parseToken TSemicolon
  return (FuncDecl name params Nothing functionType storageClass)

parseFunctionWithBody :: Parser FuncDecl
parseFunctionWithBody = do
  (functionReturnType, storageClass) <- parseTypeAndStorageClass
  name <- parseIdentifier
  (params, paramTypes) <- parseParameters
  let functionType = CFunc paramTypes functionReturnType
  FuncDecl name params . Just <$> parseBlock <*> pure functionType <*> pure storageClass

parseFunction :: Parser FuncDecl
parseFunction = parseFunctionJustDecl <|> parseFunctionWithBody

parseVoidParameter :: Parser ([Identifier], [CType])
parseVoidParameter = do
  parseToken TOpenParen
  parseToken TVoidKeyword
  parseToken TCloseParen
  return ([], [])

parseTypeSpecifier :: Parser Token
parseTypeSpecifier = Parser $ \tokens -> case tokens of
  (t : ts) ->
    if isTypeSpecifier t
      then Right (t, ts)
      else Left "No type specifier"
  _ -> Left "No type specifier"

isTypeSpecifier :: Token -> Bool
isTypeSpecifier TIntKeyword = True
isTypeSpecifier TLongKeyword = True
isTypeSpecifier _ = False

parseType :: Parser CType
parseType = do
  typeSpecifiers <- some parseTypeSpecifier
  case typeSpecifiers of
    [TIntKeyword] -> return CInt
    [TLongKeyword] -> return CLong
    [TIntKeyword, TLongKeyword] -> return CLong
    [TLongKeyword, TIntKeyword] -> return CLong
    _ -> fail "Invalid type specifier"

sepBy :: Parser a -> Parser b -> Parser [a]
sepBy p sep = (:) <$> p <*> many (sep *> p) <|> pure []

parseParameter :: Parser (Identifier, CType)
parseParameter = do
  t <- parseType
  name <- parseIdentifier
  return (name, t)

parseParameterList :: Parser ([Identifier], [CType])
parseParameterList = do
  parseToken TOpenParen
  params <- parseParameter `sepBy` parseToken TComma
  parseToken TCloseParen
  return (fst <$> params, snd <$> params)

parseParameters :: Parser ([Identifier], [CType])
parseParameters = parseParameterList <|> parseVoidParameter

parseSpecifier :: Parser Token
parseSpecifier = satisfy isSpecifier

parseTypeAndStorageClass :: Parser (CType, Maybe StorageClass)
parseTypeAndStorageClass = do
  specifiers <- some parseSpecifier
  let (typeSpecs, storageSpecs) = partition isTypeSpecifier specifiers
  validateSpecifiers typeSpecs storageSpecs
  pure (typeSpecsToType typeSpecs, listToMaybe (storageSpecToClass <$> storageSpecs))
  where
    validateSpecifiers :: [Token] -> [Token] -> Parser ()
    validateSpecifiers typeSpecs storageSpecs = do
      when (null typeSpecs) $ fail "No type specifier"
      when (length typeSpecs > 2) $ fail "Multiple type specifiers"
      when (length storageSpecs > 1) $ fail "Multiple storage class specifiers"
      unless (isValidTypeSpecs typeSpecs) $ fail "Invalid type specifier"
    isValidTypeSpecs :: [Token] -> Bool
    isValidTypeSpecs specs = specs `elem` validTypeCombos
      where
        validTypeCombos =
          [ [TIntKeyword],
            [TLongKeyword],
            [TIntKeyword, TLongKeyword],
            [TLongKeyword, TIntKeyword]
          ]
    typeSpecsToType :: [Token] -> CType
    typeSpecsToType specs
      | isLongType specs = CLong
      | specs == [TIntKeyword] = CInt
      | otherwise = error "Invalid type" -- Should never happen due to validation
      where
        isLongType ts =
          ts
            `elem` [ [TLongKeyword],
                     [TIntKeyword, TLongKeyword],
                     [TLongKeyword, TIntKeyword]
                   ]

    storageSpecToClass :: Token -> StorageClass
    storageSpecToClass = \case
      TExternKeyword -> Extern
      TStaticKeyword -> Static
      t -> error $ "Invalid storage class: " ++ show t

parseArguments :: Parser [Expr]
parseArguments = do
  parseToken TOpenParen
  first_arg <- optional parseExpr
  case first_arg of
    Nothing -> do
      parseToken TCloseParen
      return []
    Just arg -> do
      rest_args <- many (parseToken TComma *> parseExpr)
      parseToken TCloseParen
      return (arg : rest_args)

parseProgram :: Parser Program
parseProgram = Program <$> some parseDecl

maxInt :: Int
maxInt = 2 ^ 31 - 1

maxLong :: Int
maxLong = 2 ^ 63 - 1

orLeft :: Maybe a -> String -> Either String a
orLeft m msg = maybe (Left msg) Right m

parseConst :: Parser Const
parseConst = Parser $ \case
  TConstant i : ts -> do
    guard (i <= maxLong) `orLeft` "Constant is too large to represent as an int or long"
    Right ((if i <= maxInt then ConstInt else ConstLong) i, ts)
  TLongConstant i : ts -> do
    guard (i <= maxLong) `orLeft` "Constant is too large to represent as a long"
    Right (ConstLong i, ts)
  _ -> Left "Expected integer constant"

parse :: [Token] -> Program
parse tokens = case runParser parseProgram tokens of
  Right (program, []) -> program
  Right (_, rest) -> error $ "Unparsed tokens: " ++ show rest
  Left err -> error err
