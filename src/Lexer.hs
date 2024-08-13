{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Lexer where

import Data.Char (isSpace)
import Data.List (intercalate)
import Data.Maybe (mapMaybe)
import Text.Regex.PCRE ((=~))
import Prelude hiding (lex)

data Token
  = TIdentifier String
  | TConstant Int
  | TOpenParen
  | TCloseParen
  | TOpenBrace
  | TCloseBrace
  | TSemicolon
  | TTilde
  | THyphen
  | TExclamation
  | TTwoHyphens
  | TPlus
  | TTwoPlus
  | TAsterisk
  | TSlash
  | TPercent
  | TBitwiseAnd
  | TBitwiseOr
  | TBitwiseXor
  | TLeftShift
  | TRightShift
  | TLogicalAnd
  | TLogicalOr
  | TEqualTo
  | TNotEqualTo
  | TLessThan
  | TLessThanOrEqualTo
  | TGreaterThan
  | TGreaterThanOrEqualTo
  | TAssignment
  | TPlusAssignment
  | THyphenAssignment
  | TAsteriskAssignment
  | TSlashAssignment
  | TPercentAssignment
  | TBitwiseAndAssignment
  | TBitwiseOrAssignment
  | TBitwiseXorAssignment
  | TLeftShiftAssignment
  | TRightShiftAssignment
  | TIntKeyword
  | TVoidKeyword
  | TReturnKeyword
  | TIfKeyword
  | TElseKeyword
  | TQuestionMark
  | TColon
  | TGotoKeyword
  deriving (Show, Eq)

keywords :: [String]
keywords =
  [ "int",
    "void",
    "return",
    "if",
    "else",
    "goto"
  ]

identifierRegex :: String
identifierRegex = "\\A(?!(" ++ intercalate "|" keywords ++ ")\\b)[a-zA-Z_]\\w*\\b"

type TokenRegex = (String, String -> Token)

isBinOp :: Token -> Bool
isBinOp TPlus = True
isBinOp THyphen = True
isBinOp TAsterisk = True
isBinOp TSlash = True
isBinOp TPercent = True
isBinOp TBitwiseAnd = True
isBinOp TBitwiseOr = True
isBinOp TBitwiseXor = True
isBinOp TLeftShift = True
isBinOp TRightShift = True
isBinOp TLogicalAnd = True
isBinOp TLogicalOr = True
isBinOp TEqualTo = True
isBinOp TNotEqualTo = True
isBinOp TLessThan = True
isBinOp TLessThanOrEqualTo = True
isBinOp TGreaterThan = True
isBinOp TGreaterThanOrEqualTo = True
isBinOp _ = False

isPostOp :: Token -> Bool
isPostOp TTwoPlus = True
isPostOp TTwoHyphens = True
isPostOp _ = False

isUnOp :: Token -> Bool
isUnOp TTilde = True
isUnOp THyphen = True
isUnOp TTwoHyphens = True
isUnOp TTwoPlus = True
isUnOp TExclamation = True
isUnOp _ = False

isAssignmentOp :: Token -> Bool
isAssignmentOp TAssignment = True
isAssignmentOp TPlusAssignment = True
isAssignmentOp THyphenAssignment = True
isAssignmentOp TAsteriskAssignment = True
isAssignmentOp TSlashAssignment = True
isAssignmentOp TPercentAssignment = True
isAssignmentOp TBitwiseAndAssignment = True
isAssignmentOp TBitwiseOrAssignment = True
isAssignmentOp TBitwiseXorAssignment = True
isAssignmentOp TLeftShiftAssignment = True
isAssignmentOp TRightShiftAssignment = True
isAssignmentOp _ = False

tokenRegexes :: [TokenRegex]
tokenRegexes =
  [ (identifierRegex, TIdentifier),
    ("\\Aint\\b", const TIntKeyword),
    ("\\Avoid\\b", const TVoidKeyword),
    ("\\Areturn\\b", const TReturnKeyword),
    ("\\Aif\\b", const TIfKeyword),
    ("\\Aelse\\b", const TElseKeyword),
    ("\\Agoto\\b", const TGotoKeyword),
    ("\\A~", const TTilde),
    ("\\A--", const TTwoHyphens),
    ("\\A\\+\\+", const TTwoPlus),
    ("\\A-(?!(-|=))", const THyphen),
    ("\\A\\+(?!(=|\\+))", const TPlus),
    ("\\A\\*(?!=)", const TAsterisk),
    ("\\A/(?!=)", const TSlash),
    ("\\A%(?!=)", const TPercent),
    ("\\A[0-9]+\\b", TConstant . read),
    ("\\A\\(", const TOpenParen),
    ("\\A\\)", const TCloseParen),
    ("\\A{", const TOpenBrace),
    ("\\A}", const TCloseBrace),
    ("\\A;", const TSemicolon),
    ("\\A&(?!(&|=))", const TBitwiseAnd),
    ("\\A\\|(?!(\\||=))", const TBitwiseOr),
    ("\\A\\^(?!(\\^|=))", const TBitwiseXor),
    ("\\A<<(?!=)", const TLeftShift),
    ("\\A>>(?!=)", const TRightShift),
    ("\\A&&", const TLogicalAnd),
    ("\\A\\|\\|", const TLogicalOr),
    ("\\A==", const TEqualTo),
    ("\\A!=", const TNotEqualTo),
    ("\\A<=", const TLessThanOrEqualTo),
    ("\\A<(?!(<|=))", const TLessThan),
    ("\\A>=", const TGreaterThanOrEqualTo),
    ("\\A>(?!(>|=))", const TGreaterThan),
    ("\\A\\!(?!=)", const TExclamation),
    ("\\A=(?!=)", const TAssignment),
    ("\\A\\+=", const TPlusAssignment),
    ("\\A-=", const THyphenAssignment),
    ("\\A\\*=", const TAsteriskAssignment),
    ("\\A/=", const TSlashAssignment),
    ("\\A%=", const TPercentAssignment),
    ("\\A&=", const TBitwiseAndAssignment),
    ("\\A\\|=", const TBitwiseOrAssignment),
    ("\\A\\^=", const TBitwiseXorAssignment),
    ("\\A<<=", const TLeftShiftAssignment),
    ("\\A>>=", const TRightShiftAssignment),
    ("\\A\\?", const TQuestionMark),
    ("\\A:", const TColon)
  ]

matchRegex :: String -> TokenRegex -> Maybe (Token, String)
matchRegex input (regex, makeToken) =
  case input =~ regex :: (String, String, String) of
    (_, match, rest)
      | not (null match) -> Just (makeToken match, rest)
    _ -> Nothing

lex :: String -> [Token]
lex "" = []
lex input'
  | null input = []
  | null matches = error $ "No match found for input: " ++ input'
  | otherwise = (fst . head $ matches) : lex (snd . head $ matches)
  where
    input = dropWhile isSpace input'
    matches = mapMaybe (matchRegex input) tokenRegexes
