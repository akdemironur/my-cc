{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Lexer where

import Data.Char (isDigit, isSpace)
import Data.List (intercalate, isInfixOf)
import Data.Maybe (mapMaybe)
import Text.Regex.PCRE (RegexContext (match), (=~))
import Prelude hiding (lex)

data Token
  = TIdentifier String
  | TConstant Integer
  | TUnsignedConstant Integer
  | TLongConstant Integer
  | TUnsignedLongConstant Integer
  | TDoubleConstant Double
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
  | TDoKeyword
  | TWhileKeyword
  | TForKeyword
  | TBreakKeyword
  | TContinueKeyword
  | TSwitchKeyword
  | TCaseKeyword
  | TDefaultKeyword
  | TComma
  | TExternKeyword
  | TStaticKeyword
  | TLongKeyword
  | TUnsignedKeyword
  | TSignedKeyword
  | TDoubleKeyword
  deriving
    ( Show,
      Eq
    )

keywords :: [String]
keywords =
  [ "int",
    "void",
    "return",
    "if",
    "else",
    "goto",
    "do",
    "while",
    "for",
    "break",
    "continue",
    "switch",
    "case",
    "default",
    "extern",
    "static",
    "long",
    "unsigned",
    "signed",
    "double"
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

isSpecifier :: Token -> Bool
isSpecifier t = isStorageSpecifier t || isTypeSpecifier t

isStorageSpecifier :: Token -> Bool
isStorageSpecifier TStaticKeyword = True
isStorageSpecifier TExternKeyword = True
isStorageSpecifier _ = False

isTypeSpecifier :: Token -> Bool
isTypeSpecifier TIntKeyword = True
isTypeSpecifier TLongKeyword = True
isTypeSpecifier TSignedKeyword = True
isTypeSpecifier TUnsignedKeyword = True
isTypeSpecifier TDoubleKeyword = True
isTypeSpecifier _ = False

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
    ("\\A(([0-9]*\\.[0-9]+|[0-9]+\\.?)[Ee][+-]?[0-9]+|[0-9]*\\.[0-9]+|[0-9]+\\.)[^\\w.]", TDoubleConstant . read . makeDoubleLiteral),
    ("\\A[0-9]+([lL][uU]|[uU][lL])\\b", TUnsignedLongConstant . read . init . init),
    ("\\A[0-9]+\\b", TConstant . read),
    ("\\A[0-9]+(l|L)\\b", TLongConstant . read . init),
    ("\\A[0-9]+(u|U)\\b", TUnsignedConstant . read . init),
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
    ("\\A:", const TColon),
    ("\\Ado\\b", const TDoKeyword),
    ("\\Awhile\\b", const TWhileKeyword),
    ("\\Afor\\b", const TForKeyword),
    ("\\Abreak\\b", const TBreakKeyword),
    ("\\Acontinue\\b", const TContinueKeyword),
    ("\\Aswitch\\b", const TSwitchKeyword),
    ("\\Acase\\b", const TCaseKeyword),
    ("\\Adefault\\b", const TDefaultKeyword),
    ("\\A,", const TComma),
    ("\\Aextern\\b", const TExternKeyword),
    ("\\Astatic\\b", const TStaticKeyword),
    ("\\Along\\b", const TLongKeyword),
    ("\\Aunsigned\\b", const TUnsignedKeyword),
    ("\\Asigned\\b", const TSignedKeyword),
    ("\\Adouble\\b", const TDoubleKeyword)
  ]

trim :: String -> String
trim = f . f
  where
    f = reverse . dropWhile isSpace

replace :: Char -> String -> String -> String
replace target replacement input =
  let parts = splitOn target input
   in intercalate replacement parts
  where
    splitOn :: Char -> String -> [String]
    splitOn _ "" = [""]
    splitOn delim str = case break (== delim) str of
      (before, after) ->
        before : case after of
          [] -> []
          xs -> splitOn delim (tail xs)

matchRegex :: String -> TokenRegex -> Maybe (Token, String, [String], String)
matchRegex input (regex, makeToken) =
  case input =~ regex :: (String, String, String, [String]) of
    (_, match', rest, groups)
      | not (null match') -> Just (makeToken match', rest, groups, match')
    _ -> Nothing

dropWhileSame :: (Eq a) => [a] -> [a] -> [a]
dropWhileSame ys [] = ys
dropWhileSame [] xs = xs
dropWhileSame (y : ys) (x : xs)
  | x == y = dropWhileSame ys xs
  | otherwise = x : xs

takeWhileSame :: (Eq a) => [a] -> [a] -> [a]
takeWhileSame _ [] = []
takeWhileSame [] _ = []
takeWhileSame (y : ys) (x : xs)
  | x == y = x : takeWhileSame ys xs
  | otherwise = []

lex :: String -> [Token]
lex "" = []
lex input'
  | null input = []
  | null matches = error $ "No match found for input: " ++ input'
  | otherwise = token : lex rest'
  where
    input = dropWhile isSpace input'
    matches = mapMaybe (matchRegex input) tokenRegexes
    (token, rest, groups, match') = head matches
    rest' = case token of
      TDoubleConstant _ -> dropWhileSame (head groups) match' ++ rest
      _ -> rest

makeDoubleLiteral :: String -> String
makeDoubleLiteral s
  | head s == '.' = makeDoubleLiteral ("0" ++ s)
  | last s == '.' = makeDoubleLiteral (s ++ "0")
  | ".e" `isInfixOf` s || ".E" `isInfixOf` s = makeDoubleLiteral (replace '.' ".0" s)
  | not (isDigit (last s)) = makeDoubleLiteral (init s)
  | otherwise = s