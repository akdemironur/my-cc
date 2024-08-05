{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Lexer where

import Data.Char (isSpace)
import Data.List (intercalate)
import Data.Maybe (mapMaybe)
import Text.Regex.PCRE ((=~))

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
  | TTwoHyphens
  | TIntKeyword
  | TVoidKeyword
  | TReturnKeyword
  deriving (Show, Eq)

keywords :: [String]
keywords = ["int", "void", "return"]

identifierRegex :: String
identifierRegex = "\\A(?!(" ++ intercalate "|" keywords ++ ")\\b)[a-zA-Z_]\\w*\\b"

type TokenRegex = (String, String -> Token)

tokenRegexes :: [TokenRegex]
tokenRegexes =
  [ (identifierRegex, TIdentifier),
    ("\\Aint\\b", const TIntKeyword),
    ("\\Avoid\\b", const TVoidKeyword),
    ("\\Areturn\\b", const TReturnKeyword),
    ("\\A~", const TTilde),
    ("\\A--", const TTwoHyphens),
    ("\\A-(?!-)", const THyphen),
    ("\\A[0-9]+\\b", TConstant . read),
    ("\\A\\(", const TOpenParen),
    ("\\A\\)", const TCloseParen),
    ("\\A{", const TOpenBrace),
    ("\\A}", const TCloseBrace),
    ("\\A;", const TSemicolon)
  ]

matchRegex :: String -> TokenRegex -> Maybe (Token, String)
matchRegex input (regex, makeToken) =
  case input =~ regex :: (String, String, String) of
    (_, match, rest)
      | not (null match) -> Just (makeToken match, rest)
    _ -> Nothing

lexer :: String -> [Token]
lexer "" = []
lexer input'
  | null input = []
  | null matches = error $ "No match found for input: " ++ input'
  | otherwise = (fst . head $ matches) : lexer (snd . head $ matches)
  where
    input = dropWhile isSpace input'
    matches = mapMaybe (matchRegex input) tokenRegexes
