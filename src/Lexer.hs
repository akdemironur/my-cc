{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Lexer where

import Data.Char (isSpace)
import Data.Maybe (mapMaybe)
import Text.Regex.PCRE ((=~))

data Token
  = TIdentifier String
  | TConstant Int
  | TIntKeyword
  | TVoidKeyword
  | TReturnKeyword
  | TOpenParen
  | TCloseParen
  | TOpenBrace
  | TCloseBrace
  | TSemicolon
  deriving (Show, Eq)

type TokenRegex = (String, String -> Token)

tokenRegexes :: [TokenRegex]
tokenRegexes =
  [ ("\\Aint\\b", const TIntKeyword),
    ("\\Avoid\\b", const TVoidKeyword),
    ("\\Areturn\\b", const TReturnKeyword),
    ("\\A[a-zA-Z_]\\w*\\b", TIdentifier),
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
lexer input' = case matches of
  (token, rest) : _ -> token : lexer rest
  [] -> if null input then [] else error $ "No match found for input: " ++ input'
  where
    input = dropWhile isSpace input'
    matches = mapMaybe (matchRegex input) tokenRegexes
