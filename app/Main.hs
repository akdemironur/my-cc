{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Main where

import AST ()
import Codegen
import Data.Functor ((<&>))
import Lexer (lexer)
import Parser (parseProgram)
import System.Directory (removeFile)
import System.Environment (getArgs)
import System.Exit (ExitCode (..), exitWith)
import System.FilePath (dropExtension, replaceExtension)
import System.Process (callCommand)

preprocess :: FilePath -> IO String
preprocess inputFile = do
  let preprocessedFile = replaceExtension inputFile "i"
  callCommand $ "gcc -E -P " ++ inputFile ++ " -o " ++ preprocessedFile
  contents <- readFile preprocessedFile
  removeFile preprocessedFile
  return contents

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["--lex", inputFile] -> preprocess inputFile >>= print . lexer
    ["--parse", inputFile] -> preprocess inputFile >>= print . parseProgram . lexer
    ["--codegen", inputFile] -> preprocess inputFile >>= print . codegenProg . parseProgram . lexer
    ["-S", inputFile] -> compileToAssembly inputFile
    [inputFile] -> compileAndLink inputFile
    _ -> do
      putStrLn "Usage: my-cc [--lex|--parse|--codegen|-S] <input_file>"
      exitWith (ExitFailure 1)

compileAndLink :: FilePath -> IO ()
compileAndLink inputFile = do
  compileToAssembly inputFile
  callCommand $ "arch -x86_64 gcc " ++ assemblyFile ++ " -o " ++ outputFile
  removeFile assemblyFile
  where
    assemblyFile :: FilePath
    assemblyFile = replaceExtension inputFile "s"
    outputFile :: FilePath
    outputFile = dropExtension inputFile

compileToAssembly :: FilePath -> IO ()
compileToAssembly inputFile = do
  assemblyContent <- preprocess inputFile <&> (toAsm . codegenProg . parseProgram . lexer)
  assemblyContent `seq` writeFile assemblyFile assemblyContent
  where
    assemblyFile :: FilePath
    assemblyFile = replaceExtension inputFile "s"
