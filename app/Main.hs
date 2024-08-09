{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Main where

import AST ()
import Codegen
import Data.Functor ((<&>))
import Lexer (lex)
import Parser (parseProgram)
import System.Directory (removeFile)
import System.Environment (getArgs)
import System.Exit (ExitCode (..), exitWith)
import System.FilePath (dropExtension, replaceExtension)
import System.Info (arch)
import System.Process (callCommand)
import Tacky
import Prelude hiding (lex)

preprocess :: FilePath -> IO String
preprocess inputFile = do
  let preprocessedFile = replaceExtension inputFile "i"
  callCommand $ gcc ++ "-E -P " ++ inputFile ++ " -o " ++ preprocessedFile
  contents <- readFile preprocessedFile
  removeFile preprocessedFile
  return contents

gcc :: String
gcc = case arch of
  "aarch64" -> "arch -x86_64 gcc "
  _ -> "gcc "

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["--lex", inputFile] -> preprocess inputFile >>= print . lex
    ["--parse", inputFile] -> preprocess inputFile >>= print . parseProgram . lex
    ["--codegen", inputFile] -> preprocess inputFile >>= print . codeEmission . codegen . toTACProg . parseProgram . lex
    ["--tacky", inputFile] -> preprocess inputFile >>= print . toTACProg . parseProgram . lex
    ["-S", inputFile] -> compileToAssembly inputFile
    [inputFile] -> compileAndLink inputFile
    _ -> do
      putStrLn "Usage: my-cc [--lex|--parse|--codegen|--tacky|-S] <input_file>"
      exitWith (ExitFailure 1)

compileAndLink :: FilePath -> IO ()
compileAndLink inputFile = do
  compileToAssembly inputFile
  callCommand $ gcc ++ assemblyFile ++ " -o " ++ outputFile
  removeFile assemblyFile
  where
    assemblyFile :: FilePath
    assemblyFile = replaceExtension inputFile "s"
    outputFile :: FilePath
    outputFile = dropExtension inputFile

compileToAssembly :: FilePath -> IO ()
compileToAssembly inputFile = do
  assemblyContent <- preprocess inputFile <&> (codeEmission . codegen . toTACProg . parseProgram . lex)
  assemblyContent `seq` writeFile assemblyFile assemblyContent
  where
    assemblyFile :: FilePath
    assemblyFile = replaceExtension inputFile "s"
