{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Main where

import AST ()
import Codegen
import Data.Functor ((<&>))
import Lexer (lex)
import Parser (parse)
import Pass
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
  callCommand $ unwords [gcc, "-E", "-P", inputFile, "-o", preprocessedFile]
  contents <- readFile preprocessedFile
  removeFile preprocessedFile
  return contents

gcc :: String
gcc = case arch of
  "aarch64" -> "arch -x86_64 gcc"
  _ -> "gcc"

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["--lex", inputFile] ->
      preprocess inputFile
        >>= print
          . lex
    ["--parse", inputFile] ->
      preprocess inputFile
        >>= print
          . parse
          . lex
    ["--validate", inputFile] ->
      preprocess inputFile
        >>= print
          . resolveAll
          . parse
          . lex
    ["--codegen", inputFile] ->
      preprocess inputFile
        >>= print
          . codegen
          . toTACProg
          . resolveAll
          . parse
          . lex
    ["--tacky", inputFile] ->
      preprocess inputFile
        >>= print
          . toTACProg
          . resolveAll
          . parse
          . lex
    ["-S", inputFile] -> compileToAssembly inputFile
    ["-c", inputFile] -> compileAndLink "-c" inputFile
    [inputFile] -> compileAndLink "" inputFile
    _ -> do
      putStrLn "Usage: my-cc [--lex|--parse|--codegen|--tacky|-S|-c] <input_file>"
      exitWith (ExitFailure 1)

compileAndLink :: String -> FilePath -> IO ()
compileAndLink c_opt inputFile = do
  compileToAssembly inputFile
  callCommand $ unwords [gcc, c_opt, assemblyFile, "-o", outputFile]
  removeFile assemblyFile
  where
    assemblyFile :: FilePath
    assemblyFile = replaceExtension inputFile "s"
    outputFile :: FilePath
    outputFile = dropExtension inputFile ++ if c_opt == "-c" then ".o" else ""

compileToAssembly :: FilePath -> IO ()
compileToAssembly inputFile = do
  assemblyContent <-
    preprocess inputFile
      <&> ( codeEmission
              . fst
              . codegen
              . toTACProg
              . resolveAll
              . parse
              . lex
          )
  assemblyContent `seq` writeFile assemblyFile assemblyContent
  where
    assemblyFile :: FilePath
    assemblyFile = replaceExtension inputFile "s"
