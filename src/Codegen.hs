{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Codegen where

import AST
import Lexer
import Parser

data AsmProgram = AsmProgram [AsmFunction] deriving (Show, Eq)

data AsmFunction = AsmFunction String [AsmInstruction] deriving (Show, Eq)

data AsmInstruction = AsmMov AsmOperand AsmOperand | AsmRet deriving (Show, Eq)

data AsmOperand = AsmImm Int | AsmReg deriving (Show, Eq)

class ToAsm a where
  toAsm :: a -> String

instance ToAsm AsmOperand where
  toAsm :: AsmOperand -> String
  toAsm (AsmImm i) = "$" ++ show i
  toAsm AsmReg = "%eax"

instance ToAsm AsmInstruction where
  toAsm :: AsmInstruction -> String
  toAsm (AsmMov src dest) = "movl " ++ toAsm src ++ ", " ++ toAsm dest
  toAsm AsmRet = "ret"

instance ToAsm AsmFunction where
  toAsm :: AsmFunction -> String
  toAsm (AsmFunction name instructions) =
    ".globl " ++ "_" ++ name ++ "\n" ++ "_" ++ name ++ ":\n" ++ unlines (map toAsm instructions)

instance ToAsm AsmProgram where
  toAsm :: AsmProgram -> String
  toAsm (AsmProgram [f]) = toAsm f
  toAsm (AsmProgram []) = error "No functions to compile"
  toAsm _ = error "Multiple functions not supported yet"

codegenExpr :: Expression -> [AsmInstruction]
codegenExpr (ConstantExpression (IntLiteral i)) = [AsmMov (AsmImm i) AsmReg]

codegenStmt :: Statement -> [AsmInstruction]
codegenStmt (ReturnStatement e) = codegenExpr e ++ [AsmRet]

codegenFunc :: Function -> AsmFunction
codegenFunc (Function _ name _ stmts) = AsmFunction name (concatMap codegenStmt stmts)

codegenProg :: Program -> AsmProgram
codegenProg (Program funcs) = outputAsm `seq` outputAsm
  where
    outputAsm = AsmProgram (map codegenFunc funcs)
