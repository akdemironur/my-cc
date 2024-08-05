{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Codegen where

import AST
import Control.Monad.State
import Data.Map
import Lexer
import Parser
import System.Info (os)
import Tacky

data AsmProgram = AsmProgram [AsmFunction] deriving (Show, Eq)

data AsmFunction = AsmFunction String [AsmInstruction] deriving (Show, Eq)

data AsmInstruction
  = AsmMov AsmOperand AsmOperand
  | AsmUnary AsmUnaryOp AsmOperand
  | AsmAllocateStack Int
  | AsmRet
  deriving (Show, Eq)

data AsmUnaryOp = AsmNeg | AsmNot deriving (Show, Eq)

data AsmOperand = Imm Int | Register AsmReg | Pseudo String | Stack Int deriving (Show, Eq)

data AsmReg = AX | R10 deriving (Show, Eq)

class ToAsm a where
  toAsm :: a -> String

instance ToAsm AsmReg where
  toAsm :: AsmReg -> String
  toAsm AX = "%eax"
  toAsm R10 = "%r10d"

instance ToAsm AsmUnaryOp where
  toAsm :: AsmUnaryOp -> String
  toAsm AsmNeg = "negl"
  toAsm AsmNot = "notl"

instance ToAsm AsmOperand where
  toAsm :: AsmOperand -> String
  toAsm (Imm i) = "$" ++ show i
  toAsm (Register r) = toAsm r
  toAsm (Stack i) = show i ++ "(%rbp)"
  toAsm (Pseudo s) = error "Pseudo should not be used at emission"

instance ToAsm AsmInstruction where
  toAsm :: AsmInstruction -> String
  toAsm (AsmMov src dest) = "movl " ++ toAsm src ++ ", " ++ toAsm dest
  toAsm AsmRet = unlines ["movq %rbp, %rsp", "popq %rbp", "ret"]
  toAsm (AsmUnary op operand) = toAsm op ++ " " ++ toAsm operand
  toAsm (AsmAllocateStack i) = "subq $" ++ show i ++ ", %rsp"

instance ToAsm AsmFunction where
  toAsm :: AsmFunction -> String
  toAsm (AsmFunction name instructions) =
    ".globl " ++ prefix ++ name ++ "\n" ++ prefix ++ name ++ ":\n" ++ preInstr ++ unlines (fmap toAsm instructions)
    where
      preInstr = "pushq %rbp\nmovq %rsp, %rbp\n"
      prefix = case os of
        "darwin" -> "_"
        _ -> ""

instance ToAsm AsmProgram where
  toAsm :: AsmProgram -> String
  toAsm (AsmProgram [f]) = toAsm f ++ endL
    where
      endL = case os of
        "darwin" -> "\n"
        _ -> "\n.section .note.GNU-stack,\"\",@progbits\n"
  toAsm (AsmProgram []) = error "No functions to compile"
  toAsm _ = error "Multiple functions not supported yet"

codegenTVal :: TACVal -> AsmOperand
codegenTVal (TACConstant i) = Imm i
codegenTVal (TACVar name) = Pseudo name

codegenTUnaryOp :: TACUnaryOp -> AsmUnaryOp
codegenTUnaryOp TACNegate = AsmNeg
codegenTUnaryOp TACComplement = AsmNot

codegenTInstruction :: TACInstruction -> [AsmInstruction]
codegenTInstruction (TACReturn val) = [AsmMov (codegenTVal val) (Register AX), AsmRet]
codegenTInstruction (TACUnary op src dst) = [AsmMov (codegenTVal src) (codegenTVal dst), AsmUnary (codegenTUnaryOp op) (codegenTVal dst)]

codegenTFunction :: TACFunction -> AsmFunction
codegenTFunction (TACFunction name instructions) = AsmFunction name (concatMap codegenTInstruction instructions)

codegenTProgram :: TACProgram -> AsmProgram
codegenTProgram (TACProgram [f]) = AsmProgram [codegenTFunction f]
codegenTProgram _ = error "Multiple functions not supported yet"

type StackOffset = Int

type PseudoOffsetMap = Map String StackOffset

type PseudoRegisterState = State (PseudoOffsetMap, Int)

class PseudeRegisterPass a where
  pseudoRegisterPass :: a -> PseudoRegisterState a

instance PseudeRegisterPass AsmOperand where
  pseudoRegisterPass :: AsmOperand -> PseudoRegisterState AsmOperand
  pseudoRegisterPass (Pseudo s) = do
    (m, i) <- get
    case Data.Map.lookup s m of
      Just offset -> return $ Stack offset
      Nothing -> do
        let newMap = Data.Map.insert s (-(4 * (i + 1))) m
        put (newMap, i + 1)
        return $ Stack (-(4 * (i + 1)))
  pseudoRegisterPass x = return x

instance PseudeRegisterPass AsmInstruction where
  pseudoRegisterPass :: AsmInstruction -> PseudoRegisterState AsmInstruction
  pseudoRegisterPass (AsmMov src dest) = do
    src' <- pseudoRegisterPass src
    dest' <- pseudoRegisterPass dest
    return $ AsmMov src' dest'
  pseudoRegisterPass (AsmUnary op operand) = do
    operand' <- pseudoRegisterPass operand
    return $ AsmUnary op operand'
  pseudoRegisterPass x = return x

instance PseudeRegisterPass AsmFunction where
  pseudoRegisterPass :: AsmFunction -> PseudoRegisterState AsmFunction
  pseudoRegisterPass (AsmFunction name instructions) = do
    instructions' <- mapM pseudoRegisterPass instructions
    return $ AsmFunction name instructions'

instance PseudeRegisterPass AsmProgram where
  pseudoRegisterPass :: AsmProgram -> PseudoRegisterState AsmProgram
  pseudoRegisterPass (AsmProgram [f]) = do
    f' <- pseudoRegisterPass f
    return $ AsmProgram [f']
  pseudoRegisterPass _ = error "Multiple functions not supported yet"

replacePseudoRegisters :: AsmProgram -> (AsmProgram, StackOffset)
replacePseudoRegisters p = (p', offset)
  where
    (p', (_, offset)) = runState (pseudoRegisterPass p) (Data.Map.empty, 0)

addAllocateStack :: (AsmProgram, StackOffset) -> AsmProgram
addAllocateStack (AsmProgram [AsmFunction fname instrs], offset) = AsmProgram [AsmFunction fname newInstrs]
  where
    newInstrs = AsmAllocateStack (offset * 4) : instrs
addAllocateStack _ = error "Multiple functions not supported yet"

removeInvalidInstructions :: AsmProgram -> AsmProgram
removeInvalidInstructions (AsmProgram [AsmFunction fname instrs]) = AsmProgram [AsmFunction fname (fixInvalid instrs [])]
  where
    fixInvalid [] newInstrs = newInstrs
    fixInvalid (AsmMov (Stack i) (Stack j) : xs) newInstrs = fixInvalid xs (newInstrs ++ [AsmMov (Stack i) (Register R10), AsmMov (Register R10) (Stack j)])
    fixInvalid (x : xs) newInstrs = fixInvalid xs (newInstrs ++ [x])
removeInvalidInstructions _ = error "Multiple functions not supported yet"

codegen :: TACProgram -> AsmProgram
codegen = removeInvalidInstructions . addAllocateStack . replacePseudoRegisters . codegenTProgram