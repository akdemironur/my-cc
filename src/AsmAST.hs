{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module AsmAST where

import AST
import qualified Data.Map as M

newtype AsmProgram = AsmProgram [AsmTopLevel] deriving (Show, Eq)

data AsmType = Longword | Quadword deriving (Show, Eq)

data AsmSymbolTableEntry
  = Obj AsmType Bool Bool -- isStatic isSigned
  | Fun Bool -- isDefined
  deriving (Show, Eq)

type AsmSymbolTable = M.Map Identifier AsmSymbolTableEntry

data AsmTopLevel
  = AsmFunction String Bool [AsmInstruction] -- identifier isDefined instructions
  | AsmStaticVariable String Bool Integer StaticInit -- identifier isStatic alignment initial
  deriving (Show, Eq)

data AsmCondCode
  = E
  | NE
  | L
  | LE
  | G
  | GE
  | A
  | AE
  | B
  | BE
  deriving (Show, Eq)

type Offset = Int

data AsmInstruction
  = AsmMov AsmType AsmOperand AsmOperand
  | AsmMovsx AsmOperand AsmOperand
  | AsmMovZeroExtend AsmOperand AsmOperand
  | AsmUnary AsmUnaryOp AsmType AsmOperand
  | AsmBinary AsmBinaryOp AsmType AsmOperand AsmOperand
  | AsmCmp AsmType AsmOperand AsmOperand
  | AsmIdiv AsmType AsmOperand
  | AsmDiv AsmType AsmOperand
  | AsmCdq AsmType
  | AsmJmp String
  | AsmJmpCC AsmCondCode String
  | AsmSetCC AsmCondCode AsmOperand
  | AsmLabel String
  | AsmPush AsmOperand
  | AsmCall String
  | AsmRet
  deriving (Show, Eq)

data AsmUnaryOp
  = AsmNeg
  | AsmNot
  deriving (Show, Eq)

data AsmBinaryOp
  = AsmAdd
  | AsmSub
  | AsmMult
  | AsmXor
  | AsmAnd
  | AsmOr
  | AsmSal
  | AsmSar
  | AsmShr
  deriving (Show, Eq)

data AsmOperand
  = Imm Integer
  | Register AsmReg
  | Pseudo String
  | Stack Integer
  | Data String
  deriving (Show, Eq)

data AsmReg
  = AX
  | CX
  | DX
  | DI
  | SI
  | R8
  | R9
  | R10
  | R11
  | SP
  deriving (Show, Eq)