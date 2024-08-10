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

labelPrefix :: String
labelPrefix = case os of
  "darwin" -> "_"
  _ -> ".L"

data AsmCondCode
  = E
  | NE
  | L
  | LE
  | G
  | GE
  deriving (Show, Eq)

instance ToAsm AsmCondCode where
  codeEmission :: AsmCondCode -> String
  codeEmission E = "e"
  codeEmission NE = "ne"
  codeEmission L = "l"
  codeEmission LE = "le"
  codeEmission G = "g"
  codeEmission GE = "ge"

data AsmInstruction
  = AsmMov AsmOperand AsmOperand
  | AsmUnary AsmUnaryOp AsmOperand
  | AsmBinary AsmBinaryOp AsmOperand AsmOperand
  | AsmCmp AsmOperand AsmOperand
  | AsmIdiv AsmOperand
  | AsmCdq
  | AsmJmp String
  | AsmJmpCC AsmCondCode String
  | AsmSetCC AsmCondCode AsmOperand
  | AsmLabel String
  | AsmAllocateStack Int
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
  deriving (Show, Eq)

data AsmOperand
  = Imm Int
  | Register AsmReg
  | Pseudo String
  | Stack Int
  deriving (Show, Eq)

data AsmReg
  = AX
  | DX
  | R10
  | R11
  | CX
  deriving (Show, Eq)

class ToAsm a where
  codeEmission :: a -> String

instance ToAsm AsmReg where
  codeEmission :: AsmReg -> String
  codeEmission AX = "%eax"
  codeEmission R10 = "%r10d"
  codeEmission R11 = "%r11d"
  codeEmission DX = "%edx"
  codeEmission CX = "%ecx"

codeEmissionReg1 :: AsmReg -> String
codeEmissionReg1 AX = "%al"
codeEmissionReg1 DX = "%dl"
codeEmissionReg1 R10 = "%r10b"
codeEmissionReg1 R11 = "%r11b"
codeEmissionReg1 CX = "%cl"

instance ToAsm AsmUnaryOp where
  codeEmission :: AsmUnaryOp -> String
  codeEmission AsmNeg = "negl"
  codeEmission AsmNot = "notl"

instance ToAsm AsmBinaryOp where
  codeEmission :: AsmBinaryOp -> String
  codeEmission AsmAdd = "addl"
  codeEmission AsmSub = "subl"
  codeEmission AsmMult = "imull"
  codeEmission AsmXor = "xorl"
  codeEmission AsmAnd = "andl"
  codeEmission AsmOr = "orl"
  codeEmission AsmSal = "sall"
  codeEmission AsmSar = "sarl"

instance ToAsm AsmOperand where
  codeEmission :: AsmOperand -> String
  codeEmission (Imm i) = "$" ++ show i
  codeEmission (Register r) = codeEmission r
  codeEmission (Stack i) = show i ++ "(%rbp)"
  codeEmission (Pseudo _) = error "Pseudo should not be used at emission"

instance ToAsm AsmInstruction where
  codeEmission :: AsmInstruction -> String
  codeEmission (AsmLabel label) = labelPrefix ++ label ++ ":"
  codeEmission (AsmMov src dst) = "    " ++ "movl " ++ codeEmission src ++ ", " ++ codeEmission dst
  codeEmission AsmRet = unlines (fmap ("    " ++) ["movq %rbp, %rsp", "popq %rbp", "ret"])
  codeEmission (AsmUnary op operand) = "    " ++ codeEmission op ++ " " ++ codeEmission operand
  codeEmission (AsmAllocateStack i) = "    " ++ "subq $" ++ show i ++ ", %rsp"
  codeEmission AsmCdq = "    " ++ "cdq"
  codeEmission (AsmBinary AsmSal (Register reg) dst) = "    " ++ codeEmission AsmSal ++ " " ++ codeEmissionReg1 reg ++ ", " ++ codeEmission dst
  codeEmission (AsmBinary AsmSar (Register reg) dst) = "    " ++ codeEmission AsmSar ++ " " ++ codeEmissionReg1 reg ++ ", " ++ codeEmission dst
  codeEmission (AsmBinary op src dst) = "    " ++ codeEmission op ++ " " ++ codeEmission src ++ ", " ++ codeEmission dst
  codeEmission (AsmIdiv operand) = "    " ++ "idivl " ++ codeEmission operand
  codeEmission (AsmCmp op1 op2) = "    " ++ "cmpl " ++ codeEmission op1 ++ ", " ++ codeEmission op2
  codeEmission (AsmJmp label) = "    " ++ "jmp " ++ labelPrefix ++ label
  codeEmission (AsmJmpCC cc label) = "    " ++ "j" ++ codeEmission cc ++ " " ++ labelPrefix ++ label
  codeEmission (AsmSetCC cc (Register reg)) = "    " ++ "set" ++ codeEmission cc ++ " " ++ codeEmissionReg1 reg
  codeEmission (AsmSetCC cc operand) = "    " ++ "set" ++ codeEmission cc ++ " " ++ codeEmission operand

instance ToAsm AsmFunction where
  codeEmission :: AsmFunction -> String
  codeEmission (AsmFunction name instructions) =
    ".globl " ++ prefix ++ name ++ "\n" ++ prefix ++ name ++ ":\n" ++ preInstr ++ unlines (fmap codeEmission instructions)
    where
      preInstr = "    pushq %rbp\n    movq %rsp, %rbp\n"
      prefix = case os of
        "darwin" -> "_"
        _ -> ""

instance ToAsm AsmProgram where
  codeEmission :: AsmProgram -> String
  codeEmission (AsmProgram [f]) = codeEmission f ++ endL
    where
      endL = case os of
        "darwin" -> "\n"
        _ -> "\n.section .note.GNU-stack,\"\",@progbits\n"
  codeEmission (AsmProgram []) = error "No functions to compile"
  codeEmission _ = error "Multiple functions not supported yet"

codegenTVal :: TACVal -> AsmOperand
codegenTVal (TACConstant i) = Imm i
codegenTVal (TACVar name) = Pseudo name

codegenTUnaryOp :: TACUnaryOp -> AsmUnaryOp
codegenTUnaryOp TACNegate = AsmNeg
codegenTUnaryOp TACComplement = AsmNot
codegenTUnaryOp TACNot = AsmNot

codegenTBinaryOp :: TACBinaryOp -> AsmBinaryOp
codegenTBinaryOp TACAdd = AsmAdd
codegenTBinaryOp TACSubtract = AsmSub
codegenTBinaryOp TACMultiply = AsmMult
codegenTBinaryOp TACBitwiseAnd = AsmAnd
codegenTBinaryOp TACBitwiseOr = AsmOr
codegenTBinaryOp TACBitwiseXor = AsmXor
codegenTBinaryOp TACLeftShift = AsmSal
codegenTBinaryOp TACRightShift = AsmSar
codegenTBinaryOp _ = error "Division and remainder should be handled separately"

codegenTInstruction :: TACInstruction -> [AsmInstruction]
codegenTInstruction (TACReturn val) =
  [ AsmMov (codegenTVal val) (Register AX),
    AsmRet
  ]
codegenTInstruction (TACUnary TACNot src dst) =
  [ AsmCmp (Imm 0) (codegenTVal src),
    AsmMov (Imm 0) (codegenTVal dst),
    AsmSetCC E (codegenTVal dst)
  ]
codegenTInstruction (TACUnary op src dst) =
  [ AsmMov (codegenTVal src) (codegenTVal dst),
    AsmUnary (codegenTUnaryOp op) (codegenTVal dst)
  ]
codegenTInstruction (TACBinary TACDivide src1 src2 dst) =
  [ AsmMov (codegenTVal src1) (Register AX),
    AsmCdq,
    AsmIdiv (codegenTVal src2),
    AsmMov (Register AX) (codegenTVal dst)
  ]
codegenTInstruction (TACBinary TACRemainder src1 src2 dst) =
  [ AsmMov (codegenTVal src1) (Register AX),
    AsmCdq,
    AsmIdiv (codegenTVal src2),
    AsmMov (Register DX) (codegenTVal dst)
  ]
codegenTInstruction b@(TACBinary op' _ _ _)
  | isRelationalOp op' = codegenTInstructionRelational b
  | otherwise = codegenTInstructionNonRelational b
  where
    codegenTInstructionNonRelational :: TACInstruction -> [AsmInstruction]
    codegenTInstructionNonRelational (TACBinary op src1 src2 dst) =
      [ AsmMov (codegenTVal src1) (codegenTVal dst),
        AsmBinary (codegenTBinaryOp op) (codegenTVal src2) (codegenTVal dst)
      ]
    codegenTInstructionNonRelational _ = error "Not a binary operation"
    codegenTInstructionRelational :: TACInstruction -> [AsmInstruction]
    codegenTInstructionRelational (TACBinary op src1 src2 dst) =
      [ AsmCmp (codegenTVal src2) (codegenTVal src1),
        AsmMov (Imm 0) (codegenTVal dst),
        AsmSetCC (relationalOpToCC op) (codegenTVal dst)
      ]
    codegenTInstructionRelational _ = error "Not a binary operation"
codegenTInstruction (TACJump target) = [AsmJmp target]
codegenTInstruction (TACJumpIfZero condition target) =
  [ AsmCmp (Imm 0) (codegenTVal condition),
    AsmJmpCC E target
  ]
codegenTInstruction (TACJumpIfNotZero condition target) =
  [ AsmCmp (Imm 0) (codegenTVal condition),
    AsmJmpCC NE target
  ]
codegenTInstruction (TACCopy src dst) = [AsmMov (codegenTVal src) (codegenTVal dst)]
codegenTInstruction (TACLabel label) = [AsmLabel label]

relationalOpToCC :: TACBinaryOp -> AsmCondCode
relationalOpToCC TACEqual = E
relationalOpToCC TACNotEqual = NE
relationalOpToCC TACLessThan = L
relationalOpToCC TACLessThanOrEqual = LE
relationalOpToCC TACGreaterThan = G
relationalOpToCC TACGreaterThanOrEqual = GE
relationalOpToCC _ = error "Not a relational operator"

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
  pseudoRegisterPass instruction = case instruction of
    AsmMov src dst -> applyToTwo AsmMov src dst
    AsmUnary op operand -> applyToOne (AsmUnary op) operand
    AsmBinary op src dst -> applyToTwo (AsmBinary op) src dst
    AsmIdiv operand -> applyToOne AsmIdiv operand
    AsmCmp op1 op2 -> applyToTwo AsmCmp op1 op2
    AsmSetCC cc operand -> applyToOne (AsmSetCC cc) operand
    x -> return x
    where
      applyToOne constructor arg = constructor <$> pseudoRegisterPass arg
      applyToTwo constructor arg1 arg2 = do
        arg1' <- pseudoRegisterPass arg1
        arg2' <- pseudoRegisterPass arg2
        return $ constructor arg1' arg2'

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

fixInstr :: AsmInstruction -> [AsmInstruction]
fixInstr (AsmMov (Stack i) (Stack j)) =
  [ AsmMov (Stack i) (Register R10),
    AsmMov (Register R10) (Stack j)
  ]
fixInstr (AsmIdiv (Imm i)) =
  [ AsmMov (Imm i) (Register R10),
    AsmIdiv (Register R10)
  ]
fixInstr (AsmBinary AsmMult src (Stack i)) =
  [ AsmMov (Stack i) (Register R11),
    AsmBinary AsmMult src (Register R11),
    AsmMov (Register R11) (Stack i)
  ]
fixInstr (AsmBinary AsmSal (Stack i) dst) =
  [ AsmMov (Stack i) (Register CX),
    AsmBinary AsmSal (Register CX) dst
  ]
fixInstr (AsmBinary AsmSar (Stack i) dst) =
  [ AsmMov (Stack i) (Register CX),
    AsmBinary AsmSar (Register CX) dst
  ]
fixInstr (AsmCmp (Stack i) (Stack j)) =
  [ AsmMov (Stack i) (Register R10),
    AsmCmp (Register R10) (Stack j)
  ]
fixInstr (AsmCmp src (Imm i)) =
  [ AsmMov (Imm i) (Register R11),
    AsmCmp src (Register R11)
  ]
fixInstr instr@(AsmBinary op (Stack i) (Stack j))
  | op == AsmAdd
      || op == AsmSub
      || op == AsmXor
      || op == AsmAnd
      || op == AsmOr =
      [ AsmMov (Stack i) (Register R10),
        AsmBinary op (Register R10) (Stack j)
      ]
  | otherwise = [instr]
fixInstr x = [x]

removeInvalidInstructions :: AsmProgram -> AsmProgram
removeInvalidInstructions (AsmProgram [AsmFunction fname instrs]) = AsmProgram [AsmFunction fname (fixInvalid instrs [])]
  where
    fixInvalid [] newInstrs = newInstrs
    fixInvalid (i : is) newInstrs = fixInvalid is (newInstrs ++ fixInstr i)
removeInvalidInstructions _ = error "Multiple functions not supported yet"

codegen :: TACProgram -> AsmProgram
codegen =
  removeInvalidInstructions
    . addAllocateStack
    . replacePseudoRegisters
    . codegenTProgram