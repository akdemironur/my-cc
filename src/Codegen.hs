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
import Data.Map (Map, empty, insert, lookup)
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

callPrefix :: String
callPrefix = case os of
  "darwin" -> "_"
  _ -> ""

callPostfix :: String
callPostfix = case os of
  "darwin" -> ""
  _ -> "@PLT"

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

type Offset = Int

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
  | AsmAllocateStack Offset
  | AsmDeallocateStack Offset
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
  deriving (Show, Eq)

data AsmOperand
  = Imm Int
  | Register AsmReg
  | Pseudo String
  | Stack Int
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
  deriving (Show, Eq)

class ToAsm a where
  codeEmission :: a -> String

codeEmissionReg1 :: AsmReg -> String
codeEmissionReg1 AX = "%al"
codeEmissionReg1 DX = "%dl"
codeEmissionReg1 R10 = "%r10b"
codeEmissionReg1 R11 = "%r11b"
codeEmissionReg1 CX = "%cl"
codeEmissionReg1 DI = "%dil"
codeEmissionReg1 SI = "%sil"
codeEmissionReg1 R8 = "%r8b"
codeEmissionReg1 R9 = "%r9b"

codeEmissionReg4 :: AsmReg -> String
codeEmissionReg4 AX = "%eax"
codeEmissionReg4 DX = "%edx"
codeEmissionReg4 R10 = "%r10d"
codeEmissionReg4 R11 = "%r11d"
codeEmissionReg4 CX = "%ecx"
codeEmissionReg4 DI = "%edi"
codeEmissionReg4 SI = "%esi"
codeEmissionReg4 R8 = "%r8d"
codeEmissionReg4 R9 = "%r9d"

codeEmissionReg8 :: AsmReg -> String
codeEmissionReg8 AX = "%rax"
codeEmissionReg8 DX = "%rdx"
codeEmissionReg8 R10 = "%r10"
codeEmissionReg8 R11 = "%r11"
codeEmissionReg8 CX = "%rcx"
codeEmissionReg8 DI = "%rdi"
codeEmissionReg8 SI = "%rsi"
codeEmissionReg8 R8 = "%r8"
codeEmissionReg8 R9 = "%r9"

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
  codeEmission (Register r) = codeEmissionReg4 r
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
  codeEmission (AsmCall name) = "    " ++ "call " ++ callPrefix ++ name ++ callPostfix
  codeEmission (AsmPush (Register r)) = "    " ++ "pushq " ++ codeEmissionReg8 r
  codeEmission (AsmPush operand) = "    " ++ "pushq " ++ codeEmission operand
  codeEmission (AsmDeallocateStack i) = "    " ++ "addq $" ++ show i ++ ", %rsp"

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
  codeEmission (AsmProgram []) = error "No functions to compile"
  codeEmission (AsmProgram fs) = unlines (fmap codeEmission fs ++ [endL])
    where
      endL = case os of
        "darwin" -> "\n"
        _ -> "\n.section .note.GNU-stack,\"\",@progbits\n"

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
codegenTInstruction (TACFuncCall name args dst) =
  stack_padding_instr
    ++ arg_pass_to_registers
    ++ arg_pass_to_stack
    ++ funcall_instr
    ++ deallocate_instr
    ++ retrieve_result_instr
  where
    register_args = take (min (length args) 6) args
    stack_args = drop (length register_args) args
    stack_args_reverse = reverse stack_args
    arg_registers = take (length register_args) [DI, SI, DX, CX, R8, R9]
    stack_padding = if even (length stack_args) then (0 :: Offset) else 8
    stack_padding_instr = [AsmAllocateStack stack_padding | stack_padding /= 0]
    arg_pass_to_registers = zipWith (\a r -> AsmMov (codegenTVal a) (Register r)) register_args arg_registers
    arg_pass_to_stack = concatMap (passOperandToStack . codegenTVal) stack_args_reverse
    funcall_instr = [AsmCall name]
    bytes_to_remove = 8 * length stack_args + stack_padding
    deallocate_instr = [AsmDeallocateStack bytes_to_remove | bytes_to_remove /= 0]
    asm_dst = codegenTVal dst
    retrieve_result_instr = [AsmMov (Register AX) asm_dst]

passOperandToStack :: AsmOperand -> [AsmInstruction]
passOperandToStack (Imm i) = [AsmPush (Imm i)]
passOperandToStack (Register r) = [AsmPush (Register r)]
passOperandToStack asm_arg = [AsmMov asm_arg (Register AX), AsmPush (Register AX)]

relationalOpToCC :: TACBinaryOp -> AsmCondCode
relationalOpToCC TACEqual = E
relationalOpToCC TACNotEqual = NE
relationalOpToCC TACLessThan = L
relationalOpToCC TACLessThanOrEqual = LE
relationalOpToCC TACGreaterThan = G
relationalOpToCC TACGreaterThanOrEqual = GE
relationalOpToCC _ = error "Not a relational operator"

codegenTFunction :: TACFunction -> AsmFunction
codegenTFunction (TACFunction name args instructions) = AsmFunction name (move_register_args_to_params ++ move_stack_args_to_params ++ asm_instructions)
  where
    register_args = take (min (length args) 6) args
    stack_args = drop (length register_args) args
    move_register_args_to_params = zipWith (\a r -> AsmMov (Register r) (codegenTVal a)) register_args [DI, SI, DX, CX, R8, R9]
    move_stack_args_to_params = zipWith (\a i -> AsmMov (Stack (8 * i)) (codegenTVal a)) stack_args [2 ..]
    asm_instructions = concatMap codegenTInstruction instructions

codegenTProgram :: TACProgram -> AsmProgram
codegenTProgram (TACProgram fs) = AsmProgram (fmap codegenTFunction fs)

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
        let newMap = Data.Map.insert s (-(8 * (i + 1))) m
        put (newMap, i + 1)
        return $ Stack (-(8 * (i + 1)))
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
    AsmPush operand -> applyToOne AsmPush operand
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

replacePseudoRegisters :: AsmFunction -> (AsmFunction, StackOffset)
replacePseudoRegisters p = (p', offset)
  where
    (p', (_, offset)) = runState (pseudoRegisterPass p) (Data.Map.empty, 0)

addAllocateStack :: (AsmFunction, StackOffset) -> AsmFunction
addAllocateStack (AsmFunction name instrs, offset) = AsmFunction name newInstrs
  where
    offset' = if even offset then offset else offset + 1
    newInstrs = AsmAllocateStack (offset' * 8) : instrs

pseudoFix :: AsmFunction -> AsmFunction
pseudoFix = addAllocateStack . replacePseudoRegisters

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
removeInvalidInstructions (AsmProgram fs) = AsmProgram (fmap (\(AsmFunction fname instr) -> AsmFunction fname (fixInvalid instr [])) fs)
  where
    fixInvalid [] newInstrs = newInstrs
    fixInvalid (i : is) newInstrs = fixInvalid is (newInstrs ++ fixInstr i)

codegen :: TACProgram -> AsmProgram
codegen =
  removeInvalidInstructions
    . (\(AsmProgram fs) -> AsmProgram (fmap pseudoFix fs))
    . codegenTProgram

functionNameList :: AsmProgram -> [String]
functionNameList (AsmProgram fs) = fmap (\(AsmFunction name _) -> name) fs