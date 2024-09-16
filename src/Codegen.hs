{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-# HLINT ignore "Use camelCase" #-}

module Codegen where

import AST
import Control.Monad.Reader
import Control.Monad.State
import Data.List (intercalate)
import Data.Map (Map, empty, insert, lookup)
import Data.Map as M (member)
import qualified Data.Map as M
import GHC.Base (Symbol)
import Lexer
import Parser
import Pass
import System.Info (os)
import Tacky
import TypeCheck
import Util

data AsmProgram = AsmProgram [AsmTopLevel] deriving (Show, Eq)

data AsmType = Longword | Quadword deriving (Show, Eq)

data AsmSymbolTableEntry
  = Obj AsmType Bool -- isStatic
  | Fun Bool -- isDefined
  deriving (Show, Eq)

stToAsmST :: SymbolTable -> AsmSymbolTable
stToAsmST = M.map stToAsmSTEntry
  where
    stToAsmSTEntry :: SymbolTableEntry -> AsmSymbolTableEntry
    stToAsmSTEntry (CFunc {}, FuncAttr defined _) = Fun defined
    stToAsmSTEntry (CInt, LocalAttr) = Obj Longword False
    stToAsmSTEntry (CInt, StaticAttr {}) = Obj Longword True
    stToAsmSTEntry (CLong, LocalAttr) = Obj Quadword False
    stToAsmSTEntry (CLong, StaticAttr {}) = Obj Quadword True
    stToAsmSTEntry _ = error "Invalid symbol table entry"

type AsmSymbolTable = M.Map Identifier AsmSymbolTableEntry

data AsmTopLevel
  = AsmFunction String Bool [AsmInstruction] -- identifier isDefined instructions
  | AsmStaticVariable String Bool Int StaticInit -- identifier isStatic alignment initial
  deriving (Show, Eq)

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
  = AsmMov AsmType AsmOperand AsmOperand
  | AsmMovsx AsmOperand AsmOperand
  | AsmUnary AsmUnaryOp AsmType AsmOperand
  | AsmBinary AsmBinaryOp AsmType AsmOperand AsmOperand
  | AsmCmp AsmType AsmOperand AsmOperand
  | AsmIdiv AsmType AsmOperand
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
  deriving (Show, Eq)

data AsmOperand
  = Imm Int
  | Register AsmReg
  | Pseudo String
  | Stack Int
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
codeEmissionReg1 SP = "%spl"

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
codeEmissionReg4 SP = "%esp"

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
codeEmissionReg8 SP = "%rsp"

instance ToAsm AsmUnaryOp where
  codeEmission :: AsmUnaryOp -> String
  codeEmission AsmNeg = "neg"
  codeEmission AsmNot = "not"

instance ToAsm AsmBinaryOp where
  codeEmission :: AsmBinaryOp -> String
  codeEmission AsmAdd = "add"
  codeEmission AsmSub = "sub"
  codeEmission AsmMult = "imul"
  codeEmission AsmXor = "xor"
  codeEmission AsmAnd = "and"
  codeEmission AsmOr = "or"
  codeEmission AsmSal = "sal"
  codeEmission AsmSar = "sar"

instance ToAsm AsmOperand where
  codeEmission :: AsmOperand -> String
  codeEmission (Imm i) = "$" ++ show i
  codeEmission (Register r) = codeEmissionReg4 r
  codeEmission (Stack i) = show i ++ "(%rbp)"
  codeEmission (Data s) = s ++ "(%rip)"
  codeEmission (Pseudo i) = "Pseudo" ++ i

oprEmission :: AsmType -> AsmOperand -> String
oprEmission Longword (Register r) = codeEmissionReg4 r
oprEmission Quadword (Register r) = codeEmissionReg8 r
oprEmission _ op = codeEmission op

instance ToAsm AsmType where
  codeEmission :: AsmType -> String
  codeEmission Longword = "l"
  codeEmission Quadword = "q"

instance ToAsm AsmInstruction where
  codeEmission :: AsmInstruction -> String
  codeEmission = \case
    AsmLabel label -> formatLabel label
    AsmMov t src dst -> formatInstruction "mov" (Just t) [oprEmission t src, oprEmission t dst]
    AsmRet -> unlines . map indent $ ["movq %rbp, %rsp", "popq %rbp", "ret"]
    AsmUnary op t operand -> formatInstruction (codeEmission op) (Just t) [oprEmission t operand]
    AsmCdq Longword -> indent "cdq"
    AsmCdq Quadword -> indent "cqo"
    AsmBinary op t src dst -> formatBinaryInstruction op t src dst
    AsmIdiv t operand -> formatInstruction "idiv" (Just t) [oprEmission t operand]
    AsmCmp t op1 op2 -> formatInstruction "cmp" (Just t) [oprEmission t op1, oprEmission t op2]
    AsmJmp label -> formatJump "jmp" label
    AsmJmpCC cc label -> formatJump ("j" ++ codeEmission cc) label
    AsmSetCC cc (Register reg) -> formatInstruction ("set" ++ codeEmission cc) Nothing [codeEmissionReg1 reg]
    AsmSetCC cc operand -> formatInstruction ("set" ++ codeEmission cc) Nothing [codeEmission operand]
    AsmCall name -> formatCall name
    AsmPush (Register r) -> formatPush $ codeEmissionReg8 r
    AsmPush operand -> formatPush $ codeEmission operand
    AsmMovsx src dst -> formatInstruction "movslq" Nothing [oprEmission Longword src, oprEmission Quadword dst]

indent :: String -> String
indent = ("    " ++)

formatLabel :: String -> String
formatLabel label = labelPrefix ++ label ++ ":"

formatInstruction :: String -> Maybe AsmType -> [String] -> String
formatInstruction mnemonic t operands =
  indent $ mnemonic ++ typeSuffix ++ " " ++ intercalate ", " operands
  where
    typeSuffix = case t of
      Just Longword -> "l"
      Just Quadword -> "q"
      Nothing -> ""

formatBinaryInstruction :: AsmBinaryOp -> AsmType -> AsmOperand -> AsmOperand -> String
formatBinaryInstruction op t src dst = case (op, src) of
  (AsmSal, Register reg) -> formatInstruction (codeEmission AsmSal) (Just t) [codeEmissionReg1 reg, oprEmission t dst]
  (AsmSar, Register reg) -> formatInstruction (codeEmission AsmSar) (Just t) [codeEmissionReg1 reg, oprEmission t dst]
  _ -> formatInstruction (codeEmission op) (Just t) [oprEmission t src, oprEmission t dst]

formatJump :: String -> String -> String
formatJump jumpType label = indent $ jumpType ++ " " ++ labelPrefix ++ label

formatCall :: String -> String
formatCall name = indent $ "call " ++ callPrefix ++ name ++ callPostfix

formatPush :: String -> String
formatPush operand = indent $ "pushq " ++ operand

namePrefix :: String
namePrefix = case os of
  "darwin" -> "_"
  _ -> ""

globlDirective :: String -> Bool -> String
globlDirective name True = "    .globl " ++ nameDirective name
globlDirective _ False = ""

alignmentDirective :: Int -> String
alignmentDirective = ("    .balign " ++) . show

bssOrData :: StaticInit -> String
bssOrData (IntInit 0) = "    .bss"
bssOrData (LongInit 0) = "    .bss"
bssOrData _ = "    .data"

nameDirective :: String -> String
nameDirective name = namePrefix ++ name

instance ToAsm StaticInit where
  codeEmission :: StaticInit -> String
  codeEmission (IntInit 0) = indent ".zero 4"
  codeEmission (IntInit i) = indent ".long " ++ show i
  codeEmission (LongInit 0) = indent ".zero 8"
  codeEmission (LongInit i) = indent ".quad " ++ show i

instance ToAsm AsmTopLevel where
  codeEmission :: AsmTopLevel -> String
  codeEmission (AsmStaticVariable name glob alignment iv) = unlines [globlDirective name glob, bssOrData iv, alignmentDirective alignment, nameDirective name ++ ":", codeEmission iv]
  codeEmission (AsmFunction name glob instructions) = unlines $ [globlDirective name glob, indent ".text", nameDirective name ++ ":", indent "pushq %rbp", indent "movq %rsp, %rbp"] ++ fmap codeEmission instructions

instance ToAsm AsmProgram where
  codeEmission :: AsmProgram -> String
  codeEmission (AsmProgram []) = error "No functions to compile"
  codeEmission (AsmProgram fs) = unlines (fmap codeEmission fs ++ [endL])
    where
      endL = case os of
        "darwin" -> "\n"
        _ -> "\n.section .note.GNU-stack,\"\",@progbits\n"

codegenTVal :: TACVal -> AsmOperand
codegenTVal (TACConstant (ConstInt i)) = Imm i
codegenTVal (TACConstant (ConstLong i)) = Imm i
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

codegenTInstruction :: AsmSymbolTable -> TACInstruction -> [AsmInstruction]
codegenTInstruction st (TACReturn val) =
  [ AsmMov (getType st val) (codegenTVal val) (Register AX),
    AsmRet
  ]
codegenTInstruction st (TACUnary TACNot src dst) =
  [ AsmCmp (getType st src) (Imm 0) (codegenTVal src),
    AsmMov (getType st dst) (Imm 0) (codegenTVal dst),
    AsmSetCC E (codegenTVal dst)
  ]
codegenTInstruction st (TACUnary op src dst) =
  [ AsmMov (getType st src) (codegenTVal src) (codegenTVal dst),
    AsmUnary (codegenTUnaryOp op) (getType st src) (codegenTVal dst)
  ]
codegenTInstruction st (TACBinary TACDivide src1 src2 dst) =
  [ AsmMov (getType st src1) (codegenTVal src1) (Register AX),
    AsmCdq (getType st src1),
    AsmIdiv (getType st src1) (codegenTVal src2),
    AsmMov (getType st src1) (Register AX) (codegenTVal dst)
  ]
codegenTInstruction st (TACBinary TACRemainder src1 src2 dst) =
  [ AsmMov (getType st src1) (codegenTVal src1) (Register AX),
    AsmCdq (getType st src1),
    AsmIdiv (getType st src2) (codegenTVal src2),
    AsmMov (getType st dst) (Register DX) (codegenTVal dst)
  ]
codegenTInstruction st b@(TACBinary op' _ _ _)
  | isRelationalOp op' = codegenTInstructionRelational b
  | otherwise = codegenTInstructionNonRelational b
  where
    codegenTInstructionNonRelational :: TACInstruction -> [AsmInstruction]
    codegenTInstructionNonRelational (TACBinary op src1 src2 dst) =
      [ AsmMov (getType st src1) (codegenTVal src1) (codegenTVal dst),
        AsmBinary (codegenTBinaryOp op) (getType st dst) (codegenTVal src2) (codegenTVal dst)
      ]
    codegenTInstructionNonRelational _ = error "Not a binary operation"
    codegenTInstructionRelational :: TACInstruction -> [AsmInstruction]
    codegenTInstructionRelational (TACBinary op src1 src2 dst) =
      [ AsmCmp (getType st src1) (codegenTVal src2) (codegenTVal src1),
        AsmMov (getType st dst) (Imm 0) (codegenTVal dst),
        AsmSetCC (relationalOpToCC op) (codegenTVal dst)
      ]
    codegenTInstructionRelational _ = error "Not a binary operation"
codegenTInstruction _ (TACJump target) = [AsmJmp target]
codegenTInstruction st (TACJumpIfZero condition target) =
  [ AsmCmp (getType st condition) (Imm 0) (codegenTVal condition),
    AsmJmpCC E target
  ]
codegenTInstruction st (TACJumpIfNotZero condition target) =
  [ AsmCmp (getType st condition) (Imm 0) (codegenTVal condition),
    AsmJmpCC NE target
  ]
codegenTInstruction st (TACCopy src dst) = [AsmMov (getType st dst) (codegenTVal src) (codegenTVal dst)]
codegenTInstruction _ (TACLabel label) = [AsmLabel label]
codegenTInstruction st (TACFuncCall name args dst) =
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
    stack_padding = if even (length stack_args) then 0 else 8
    stack_padding_instr = allocateStack stack_padding
    arg_pass_to_registers = zipWith (\a r -> AsmMov (getType st a) (codegenTVal a) (Register r)) register_args arg_registers
    arg_pass_to_stack = concatMap (passOperandToStack st) stack_args_reverse
    funcall_instr = [AsmCall name]
    bytes_to_remove = length stack_args * 8 + stack_padding
    deallocate_instr = deallocateStack bytes_to_remove
    asm_dst = codegenTVal dst
    retrieve_result_instr = [AsmMov (getType st dst) (Register AX) asm_dst]
codegenTInstruction _ (TACSignExtend src dst) = [AsmMovsx (codegenTVal src) (codegenTVal dst)]
codegenTInstruction _ (TACTruncate src dst) = [AsmMov Longword (codegenTVal src) (codegenTVal dst)]

allocateStack :: Int -> [AsmInstruction]
allocateStack i = [AsmBinary AsmSub Quadword (Imm i) (Register SP) | i /= 0]

deallocateStack :: Int -> [AsmInstruction]
deallocateStack i = [AsmBinary AsmAdd Quadword (Imm i) (Register SP) | i /= 0]

getType :: AsmSymbolTable -> TACVal -> AsmType
getType st val = case val of
  TACVar name -> asmSTEntryToAsmType $ M.findWithDefault (Obj Quadword undefined) name st
  TACConstant (ConstInt _) -> Longword
  TACConstant (ConstLong _) -> Quadword
  where
    asmSTEntryToAsmType :: AsmSymbolTableEntry -> AsmType
    asmSTEntryToAsmType (Obj t _) = t
    asmSTEntryToAsmType _ = error "Invalid symbol table entry"

passOperandToStack :: AsmSymbolTable -> TACVal -> [AsmInstruction]
passOperandToStack st val = case (typeOfArg, asmArg) of
  (Quadword, _) -> [AsmPush asmArg]
  (_, Imm _) -> [AsmPush asmArg]
  (_, Register _) -> [AsmPush asmArg]
  _ -> [AsmMov Longword asmArg (Register AX), AsmPush (Register AX)]
  where
    asmArg = codegenTVal val
    typeOfArg = getType st val

relationalOpToCC :: TACBinaryOp -> AsmCondCode
relationalOpToCC TACEqual = E
relationalOpToCC TACNotEqual = NE
relationalOpToCC TACLessThan = L
relationalOpToCC TACLessThanOrEqual = LE
relationalOpToCC TACGreaterThan = G
relationalOpToCC TACGreaterThanOrEqual = GE
relationalOpToCC _ = error "Not a relational operator"

codegenTTopLevel :: AsmSymbolTable -> TACTopLevel -> AsmTopLevel
codegenTTopLevel st (TACFunction name glob args instructions) = AsmFunction name glob (move_register_args_to_params ++ move_stack_args_to_params ++ asm_instructions)
  where
    register_args = fmap TACVar (take (min (length args) 6) args)
    stack_args = fmap TACVar (drop (length register_args) args)
    stack_offsets = take (length stack_args) [16, 24 ..]
    move_register_args_to_params = zipWith (\a r -> AsmMov (getType st a) (Register r) (codegenTVal a)) register_args [DI, SI, DX, CX, R8, R9]
    move_stack_args_to_params = zipWith (\a i -> AsmMov (getType st a) (Stack i) (codegenTVal a)) stack_args stack_offsets
    asm_instructions = concatMap (codegenTInstruction st) instructions
codegenTTopLevel _ (TACStaticVariable name glob ctype iv) = AsmStaticVariable name glob (alignmentSize $ ctypeToAsmType ctype) iv

-- stack_arg_offset :: Offset -> [AsmType] -> [Offset]
-- stack_arg_offset initialOffset args = stack_arg_offset' initialOffset args []
--   where
--     stack_arg_offset' :: Offset -> [AsmType] -> [Offset] -> [Offset]
--     stack_arg_offset' offset [] acc = acc ++ [offset]
--     stack_arg_offset' offset (t : ts) acc = stack_arg_offset' (alignedOffset + size t) ts (acc ++ [alignedOffset])
--       where
--         alignedOffset = roundAwayFromZero (alignmentSize t) offset

ctypeToAsmType :: CType -> AsmType
ctypeToAsmType CInt = Longword
ctypeToAsmType CLong = Quadword
ctypeToAsmType (CFunc {}) = error "Function type not supported"

alignmentSize :: AsmType -> Int
alignmentSize Longword = 4
alignmentSize Quadword = 8
alignmentSize _ = error "Invalid type"

size :: AsmType -> Int
size Longword = 4
size Quadword = 8

codegenTProgram :: AsmSymbolTable -> TACProgram -> AsmProgram
codegenTProgram st (TACProgram fs) = AsmProgram (fmap (codegenTTopLevel st) fs)

type StackOffset = Int

type PseudoOffsetMap = Map String StackOffset

type PseudoRegisterState = State (PseudoOffsetMap, Int, AsmSymbolTable)

class PseudeRegisterPass a where
  pseudoRegisterPass :: a -> PseudoRegisterState a

instance PseudeRegisterPass AsmOperand where
  pseudoRegisterPass :: AsmOperand -> PseudoRegisterState AsmOperand
  pseudoRegisterPass (Pseudo s) = do
    (m, currentOffset, st) <- get
    case Data.Map.lookup s st of
      Just (Obj _ True) -> return $ Data (namePrefix ++ s)
      Just (Obj t _) -> case Data.Map.lookup s m of
        Just offset -> return $ Stack (-offset)
        Nothing -> do
          let alignedOffset = roundAwayFromZero (alignmentSize t) currentOffset
              newOffset = alignedOffset + size t
              newMap = Data.Map.insert s newOffset m
          put (newMap, newOffset, st)
          return $ Stack (-newOffset)
      _ -> return $ Pseudo s
  pseudoRegisterPass x = return x

instance PseudeRegisterPass AsmInstruction where
  pseudoRegisterPass :: AsmInstruction -> PseudoRegisterState AsmInstruction
  pseudoRegisterPass instruction = case instruction of
    AsmMov t src dst -> applyToTwo (AsmMov t) src dst
    AsmUnary op t operand -> applyToOne (AsmUnary op t) operand
    AsmBinary op t src dst -> applyToTwo (AsmBinary op t) src dst
    AsmIdiv t operand -> applyToOne (AsmIdiv t) operand
    AsmCmp t op1 op2 -> applyToTwo (AsmCmp t) op1 op2
    AsmSetCC cc operand -> applyToOne (AsmSetCC cc) operand
    AsmPush operand -> applyToOne AsmPush operand
    AsmMovsx src dst -> applyToTwo AsmMovsx src dst
    AsmJmp _ -> return instruction
    AsmJmpCC _ _ -> return instruction
    AsmLabel _ -> return instruction
    AsmCall _ -> return instruction
    AsmRet -> return instruction
    AsmCdq _ -> return instruction
    where
      applyToOne constructor arg = constructor <$> pseudoRegisterPass arg
      applyToTwo constructor arg1 arg2 = do
        arg1' <- pseudoRegisterPass arg1
        arg2' <- pseudoRegisterPass arg2
        return $ constructor arg1' arg2'

instance PseudeRegisterPass AsmTopLevel where
  pseudoRegisterPass :: AsmTopLevel -> PseudoRegisterState AsmTopLevel
  pseudoRegisterPass (AsmFunction name glob instructions) = do
    instructions' <- mapM pseudoRegisterPass instructions
    return $ AsmFunction name glob instructions'
  pseudoRegisterPass x = return x

replacePseudoRegisters :: AsmSymbolTable -> AsmTopLevel -> (AsmTopLevel, StackOffset)
replacePseudoRegisters st p = (p', offset)
  where
    (p', (_, offset, _)) = runState (pseudoRegisterPass p) (Data.Map.empty, 0, st)

addAllocateStack :: (AsmTopLevel, StackOffset) -> AsmTopLevel
addAllocateStack (AsmFunction name glob instrs, offset) = AsmFunction name glob newInstrs
  where
    offset' = roundAwayFromZero 16 offset
    newInstrs = allocateStack offset' ++ instrs
addAllocateStack x = fst x

pseudoFix :: AsmSymbolTable -> AsmTopLevel -> AsmTopLevel
pseudoFix st = addAllocateStack . replacePseudoRegisters st

fixInstr :: AsmInstruction -> [AsmInstruction]
fixInstr (AsmMov Longword (Imm i) dst)
  | i > 0x7FFFFFFF || i < -0x80000000 =
      fixInstr $ AsmMov Longword (Imm (castLongToInt i)) dst
fixInstr (AsmMov Quadword (Imm i) dst)
  | (i > 0x7FFFFFFF || i < -0x80000000) && isMemoryOperand dst =
      [ AsmMov Quadword (Imm i) (Register R10),
        AsmMov Quadword (Register R10) dst
      ]
fixInstr (AsmMov t src dst)
  | isMemoryOperand src && isMemoryOperand dst =
      [ AsmMov t src (Register R10),
        AsmMov t (Register R10) dst
      ]
  | otherwise = [AsmMov t src dst]
fixInstr (AsmIdiv t (Imm i)) =
  [ AsmMov t (Imm i) (Register R10),
    AsmIdiv t (Register R10)
  ]
fixInstr (AsmCmp Quadword (Imm i) dst)
  | i > 0x7FFFFFFF || i < -0x80000000 =
      AsmMov Quadword (Imm i) (Register R10)
        : fixInstr (AsmCmp Quadword (Register R10) dst)
fixInstr (AsmCmp t src (Imm i)) =
  [ AsmMov t (Imm i) (Register R11),
    AsmCmp t src (Register R11)
  ]
fixInstr (AsmCmp t src dst)
  | isMemoryOperand src && isMemoryOperand dst =
      [ AsmMov t src (Register R10),
        AsmCmp t (Register R10) dst
      ]
  | otherwise = [AsmCmp t src dst]
fixInstr (AsmPush (Imm i))
  | i > 0x7FFFFFFF || i < -0x80000000 =
      [ AsmMov Quadword (Imm i) (Register R10),
        AsmPush (Register R10)
      ]
fixInstr (AsmBinary op Quadword (Imm i) dst)
  | ( op == AsmAdd
        || op == AsmSub
        || op == AsmMult
        || op == AsmAnd
        || op == AsmOr
        || op == AsmXor
    )
      && not (is32BitRange i) =
      AsmMov Quadword (Imm i) (Register R10)
        : fixInstr (AsmBinary op Quadword (Register R10) dst)
fixInstr instr@(AsmBinary op t src dst)
  | isMemoryOperand src
      && isMemoryOperand dst
      && ( op == AsmAdd
             || op == AsmSub
             || op == AsmXor
             || op == AsmAnd
             || op == AsmOr
         ) =
      [ AsmMov t src (Register R10),
        AsmBinary op t (Register R10) dst
      ]
  | isMemoryOperand dst && op == AsmMult =
      [ AsmMov t dst (Register R11),
        AsmBinary op t src (Register R11),
        AsmMov t (Register R11) dst
      ]
  | isMemoryOperand src && (op == AsmSar || op == AsmSal || op == AsmMult) =
      [ AsmMov t src (Register CX),
        AsmBinary op t (Register CX) dst
      ]
  | otherwise = [instr]
fixInstr (AsmMovsx src dst) = srcInstr ++ [AsmMovsx src' dst'] ++ dstInstr
  where
    dst' = if isMemoryOperand dst then Register R11 else dst
    src' = if isImmediateOperand src then Register R10 else src
    srcInstr = [AsmMov Longword src src' | isImmediateOperand src]
    dstInstr = [AsmMov Quadword dst' dst | isMemoryOperand dst]
fixInstr x = [x]

isMemoryOperand :: AsmOperand -> Bool
isMemoryOperand (Stack _) = True
isMemoryOperand (Data _) = True
isMemoryOperand _ = False

isImmediateOperand :: AsmOperand -> Bool
isImmediateOperand (Imm _) = True
isImmediateOperand _ = False

removeInvalidInstructions :: AsmProgram -> AsmProgram
removeInvalidInstructions (AsmProgram fs) = AsmProgram (fmap fixInvalidHelper fs)
  where
    fixInvalid [] newInstrs = newInstrs
    fixInvalid (i : is) newInstrs = fixInvalid is (newInstrs ++ fixInstr i)
    fixInvalidHelper (AsmFunction fname glob instr) = AsmFunction fname glob (fixInvalid instr [])
    fixInvalidHelper x = x

codegen :: (TACProgram, SymbolTable) -> AsmProgram
codegen (program, symbolTable) = processedFunctions
  where
    asmSymbolTable = stToAsmST symbolTable
    initialAsm = codegenTProgram asmSymbolTable program
    processedFunctions = case initialAsm of
      AsmProgram fs -> removeInvalidInstructions . AsmProgram $ map (pseudoFix asmSymbolTable) fs
