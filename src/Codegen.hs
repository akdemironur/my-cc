{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Codegen where

import AST
import AsmAST
import Control.Monad.State
import Data.Char (toLower)
import Data.List (intercalate)
import Data.Map (Map)
import qualified Data.Map as M
import Numeric (showHFloat)
import System.Info (os)
import Tacky
import Util

type CodegenT a = StateT (AsmSymbolTable, GlobalNameMap, StaticConstMap) (Either String) a

generateNewName :: String -> CodegenT String
generateNewName prefix = do
  (st, globMap, sc) <- get
  let num = M.findWithDefault 0 prefix globMap
      newNum = num + 1
  put (st, M.insert prefix newNum globMap, sc)
  return $ prefix ++ show newNum

addOrGetStaticConst :: Const -> CodegenT AsmOperand
addOrGetStaticConst c = do
  (_, _, sc') <- get
  case M.lookup c sc' of
    Just name -> return $ Data name
    Nothing -> do
      nameOfConst <- generateNewName "const."
      (st, gn, sc) <- get
      let newSt = M.insert nameOfConst (Obj (ctypeToAsmType (constType c)) True (signed (constType c)) True) st
          newSc = M.insert c nameOfConst sc
      put (newSt, gn, newSc)
      return $ Data nameOfConst

class ToAsm a where
  codeEmission :: a -> String

instance ToAsm AsmCondCode where
  codeEmission :: AsmCondCode -> String
  codeEmission = fmap toLower . show

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
codeEmissionReg1 XMM0 = "%xmm0"
codeEmissionReg1 XMM1 = "%xmm1"
codeEmissionReg1 XMM2 = "%xmm2"
codeEmissionReg1 XMM3 = "%xmm3"
codeEmissionReg1 XMM4 = "%xmm4"
codeEmissionReg1 XMM5 = "%xmm5"
codeEmissionReg1 XMM6 = "%xmm6"
codeEmissionReg1 XMM7 = "%xmm7"
codeEmissionReg1 XMM8 = "%xmm8"
codeEmissionReg1 XMM9 = "%xmm9"
codeEmissionReg1 XMM10 = "%xmm10"
codeEmissionReg1 XMM11 = "%xmm11"
codeEmissionReg1 XMM12 = "%xmm12"
codeEmissionReg1 XMM13 = "%xmm13"
codeEmissionReg1 XMM14 = "%xmm14"
codeEmissionReg1 XMM15 = "%xmm15"

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
codeEmissionReg4 r = codeEmissionReg1 r

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
codeEmissionReg8 r = codeEmissionReg1 r

instance ToAsm AsmUnaryOp where
  codeEmission :: AsmUnaryOp -> String
  codeEmission AsmNeg = "neg"
  codeEmission AsmNot = "not"
  codeEmission AsmUShr = "shr"

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
  codeEmission AsmShr = "shr"
  codeEmission AsmDivDouble = "div"

instance ToAsm AsmOperand where
  codeEmission :: AsmOperand -> String
  codeEmission (Imm i) = "$" ++ show i
  codeEmission (Register r) = codeEmissionReg4 r
  codeEmission (Stack i) = show i ++ "(%rbp)"
  codeEmission (Data s) = labelPrefix ++ s ++ "(%rip)"
  codeEmission (Pseudo i) = "Pseudo" ++ i

oprEmission :: AsmType -> AsmOperand -> String
oprEmission Longword (Register r) = codeEmissionReg4 r
oprEmission Quadword (Register r) = codeEmissionReg8 r
oprEmission AsmDouble (Register r) = codeEmissionReg8 r
oprEmission _ op = codeEmission op

instance ToAsm AsmType where
  codeEmission :: AsmType -> String
  codeEmission Longword = "l"
  codeEmission Quadword = "q"
  codeEmission AsmDouble = "sd"

instance ToAsm AsmInstruction where
  codeEmission :: AsmInstruction -> String
  codeEmission = \case
    AsmLabel label -> formatLabel label
    AsmMov t src dst -> formatInstruction "mov" (Just t) [oprEmission t src, oprEmission t dst]
    AsmRet -> unlines . map indent $ ["movq %rbp, %rsp", "popq %rbp", "ret"]
    AsmUnary op t operand -> formatInstruction (codeEmission op) (Just t) [oprEmission t operand]
    AsmCdq Longword -> indent "cdq"
    AsmCdq Quadword -> indent "cqo"
    AsmBinary AsmXor AsmDouble src dst -> formatInstruction "xorpd" Nothing [oprEmission AsmDouble src, oprEmission AsmDouble dst]
    AsmBinary AsmMult AsmDouble src dst -> formatInstruction "mulsd" Nothing [oprEmission AsmDouble src, oprEmission AsmDouble dst]
    AsmBinary op t src dst -> formatBinaryInstruction op t src dst
    AsmIdiv t operand -> formatInstruction "idiv" (Just t) [oprEmission t operand]
    AsmCmp AsmDouble op1 op2 -> formatInstruction "comisd" Nothing [oprEmission AsmDouble op1, oprEmission AsmDouble op2]
    AsmCmp t op1 op2 -> formatInstruction "cmp" (Just t) [oprEmission t op1, oprEmission t op2]
    AsmJmp label -> formatJump "jmp" label
    AsmJmpCC cc label -> formatJump ("j" ++ codeEmission cc) label
    AsmSetCC cc (Register reg) -> formatInstruction ("set" ++ codeEmission cc) Nothing [codeEmissionReg1 reg]
    AsmSetCC cc operand -> formatInstruction ("set" ++ codeEmission cc) Nothing [codeEmission operand]
    AsmCall name -> formatCall name
    AsmPush (Register r) -> formatPush $ codeEmissionReg8 r
    AsmPush operand -> formatPush $ codeEmission operand
    AsmMovsx src dst -> formatInstruction "movslq" Nothing [oprEmission Longword src, oprEmission Quadword dst]
    AsmDiv t operand -> formatInstruction "div" (Just t) [oprEmission t operand]
    AsmMovZeroExtend _ _ -> error "MovZeroExtend shouldnt exist at emission"
    AsmCvtsi2sd t src dst -> formatInstruction "cvtsi2sd" (Just t) [oprEmission t src, oprEmission AsmDouble dst]
    AsmCvttsd2si t src dst -> formatInstruction "cvttsd2si" (Just t) [oprEmission AsmDouble src, oprEmission t dst]
    AsmCdq AsmDouble -> error "Cdq AsmDouble shouldnt exist at emission"

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
      Just AsmDouble -> "sd"
      Nothing -> ""

formatBinaryInstruction :: AsmBinaryOp -> AsmType -> AsmOperand -> AsmOperand -> String
formatBinaryInstruction op t src dst
  | isAsmBitshiftOp op && isRegisterOperand src = formatInstruction (codeEmission op) (Just t) [codeEmissionReg1 (getRegister src), oprEmission t dst]
  | otherwise = formatInstruction (codeEmission op) (Just t) [oprEmission t src, oprEmission t dst]

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

alignmentDirective :: Integer -> String
alignmentDirective = ("    .balign " ++) . show

bssOrData :: StaticInit -> String
bssOrData (StaticInit (IntConst _ 0)) = "    .bss"
bssOrData _ = "    .data"

nameDirective :: String -> String
nameDirective name = namePrefix ++ name

instance ToAsm StaticInit where
  codeEmission :: StaticInit -> String
  codeEmission (StaticInit (IntConst ty 0)) = indent ".zero " ++ show (size ty)
  codeEmission (StaticInit (IntConst ty i)) = indent (if size ty == 4 then ".long " else ".quad ") ++ show i
  codeEmission (StaticInit (DoubleConst _ d))
    | d == 1.0 / 0 = indent ".double 0x1.0p+1024"
    | d == (-1.0) / 0.0 = indent ".double -0x1.0p+1024"
    | otherwise = indent ".double " ++ showHFloat d ""

instance ToAsm AsmTopLevel where
  codeEmission :: AsmTopLevel -> String
  codeEmission (AsmStaticVariable name glob alignment iv) = unlines [globlDirective name glob, bssOrData iv, alignmentDirective alignment, nameDirective name ++ ":", codeEmission iv]
  codeEmission (AsmFunction name glob instructions) = unlines $ [globlDirective name glob, indent ".text", nameDirective name ++ ":", indent "pushq %rbp", indent "movq %rsp, %rbp"] ++ fmap codeEmission instructions
  codeEmission (AsmStaticConstant name alignment iv) = case os of
    "darwin" -> unlines [".literal" ++ show alignment, alignmentDirective alignment, nameDirective name ++ ":", codeEmission iv, if alignment == 16 then ".quad 0" else ""]
    _ -> unlines [".section .rodata", alignmentDirective alignment, nameDirective name ++ ":", codeEmission iv]

instance ToAsm AsmProgram where
  codeEmission :: AsmProgram -> String
  codeEmission (AsmProgram []) = error "No functions to compile"
  codeEmission (AsmProgram fs) = unlines (fmap codeEmission fs ++ [endL])
    where
      endL = case os of
        "darwin" -> "\n"
        _ -> "\n.section .note.GNU-stack,\"\",@progbits\n"

codegenTVal :: TACVal -> AsmOperand
codegenTVal (TACConstant (IntConst _ c)) = Imm c
codegenTVal (TACVar name) = Pseudo name
codegenTVal (TACData s _) = Data s
codegenTVal (TACConstant (DoubleConst _ _)) = error "Double constants should be handled separately"

codegenTUnaryOp :: TACUnaryOp -> AsmUnaryOp
codegenTUnaryOp TACNegate = AsmNeg
codegenTUnaryOp TACComplement = AsmNot
codegenTUnaryOp TACNot = AsmNot

codegenTBinaryOp :: Bool -> TACBinaryOp -> AsmBinaryOp
codegenTBinaryOp _ TACAdd = AsmAdd
codegenTBinaryOp _ TACSubtract = AsmSub
codegenTBinaryOp _ TACMultiply = AsmMult
codegenTBinaryOp _ TACBitwiseAnd = AsmAnd
codegenTBinaryOp _ TACBitwiseOr = AsmOr
codegenTBinaryOp _ TACBitwiseXor = AsmXor
codegenTBinaryOp _ TACLeftShift = AsmSal
codegenTBinaryOp True TACRightShift = AsmSar
codegenTBinaryOp False TACRightShift = AsmShr
codegenTBinaryOp _ _ = error "Division and remainder should be handled separately"

typeOfT :: TACVal -> CodegenT AsmType
typeOfT (TACVar name) = do
  (st, _, _) <- get
  return $ typeOf st name
typeOfT (TACConstant (IntConst ty _)) = return $ ctypeToAsmType ty
typeOfT (TACConstant (DoubleConst _ _)) = return AsmDouble
typeOfT (TACData _ c) = return $ ctypeToAsmType $ constType c

signedT :: TACVal -> CodegenT Bool
signedT (TACVar name) = do
  (st, _, _) <- get
  return $ getSigned st (TACVar name)
signedT (TACConstant (IntConst ty _)) = return $ signed ty
signedT (TACConstant (DoubleConst _ _)) = return True
signedT (TACData _ c) = return $ signed $ constType c

codegenTInstructionT :: TACInstruction -> CodegenT [AsmInstruction]
codegenTInstructionT (TACReturn val) = do
  valType <- typeOfT val
  return [AsmMov valType (codegenTVal val) (Register $ returnRegister valType), AsmRet]
codegenTInstructionT (TACUnary TACNot src dst) = do
  srcType <- typeOfT src
  dstType <- typeOfT dst
  return $
    if srcType /= AsmDouble
      then
        [ AsmCmp srcType (Imm 0) (codegenTVal src),
          AsmMov srcType (Imm 0) (codegenTVal dst),
          AsmSetCC E (codegenTVal dst)
        ]
      else
        [ zeroOutRegister XMM13,
          AsmCmp AsmDouble (codegenTVal src) (Register XMM13),
          AsmMov dstType (Imm 0) (codegenTVal dst),
          AsmSetCC E (codegenTVal dst)
        ]
codegenTInstructionT (TACUnary op src dst) = do
  srcType <- typeOfT src
  if op == TACNegate && srcType == AsmDouble
    then do
      negZero <- addOrGetStaticConst negZeroConst
      return
        [ AsmMov AsmDouble (codegenTVal src) (codegenTVal dst),
          AsmBinary AsmXor AsmDouble negZero (codegenTVal dst)
        ]
    else
      return
        [ AsmMov srcType (codegenTVal src) (codegenTVal dst),
          AsmUnary (codegenTUnaryOp op) srcType (codegenTVal dst)
        ]
codegenTInstructionT (TACBinary TACDivide src1 src2 dst) = do
  src1Type <- typeOfT src1
  src1Signed <- signedT src1
  let divInstr = if src1Signed then AsmIdiv else AsmDiv
      secondInstr = if src1Signed then AsmCdq src1Type else AsmMov src1Type (Imm 0) (Register DX)
  return $
    if src1Type == AsmDouble
      then
        [ AsmMov AsmDouble (codegenTVal src1) (codegenTVal dst),
          AsmBinary AsmDivDouble AsmDouble (codegenTVal src2) (codegenTVal dst)
        ]
      else
        [ AsmMov src1Type (codegenTVal src1) (Register AX),
          secondInstr,
          divInstr src1Type (codegenTVal src2),
          AsmMov src1Type (Register AX) (codegenTVal dst)
        ]
codegenTInstructionT (TACBinary TACRemainder src1 src2 dst) = do
  src1Type <- typeOfT src1
  src1Signed <- signedT src1
  let divInstr = if src1Signed then AsmIdiv else AsmDiv
      secondInstr = if src1Signed then AsmCdq src1Type else AsmMov src1Type (Imm 0) (Register DX)
  return
    [ AsmMov src1Type (codegenTVal src1) (Register AX),
      secondInstr,
      divInstr src1Type (codegenTVal src2),
      AsmMov src1Type (Register DX) (codegenTVal dst)
    ]
codegenTInstructionT (TACBinary op src1 src2 dst)
  | isRelationalOp op = do
      src1Type <- typeOfT src1
      dstType <- typeOfT dst
      src1Signed <- signedT src1
      if src1Type == AsmDouble
        then
          if op /= TACNotEqual
            then do
              endLabel <- generateNewName "endNan"
              return
                [ AsmCmp src1Type (codegenTVal src2) (codegenTVal src1),
                  AsmMov dstType (Imm 0) (codegenTVal dst),
                  AsmJmpCC AsmAST.P endLabel,
                  AsmSetCC (relationalOpToCC (src1Signed && src1Type /= AsmDouble) op) (codegenTVal dst),
                  AsmLabel endLabel
                ]
            else do
              endLabel <- generateNewName "endNan"
              nanLabel <- generateNewName "nan"
              return
                [ AsmCmp src1Type (codegenTVal src2) (codegenTVal src1),
                  AsmJmpCC AsmAST.P nanLabel,
                  AsmMov dstType (Imm 0) (codegenTVal dst),
                  AsmSetCC (relationalOpToCC (src1Signed && src1Type /= AsmDouble) op) (codegenTVal dst),
                  AsmJmp endLabel,
                  AsmLabel nanLabel,
                  AsmMov dstType (Imm 1) (codegenTVal dst),
                  AsmLabel endLabel
                ]
        else
          return
            [ AsmCmp src1Type (codegenTVal src2) (codegenTVal src1),
              AsmMov dstType (Imm 0) (codegenTVal dst),
              AsmSetCC (relationalOpToCC (src1Signed && src1Type /= AsmDouble) op) (codegenTVal dst)
            ]
  | otherwise = do
      src1Type <- typeOfT src1
      src1Signed <- signedT src1
      dstType <- typeOfT dst
      return
        [ AsmMov src1Type (codegenTVal src1) (codegenTVal dst),
          AsmBinary (codegenTBinaryOp src1Signed op) dstType (codegenTVal src2) (codegenTVal dst)
        ]
codegenTInstructionT (TACZeroExtend src dst) = return [AsmMovZeroExtend (codegenTVal src) (codegenTVal dst)]
codegenTInstructionT (TACJump target) = return [AsmJmp target]
codegenTInstructionT (TACJumpIfZero condition target) = do
  conditionType <- typeOfT condition
  return $
    if conditionType == AsmDouble
      then
        [ zeroOutRegister XMM13,
          AsmCmp AsmDouble (codegenTVal condition) (Register XMM13),
          AsmJmpCC E target
        ]
      else
        [ AsmCmp conditionType (Imm 0) (codegenTVal condition),
          AsmJmpCC E target
        ]
codegenTInstructionT (TACJumpIfNotZero condition target) = do
  conditionType <- typeOfT condition
  return $
    if conditionType == AsmDouble
      then
        [ zeroOutRegister XMM13,
          AsmCmp AsmDouble (codegenTVal condition) (Register XMM13),
          AsmJmpCC NE target
        ]
      else
        [ AsmCmp conditionType (Imm 0) (codegenTVal condition),
          AsmJmpCC NE target
        ]
codegenTInstructionT (TACCopy src dst) = do
  srcType <- typeOfT src
  return [AsmMov srcType (codegenTVal src) (codegenTVal dst)]
codegenTInstructionT (TACLabel label) = return [AsmLabel label]
codegenTInstructionT (TACFuncCall name args dst) = do
  (st, _, _) <- get
  let (intRegisterArgs, doubleRegisterArgs, stackArgs) = classifyParameters st args
      stackArgsReverse = reverse stackArgs
      stackPadding = if even (length stackArgs) then 0 else 8
      stackPaddingInstr = allocateStack stackPadding
      moveIntArgsToRegisters = moveArgsToRegisters st intRegisterArgs intRegisters
      moveDblArgsToRegisters = moveArgsToRegisters st doubleRegisterArgs dblRegisters
      moveArgsToStack = concatMap (passOperandToStack st) stackArgsReverse
      funcallInstr = [AsmCall name]
      bytesToRemove = fromIntegral (length stackArgs * 8) + stackPadding
      deallocateInstr = deallocateStack bytesToRemove
      returnType = getType st dst
      retrieveResultInstr = [AsmMov returnType (Register $ returnRegister returnType) (codegenTVal dst)]
  return $ stackPaddingInstr ++ moveIntArgsToRegisters ++ moveDblArgsToRegisters ++ moveArgsToStack ++ funcallInstr ++ deallocateInstr ++ retrieveResultInstr
  where
    classifyParameters :: AsmSymbolTable -> [TACVal] -> ([TACVal], [TACVal], [TACVal])
    classifyParameters st params = (intRegisterParams, doubleRegisterParams, stackParams)
      where
        intRegisterParams = take 6 $ filter (\x -> getType st x /= AsmDouble) params
        doubleRegisterParams = take 8 $ filter (\x -> getType st x == AsmDouble) params
        stackParams = filter (\x -> notElem x intRegisterParams && notElem x doubleRegisterParams) params
    moveArgsToRegisters :: AsmSymbolTable -> [TACVal] -> [AsmReg] -> [AsmInstruction]
    moveArgsToRegisters st = zipWith (\a r -> AsmMov (getType st a) (codegenTVal a) (Register r))
codegenTInstructionT (TACSignExtend src dst) = return [AsmMovsx (codegenTVal src) (codegenTVal dst)]
codegenTInstructionT (TACTruncate src dst) = return [AsmMov Longword (codegenTVal src) (codegenTVal dst)]
codegenTInstructionT (TACIntToDouble src dst) = do
  srcType <- typeOfT src
  return [AsmCvtsi2sd srcType (codegenTVal src) (codegenTVal dst)]
codegenTInstructionT (TACDoubleToInt src dst) = do
  dstType <- typeOfT dst
  return [AsmCvttsd2si dstType (codegenTVal src) (codegenTVal dst)]
codegenTInstructionT (TACUIntToDouble src dst) = do
  srcType <- typeOfT src
  if srcType == Longword
    then
      return
        [ AsmMovZeroExtend (codegenTVal src) (Register R8),
          AsmCvtsi2sd Quadword (Register R8) (codegenTVal dst)
        ]
    else do
      label1 <- generateNewName "uint_to_double_negative"
      label2 <- generateNewName "uint_to_double_end"
      return
        [ AsmCmp Quadword (Imm 0) (codegenTVal src),
          AsmJmpCC L label1,
          AsmCvtsi2sd Quadword (codegenTVal src) (codegenTVal dst),
          AsmJmp label2,
          AsmLabel label1,
          AsmMov Quadword (codegenTVal src) (Register R8),
          AsmMov Quadword (Register R8) (Register R9),
          AsmUnary AsmUShr Quadword (Register R9),
          AsmBinary AsmAnd Quadword (Imm 1) (Register R8),
          AsmBinary AsmOr Quadword (Register R8) (Register R9),
          AsmCvtsi2sd Quadword (Register R9) (codegenTVal dst),
          AsmBinary AsmAdd AsmDouble (codegenTVal dst) (codegenTVal dst),
          AsmLabel label2
        ]
codegenTInstructionT (TACDoubleToUInt src dst) = do
  dstType <- typeOfT dst
  if dstType == Longword
    then
      return
        [ AsmCvttsd2si Quadword (codegenTVal src) (Register R8),
          AsmMov Longword (Register R8) (codegenTVal dst)
        ]
    else do
      dblUB <- addOrGetStaticConst dblUpperBoundConst
      label1 <- generateNewName "double_to_uint_overflow"
      label2 <- generateNewName "double_to_uint_end"
      return
        [ AsmCmp AsmDouble dblUB (codegenTVal src),
          AsmJmpCC AE label1,
          AsmCvttsd2si Quadword (codegenTVal src) (codegenTVal dst),
          AsmJmp label2,
          AsmLabel label1,
          AsmMov AsmDouble (codegenTVal src) (Register XMM13),
          AsmBinary AsmSub AsmDouble dblUB (Register XMM13),
          AsmCvttsd2si Quadword (Register XMM13) (codegenTVal dst),
          AsmMov Quadword (Imm (2 ^ (63 :: Integer))) (Register R8),
          AsmBinary AsmAdd Quadword (Register R8) (codegenTVal dst),
          AsmLabel label2
        ]

allocateStack :: Integer -> [AsmInstruction]
allocateStack i = [AsmBinary AsmSub Quadword (Imm i) (Register SP) | i /= 0]

deallocateStack :: Integer -> [AsmInstruction]
deallocateStack i = [AsmBinary AsmAdd Quadword (Imm i) (Register SP) | i /= 0]

getType :: AsmSymbolTable -> TACVal -> AsmType
getType st val = case val of
  TACVar name -> (\(Obj ty _ _ _) -> ty) $ case st M.!? name of Just x -> x; Nothing -> error ("Variable " ++ name ++ " not found")
  TACConstant c -> ctypeToAsmType . constType $ c
  TACData _ c -> ctypeToAsmType . constType $ c

typeOf :: AsmSymbolTable -> TACIdentifier -> AsmType
typeOf st name = case st M.!? name of
  Just (Obj ty _ _ _) -> ty
  _ -> error "Not a variable"

getSigned :: AsmSymbolTable -> TACVal -> Bool
getSigned st val = case val of
  TACVar name -> (\(Obj _ _ s _) -> s) $ case st M.!? name of Just x -> x; Nothing -> error ("Variable " ++ name ++ " not found")
  TACConstant (IntConst ty _) -> signed ty
  TACConstant (DoubleConst _ _) -> True
  TACData _ _ -> True

passOperandToStack :: AsmSymbolTable -> TACVal -> [AsmInstruction]
passOperandToStack st val = case (typeOfArg, asmArg) of
  (AsmDouble, _) -> [AsmPush asmArg]
  (Quadword, _) -> [AsmPush asmArg]
  (_, Imm _) -> [AsmPush asmArg]
  (_, Register _) -> [AsmPush asmArg]
  _ -> [AsmMov typeOfArg asmArg (Register AX), AsmPush (Register AX)]
  where
    asmArg = codegenTVal val
    typeOfArg = getType st val

relationalOpToCC :: Bool -> TACBinaryOp -> AsmCondCode
relationalOpToCC _ TACEqual = E
relationalOpToCC _ TACNotEqual = NE
relationalOpToCC True TACLessThan = L
relationalOpToCC False TACLessThan = AsmAST.B
relationalOpToCC True TACLessThanOrEqual = LE
relationalOpToCC False TACLessThanOrEqual = BE
relationalOpToCC True TACGreaterThan = G
relationalOpToCC False TACGreaterThan = AsmAST.A
relationalOpToCC True TACGreaterThanOrEqual = GE
relationalOpToCC False TACGreaterThanOrEqual = AE
relationalOpToCC _ _ = error "Not a relational operator"

codegenTTopLevelT :: TACTopLevel -> CodegenT AsmTopLevel
codegenTTopLevelT (TACFunction name glob params instructions) = do
  (st, _, _) <- get
  asm_instructions <- concat <$> mapM codegenTInstructionT instructions
  let params_tacval = fmap TACVar params
      int_register_args = take 6 $ filter (\x -> getType st x /= AsmDouble) params_tacval
      dbl_register_args = take 8 $ filter (\x -> getType st x == AsmDouble) params_tacval
      stack_params = filter (\x -> notElem x int_register_args && notElem x dbl_register_args) params_tacval
      stack_offsets = take (length stack_params) [16, 24 ..]
      move_params_from_int_registers = moveParamsFromRegisters int_register_args intRegisters
      move_params_from_dbl_registers = moveParamsFromRegisters dbl_register_args dblRegisters
      move_params_from_stack = zipWith (\a i -> AsmMov (getType st a) (Stack i) (codegenTVal a)) stack_params stack_offsets
      moveParamsFromRegisters :: [TACVal] -> [AsmReg] -> [AsmInstruction]
      moveParamsFromRegisters = zipWith (\a r -> AsmMov (getType st a) (Register r) (codegenTVal a))
  return $ AsmFunction name glob (move_params_from_int_registers ++ move_params_from_dbl_registers ++ move_params_from_stack ++ asm_instructions)
codegenTTopLevelT (TACStaticVariable name glob ctype iv) = return $ AsmStaticVariable name glob (alignmentSize $ ctypeToAsmType ctype) iv
codegenTTopLevelT (TACStaticConstant name ctype iv@(StaticInit c))
  | c == negZeroConst = return $ AsmStaticConstant name 16 iv
  | otherwise = return $ AsmStaticConstant name (alignmentSize . ctypeToAsmType $ ctype) iv

codegenTProgramT :: TACProgram -> CodegenT AsmProgram
codegenTProgramT (TACProgram fs) = do
  asmTopLevels <- mapM codegenTTopLevelT fs
  return $ AsmProgram asmTopLevels

type StackOffset = Integer

type PseudoOffsetMap = Map String StackOffset

type PseudoRegisterState = State (PseudoOffsetMap, Integer, AsmSymbolTable)

class PseudeRegisterPass a where
  pseudoRegisterPass :: a -> PseudoRegisterState a

instance PseudeRegisterPass AsmOperand where
  pseudoRegisterPass :: AsmOperand -> PseudoRegisterState AsmOperand
  pseudoRegisterPass (Pseudo s) = do
    (m, currentOffset, st) <- get
    case M.lookup s st of
      Just (Obj _ True _ _) -> return $ Data s
      Just (Obj t _ _ _) -> case M.lookup s m of
        Just offset -> return $ Stack (-offset)
        Nothing -> do
          let alignedOffset = roundAwayFromZero (alignmentSize t) currentOffset
              newOffset = alignedOffset + size t
              newMap = M.insert s newOffset m
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
    AsmDiv t operand -> applyToOne (AsmDiv t) operand
    AsmCmp t op1 op2 -> applyToTwo (AsmCmp t) op1 op2
    AsmSetCC cc operand -> applyToOne (AsmSetCC cc) operand
    AsmPush operand -> applyToOne AsmPush operand
    AsmMovsx src dst -> applyToTwo AsmMovsx src dst
    AsmMovZeroExtend src dst -> applyToTwo AsmMovZeroExtend src dst
    AsmJmp _ -> return instruction
    AsmJmpCC _ _ -> return instruction
    AsmLabel _ -> return instruction
    AsmCall _ -> return instruction
    AsmRet -> return instruction
    AsmCdq _ -> return instruction
    AsmCvtsi2sd t src dst -> applyToTwo (AsmCvtsi2sd t) src dst
    AsmCvttsd2si t src dst -> applyToTwo (AsmCvttsd2si t) src dst
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
    (p', (_, offset, _)) = runState (pseudoRegisterPass p) (M.empty, 0, st)

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
      fixInstr $ AsmMov Longword (Imm (cast CLong CInt i)) dst
fixInstr (AsmMov Quadword (Imm i) dst)
  | (i > 0x7FFFFFFF || i < -0x80000000) && isMemoryOperand dst =
      [ AsmMov Quadword (Imm i) (Register R10),
        AsmMov Quadword (Register R10) dst
      ]
fixInstr (AsmMov t src dst)
  | isMemoryOperand src && isMemoryOperand dst =
      let r = Register $ if t == AsmDouble then XMM14 else R10
       in [ AsmMov t src r,
            AsmMov t r dst
          ]
  | otherwise = [AsmMov t src dst]
fixInstr (AsmIdiv t (Imm i)) =
  [ AsmMov t (Imm i) (Register R10),
    AsmIdiv t (Register R10)
  ]
fixInstr (AsmDiv t (Imm i)) =
  [ AsmMov t (Imm i) (Register R10),
    AsmDiv t (Register R10)
  ]
fixInstr (AsmMovZeroExtend src (Register rdst)) = [AsmMov Longword src (Register rdst)]
fixInstr (AsmMovZeroExtend src dst) = [AsmMov Longword src (Register R11), AsmMov Quadword (Register R11) dst]
fixInstr (AsmCmp AsmDouble src dst) = case dst of
  Register _ -> [AsmCmp AsmDouble src dst]
  _ -> concatMap fixInstr [AsmMov AsmDouble dst (Register XMM15), AsmCmp AsmDouble src (Register XMM15)]
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
fixInstr (AsmBinary op AsmDouble src dst) = case (dst, op `elem` [AsmAdd, AsmSub, AsmMult, AsmDivDouble, AsmXor]) of
  (Register _, _) -> [AsmBinary op AsmDouble src dst]
  (_, False) -> [AsmBinary op AsmDouble src dst]
  _ -> concatMap fixInstr [AsmMov AsmDouble dst (Register XMM15), AsmBinary op AsmDouble src (Register XMM15), AsmMov AsmDouble (Register XMM15) dst]
fixInstr (AsmBinary op Quadword (Imm i) dst)
  | ( op == AsmAdd
        || op == AsmSub
        || op == AsmMult
        || op == AsmAnd
        || op == AsmOr
        || op == AsmXor
    )
      && not (validInt i) =
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
  | isMemoryOperand src && (op == AsmSar || op == AsmSal || op == AsmShr || op == AsmMult) =
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
fixInstr (AsmCvttsd2si t src dst) = case dst of
  Register _ -> [AsmCvttsd2si t src dst]
  _ -> concatMap fixInstr [AsmCvttsd2si t src (Register R11), AsmMov t (Register R11) dst]
fixInstr (AsmCvtsi2sd t (Imm i) dst) = concatMap fixInstr [AsmMov t (Imm i) (Register R10), AsmCvtsi2sd t (Register R10) dst]
fixInstr (AsmCvtsi2sd t src dst) = case dst of
  Register _ -> [AsmCvtsi2sd t src dst]
  _ -> concatMap fixInstr [AsmCvtsi2sd t src (Register XMM15), AsmMov AsmDouble (Register XMM15) dst]
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

codegen :: (TACProgram, SymbolTable, GlobalNameMap, StaticConstMap) -> (AsmProgram, (AsmSymbolTable, GlobalNameMap, StaticConstMap))
codegen (program, symbolTable, globalNameMap, staticConstMap) = (processedFunctions, states)
  where
    staticConstToTACTopLevel :: StaticConstMap -> [AsmTopLevel]
    staticConstToTACTopLevel dm = fmap convert (M.toList dm)
      where
        convert (c, name)
          | c == negZeroConst = AsmStaticConstant name 16 (StaticInit c)
          | otherwise = AsmStaticConstant name (alignmentSize . ctypeToAsmType . constType $ c) (StaticInit c)
    asmSymbolTable = stToAsmST symbolTable
    (AsmProgram initialAsmTopLevel, states@(_, _, staticConstMap')) = case runStateT (codegenTProgramT program) (asmSymbolTable, globalNameMap, staticConstMap) of
      Right (x, newStates) -> (x, newStates)
      Left e -> error e
    asmWithStaticConsts = AsmProgram $ staticConstToTACTopLevel staticConstMap' ++ initialAsmTopLevel
    processedFunctions = case asmWithStaticConsts of
      AsmProgram fs -> removeInvalidInstructions . AsmProgram $ map (pseudoFix asmSymbolTable) fs
