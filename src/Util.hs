{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Util where

import AST
import AsmAST
import qualified Data.Map as M
import Data.Maybe (fromJust)
import qualified Foreign.C.Types as CT

intRegisters :: [AsmReg]
intRegisters = [DI, SI, DX, CX, R8, R9]

dblRegisters :: [AsmReg]
dblRegisters = [XMM0, XMM1, XMM2, XMM3, XMM4, XMM5, XMM6, XMM7]

returnRegister :: AsmType -> AsmReg
returnRegister AsmDouble = XMM0
returnRegister _ = AX

dblUpperBoundConst :: Const
dblUpperBoundConst = DoubleConst CDouble (2.0 ** 63)

negZeroConst :: Const
negZeroConst = DoubleConst CDouble (-0.0)

zeroOutRegister :: AsmReg -> AsmInstruction
zeroOutRegister r = AsmBinary AsmXor AsmDouble (Register r) (Register r)

data SCType (t :: CType) where
  SInt :: SCType 'CInt
  SUInt :: SCType 'CUInt
  SLong :: SCType 'CLong
  SULong :: SCType 'CULong

type family ToCType (t :: CType) where
  ToCType 'CInt = CT.CInt
  ToCType 'CUInt = CT.CUInt
  ToCType 'CLong = CT.CLong
  ToCType 'CULong = CT.CULong

toSCType :: CType -> (forall t. (Integral (ToCType t), Num (ToCType t)) => SCType t -> r) -> r
toSCType CInt f = f SInt
toSCType CUInt f = f SUInt
toSCType CLong f = f SLong
toSCType CULong f = f SULong
toSCType (CFunc _ _) _ = error "Function type in toSCType"
toSCType CDouble _ = error "Double type in toSCType"

cast :: CType -> CType -> Integer -> Integer
cast srcType destType x =
  toSCType srcType $ \(_ :: SCType s) ->
    toSCType destType $ \(_ :: SCType d) ->
      if srcType == destType
        then x
        else fromIntegral (fromIntegral (fromIntegral x :: ToCType s) :: ToCType d)

castFromDouble :: CType -> Double -> Integer
castFromDouble dstType x =
  toSCType dstType $ \(_ :: SCType d) ->
    fromIntegral (fromIntegral (truncate x :: Integer) :: ToCType d)

roundAwayFromZero :: Integer -> Integer -> Integer
roundAwayFromZero n x
  | x `mod` n == 0 = x
  | x < 0 = x - n - (x `mod` n)
  | otherwise = x + n - (x `mod` n)

validInt :: Integer -> Bool
validInt x = x >= -2147483648 && x <= 2147483647

validLong :: Integer -> Bool
validLong x = x >= -9223372036854775808 && x <= 9223372036854775807

validUInt :: Integer -> Bool
validUInt x = x >= 0 && x <= 4294967295

validULong :: Integer -> Bool
validULong x = x >= 0 && x <= 18446744073709551615

convertConst :: CType -> Const -> Const
convertConst (CFunc _ _) _ = error "Function type in convertConst"
convertConst CDouble (IntConst _ x) = DoubleConst CDouble (fromIntegral x)
convertConst CDouble (DoubleConst _ x) = DoubleConst CDouble x
convertConst trgType (IntConst srcType srcVal) = IntConst trgType (cast srcType trgType srcVal)
convertConst trgType (DoubleConst _ x) = IntConst trgType (castFromDouble trgType x)

constToInit :: Const -> StaticInit
constToInit = StaticInit

constType :: Const -> CType
constType (IntConst CDouble _) = error "IntConst with double type"
constType (IntConst ty _) = ty
constType (DoubleConst CDouble _) = CDouble
constType (DoubleConst _ _) = error "DoubleConst with non-double type"

isIntegral :: Maybe CType -> Bool
isIntegral (Just CDouble) = False
isIntegral (Just (CFunc _ _)) = False
isIntegral Nothing = False
isIntegral _ = True

reduceLong :: Integer -> Integer
reduceLong x = x - y
  where
    y :: Integer
    y = 2 ^ (32 :: Int)

signed :: CType -> Bool
signed CInt = True
signed CLong = True
signed CDouble = True
signed _ = False

getCommonType :: CType -> CType -> CType
getCommonType (CFunc _ _) _ = error "Function type in getCommonType"
getCommonType _ (CFunc _ _) = error "Function type in getCommonType"
getCommonType t1 t2
  | t1 == t2 = t1
  | t1 == CDouble || t2 == CDouble = CDouble
  | size t1 == size t2 && signed t1 = t2
  | size t1 == size t2 = t1
  | size t1 > size t2 = t1
  | otherwise = t2

class Sizeable a where
  size :: a -> Integer

instance Sizeable CType where
  size :: CType -> Integer
  size CInt = 4
  size CLong = 8
  size CUInt = 4
  size CULong = 8
  size CDouble = 8
  size (CFunc _ _) = error "Function type in size"

instance Sizeable AsmType where
  size :: AsmType -> Integer
  size Longword = 4
  size Quadword = 8
  size AsmDouble = 8

ctypeToAsmType :: CType -> AsmType
ctypeToAsmType CInt = Longword
ctypeToAsmType CLong = Quadword
ctypeToAsmType CUInt = Longword
ctypeToAsmType CULong = Quadword
ctypeToAsmType CDouble = AsmDouble
ctypeToAsmType (CFunc {}) = error "Function type not supported"

alignmentSize :: AsmType -> Integer
alignmentSize Longword = 4
alignmentSize Quadword = 8
alignmentSize AsmDouble = 8

isFunction :: Maybe CType -> Bool
isFunction (Just (CFunc _ _)) = True
isFunction _ = False

convertTo :: TypedExpr -> Maybe CType -> TypedExpr
convertTo e@(TypedExpr expr t1) t2
  | t1 == t2 = TypedExpr expr t1
  | otherwise = TypedExpr (Cast (fromJust t2) e) t2

stToAsmST :: SymbolTable -> AsmSymbolTable
stToAsmST = M.map stToAsmSTEntry
  where
    stToAsmSTEntry :: SymbolTableEntry -> AsmSymbolTableEntry
    stToAsmSTEntry (CFunc {}, FuncAttr defined _) = Fun defined
    stToAsmSTEntry (ty, LocalAttr) = Obj (ctypeToAsmType ty) False (signed ty) False
    stToAsmSTEntry (ty, StaticAttr {}) = Obj (ctypeToAsmType ty) True (signed ty) False
    stToAsmSTEntry (ty, StaticConst _) = Obj (ctypeToAsmType ty) True (signed ty) True
    stToAsmSTEntry _ = error "Invalid symbol table entry"

isAsmBitshiftOp :: AsmBinaryOp -> Bool
isAsmBitshiftOp AsmSal = True
isAsmBitshiftOp AsmShr = True
isAsmBitshiftOp AsmSar = True
isAsmBitshiftOp _ = False

isRegisterOperand :: AsmOperand -> Bool
isRegisterOperand (Register _) = True
isRegisterOperand _ = False

getRegister :: AsmOperand -> AsmReg
getRegister (Register r) = r
getRegister _ = error "Not a register"