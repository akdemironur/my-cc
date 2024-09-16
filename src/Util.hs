{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Util where

import AST
import Data.Bits (complement, testBit, (.&.), (.|.))

roundAwayFromZero :: Int -> Int -> Int
roundAwayFromZero n x
  | x `mod` n == 0 = x
  | x < 0 = x - n - (x `mod` n)
  | otherwise = x + n - (x `mod` n)

is32BitRange :: Int -> Bool
is32BitRange x = x >= -2147483648 && x <= 2147483647

castLongToInt :: Int -> Int
castLongToInt x =
  let masked = x .&. 0xFFFFFFFF
      isNegative = testBit masked 31
      signExtended =
        if isNegative
          then masked .|. complement 0xFFFFFFFF
          else masked
   in signExtended

convertConst :: CType -> Const -> Const
convertConst CInt (ConstLong x) = ConstInt (castLongToInt x)
convertConst CLong (ConstInt x) = ConstLong x
convertConst _ x = x

reduceLong :: Int -> Int
reduceLong x = x - y
  where
    y :: Int
    y = 2 ^ (32 :: Int)

getCommonType :: CType -> CType -> CType
getCommonType (CFunc _ _) _ = error "Function type in getCommonType"
getCommonType _ (CFunc _ _) = error "Function type in getCommonType"
getCommonType t1 t2 = if t1 == t2 then t1 else CLong