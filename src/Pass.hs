{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Pass where

import AST
import CaseResolve
import LabelResolve
import LoopLabeling
import TypeCheck
import VarResolve

resolveAll :: Program -> (Program, SymbolTable)
resolveAll program = case varResolve program
  >>= labelResolve
  >>= loopLabeling
  >>= typeChecker
  >>= caseResolve of
  Left err -> error err
  Right (p, st) -> (p, st)