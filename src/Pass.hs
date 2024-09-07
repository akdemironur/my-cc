module Pass where

import AST (Program)
import CaseResolve
import LabelResolve
import LoopLabeling
import TypeCheck
import VarResolve

resolveAll :: Program -> (Program, SymbolTable)
resolveAll program = case varResolve program
  >>= labelResolve
  >>= loopLabeling
  >>= caseResolve
  >>= typeChecker of
  Left err -> error err
  Right p -> p