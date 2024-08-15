module Pass (resolveAll) where

import AST (Program)
import CaseResolve
import LabelResolve
import LoopLabeling
import VarResolve

resolveAll :: Program -> Program
resolveAll program = case varResolve program
  >>= labelResolve
  >>= loopLabeling
  >>= caseResolve of
  Left err -> error err
  Right p -> p