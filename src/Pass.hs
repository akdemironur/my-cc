module Pass where

import AST (Program)
import LabelResolve
import VarResolve

resolveAll :: Program -> Program
resolveAll program = case varResolve program >>= labelResolve of
  Left err -> error err
  Right p -> p