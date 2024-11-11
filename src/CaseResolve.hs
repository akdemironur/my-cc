{-# LANGUAGE InstanceSigs #-}

module CaseResolve (caseResolve) where

import AST
import Control.Monad.State
import qualified Data.Map as M
import Data.Maybe (fromJust)
import qualified Data.Set as S
import Util

type CaseMap = M.Map String (S.Set Const)

type SwitchTypeMap = M.Map String CType

type DefaultMap = M.Map String Bool

type CaseDefaultState = (CaseMap, DefaultMap, SwitchTypeMap)

type CaseResolveT a = StateT CaseDefaultState (Either String) a

class CaseResolve a where
  resolve :: a -> CaseResolveT a

instance CaseResolve Program where
  resolve :: Program -> CaseResolveT Program
  resolve (Program functions) = Program <$> traverse resolve functions

instance CaseResolve Block where
  resolve :: Block -> CaseResolveT Block
  resolve (Block items) = Block <$> traverse resolve items

instance CaseResolve BlockItem where
  resolve :: BlockItem -> CaseResolveT BlockItem
  resolve (BlockStmt stmt) = BlockStmt <$> resolve stmt
  resolve (BlockDecl decl) = BlockDecl <$> resolve decl

instance CaseResolve VarDecl where
  resolve :: VarDecl -> CaseResolveT VarDecl
  resolve = pure

instance CaseResolve FuncDecl where
  resolve :: FuncDecl -> CaseResolveT FuncDecl
  resolve (FuncDecl name args block ftype sc) = FuncDecl name args <$> traverse resolve block <*> pure ftype <*> pure sc

instance CaseResolve Decl where
  resolve :: Decl -> CaseResolveT Decl
  resolve (VDecl decl) = VDecl <$> resolve decl
  resolve (FDecl decl) = FDecl <$> resolve decl

instance CaseResolve Stmt where
  resolve :: Stmt -> CaseResolveT Stmt
  resolve (CaseStmt _ (TypedExpr (Constant (DoubleConst _ _)) _) _) = lift $ Left "Case with double, this should not happen"
  resolve (CaseStmt (Just name) (TypedExpr (Constant i') _) stmt) = do
    (caseMap, defaultMap, switchTypeMap) <- get
    let caseSet = M.findWithDefault S.empty name caseMap
        switchType = fromJust $ M.lookup name switchTypeMap
        i = convertConst switchType i'
    if S.member i caseSet
      then lift $ Left "Duplicate case statement"
      else do
        put (M.insert name (S.insert i caseSet) caseMap, defaultMap, switchTypeMap)
        CaseStmt (Just name) (TypedExpr (Constant i) (Just switchType)) <$> resolve stmt
  resolve d@(DefaultStmt (Just l)) = do
    (caseMap, defaultMap, switchTypeMap) <- get
    if M.findWithDefault False l defaultMap
      then lift $ Left "Duplicate default statement"
      else do
        put (caseMap, M.insert l True defaultMap, switchTypeMap)
        return d
  resolve (CaseStmt {}) = lift $ Left "Invalid case"
  resolve (CompoundStmt stmt) = CompoundStmt <$> resolve stmt
  resolve (DoWhileStmt l stmt expr) = DoWhileStmt l <$> resolve stmt <*> pure expr
  resolve (WhileStmt l expr stmt) = WhileStmt l expr <$> resolve stmt
  resolve (ForStmt l forinit expr inc stmt) = ForStmt l forinit expr inc <$> resolve stmt
  resolve (IfStmt expr thenStmt elseStmt) = IfStmt expr <$> resolve thenStmt <*> traverse resolve elseStmt
  resolve (SwitchStmt label@(Just l) s d expr block) = do
    (caseMap', defaultMap', switchTypeMap) <- get
    let newSwitchTypeMap = M.insert l (fromJust $ tyType expr) switchTypeMap
    put (caseMap', defaultMap', newSwitchTypeMap)
    block' <- resolve block
    (caseMap, defaultMap, _) <- get
    let thisCaseSet = M.findWithDefault S.empty l caseMap
        thisDefault = M.findWithDefault False l defaultMap
    if d || not (S.null s)
      then lift $ Left "This switch statement has already been resolved, this should not happen"
      else return $ SwitchStmt label thisCaseSet thisDefault expr block'
  resolve (ReturnStmt expr) = return $ ReturnStmt expr
  resolve (BreakStmt l) = return $ BreakStmt l
  resolve (ContinueStmt l) = return $ ContinueStmt l
  resolve (LabeledStmt l s) = LabeledStmt l <$> resolve s
  resolve (GotoStmt l) = return $ GotoStmt l
  resolve (ExprStmt expr) = return $ ExprStmt expr
  resolve NullStmt = return NullStmt
  resolve (DefaultStmt _) = lift $ Left "Case without label, this should not happen"
  resolve (SwitchStmt {}) = lift $ Left "Switch without label, this should not happen"

caseResolve :: (Program, SymbolTable) -> Either String (Program, SymbolTable)
caseResolve (program, st) = case evalStateT (resolve program) (M.empty, M.empty, M.empty) of
  Left err -> Left err
  Right p -> Right (p, st)