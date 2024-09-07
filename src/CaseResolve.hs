{-# LANGUAGE InstanceSigs #-}

module CaseResolve (caseResolve) where

import AST
import Control.Monad.State
import qualified Data.Map as M
import qualified Data.Set as S

type CaseMap = M.Map String (S.Set IntLiteral)

type DefaultMap = M.Map String Bool

type CaseDefaultState = (CaseMap, DefaultMap)

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
  resolve (FuncDecl name args block sc) = FuncDecl name args <$> traverse resolve block <*> pure sc

instance CaseResolve Decl where
  resolve :: Decl -> CaseResolveT Decl
  resolve (VDecl decl) = VDecl <$> resolve decl
  resolve (FDecl decl) = FDecl <$> resolve decl

instance CaseResolve Stmt where
  resolve :: Stmt -> CaseResolveT Stmt
  resolve (CaseStmt (Just (Identifier name)) (Constant i) stmt) = do
    (caseMap, defaultMap) <- get
    let caseSet = M.findWithDefault S.empty name caseMap
    if S.member i caseSet
      then lift $ Left "Duplicate case statement"
      else do
        put (M.insert name (S.insert i caseSet) caseMap, defaultMap)
        CaseStmt (Just (Identifier name)) (Constant i) <$> resolve stmt
  resolve d@(DefaultStmt (Just (Identifier l))) = do
    (caseMap, defaultMap) <- get
    if M.findWithDefault False l defaultMap
      then lift $ Left "Duplicate default statement"
      else do
        put (caseMap, M.insert l True defaultMap)
        return d
  resolve (CaseStmt {}) = lift $ Left "Invalid case"
  resolve (CompoundStmt stmt) = CompoundStmt <$> resolve stmt
  resolve (DoWhileStmt l stmt expr) = DoWhileStmt l <$> resolve stmt <*> pure expr
  resolve (WhileStmt l expr stmt) = WhileStmt l expr <$> resolve stmt
  resolve (ForStmt l forinit expr inc stmt) = ForStmt l forinit expr inc <$> resolve stmt
  resolve (IfStmt expr thenStmt elseStmt) = IfStmt expr <$> resolve thenStmt <*> traverse resolve elseStmt
  resolve (SwitchStmt label@(Just (Identifier l)) s d expr block) = do
    block' <- resolve block
    (caseMap, defaultMap) <- get
    let thisCaseSet = M.findWithDefault S.empty l caseMap
    let thisDefault = M.findWithDefault False l defaultMap
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

caseResolve :: Program -> Either String Program
caseResolve program = evalStateT (resolve program) (M.empty, M.empty)