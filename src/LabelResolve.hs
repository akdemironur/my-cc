{-# LANGUAGE InstanceSigs #-}

module LabelResolve (labelResolve) where

import AST
import Control.Monad.State
import qualified Data.Set as S

type LabelStateT a = StateT LabelSet (Either String) a

type LabelSet = S.Set Identifier

class LabelResolvePass1 a where
  resolveP1 :: a -> LabelStateT a

instance LabelResolvePass1 Program where
  resolveP1 :: Program -> LabelStateT Program
  resolveP1 (Program decls) = fmap Program (traverse resolveP1 decls)

instance LabelResolvePass1 Function where
  resolveP1 :: Function -> LabelStateT Function
  resolveP1 (Function name block) = fmap (Function name) (resolveP1 block)

instance LabelResolvePass1 Block where
  resolveP1 :: Block -> LabelStateT Block
  resolveP1 (Block items) = fmap Block (traverse resolveP1 items)

instance LabelResolvePass1 BlockItem where
  resolveP1 :: BlockItem -> LabelStateT BlockItem
  resolveP1 (BlockStmt stmt) = fmap BlockStmt (resolveP1 stmt)
  resolveP1 (BlockDecl decl) = return (BlockDecl decl)

instance LabelResolvePass1 Stmt where
  resolveP1 :: Stmt -> LabelStateT Stmt
  resolveP1 (LabeledStmt label stmt) = do
    labels <- get
    if S.member label labels
      then lift $ Left $ "Label " ++ show label ++ " already declared"
      else do
        put (S.insert label labels)
        LabeledStmt label <$> resolveP1 stmt
  resolveP1 (IfStmt cond thenBlock elseBlock) = IfStmt cond <$> resolveP1 thenBlock <*> traverse resolveP1 elseBlock
  resolveP1 (CompoundStmt block) = CompoundStmt <$> resolveP1 block
  resolveP1 (WhileStmt label cond block) = WhileStmt label cond <$> resolveP1 block
  resolveP1 (DoWhileStmt label block cond) = DoWhileStmt label <$> resolveP1 block <*> return cond
  resolveP1 (ForStmt label forInit cond iter block) = ForStmt label forInit cond iter <$> resolveP1 block
  resolveP1 (SwitchStmt label set def expr block) = SwitchStmt label set def expr <$> resolveP1 block
  resolveP1 (CaseStmt label expr stmt) = CaseStmt label expr <$> resolveP1 stmt
  resolveP1 stmt = return stmt

instance LabelResolvePass1 Declaration where
  resolveP1 :: Declaration -> LabelStateT Declaration
  resolveP1 = return

class LabelResolvePass2 a where
  resolveP2 :: a -> LabelStateT a

instance LabelResolvePass2 Program where
  resolveP2 :: Program -> LabelStateT Program
  resolveP2 (Program decls) = fmap Program (traverse resolveP2 decls)

instance LabelResolvePass2 Function where
  resolveP2 :: Function -> LabelStateT Function
  resolveP2 (Function name stmts) = fmap (Function name) (resolveP2 stmts)

instance LabelResolvePass2 Block where
  resolveP2 :: Block -> LabelStateT Block
  resolveP2 (Block items) = fmap Block (traverse resolveP2 items)

instance LabelResolvePass2 BlockItem where
  resolveP2 :: BlockItem -> LabelStateT BlockItem
  resolveP2 (BlockStmt stmt) = fmap BlockStmt (resolveP2 stmt)
  resolveP2 (BlockDecl decl) = return (BlockDecl decl)

instance LabelResolvePass2 Stmt where
  resolveP2 :: Stmt -> LabelStateT Stmt
  resolveP2 (GotoStmt label) = do
    labels <- get
    if S.member label labels
      then return $ GotoStmt label
      else lift $ Left $ "Label " ++ show label ++ " not found"
  resolveP2 (IfStmt cond thenBlock elseBlock) = IfStmt cond <$> resolveP2 thenBlock <*> traverse resolveP2 elseBlock
  resolveP2 (CompoundStmt block) = CompoundStmt <$> resolveP2 block
  resolveP2 (WhileStmt label cond block) = WhileStmt label cond <$> resolveP2 block
  resolveP2 (DoWhileStmt label block cond) = DoWhileStmt label <$> resolveP2 block <*> return cond
  resolveP2 (ForStmt label forInit cond iter block) = ForStmt label forInit cond iter <$> resolveP2 block
  resolveP2 (SwitchStmt label set def expr block) = SwitchStmt label set def expr <$> resolveP2 block
  resolveP2 (LabeledStmt label stmt) = LabeledStmt label <$> resolveP2 stmt
  resolveP2 (CaseStmt label expr stmt) = CaseStmt label expr <$> resolveP2 stmt
  resolveP2 stmt = return stmt

labelResolve :: Program -> Either String Program
labelResolve program = evalStateT (resolveP1 program >>= resolveP2) S.empty
