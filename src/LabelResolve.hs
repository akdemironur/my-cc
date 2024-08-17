{-# LANGUAGE InstanceSigs #-}

module LabelResolve (labelResolve) where

import AST
import Control.Monad.State
  ( MonadState (get, put),
    MonadTrans (lift),
    StateT,
    evalStateT,
  )
import qualified Data.Set as S

type LabelStateT a = StateT LabelSet (Either String) a

type LabelSet = S.Set Identifier

class LabelResolve a where
  labelResolve :: a -> Either String a

instance LabelResolve Program where
  labelResolve :: Program -> Either String Program
  labelResolve (Program decls) = Program <$> traverse labelResolve decls

instance LabelResolve FuncDecl where
  labelResolve :: FuncDecl -> Either String FuncDecl
  labelResolve (FuncDecl name@(Identifier fname) args block) = do
    let p = evalStateT (traverse (resolveP1 fname) block >>= traverse resolveP2) S.empty
    case p of
      Left err -> Left err
      Right p' -> return $ FuncDecl name args p'

class LabelResolvePass1 a where
  resolveP1 :: String -> a -> LabelStateT a

instance LabelResolvePass1 Block where
  resolveP1 :: String -> Block -> LabelStateT Block
  resolveP1 fname (Block items) = fmap Block (traverse (resolveP1 fname) items)

instance LabelResolvePass1 BlockItem where
  resolveP1 :: String -> BlockItem -> LabelStateT BlockItem
  resolveP1 fname (BlockStmt stmt) = fmap BlockStmt (resolveP1 fname stmt)
  resolveP1 _ (BlockDecl decl) = return (BlockDecl decl)

instance LabelResolvePass1 Stmt where
  resolveP1 :: String -> Stmt -> LabelStateT Stmt
  resolveP1 fname (LabeledStmt (Identifier label') stmt) = do
    let label = Identifier $ fname ++ "_fun_" ++ label'
    labels <- get
    if S.member label labels
      then lift $ Left $ "Label " ++ show label ++ " already declared"
      else do
        put (S.insert label labels)
        LabeledStmt label <$> resolveP1 fname stmt
  resolveP1 fname (IfStmt cond thenBlock elseBlock) = IfStmt cond <$> resolveP1 fname thenBlock <*> traverse (resolveP1 fname) elseBlock
  resolveP1 fname (CompoundStmt block) = CompoundStmt <$> resolveP1 fname block
  resolveP1 fname (WhileStmt label cond block) = WhileStmt label cond <$> resolveP1 fname block
  resolveP1 fname (DoWhileStmt label block cond) = DoWhileStmt label <$> resolveP1 fname block <*> return cond
  resolveP1 fname (ForStmt label forInit cond iter block) = ForStmt label forInit cond iter <$> resolveP1 fname block
  resolveP1 fname (SwitchStmt label set def expr block) = SwitchStmt label set def expr <$> resolveP1 fname block
  resolveP1 fname (CaseStmt label expr stmt) = CaseStmt label expr <$> resolveP1 fname stmt
  resolveP1 fname (GotoStmt (Identifier label)) = return . GotoStmt . Identifier $ fname ++ "_fun_" ++ label
  resolveP1 _ stmt = return stmt

instance LabelResolvePass1 VarDecl where
  resolveP1 :: String -> VarDecl -> LabelStateT VarDecl
  resolveP1 _ = return

class LabelResolvePass2 a where
  resolveP2 :: a -> LabelStateT a

instance LabelResolvePass2 Program where
  resolveP2 :: Program -> LabelStateT Program
  resolveP2 (Program decls) = fmap Program (traverse resolveP2 decls)

instance LabelResolvePass2 FuncDecl where
  resolveP2 :: FuncDecl -> LabelStateT FuncDecl
  resolveP2 (FuncDecl name args block) = FuncDecl name args <$> traverse resolveP2 block

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
