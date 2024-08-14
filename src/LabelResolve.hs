{-# LANGUAGE InstanceSigs #-}

module LabelResolve where

import AST
import Control.Monad.State
import qualified Data.Set as S

type LabelSet = S.Set Identifier

type LabelState a = State LabelSet a

class LabelResolvePass1 a where
  resolveP1 :: a -> LabelState (Either String a)

instance LabelResolvePass1 Program where
  resolveP1 :: Program -> LabelState (Either String Program)
  resolveP1 (Program decls) = fmap (fmap Program . sequence) (traverse resolveP1 decls)

instance LabelResolvePass1 Function where
  resolveP1 :: Function -> LabelState (Either String Function)
  resolveP1 (Function name block) = fmap (fmap (Function name)) (resolveP1 block)

instance LabelResolvePass1 Block where
  resolveP1 :: Block -> LabelState (Either String Block)
  resolveP1 (Block items) = fmap (fmap Block . sequence) (traverse resolveP1 items)

instance LabelResolvePass1 BlockItem where
  resolveP1 :: BlockItem -> LabelState (Either String BlockItem)
  resolveP1 (BlockStmt stmt) = fmap (fmap BlockStmt) (resolveP1 stmt)
  resolveP1 (BlockDecl decl) = return (Right $ BlockDecl decl)

instance LabelResolvePass1 Stmt where
  resolveP1 :: Stmt -> LabelState (Either String Stmt)
  resolveP1 (LabelStmt label) = do
    labels <- get
    ( if S.member label labels
        then return $ Left $ "Label " ++ show label ++ " already declared"
        else
          ( do
              put (S.insert label labels)
              return $ Right $ LabelStmt label
          )
      )
  resolveP1 (IfStmt cond thenBlock elseBlock) = do
    thenBlock' <- resolveP1 thenBlock
    elseBlock' <- mapM resolveP1 elseBlock
    case (thenBlock', sequence elseBlock') of
      (Left err, _) -> return $ Left err
      (_, Left err) -> return $ Left err
      (Right thenBlock'', Right elseBlock'') -> return $ Right $ IfStmt cond thenBlock'' elseBlock''
  resolveP1 (CompoundStmt block) = fmap (fmap CompoundStmt) (resolveP1 block)
  resolveP1 stmt = return $ Right stmt

instance LabelResolvePass1 Declaration where
  resolveP1 :: Declaration -> LabelState (Either String Declaration)
  resolveP1 decl = return $ Right decl

class LabelResolvePass2 a where
  resolveP2 :: a -> LabelState (Either String a)

instance LabelResolvePass2 Program where
  resolveP2 :: Program -> LabelState (Either String Program)
  resolveP2 (Program decls) = fmap (fmap Program . sequence) (traverse resolveP2 decls)

instance LabelResolvePass2 Function where
  resolveP2 :: Function -> LabelState (Either String Function)
  resolveP2 (Function name stmts) = fmap (fmap (Function name)) (resolveP2 stmts)

instance LabelResolvePass2 Block where
  resolveP2 :: Block -> LabelState (Either String Block)
  resolveP2 (Block items) = fmap (fmap Block . sequence) (traverse resolveP2 items)

instance LabelResolvePass2 BlockItem where
  resolveP2 :: BlockItem -> LabelState (Either String BlockItem)
  resolveP2 (BlockStmt stmt) = fmap (fmap BlockStmt) (resolveP2 stmt)
  resolveP2 (BlockDecl decl) = return (Right $ BlockDecl decl)

instance LabelResolvePass2 Stmt where
  resolveP2 :: Stmt -> LabelState (Either String Stmt)
  resolveP2 (GotoStmt label) = do
    labels <- get
    if S.member label labels
      then return $ Right $ GotoStmt label
      else return $ Left $ "Label " ++ show label ++ " not found"
  resolveP2 (IfStmt cond thenBlock elseBlock) = do
    thenBlock' <- resolveP2 thenBlock
    elseBlock' <- mapM resolveP2 elseBlock
    case (thenBlock', sequence elseBlock') of
      (Left err, _) -> return $ Left err
      (_, Left err) -> return $ Left err
      (Right thenBlock'', Right elseBlock'') -> return $ Right $ IfStmt cond thenBlock'' elseBlock''
  resolveP2 (CompoundStmt block) = fmap (fmap CompoundStmt) (resolveP2 block)
  resolveP2 stmt = return $ Right stmt

labelResolvePass :: Program -> LabelState (Either String Program)
labelResolvePass program = resolveP1 program >>= either (return . Left) resolveP2

labelResolve :: Program -> Either String Program
labelResolve program = evalState (labelResolvePass program) S.empty