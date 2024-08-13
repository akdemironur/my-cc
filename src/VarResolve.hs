{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module VarResolve where

import AST
import Control.Monad.State
import Data.Functor ((<&>))
import qualified Data.Map as M

type VarMap = M.Map Identifier Identifier

type VarResolve a = State (VarMap, Int) a

class Resolve a where
  resolve :: a -> VarResolve (Either String a)

uniqueNameGenerator :: Identifier -> VarResolve (Maybe Identifier)
uniqueNameGenerator identifier@(Identifier name) = do
  (varMap, num) <- get
  if M.member identifier varMap
    then return Nothing
    else do
      let newIdentifier = Identifier (name ++ "." ++ show num)
      put (M.insert identifier newIdentifier varMap, num + 1)
      return $ Just newIdentifier

instance Resolve Declaration where
  resolve :: Declaration -> VarResolve (Either String Declaration)
  resolve (Declaration name expr) = do
    newName <- uniqueNameGenerator name
    case newName of
      Nothing -> pure $ Left $ "Variable " ++ show name ++ " already declared"
      Just newIdentifier -> do
        case expr of
          Nothing -> pure $ Right $ Declaration newIdentifier Nothing
          Just e ->
            resolve e >>= \case
              Left err -> pure $ Left err
              Right newExpr -> pure $ Right $ Declaration newIdentifier (Just newExpr)

instance Resolve Stmt where
  resolve :: Stmt -> VarResolve (Either String Stmt)
  resolve stmt = case stmt of
    s@(LabelStmt _) -> pure $ Right s
    s@(GotoStmt _) -> pure $ Right s
    ExprStmt e -> resolveHelper ExprStmt e
    ReturnStmt e -> resolveHelper ReturnStmt e
    NullStmt -> pure $ Right NullStmt
    IfStmt c t e -> do
      c' <- resolve c
      t' <- resolve t
      e' <- mapM resolve e
      return $ IfStmt <$> c' <*> t' <*> sequence e'
    where
      resolveHelper constr e =
        resolve e >>= \case
          Left err -> pure $ Left err
          Right newExpr -> pure $ Right $ constr newExpr

instance Resolve Identifier where
  resolve :: Identifier -> VarResolve (Either String Identifier)
  resolve identifier = do
    (varMap, _) <- get
    if M.member identifier varMap
      then pure $ Right identifier
      else pure $ Left $ "Variable " ++ show identifier ++ " not declared."

instance Resolve Expr where
  resolve :: Expr -> VarResolve (Either String Expr)
  resolve (Assignment op l@(Var _) r) = do
    l' <- resolve l
    r' <- resolve r
    return $ Assignment op <$> l' <*> r'
  resolve (Assignment {}) = return $ Left "Invalid lvalue."
  resolve (Var identifier) = do
    (varMap, _) <- get
    if M.member identifier varMap
      then pure $ Right $ Var (varMap M.! identifier)
      else pure $ Left $ "Variable " ++ show identifier ++ " not declared."
  resolve (Unary PreDecrement e@(Var _)) = resolve e <&> (Unary PreDecrement <$>)
  resolve (Unary PreIncrement e@(Var _)) = resolve e <&> (Unary PreIncrement <$>)
  resolve (Unary PreDecrement _) = return . Left $ "Invalid pre-decrement expression."
  resolve (Unary PreIncrement _) = return . Left $ "Invalid pre-increment expression."
  resolve (Unary op e) = resolve e <&> (Unary op <$>)
  resolve (Binary op l r) = do
    l' <- resolve l
    r' <- resolve r
    return $ Binary op <$> l' <*> r'
  resolve (PostFix e@(Var _) op) = do
    e' <- resolve e
    return $ PostFix <$> e' <*> pure op
  resolve (PostFix e _) = return . Left $ "Invalid post-fix expression: " ++ show e
  resolve (Constant i) = pure $ Right $ Constant i
  resolve (Conditional c t e) = do
    c' <- resolve c
    t' <- resolve t
    e' <- resolve e
    return $ Conditional <$> c' <*> t' <*> e'

instance Resolve Program where
  resolve :: Program -> VarResolve (Either String Program)
  resolve (Program funcs) = do
    funcs' <- traverse resolve funcs
    return $ Program <$> sequence funcs'

instance Resolve Function where
  resolve :: Function -> VarResolve (Either String Function)
  resolve (Function name stmts) = do
    stmts' <- traverse resolve stmts
    return $ Function name <$> sequence stmts'

instance Resolve BlockItem where
  resolve :: BlockItem -> VarResolve (Either String BlockItem)
  resolve blockItem = case blockItem of
    BlockStmt stmt -> resolveHelper BlockStmt stmt
    BlockDecl decl -> resolveHelper BlockDecl decl
    where
      resolveHelper :: (Resolve a) => (a -> BlockItem) -> a -> VarResolve (Either String BlockItem)
      resolveHelper constr bi = do
        blockItem' <- resolve bi
        return $ constr <$> blockItem'

varResolve :: Program -> Either String Program
varResolve program = evalState (resolve program) (M.empty, 0)

resolveAll :: Program -> Program
resolveAll program = case evalState (resolve program) (M.empty, 0) of
  Left err -> error err
  Right program' -> program'