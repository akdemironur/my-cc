{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module VarResolve where

import AST
import Control.Applicative ((<|>))
import Control.Monad.State
import Data.Functor ((<&>))
import qualified Data.Map as M

data ScopeVarMap = ScopeVarMap VarMap (Maybe ScopeVarMap) deriving (Eq, Show)

declareVar :: Identifier -> (ScopeVarMap, Int) -> ((ScopeVarMap, Int), Identifier)
declareVar identifier@(Identifier name) (ScopeVarMap varMap parent, num) = ((ScopeVarMap (M.insert identifier newIdentifier varMap) parent, num + 1), newIdentifier)
  where
    newIdentifier = Identifier (name ++ "." ++ show num)

isDeclared :: Identifier -> ScopeVarMap -> Bool
isDeclared identifier (ScopeVarMap varMap parent) =
  M.member identifier varMap || case parent of
    Nothing -> False
    Just p -> isDeclared identifier p

lookupVar :: Identifier -> ScopeVarMap -> Maybe Identifier
lookupVar identifier (ScopeVarMap varMap parent) =
  M.lookup identifier varMap <|> case parent of
    Nothing -> Nothing
    Just p -> lookupVar identifier p

isDeclaredInCurrentScope :: Identifier -> ScopeVarMap -> Bool
isDeclaredInCurrentScope identifier (ScopeVarMap varMap _) = M.member identifier varMap

updateVar :: Identifier -> Identifier -> ScopeVarMap -> ScopeVarMap
updateVar old new current@(ScopeVarMap varMap parent) =
  if isDeclaredInCurrentScope old current
    then ScopeVarMap (M.insert old new varMap) parent
    else ScopeVarMap varMap (fmap (updateVar old new) parent)

type VarMap = M.Map Identifier Identifier

type VarResolve a = State (ScopeVarMap, Int) a

class Resolve a where
  resolve :: a -> VarResolve (Either String a)

-- uniqueNameGenerator :: Identifier -> VarResolve (Maybe Identifier)
-- uniqueNameGenerator identifier@(Identifier name) = do
--   (varMap, num) <- get
--   if M.member identifier varMap
--     then return Nothing
--     else do
--       let newIdentifier = Identifier (name ++ "." ++ show num)
--       put (M.insert identifier newIdentifier varMap, num + 1)
--       return $ Just newIdentifier

instance Resolve Declaration where
  resolve :: Declaration -> VarResolve (Either String Declaration)
  resolve (Declaration name expr) = do
    s <- get
    if isDeclaredInCurrentScope name (fst s)
      then pure $ Left $ "Variable " ++ show name ++ " already declared."
      else do
        let ((newScope, newNum), newIdentifier) = declareVar name s
        put (newScope, newNum)
        case expr of
          Nothing -> return $ Right $ Declaration newIdentifier Nothing
          Just expr' -> resolve expr' <&> fmap (Declaration newIdentifier . Just)

instance Resolve Block where
  resolve :: Block -> VarResolve (Either String Block)
  resolve (Block items) = do
    (scope, num) <- get
    put (ScopeVarMap M.empty (Just scope), num)
    items' <- traverse resolve items
    (ScopeVarMap _ outerScope, num') <- get
    case outerScope of
      Nothing -> put (ScopeVarMap M.empty Nothing, num')
      Just scope' -> put (scope', num')
    return $ Block <$> sequence items'

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
    CompoundStmt block -> do block' <- resolve block; return $ CompoundStmt <$> block'
    where
      resolveHelper constr e =
        resolve e >>= \case
          Left err -> pure $ Left err
          Right newExpr -> pure $ Right $ constr newExpr

instance Resolve Identifier where
  resolve :: Identifier -> VarResolve (Either String Identifier)
  resolve identifier = do
    (scope, _) <- get
    if isDeclared identifier scope
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
    (scope, _) <- get
    case lookupVar identifier scope of
      Just newIdentifier -> return $ Right $ Var newIdentifier
      Nothing -> return $ Left $ "Variable " ++ show identifier ++ " not declared."
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
  resolve (Function name block) = do block' <- resolve block; return $ Function name <$> block'

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
varResolve program = evalState (resolve program) (ScopeVarMap M.empty Nothing, 0)

resolveAll :: Program -> Program
resolveAll program = case evalState (resolve program) (ScopeVarMap M.empty Nothing, 0) of
  Left err -> error err
  Right program' -> program'