{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module VarResolve where

import AST
import Control.Applicative ((<|>))
import Control.Monad.State
import qualified Data.Map as M

data ScopeVarMap = ScopeVarMap VarMap (Maybe ScopeVarMap) deriving (Eq, Show)

type VarMap = M.Map Identifier Identifier

type VarResolve a = StateT (ScopeVarMap, Int) (Either String) a

class Resolve a where
  resolve :: a -> VarResolve a

declareVarT :: Identifier -> VarResolve Identifier
declareVarT identifier@(Identifier name) = do
  (scope@(ScopeVarMap varMap parent), num) <- get
  if isDeclaredInCurrentScope identifier scope
    then lift $ Left $ "Variable " ++ name ++ " already declared."
    else do
      let identifier' = Identifier $ name ++ "." ++ show num
      put (ScopeVarMap (M.insert identifier identifier' varMap) parent, num + 1)
      return identifier'

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

lookupT :: Identifier -> VarResolve Identifier
lookupT identifier = do
  (scope, _) <- get
  case lookupVar identifier scope of
    Just identifier' -> return identifier'
    Nothing -> lift $ Left $ "Variable " ++ show identifier ++ " not declared."

isDeclaredInCurrentScope :: Identifier -> ScopeVarMap -> Bool
isDeclaredInCurrentScope identifier (ScopeVarMap varMap _) = M.member identifier varMap

instance Resolve Declaration where
  resolve :: Declaration -> VarResolve Declaration
  resolve (Declaration name expr) = Declaration <$> declareVarT name <*> traverse resolve expr

instance Resolve Block where
  resolve :: Block -> VarResolve Block
  resolve (Block items) = do
    (scope, num) <- get
    put (ScopeVarMap M.empty (Just scope), num)
    items' <- traverse resolve items
    (ScopeVarMap _ outerScope, num') <- get
    case outerScope of
      Nothing -> put (ScopeVarMap M.empty Nothing, num')
      Just scope' -> put (scope', num')
    return $ Block items'

instance Resolve Stmt where
  resolve :: Stmt -> VarResolve Stmt
  resolve stmt = case stmt of
    s@(LabelStmt _) -> return s
    s@(GotoStmt _) -> return s
    ExprStmt e -> ExprStmt <$> resolve e
    ReturnStmt e -> ReturnStmt <$> resolve e
    NullStmt -> return NullStmt
    IfStmt c t e -> IfStmt <$> resolve c <*> resolve t <*> traverse resolve e
    CompoundStmt block -> CompoundStmt <$> resolve block

instance Resolve Identifier where
  resolve :: Identifier -> VarResolve Identifier
  resolve identifier = do
    (scope, _) <- get
    if isDeclared identifier scope
      then return identifier
      else lift $ Left $ "Variable " ++ show identifier ++ " not declared."

instance Resolve Expr where
  resolve :: Expr -> VarResolve Expr
  resolve (Assignment op l@(Var _) r) = Assignment op <$> resolve l <*> resolve r
  resolve (Assignment {}) = lift $ Left "Invalid lvalue."
  resolve (Var identifier) = Var <$> lookupT identifier
  resolve (Unary PreDecrement e@(Var _)) = Unary PreDecrement <$> resolve e
  resolve (Unary PreIncrement e@(Var _)) = Unary PreIncrement <$> resolve e
  resolve (Unary PreDecrement _) = lift $ Left "Invalid pre-decrement expression."
  resolve (Unary PreIncrement _) = lift $ Left "Invalid pre-increment expression."
  resolve (Unary op e) = Unary op <$> resolve e
  resolve (Binary op l r) = Binary op <$> resolve l <*> resolve r
  resolve (PostFix e@(Var _) op) = PostFix <$> resolve e <*> pure op
  resolve (PostFix e _) = lift $ Left $ "Invalid post-fix expression: " ++ show e
  resolve (Constant i) = return $ Constant i
  resolve (Conditional c t e) = Conditional <$> resolve c <*> resolve t <*> resolve e

instance Resolve Program where
  resolve :: Program -> VarResolve Program
  resolve (Program funcs) = Program <$> traverse resolve funcs

instance Resolve Function where
  resolve :: Function -> VarResolve Function
  resolve (Function name block) = Function name <$> resolve block

instance Resolve BlockItem where
  resolve :: BlockItem -> VarResolve BlockItem
  resolve (BlockStmt stmt) = BlockStmt <$> resolve stmt
  resolve (BlockDecl decl) = BlockDecl <$> resolve decl

varResolve :: Program -> Either String Program
varResolve program = evalStateT (resolve program) (ScopeVarMap M.empty Nothing, 0)
