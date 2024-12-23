{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module VarResolve where

import AST
import Control.Applicative ((<|>))
import Control.Monad.Except (throwError)
import Control.Monad.State
import qualified Data.Map as M
import Data.Maybe (isJust)

data ScopeVarMap = ScopeVarMap VarMap (Maybe ScopeVarMap) deriving (Eq, Show)

type VarMap = M.Map Identifier (Identifier, Bool)

type VarResolve a = StateT (ScopeVarMap, Int) (Either String) a

class Resolve a where
  resolve :: a -> VarResolve a

declareVarT :: Maybe StorageClass -> Identifier -> VarResolve Identifier
declareVarT sc name = do
  (scope@(ScopeVarMap varMap parent), num) <- get
  let oldDecl = lookupVarLink name scope
  let inCurrentScope = isDeclaredInCurrentScope name scope
  case (inCurrentScope, oldDecl, sc) of
    (True, Just (_, _), Nothing) -> throwError $ "Variable " ++ name ++ " already declared."
    (True, Just (_, False), _) -> throwError $ "Conflicting linkage for variable: " ++ name
    (True, Just _, Just Static) -> throwError $ "Conflicting linkage for variable: " ++ name
    _ -> return ()
  let identifier' = name ++ "." ++ show num
  put (ScopeVarMap (M.insert name (identifier', sc == Just Extern) varMap) parent, num + 1)
  return identifier'

declareFunT :: Identifier -> VarResolve Identifier
declareFunT name = do
  (scope@(ScopeVarMap varMap parent), num) <- get
  case (lookupVarLink name scope, isDeclaredInCurrentScope name scope) of
    (Just (_, False), True) -> lift $ Left $ "Function " ++ name ++ " already declared."
    _ -> do
      put (ScopeVarMap (M.insert name (name, True) varMap) parent, num)
      return name

isDeclared :: Identifier -> ScopeVarMap -> Bool
isDeclared identifier (ScopeVarMap varMap parent) =
  M.member identifier varMap || case parent of
    Nothing -> False
    Just p -> isDeclared identifier p

lookupVarLink :: Identifier -> ScopeVarMap -> Maybe (Identifier, Bool)
lookupVarLink identifier (ScopeVarMap varMap parent) =
  M.lookup identifier varMap <|> case parent of
    Nothing -> Nothing
    Just p -> lookupVarLink identifier p

lookupT :: Identifier -> VarResolve Identifier
lookupT identifier = do
  (scope, _) <- get
  case lookupVarLink identifier scope of
    Just (identifier', _) -> return identifier'
    Nothing -> lift $ Left $ "Variable " ++ show identifier ++ " not declared."

isDeclaredInCurrentScope :: Identifier -> ScopeVarMap -> Bool
isDeclaredInCurrentScope identifier (ScopeVarMap varMap _) = M.member identifier varMap

declareVarTFileScope :: Identifier -> VarResolve Identifier
declareVarTFileScope identifier = do
  (ScopeVarMap varMap parent, num) <- get
  put (ScopeVarMap (M.insert identifier (identifier, True) varMap) parent, num)
  return identifier

instance Resolve VarDecl where
  resolve :: VarDecl -> VarResolve VarDecl
  resolve (VarDecl name expr vtype sc) = do
    (scope@(ScopeVarMap _ outerScope), _) <- get
    case outerScope of
      Nothing -> VarDecl <$> declareVarTFileScope name <*> traverse resolve expr <*> pure vtype <*> pure sc
      Just _ -> do
        case (lookupVarLink name scope, isDeclaredInCurrentScope name scope, sc) of
          (Just (_, False), True, _) -> lift $ Left $ "Conflicting local variable declaration: " ++ show name
          _ -> return ()
        if sc == Just Extern
          then VarDecl <$> declareVarTFileScope name <*> traverse resolve expr <*> pure vtype <*> pure sc
          else VarDecl <$> declareVarT sc name <*> traverse resolve expr <*> pure vtype <*> pure sc

restoreOuterScope :: VarResolve ()
restoreOuterScope = do
  (ScopeVarMap _ outerScope, num') <- get
  case outerScope of
    Nothing -> error "FuncDecl Resolve: Outer scope should not be Nothing."
    Just scope' -> put (scope', num')

instance Resolve FuncDecl where
  resolve :: FuncDecl -> VarResolve FuncDecl
  resolve (FuncDecl name args block ftype sc) = do
    (ScopeVarMap _ outerScope, _) <- get
    when (isJust outerScope && isJust block) $ lift $ Left "Nested function definition not allowed."
    when (isJust outerScope && sc == Just Static) $ lift $ Left "Static function definition not allowed in block scope."
    name' <- declareFunT name
    (scope, num) <- get
    put (ScopeVarMap M.empty (Just scope), num)
    args' <- traverse (declareVarT Nothing) args
    case block of
      Nothing -> do
        restoreOuterScope
        return $ FuncDecl name' args' Nothing ftype sc
      Just (Block items) -> do
        items' <- traverse resolve items
        restoreOuterScope
        return $ FuncDecl name' args' (Just (Block items')) ftype sc

instance Resolve Block where
  resolve :: Block -> VarResolve Block
  resolve (Block items) = do
    (scope, num) <- get
    put (ScopeVarMap M.empty (Just scope), num)
    items' <- traverse resolve items
    restoreOuterScope
    return $ Block items'

instance Resolve Stmt where
  resolve :: Stmt -> VarResolve Stmt
  resolve (LabeledStmt label stmt) = LabeledStmt label <$> resolve stmt
  resolve s@(GotoStmt _) = return s
  resolve (ExprStmt e) = ExprStmt <$> resolve e
  resolve (ReturnStmt e) = ReturnStmt <$> resolve e
  resolve NullStmt = return NullStmt
  resolve (IfStmt c t e) = IfStmt <$> resolve c <*> resolve t <*> traverse resolve e
  resolve (CompoundStmt block) = CompoundStmt <$> resolve block
  resolve s@(BreakStmt _) = return s
  resolve s@(ContinueStmt _) = return s
  resolve (DoWhileStmt label block e) = DoWhileStmt label <$> resolve block <*> resolve e
  resolve (WhileStmt label e block) = WhileStmt label <$> resolve e <*> resolve block
  resolve (ForStmt label forInit cond iter block) = do
    (scope, num) <- get
    put (ScopeVarMap M.empty (Just scope), num)
    forInit' <- resolve forInit
    cond' <- traverse resolve cond
    iter' <- traverse resolve iter
    block' <- resolve block
    (ScopeVarMap _ outerScope, num') <- get
    case outerScope of
      Nothing -> error "For Resolve: Outer scope should not be Nothing."
      Just scope' -> put (scope', num')
    return $ ForStmt label forInit' cond' iter' block'
  resolve (SwitchStmt l s d e stmt) = SwitchStmt l s d <$> resolve e <*> resolve stmt
  resolve (CaseStmt l e@(TypedExpr (Constant _) _) s) = CaseStmt l e <$> resolve s
  resolve (CaseStmt {}) = lift $ Left "Case statement must have an integer literal."
  resolve (DefaultStmt l) = return $ DefaultStmt l

instance Resolve ForInit where
  resolve :: ForInit -> VarResolve ForInit
  resolve (InitDecl decl@(VarDecl _ _ _ Nothing)) = InitDecl <$> resolve decl
  resolve (InitDecl _) = lift $ Left "for loop initializer cannot have storage class."
  resolve (InitExpr e) = InitExpr <$> traverse resolve e

instance Resolve TypedExpr where
  resolve :: TypedExpr -> VarResolve TypedExpr
  resolve (TypedExpr expr t) = TypedExpr <$> resolve expr <*> pure t

instance Resolve Expr where
  resolve :: Expr -> VarResolve Expr
  resolve (Assignment op l@(TypedExpr (Var _) _) r) = Assignment op <$> resolve l <*> resolve r
  resolve (Assignment {}) = lift $ Left "Invalid lvalue."
  resolve (Var identifier) = Var <$> lookupT identifier
  resolve (Unary PreDecrement e@(TypedExpr (Var _) _)) = Unary PreDecrement <$> resolve e
  resolve (Unary PreIncrement e@(TypedExpr (Var _) _)) = Unary PreIncrement <$> resolve e
  resolve (Unary PreDecrement _) = lift $ Left "Invalid pre-decrement expression."
  resolve (Unary PreIncrement _) = lift $ Left "Invalid pre-increment expression."
  resolve (Unary op e) = Unary op <$> resolve e
  resolve (Binary op l r) = Binary op <$> resolve l <*> resolve r
  resolve (PostFix e@(TypedExpr (Var _) _) op) = PostFix <$> resolve e <*> pure op
  resolve (PostFix e _) = lift $ Left $ "Invalid post-fix expression: " ++ show e
  resolve (Constant i) = return $ Constant i
  resolve (Conditional c t e) = Conditional <$> resolve c <*> resolve t <*> resolve e
  resolve (FunctionCall f args) = FunctionCall <$> lookupT f <*> traverse resolve args
  resolve (Cast t e) = Cast t <$> resolve e

instance Resolve Program where
  resolve :: Program -> VarResolve Program
  resolve (Program funcs) = Program <$> traverse resolve funcs

instance Resolve Decl where
  resolve :: Decl -> VarResolve Decl
  resolve (VDecl decl) = VDecl <$> resolve decl
  resolve (FDecl decl) = FDecl <$> resolve decl

instance Resolve BlockItem where
  resolve :: BlockItem -> VarResolve BlockItem
  resolve (BlockStmt stmt) = BlockStmt <$> resolve stmt
  resolve (BlockDecl decl) = BlockDecl <$> resolve decl

varResolve :: Program -> Either String Program
varResolve program = evalStateT (resolve program) (ScopeVarMap M.empty Nothing, 0)
