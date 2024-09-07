{-# LANGUAGE TupleSections #-}

module TypeCheck where

import AST
import Control.Monad.State
import qualified Data.Map as M
import Data.Maybe (isJust, isNothing)

data IdentifierAttrs
  = FuncAttr Bool Bool
  | StaticAttr InitialValue Bool
  | LocalAttr
  deriving (Show, Eq)

data InitialValue
  = Tentative
  | Initial Int
  | NoInitializer
  deriving (Show, Eq)

type TypeCheckT a = StateT SymbolTable (Either String) a

type SymbolTable = M.Map Identifier (CType, IdentifierAttrs)

data CType = CInt | CFunc Int deriving (Show, Eq)

class TypeCheck a where
  typeCheck :: a -> TypeCheckT a

lookupSymbol :: Identifier -> TypeCheckT (Maybe (CType, IdentifierAttrs))
lookupSymbol identifier = gets (M.lookup identifier)

instance TypeCheck Program where
  typeCheck (Program decls) = Program <$> traverse typeCheckDeclFileScope decls

instance TypeCheck Block where
  typeCheck (Block items) = Block <$> traverse typeCheck items

instance TypeCheck BlockItem where
  typeCheck (BlockStmt stmt) = BlockStmt <$> typeCheck stmt
  typeCheck (BlockDecl decl) = BlockDecl <$> typeCheck decl

instance TypeCheck Decl where
  typeCheck (VDecl decl) = VDecl <$> typeCheckVarDeclLocalScope decl
  typeCheck (FDecl decl) = FDecl <$> typeCheck decl

typeCheckDeclFileScope :: Decl -> TypeCheckT Decl
typeCheckDeclFileScope (VDecl decl) = VDecl <$> typeCheckVarDeclFileScope decl
typeCheckDeclFileScope (FDecl decl) = FDecl <$> typeCheck decl

initialValueT :: Maybe Expr -> Maybe StorageClass -> TypeCheckT InitialValue
initialValueT (Just (Constant (IntLiteral c))) _ = return $ Initial c
initialValueT Nothing (Just Extern) = return NoInitializer
initialValueT Nothing _ = return Tentative
initialValueT _ _ = lift . Left $ "Non constant initializer for variable"

initialValue2T :: Maybe (CType, IdentifierAttrs) -> InitialValue -> TypeCheckT InitialValue
initialValue2T (Just (CFunc _, _)) _ = lift . Left $ "Variable already declared as function"
initialValue2T (Just (_, FuncAttr _ _)) _ = lift . Left $ "Variable already declared as function"
initialValue2T (Just (_, LocalAttr)) _ = lift . Left $ "Variable already declared as local"
initialValue2T (Just (_, StaticAttr (Initial _) _)) (Initial _) = lift . Left $ "Conflicting file scope variable definitions"
initialValue2T (Just (_, StaticAttr (Initial i) _)) _ = return (Initial i)
initialValue2T (Just (_, StaticAttr Tentative _)) Tentative = return Tentative
initialValue2T (Just (_, StaticAttr Tentative _)) NoInitializer = return Tentative
initialValue2T (Just (_, StaticAttr _ _)) NoInitializer = return NoInitializer
initialValue2T (Just (_, StaticAttr _ _)) (Initial i) = return (Initial i)
initialValue2T (Just (_, StaticAttr NoInitializer _)) iv = return iv
initialValue2T Nothing iv = return iv

varGlobalT :: Maybe (CType, IdentifierAttrs) -> Maybe StorageClass -> TypeCheckT Bool
varGlobalT (Just (_, StaticAttr _ global)) (Just Extern) = return global
varGlobalT (Just (_, StaticAttr _ prevGlob)) sc
  | (sc /= Just Static) == prevGlob = return prevGlob
  | otherwise = lift . Left $ "Conflicting variable linkage"
varGlobalT Nothing sc = return (sc /= Just Static)
varGlobalT (Just (_, FuncAttr _ _)) _ = lift . Left $ "Variable already declared as function"
varGlobalT (Just (_, LocalAttr)) _ = lift . Left $ "Conflicting variable linkage"

typeCheckVarDeclFileScope :: VarDecl -> TypeCheckT VarDecl
typeCheckVarDeclFileScope (VarDecl name expr sc) = do
  currentInitial <- initialValueT expr sc
  oldSymbol <- lookupSymbol name
  global <- varGlobalT oldSymbol sc
  initialValue <- initialValue2T oldSymbol currentInitial
  let attrs = StaticAttr initialValue global
  modify (M.insert name (CInt, attrs))
  VarDecl name <$> traverse typeCheck expr <*> pure sc

typeCheckVarDeclLocalScope :: VarDecl -> TypeCheckT VarDecl
typeCheckVarDeclLocalScope (VarDecl name (Just _) (Just Extern)) = lift . Left $ "Initializer on local extern variable declaration: " ++ show name
typeCheckVarDeclLocalScope (VarDecl name Nothing (Just Extern)) = do
  oldSymbol <- lookupSymbol name
  case oldSymbol of
    Just (CFunc _, _) -> lift . Left $ "Variable already declared as function: " ++ show name
    Just _ -> return ()
    _ -> modify (M.insert name (CInt, StaticAttr NoInitializer True))
  return $ VarDecl name Nothing (Just Extern)
typeCheckVarDeclLocalScope (VarDecl name expr (Just Static)) = do
  case expr of
    Just (Constant _) -> return ()
    Nothing -> return ()
    _ -> lift . Left $ "Non constant initializer for static variable: " ++ show name
  let iv = case expr of
        Just (Constant (IntLiteral c)) -> Initial c
        Nothing -> Initial 0
        _ -> error "Unreachable"
  modify (M.insert name (CInt, StaticAttr iv False))
  VarDecl name <$> traverse typeCheck expr <*> pure (Just Static)
typeCheckVarDeclLocalScope (VarDecl name expr Nothing) = do
  modify (M.insert name (CInt, LocalAttr))
  VarDecl name <$> traverse typeCheck expr <*> pure Nothing

instance TypeCheck FuncDecl where
  typeCheck (FuncDecl name args block sc) = do
    let funcType = CFunc $ length args
    let hasBody = isJust block
    let global = sc /= Just Static
    oldSymbol <- lookupSymbol name
    case oldSymbol of
      Just (oldType@(CFunc _), FuncAttr oldDefined oldGlobal) -> do
        when (oldType /= funcType) . lift . Left $ "Function " ++ show name ++ " already declared with different type"
        when (oldDefined && hasBody) . lift . Left $ "Function " ++ show name ++ " already defined"
        when (oldGlobal && sc == Just Static) . lift . Left $ "Static function declaration follows non-static declaration"
        let attrs = FuncAttr (oldDefined || hasBody) oldGlobal
        modify (M.insert name (funcType, attrs))
      Nothing -> modify (M.insert name (funcType, FuncAttr hasBody global))
      _ -> lift . Left $ "Function " ++ show name ++ " already declared as variable"

    modify . M.union . M.fromList $ map (,(CInt, LocalAttr)) args
    if hasBody
      then FuncDecl name args <$> traverse typeCheck block <*> pure sc
      else return $ FuncDecl name args block sc

instance TypeCheck Stmt where
  typeCheck (CompoundStmt block) = CompoundStmt <$> typeCheck block
  typeCheck (DoWhileStmt l stmt expr) = DoWhileStmt l <$> typeCheck stmt <*> typeCheck expr
  typeCheck (WhileStmt l expr stmt) = WhileStmt l <$> typeCheck expr <*> typeCheck stmt
  typeCheck (ForStmt l forinit cond inc stmt) = ForStmt l <$> typeCheck forinit <*> traverse typeCheck cond <*> traverse typeCheck inc <*> typeCheck stmt
  typeCheck (ContinueStmt l) = return $ ContinueStmt l
  typeCheck (ReturnStmt expr) = ReturnStmt <$> typeCheck expr
  typeCheck (ExprStmt expr) = ExprStmt <$> typeCheck expr
  typeCheck (IfStmt expr thenStmt elseStmt) = IfStmt <$> typeCheck expr <*> typeCheck thenStmt <*> traverse typeCheck elseStmt
  typeCheck (SwitchStmt identifier caseSet hasDefault expr stmt) = SwitchStmt identifier caseSet hasDefault <$> typeCheck expr <*> typeCheck stmt
  typeCheck (CaseStmt identifier constant stmt) = CaseStmt identifier constant <$> typeCheck stmt
  typeCheck (DefaultStmt identifier) = return $ DefaultStmt identifier
  typeCheck (GotoStmt identifier) = return $ GotoStmt identifier
  typeCheck (LabeledStmt identifier stmt) = LabeledStmt identifier <$> typeCheck stmt
  typeCheck (BreakStmt l) = return $ BreakStmt l
  typeCheck NullStmt = return NullStmt

instance TypeCheck Expr where
  typeCheck (FunctionCall name args) = do
    symbol <- lookupSymbol name
    when (isNothing symbol) . lift . Left $ "Function " ++ show name ++ " not declared"
    let funcType = (\(Just (t, _)) -> t) symbol
    when (funcType == CInt) . lift . Left $ "Function " ++ show name ++ " is not a function."
    when (funcType /= CFunc (length args)) . lift . Left $ "Function " ++ show name ++ " called with wrong number of arguments"
    FunctionCall name <$> traverse typeCheck args
  typeCheck (Var identifier) = do
    symbol <- lookupSymbol identifier
    when (isNothing symbol) . lift . Left $ "Variable " ++ show identifier ++ " not declared"
    let varType = (\(Just (t, _)) -> t) symbol
    when (varType /= CInt) . lift . Left $ "Variable " ++ show identifier ++ " is not an integer."
    return $ Var identifier
  typeCheck (Unary op expr) = Unary op <$> typeCheck expr
  typeCheck (Binary op expr1 expr2) = Binary op <$> typeCheck expr1 <*> typeCheck expr2
  typeCheck (PostFix expr op) = do
    expr' <- typeCheck expr
    return $ PostFix expr' op
  typeCheck (Assignment op lhs rhs) = Assignment op <$> typeCheck lhs <*> typeCheck rhs
  typeCheck (Constant c) = return $ Constant c
  typeCheck (Conditional cond thenExpr elseExpr) = Conditional <$> typeCheck cond <*> typeCheck thenExpr <*> typeCheck elseExpr

instance TypeCheck ForInit where
  typeCheck (InitDecl decl) = InitDecl <$> typeCheckVarDeclLocalScope decl
  typeCheck (InitExpr expr) = InitExpr <$> traverse typeCheck expr

typeChecker :: Program -> Either String (Program, SymbolTable)
typeChecker program = runStateT (typeCheck program) M.empty