{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module TypeCheck where

import AST
import Control.Applicative (liftA2)
import Control.Monad.State
import Data.Bifunctor (first, second)
import qualified Data.Map as M
import Data.Maybe (fromJust, isJust, isNothing)
import Util

data IdentifierAttrs
  = FuncAttr Bool Bool -- Defined, Global
  | StaticAttr InitialValue Bool -- InitialValue, Global
  | LocalAttr
  deriving (Show, Eq)

data StaticInit = IntInit Int | LongInit Int deriving (Show, Eq)

data InitialValue
  = Tentative
  | Initial StaticInit
  | NoInitializer
  deriving (Show, Eq)

type TypeCheckT a = StateT (SymbolTable, Identifier) (Either String) a

type SymbolTableEntry = (CType, IdentifierAttrs)

type SymbolTable = M.Map Identifier SymbolTableEntry

class TypeCheck a where
  typeCheck :: a -> TypeCheckT a

lookupSymbol :: Identifier -> TypeCheckT (Maybe (CType, IdentifierAttrs))
lookupSymbol identifier = do
  (symbolTable, _) <- get
  return $ M.lookup identifier symbolTable

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
initialValueT (Just (Constant (ConstInt c))) _ = return $ Initial (IntInit c)
initialValueT (Just (Constant (ConstLong c))) _ = return $ Initial (LongInit c)
initialValueT Nothing (Just Extern) = return NoInitializer
initialValueT Nothing _ = return Tentative
initialValueT _ _ = lift . Left $ "Non constant initializer for variable"

initialValue2T :: Maybe (CType, IdentifierAttrs) -> InitialValue -> TypeCheckT InitialValue
initialValue2T (Just (CFunc _ _, _)) _ = lift . Left $ "Variable already declared as function"
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
typeCheckVarDeclFileScope (VarDecl name expr t sc) = do
  typeCheckCompareOldSymbol name t
  currentInitial <- initialValueT (tyExpr <$> expr) sc
  oldSymbol <- lookupSymbol name
  let oldType = fmap fst oldSymbol
  when (isJust oldType && oldType /= Just t) . lift . Left $ "Variable " ++ show name ++ " already declared with different type"
  global <- varGlobalT oldSymbol sc
  initialValue <- initialValue2T oldSymbol currentInitial
  modify (first (M.insert name (t, StaticAttr initialValue global)))
  VarDecl name <$> traverse typeCheck expr <*> pure t <*> pure sc

typeCheckCompareOldSymbol :: Identifier -> CType -> TypeCheckT ()
typeCheckCompareOldSymbol name t = do
  oldSymbol <- lookupSymbol name
  let oldType = fmap fst oldSymbol
  when (isJust oldType && oldType /= Just t) . lift . Left $ "Variable " ++ show name ++ " already declared with different type"
  return ()

typeCheckVarDeclLocalScope :: VarDecl -> TypeCheckT VarDecl
typeCheckVarDeclLocalScope (VarDecl name (Just _) _ (Just Extern)) = lift . Left $ "Initializer on local extern variable declaration: " ++ show name
typeCheckVarDeclLocalScope (VarDecl name Nothing t (Just Extern)) = do
  typeCheckCompareOldSymbol name t
  oldSymbol <- lookupSymbol name
  case oldSymbol of
    Just (CFunc _ _, _) -> lift . Left $ "Variable already declared as function: " ++ show name
    Just _ -> return ()
    _ -> modify (first (M.insert name (t, StaticAttr NoInitializer True)))
  return $ VarDecl name Nothing t (Just Extern)
typeCheckVarDeclLocalScope (VarDecl name expr t (Just Static)) = do
  typeCheckCompareOldSymbol name t
  case expr of
    Just (TypedExpr (Constant _) _) -> return ()
    Nothing -> return ()
    _ -> lift . Left $ "Non constant initializer for static variable: " ++ show name
  let iv = case expr of
        Just (TypedExpr (Constant (ConstInt c)) _) -> Initial (IntInit c)
        Just (TypedExpr (Constant (ConstLong c)) _) ->
          Initial
            ( if t == CLong
                then LongInit c
                else IntInit (if is32BitRange c then reduceLong c else c)
            )
        Nothing -> Initial (IntInit 0)
        _ -> error "Unreachable"
  modify (first (M.insert name (t, StaticAttr iv False)))
  VarDecl name <$> traverse typeCheck expr <*> pure t <*> pure (Just Static)
typeCheckVarDeclLocalScope (VarDecl name expr t Nothing) = do
  typeCheckCompareOldSymbol name t
  modify (first (M.insert name (t, LocalAttr)))
  let expr' = case expr of
        Just e -> Just (convertTo e (Just t))
        Nothing -> Nothing
  VarDecl name <$> traverse typeCheck expr' <*> pure t <*> pure Nothing

instance TypeCheck FuncDecl where
  typeCheck :: FuncDecl -> TypeCheckT FuncDecl
  typeCheck (FuncDecl name args block ftype sc) = do
    let hasBody = isJust block
        global = sc /= Just Static
    oldSymbol <- lookupSymbol name
    case oldSymbol of
      Just (oldType@(CFunc _ _), FuncAttr oldDefined oldGlobal) -> do
        when (oldType /= ftype) . lift . Left $ "Function " ++ show name ++ " already declared with different type"
        when (oldDefined && hasBody) . lift . Left $ "Function " ++ show name ++ " already defined"
        when (oldGlobal && sc == Just Static) . lift . Left $ "Static function declaration follows non-static declaration"
        let attrs = FuncAttr (oldDefined || hasBody) oldGlobal
        modify (first (M.insert name (ftype, attrs)))
      Nothing -> modify (first (M.insert name (ftype, FuncAttr hasBody global)))
      _ -> lift . Left $ "Function " ++ show name ++ " already declared as variable"
    let argTypes = (\(CFunc argTypes _) -> argTypes) ftype
    modify . first . M.union . M.fromList $ zipWith (\arg argType -> (arg, (argType, LocalAttr))) args argTypes
    if hasBody
      then do
        (_, oldActiveFunction) <- get
        modify (second (const name))
        typeCheckedFuncDecl <- FuncDecl name args <$> traverse typeCheck block <*> pure ftype <*> pure sc
        modify (second (const oldActiveFunction))
        return typeCheckedFuncDecl
      else return $ FuncDecl name args block ftype sc

instance TypeCheck Stmt where
  typeCheck (CompoundStmt block) = CompoundStmt <$> typeCheck block
  typeCheck (DoWhileStmt l stmt expr) = DoWhileStmt l <$> typeCheck stmt <*> typeCheck expr
  typeCheck (WhileStmt l expr stmt) = WhileStmt l <$> typeCheck expr <*> typeCheck stmt
  typeCheck (ForStmt l forinit cond inc stmt) = ForStmt l <$> typeCheck forinit <*> traverse typeCheck cond <*> traverse typeCheck inc <*> typeCheck stmt
  typeCheck (ContinueStmt l) = return $ ContinueStmt l
  typeCheck (ReturnStmt expr) = do
    (st, activeFunction) <- get
    let (CFunc _ returnType) = fst . fromJust $ M.lookup activeFunction st
    typeCheckedExpr <- typeCheck expr
    if tyType expr == Just returnType
      then return $ ReturnStmt typeCheckedExpr
      else return $ ReturnStmt (convertTo typeCheckedExpr (Just returnType))
  typeCheck (ExprStmt expr) = ExprStmt <$> typeCheck expr
  typeCheck (IfStmt expr thenStmt elseStmt) = IfStmt <$> typeCheck expr <*> typeCheck thenStmt <*> traverse typeCheck elseStmt
  typeCheck (SwitchStmt identifier caseSet hasDefault expr stmt) = SwitchStmt identifier caseSet hasDefault <$> typeCheck expr <*> typeCheck stmt
  typeCheck (CaseStmt identifier constant stmt) = CaseStmt identifier constant <$> typeCheck stmt
  typeCheck (DefaultStmt identifier) = return $ DefaultStmt identifier
  typeCheck (GotoStmt identifier) = return $ GotoStmt identifier
  typeCheck (LabeledStmt identifier stmt) = LabeledStmt identifier <$> typeCheck stmt
  typeCheck (BreakStmt l) = return $ BreakStmt l
  typeCheck NullStmt = return NullStmt

isFunction :: Maybe CType -> Bool
isFunction (Just (CFunc _ _)) = True
isFunction _ = False

convertTo :: TypedExpr -> Maybe CType -> TypedExpr
convertTo e@(TypedExpr expr t1) t2
  | t1 == t2 = TypedExpr expr t1
  | otherwise = TypedExpr (Cast (fromJust t2) e) t2

instance TypeCheck TypedExpr where
  typeCheck (TypedExpr (Var identifier) _) = do
    symbol <- lookupSymbol identifier
    when (isNothing symbol) . lift . Left $ "Variable " ++ show identifier ++ " not declared"
    let oldType = fmap fst symbol
    when (isFunction oldType) . lift . Left $ "Function name used as variable: " ++ show identifier
    return $ TypedExpr (Var identifier) oldType
  typeCheck (TypedExpr c@(Constant (ConstInt _)) _) = return $ TypedExpr c (Just CInt)
  typeCheck (TypedExpr c@(Constant (ConstLong _)) _) = return $ TypedExpr c (Just CLong)
  typeCheck (TypedExpr (Cast targetType expr) _) = (TypedExpr . Cast targetType <$> typeCheck expr) <*> pure (Just targetType)
  typeCheck (TypedExpr (Unary Not expr) _) = (TypedExpr . Unary Not <$> typeCheck expr) <*> pure (Just CInt)
  typeCheck (TypedExpr (Unary op expr) _) = (TypedExpr . Unary op <$> typeCheck expr) <*> (tyType <$> typeCheck expr)
  typeCheck (TypedExpr (Binary op expr1 expr2) _) = do
    te1 <- typeCheck expr1
    te2 <- typeCheck expr2
    case op of
      op' | isLogicalOp op' -> pure $ TypedExpr (Binary op te1 te2) (Just CInt)
      op' | isBitshiftOp op' -> pure $ TypedExpr (Binary op te1 te2) (tyType te1)
      _ -> do
        let resultType = liftA2 getCommonType (tyType te1) (tyType te2)
            converted = Binary op (convertTo te1 resultType) (convertTo te2 resultType)
        pure $
          TypedExpr converted $
            if isComparisonOp op
              then Just CInt
              else resultType
  typeCheck te@(TypedExpr (Assignment Assign _ _) _) = compoundTypeCheckT1 te
  typeCheck te@(TypedExpr (Assignment {}) _) = compoundTypeCheckT2 te
  typeCheck (TypedExpr (Conditional cond thenExpr elseExpr) _) = do
    typedCond <- typeCheck cond
    typedThen <- typeCheck thenExpr
    typedElse <- typeCheck elseExpr
    let thenType = tyType typedThen
        elseType = tyType typedElse
        commonType = getCommonType <$> thenType <*> elseType
        convertedThen = convertTo typedThen commonType
        convertedElse = convertTo typedElse commonType
        condExp = Conditional typedCond convertedThen convertedElse
    pure $ TypedExpr condExp commonType
  typeCheck (TypedExpr (FunctionCall name args) _) = do
    funcInfo <- validateFunctionCall name args
    typedArgs <- processArguments args funcInfo
    return $ makeFunctionCallExpr name typedArgs funcInfo
    where
      validateFunctionCall :: Identifier -> [TypedExpr] -> TypeCheckT CType
      validateFunctionCall name args = do
        symbol <- lookupSymbol name
        case symbol of
          Nothing ->
            lift . Left $ "Function " ++ show name ++ " not declared"
          Just (typ, _) -> do
            unless (isFunction (Just typ)) $
              lift . Left $
                "Variable " ++ show name ++ " used as function"
            let CFunc argTypes _ = typ
            unless (length args == length argTypes) $
              lift . Left $
                "Function " ++ show name ++ " called with wrong number of arguments"
            return typ

      processArguments :: [TypedExpr] -> CType -> TypeCheckT [TypedExpr]
      processArguments args (CFunc expectedTypes _) = do
        typedArgs <- traverse typeCheck args
        return $ zipWith convertTo typedArgs (map Just expectedTypes)
      processArguments _ _ = error "Function type in processArguments"
      makeFunctionCallExpr :: Identifier -> [TypedExpr] -> CType -> TypedExpr
      makeFunctionCallExpr name processedArgs (CFunc _ returnType) = TypedExpr (FunctionCall name processedArgs) (Just returnType)
      makeFunctionCallExpr _ _ _ = error "Function type in makeFunctionCallExpr"
  typeCheck (TypedExpr (PostFix expr op) _) = do
    typedExpr <- typeCheck expr
    pure $ TypedExpr (PostFix typedExpr op) (tyType typedExpr)

instance TypeCheck ForInit where
  typeCheck (InitDecl decl) = InitDecl <$> typeCheckVarDeclLocalScope decl
  typeCheck (InitExpr expr) = InitExpr <$> traverse typeCheck expr

typeChecker :: Program -> Either String (Program, SymbolTable)
typeChecker program = case runStateT (typeCheck program) (M.empty, "") of
  Left err -> Left err
  Right (p, (st, _)) -> Right (p, st)

compoundTypeCheckT1 :: TypedExpr -> StateT (SymbolTable, Identifier) (Either String) TypedExpr
compoundTypeCheckT1 (TypedExpr (Assignment op lhs rhs) _) = do
  typedLhs <- typeCheck lhs
  typedRhs <- typeCheck rhs
  let leftType = tyType typedLhs
      convertedRhs = convertTo typedRhs leftType
      assignExp = Assignment op typedLhs convertedRhs
  pure $ TypedExpr assignExp leftType
compoundTypeCheckT1 _ = error "Not an assignment"

compoundTypeCheckT2 :: TypedExpr -> StateT (SymbolTable, Identifier) (Either String) TypedExpr
compoundTypeCheckT2 (TypedExpr (Assignment op lhs rhs) _) = do
  typedLhs <- typeCheck lhs
  typedRhs <- typeCheck rhs
  return $ TypedExpr (Assignment op typedLhs typedRhs) (tyType typedLhs)
compoundTypeCheckT2 _ = error "Not an assignment"