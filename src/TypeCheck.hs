{-# LANGUAGE TupleSections #-}

module TypeCheck (typeChecker) where

import AST
import Control.Monad.State
import Data.Bifunctor (first, second)
import qualified Data.Map as M
import Data.Maybe (isJust)
import qualified Data.Set as S

type TypeCheckT a = StateT (TypeMap, DefinedFuncs) (Either String) a

type TypeMap = M.Map Identifier CType

type DefinedFuncs = S.Set Identifier

data CType = CInt | CFunc Int deriving (Show, Eq)

class TypeCheck a where
  typeCheck :: a -> TypeCheckT a

instance TypeCheck Program where
  typeCheck (Program decls) = Program <$> traverse typeCheck decls

instance TypeCheck Block where
  typeCheck (Block items) = Block <$> traverse typeCheck items

instance TypeCheck BlockItem where
  typeCheck (BlockStmt stmt) = BlockStmt <$> typeCheck stmt
  typeCheck (BlockDecl decl) = BlockDecl <$> typeCheck decl

instance TypeCheck Decl where
  typeCheck (VDecl decl) = VDecl <$> typeCheck decl
  typeCheck (FDecl decl) = FDecl <$> typeCheck decl

instance TypeCheck VarDecl where
  typeCheck (VarDecl name expr) = do
    modify $ first (M.insert name CInt)
    VarDecl name <$> traverse typeCheck expr

instance TypeCheck FuncDecl where
  typeCheck (FuncDecl name args block) = do
    let funcType = CFunc $ length args
    let hasBody = isJust block
    (typeMap, definedFuncs) <- get
    let existingType = typeMap M.! name
    let defined = S.member name definedFuncs
    let declared = M.member name typeMap
    when (declared && existingType /= funcType) . lift . Left $ "Function " ++ show name ++ " already declared with different type"
    when (declared && hasBody && defined) . lift . Left $ "Function " ++ show name ++ " already defined"
    let newTypeMap = M.insert name funcType typeMap
    put (newTypeMap, definedFuncs)
    if hasBody
      then do
        modify $ first (M.union $ M.fromList $ map (,CInt) args)
        modify $ second (S.insert name)
        FuncDecl name args <$> traverse typeCheck block
      else return $ FuncDecl name args block

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
    (typeMap, _) <- get
    unless (M.member name typeMap) . lift . Left $ "Function " ++ show name ++ " not declared"
    let funcType = typeMap M.! name
    when (funcType == CInt) . lift . Left $ "Function " ++ show name ++ " is not a function."
    when (funcType /= CFunc (length args)) . lift . Left $ "Function " ++ show name ++ " called with wrong number of arguments"
    FunctionCall name <$> traverse typeCheck args
  typeCheck (Var identifier) = do
    (typeMap, _) <- get
    unless (M.member identifier typeMap) . lift . Left $ "Variable " ++ show identifier ++ " not declared"
    let varType = typeMap M.! identifier
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
  typeCheck (InitDecl decl) = InitDecl <$> typeCheck decl
  typeCheck (InitExpr expr) = InitExpr <$> traverse typeCheck expr

typeChecker :: Program -> Either String Program
typeChecker program = evalStateT (typeCheck program) (M.empty, S.empty)