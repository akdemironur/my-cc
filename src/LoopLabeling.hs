{-# LANGUAGE InstanceSigs #-}

module LoopLabeling (loopLabeling) where

import AST
import Control.Monad.State

data LLabelType = While | Do | For | Switch Bool deriving (Eq)

instance Show LLabelType where
  show While = "while"
  show Do = "do"
  show For = "for"
  show (Switch _) = "switch"

data LLabel = LLabel LLabelType Int deriving (Show, Eq)

llabelToIdentifier :: LLabel -> Identifier
llabelToIdentifier (LLabel l num) = show l ++ show num

data LScope = LScope LLabel (Maybe LScope) deriving (Show, Eq)

type LoopLabelingState = (Maybe LScope, Int)

getFirstLoop :: Maybe LScope -> Maybe LLabel
getFirstLoop Nothing = Nothing
getFirstLoop (Just (LScope label outer)) = case label of
  LLabel While _ -> Just label
  LLabel Do _ -> Just label
  LLabel For _ -> Just label
  LLabel (Switch _) _ -> getFirstLoop outer

getFirstSwitch :: Maybe LScope -> Maybe LLabel
getFirstSwitch Nothing = Nothing
getFirstSwitch (Just (LScope (LLabel (Switch b) n) _)) = Just (LLabel (Switch b) n)
getFirstSwitch (Just (LScope _ outer)) = getFirstSwitch outer

updateFirstSwitch :: Maybe LScope -> Maybe LScope
updateFirstSwitch Nothing = Nothing
updateFirstSwitch (Just (LScope (LLabel (Switch True) _) _)) = Nothing
updateFirstSwitch (Just (LScope (LLabel (Switch False) n) outer)) = Just (LScope (LLabel (Switch True) n) outer)
updateFirstSwitch (Just (LScope label outer)) = Just (LScope label (updateFirstSwitch outer))

getFirstLS :: Maybe LScope -> Maybe LLabel
getFirstLS Nothing = Nothing
getFirstLS (Just (LScope label _)) = Just label

type LoopLabelingT a = StateT LoopLabelingState (Either String) a

class LoopLabelingPass a where
  labelLoops :: a -> LoopLabelingT a

instance LoopLabelingPass Program where
  labelLoops :: Program -> LoopLabelingT Program
  labelLoops (Program functions) = Program <$> traverse labelLoops functions

instance LoopLabelingPass FuncDecl where
  labelLoops :: FuncDecl -> LoopLabelingT FuncDecl
  labelLoops (FuncDecl name args block ftype sc) = FuncDecl name args <$> traverse labelLoops block <*> pure ftype <*> pure sc

instance LoopLabelingPass Decl where
  labelLoops :: Decl -> LoopLabelingT Decl
  labelLoops (VDecl decl) = return $ VDecl decl
  labelLoops (FDecl decl) = FDecl <$> labelLoops decl

instance LoopLabelingPass Block where
  labelLoops :: Block -> LoopLabelingT Block
  labelLoops (Block items) = Block <$> mapM labelLoops items

instance LoopLabelingPass BlockItem where
  labelLoops :: BlockItem -> LoopLabelingT BlockItem
  labelLoops (BlockStmt stmt) = BlockStmt <$> labelLoops stmt
  labelLoops (BlockDecl decl) = return $ BlockDecl decl

instance LoopLabelingPass Stmt where
  labelLoops :: Stmt -> LoopLabelingT Stmt
  labelLoops w@(WhileStmt (Just _) _ _) = return w
  labelLoops d@(DoWhileStmt (Just _) _ _) = return d
  labelLoops f@(ForStmt (Just _) _ _ _ _) = return f
  labelLoops s@(SwitchStmt (Just _) _ _ _ _) = return s
  labelLoops b@(BreakStmt (Just _)) = return b
  labelLoops c@(ContinueStmt (Just _)) = return c
  labelLoops c@(CaseStmt (Just _) _ _) = return c
  labelLoops d@(DefaultStmt (Just _)) = return d
  labelLoops (LabeledStmt label stmt) = LabeledStmt label <$> labelLoops stmt
  labelLoops g@(GotoStmt _) = return g
  labelLoops r@(ReturnStmt _) = return r
  labelLoops e@(ExprStmt _) = return e
  labelLoops NullStmt = return NullStmt
  labelLoops (WhileStmt Nothing cond block) = do
    (loopIdentifier, block') <- labelLoop While block
    return $ WhileStmt loopIdentifier cond block'
  labelLoops (DoWhileStmt Nothing block cond) = do
    (loopIdentifier, block') <- labelLoop Do block
    return $ DoWhileStmt loopIdentifier block' cond
  labelLoops (ForStmt Nothing forinit cond update block) = do
    (loopIdentifier, block') <- labelLoop For block
    return $ ForStmt loopIdentifier forinit cond update block'
  labelLoops (IfStmt cond thenBlock elseBlock) = IfStmt cond <$> labelLoops thenBlock <*> traverse labelLoops elseBlock
  labelLoops (CompoundStmt block) = CompoundStmt <$> labelLoops block
  labelLoops (BreakStmt Nothing) = do
    (scope, _) <- get
    let firstLooporSwitch = getFirstLS scope
    case firstLooporSwitch of
      Just _ -> return $ BreakStmt (llabelToIdentifier <$> firstLooporSwitch)
      Nothing -> lift $ Left "Break statement outside of loop/switch"
  labelLoops (ContinueStmt Nothing) = do
    (scope, _) <- get
    let firstLoop = getFirstLoop scope
    case firstLoop of
      Just _ -> return $ ContinueStmt (llabelToIdentifier <$> firstLoop)
      Nothing -> lift $ Left "Continue statement outside of loop"
  labelLoops (CaseStmt Nothing e s) = do
    (scope, _) <- get
    let switch = getFirstSwitch scope
    case getFirstSwitch scope of
      Just _ -> CaseStmt (llabelToIdentifier <$> switch) e <$> labelLoops s
      Nothing -> lift $ Left "Case statement outside of switch"
  labelLoops (DefaultStmt Nothing) = do
    (scope, n) <- get
    case getFirstSwitch scope of
      Just (LLabel (Switch defaultSeen) num) -> do
        if defaultSeen
          then lift $ Left $ "Multiple default statements in switch (label: switch" ++ show num ++ ")"
          else do
            let newScope = updateFirstSwitch scope
            put (newScope, n)
            return $ DefaultStmt (llabelToIdentifier <$> getFirstSwitch scope)
      _ -> lift $ Left "Default statement outside of switch"
  labelLoops (SwitchStmt Nothing set d e block) = do
    (label, num) <- get
    let s = LLabel (Switch False) num
    let slabel = Just (LScope s label)
    let sidentifier = Just $ llabelToIdentifier s
    put (slabel, num + 1)
    block' <- labelLoops block
    (_, num') <- get
    put (label, num')
    return $ SwitchStmt sidentifier set d e block'

labelLoop :: (LoopLabelingPass b) => LLabelType -> b -> StateT LoopLabelingState (Either String) (Maybe Identifier, b)
labelLoop loopType block = do
  (outerScope, num) <- get
  let loopLabel = LLabel loopType num
  let loopScope = Just (LScope loopLabel outerScope)
  let loopIdentifier = Just $ llabelToIdentifier loopLabel
  put (loopScope, num + 1)
  block' <- labelLoops block
  (updatedScope, num') <- get
  case updatedScope of
    Just (LScope (LLabel l _) updatedOuterScope) ->
      if l /= loopType
        then lift $ Left "Something went wrong"
        else do
          put (updatedOuterScope, num')
          return (loopIdentifier, block')
    _ -> lift $ Left "Something went wrong"

loopLabeling :: Program -> Either String Program
loopLabeling program = evalStateT (labelLoops program) (Nothing, 0)
