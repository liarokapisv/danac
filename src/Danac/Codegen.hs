{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}

module Danac.Codegen where

import LLVM.IRBuilder.Module
import LLVM.IRBuilder.Monad
import LLVM.IRBuilder.Instruction as I
import LLVM.IRBuilder.Constant
import qualified LLVM.AST as LLVM
import qualified LLVM.AST.IntegerPredicate as IP
import qualified LLVM.AST.Constant as C
import LLVM.AST.IntegerPredicate 
import LLVM.AST.Type (i8, i32, ptr, void)

import qualified Danac.Renamer as RN
import qualified Danac.TypeChecker as TC
import Danac.Ast
import Danac.Util.Annotation
import Data.Text (Text, unpack, any)
import Data.Text.Encoding (encodeUtf8)
import Data.ByteString.Short (ShortByteString, toShort)

import Data.Comp.Multi.HFunctor
import Data.Comp.Multi.HFoldable
import Data.Comp.Multi.Term
import Data.Comp.Multi.Algebra

import Data.Maybe (maybeToList)
import Data.Foldable (for_)
import Data.Traversable (for)
import qualified Data.Map as Map
import Control.Monad (unless, join)
import Control.Monad.State (MonadState, evalState, put, get, gets, modify)
import Control.Monad.Fix (MonadFix)

dataType :: DataType -> LLVM.Type
dataType Byte = i8
dataType Integ = i32

maybeDataType :: Maybe DataType -> LLVM.Type
maybeDataType Nothing = void
maybeDataType (Just dt) = dataType dt

ttype :: Type -> LLVM.Type
ttype (DType dt) = dataType dt
ttype (AType t i) = LLVM.ArrayType (fromInteger i) (ttype t)

fparType :: FparType -> LLVM.Type
fparType (Pointer t) = ptr $ ttype t
fparType (Ref t) = ptr $ dataType t
fparType (Value t@(AType _ _)) = ptr $ ttype t
fparType (Value t@(DType _)) = ttype t

data Env = Env {
    functions :: Map.Map LLVM.Name LLVM.Operand,
    labels :: Map.Map Text (LLVM.Name, LLVM.Name),
    latestLabel :: Maybe (LLVM.Name, LLVM.Name)
} deriving Show

emptyEnv :: Env
emptyEnv = Env { functions = Map.empty, labels = Map.empty, latestLabel = Nothing }

declBuiltinFunction :: (MonadState Env m, MonadModuleBuilder m) => ShortByteString -> LLVM.Type -> [LLVM.Type] -> m ()
declBuiltinFunction name rt args = do
    fn <- extern (LLVM.Name ("__" <> name)) args rt
    modify $ \s -> s { functions = Map.insert (LLVM.Name name) fn $ functions s}

codegen :: Text -> Term (T :&&: TC.Ann) Ast -> LLVM.Module
codegen name (Ast f :&&.: _) = flip evalState emptyEnv $ buildModuleT (toShort $ encodeUtf8 name) $ do
    declBuiltinFunction "writeInteger" void [i32]
    declBuiltinFunction "writeByte" void [i8]
    declBuiltinFunction "writeChar" void [i8]
    declBuiltinFunction "writeString" void [ptr i8]
    declBuiltinFunction "readInteger" i32 []
    declBuiltinFunction "readByte" i8 []
    declBuiltinFunction "readChar" i8 []
    declBuiltinFunction "readString" void [i32, ptr i8]
    declBuiltinFunction "extend" i32 [i8]
    declBuiltinFunction "shrink" i8 [i32]
    declBuiltinFunction "strlen" i32 [ptr i8]
    declBuiltinFunction "strcmp" i32 [ptr i8, ptr i8]
    declBuiltinFunction "strcpy" void [ptr i8, ptr i8]
    declBuiltinFunction "strcat" void [ptr i8, ptr i8]
    codegenFuncDef Nothing f

createFrame :: Maybe LLVM.Type -> Term (T :&&: TC.Ann) i -> LLVM.Type
createFrame mparent y = LLVM.StructureType False (maybeToList (fmap ptr mparent) ++ unK (cata collectAlg y))
    where collectAlg :: Alg (T :&&: TC.Ann) (K [LLVM.Type])
          collectAlg (FparDef _ t :&&: _) = K [fparType t]
          collectAlg (VarDef _ t :&&: _) = K [ttype t]
          collectAlg (LocalDefFuncDecl _ :&&: _) = K []
          collectAlg (LocalDefFuncDef _ :&&: _) = K []
          collectAlg (x :&&: _) = K $ hfoldMap unK x

lookupFunction :: MonadState Env m => LLVM.Name -> m LLVM.Operand
lookupFunction t = do
    mop <- gets $ Map.lookup t . functions
    case mop of
        Nothing -> error $ "Internal compiler error when looking up for function " ++ show t ++ " - most likely unimplemented stdlib function"
        Just o -> pure o

lookupLabel :: MonadState Env m => Text -> m (LLVM.Name, LLVM.Name)
lookupLabel t = do
    mop <- gets $ Map.lookup t . labels
    case mop of
        Nothing -> error "Internal compiler error on lookupLabel"
        Just o -> pure o

lookupLatestLabel :: MonadState Env m => m (LLVM.Name, LLVM.Name)
lookupLatestLabel = do
    mop <- gets latestLabel
    case mop of
        Nothing -> error "Internal compiler error on lookupLatestLabel"
        Just o -> pure o

codegenFuncDef :: (MonadState Env m, MonadFix m, MonadModuleBuilder m) => Maybe LLVM.Type -> Term (T :&&: TC.Ann) FuncDef -> m ()
codegenFuncDef mparent f@(FuncDef header@(Header _ rt _ :&&.: _) localDefs body :&&.: _) = mdo
    let (name, paramTypes, retType) = codegenHeader mparent header
    let frameType = createFrame mparent f
    let op = LLVM.ConstantOperand $ C.GlobalReference (ptr $ LLVM.FunctionType retType (fst <$> paramTypes) False) name
    modify $ (\s -> s { functions = Map.insert name op $ functions s })
    _ <- function name paramTypes retType $ \params -> do
        _entry <- block `named` "entry"
        for_ localDefs $ codegenLocalDef frameType
        codegenBody frameType body params
        ensureRet rt
    pure ()


codegenFuncDecl :: (MonadState Env m, MonadFix m, MonadModuleBuilder m) => Maybe LLVM.Type -> Term (T :&&: TC.Ann) FuncDecl -> m ()
codegenFuncDecl mparent (FuncDecl header :&&.: _) = mdo
    let (name, paramTypes, retType) = codegenHeader mparent header
    op <- extern name (fmap fst paramTypes) retType
    modify $ (\s -> s { functions = Map.insert name op $ functions s })
    pure ()

codegenHeader :: Maybe LLVM.Type -> Term (T :&&: TC.Ann) Header -> (LLVM.Name, [(LLVM.Type, ParameterName)], LLVM.Type)
codegenHeader mparent (Header name mdt fparDefs :&&.: _) = 
    (LLVM.Name $ toShort $ encodeUtf8 name, 
     maybeToList (fmap (\t -> (ptr t, ParameterName "parent")) mparent) ++  fmap codegenFparDef fparDefs, 
     maybeDataType mdt)

codegenFparDef :: Term (T :&&: TC.Ann) FparDef -> (LLVM.Type, ParameterName)
codegenFparDef (FparDef name ftp :&&.: _) = (fparType ftp, ParameterName $ toShort $ encodeUtf8 name)

codegenLocalDef :: (MonadState Env m, MonadFix m, MonadModuleBuilder m) => LLVM.Type -> Term (T :&&: TC.Ann) LocalDef -> m ()
codegenLocalDef parent (LocalDefFuncDef fd :&&.: _) = codegenFuncDef (Just parent) fd
codegenLocalDef parent (LocalDefFuncDecl fd :&&.: _) = codegenFuncDecl (Just parent) fd  
codegenLocalDef _ _ = pure ()

codegenBody :: (MonadState Env m, MonadFix m, MonadModuleBuilder m, MonadIRBuilder m) => LLVM.Type -> Term (T :&&: TC.Ann) Block -> [LLVM.Operand] -> m ()
codegenBody framet b params = do
    frame <- alloca framet Nothing 0
    for_ (zip [0..] params) $ \(i, param) -> do
        p <- gep frame [int32 0,int32 i]
        store p 0 param
    codegenBlock frame b
    pure ()

codegenBlock :: (MonadState Env m, MonadFix m, MonadModuleBuilder m, MonadIRBuilder m) => LLVM.Operand -> Term (T :&&: TC.Ann) Block -> m ()
codegenBlock frame (Block stmts :&&.: _) = do
    for_ stmts $ codegenStmt frame

getFrame :: (Show i, Integral i, MonadModuleBuilder m, MonadIRBuilder m) => LLVM.Operand -> i -> m LLVM.Operand
getFrame frame 0 = pure frame
getFrame frame n = do
    p <- gep frame [int32 0, int32 0]
    f <- load p 0
    getFrame f (n-1)

getFrameElement :: (Show i, Integral i, MonadModuleBuilder m, MonadIRBuilder m) => LLVM.Operand -> i -> i -> m LLVM.Operand
getFrameElement frame n offset = do
    f <- getFrame frame n
    gep f [int32 0 , int32 $ toInteger offset]

codegenVarId :: (MonadModuleBuilder m, MonadIRBuilder m) => LLVM.Operand -> Term (T :&&: TC.Ann) VarIdentifier -> m LLVM.Operand
codegenVarId frame (VarIdentifier _ :&&.: TC.AnnVariable _ f links offset vt) = do
    p <- getFrameElement frame links (offset + (hasParent f :: Int))
    case vt of
        RN.Param (Value (DType _)) -> pure p
        RN.Param (Value (AType _ _)) -> load p 0
        RN.Param (Ref _) -> load p 0
        RN.Param (Pointer _) -> load p 0
        RN.Local (AType _ _) -> pure p
        RN.Local (DType _) -> pure p
        where hasParent t = if Data.Text.any (=='.') t then 1 else 0

codegenLvalue :: (MonadState Env m, MonadIRBuilder m, MonadModuleBuilder m) => LLVM.Operand -> Term (T :&&: TC.Ann) Lvalue -> m LLVM.Operand
codegenLvalue frame (LvalueId x :&&.: _) = codegenVarId frame x
codegenLvalue _ (LvalueStr x :&&.: _) = do 
    name <- freshName  $ toShort $ encodeUtf8 x
    c <- globalStringPtr (unpack x) name
    pure $ LLVM.ConstantOperand c
codegenLvalue frame (LvalueAx l@(LvalueId (VarIdentifier _ :&&.: TC.AnnVariable _ _ _ _ (RN.Param (Pointer _))) :&&.: _) e :&&.: _) = do
    l' <- codegenLvalue frame l
    e' <- codegenExprLoad frame e
    gep l' [e']
codegenLvalue _ (LvalueAx (LvalueId (VarIdentifier _ :&&.: TC.AnnVariable _ _ _ _ (RN.Param (Value (DType _)))) :&&.: _) _ :&&.: _) = error "Internal compiler error on LvalueAx"
codegenLvalue _ (LvalueAx (LvalueId (VarIdentifier _ :&&.: TC.AnnVariable _ _ _ _ (RN.Local (DType _ ))) :&&.: _) _ :&&.: _) = error "Internal compiler error on LvalueAx"
codegenLvalue _ (LvalueAx (LvalueId (VarIdentifier _ :&&.: TC.AnnVariable _ _ _ _ (RN.Param (Ref _))) :&&.: _) _ :&&.: _) = error "Internal compiler error on LvalueAx"
codegenLvalue frame (LvalueAx l e :&&.: _) = do
    l' <- codegenLvalue frame l
    e' <- codegenExprLoad frame e
    gep l' [int32 0, e']

data ValueTag = GlobalString | InFrame | Temporary

codegenExprWithTag :: (MonadState Env m, MonadModuleBuilder m, MonadIRBuilder m) => LLVM.Operand -> Term (T :&&: TC.Ann) Expr -> m (ValueTag, LLVM.Operand)
codegenExprWithTag frame x@(ExprLvalue (LvalueStr _ :&&.: _) :&&.: _) = codegenExpr frame x >>= pure . (GlobalString,)
codegenExprWithTag frame x@(ExprLvalue _ :&&.: _) = codegenExpr frame x >>= pure . (InFrame,)
codegenExprWithTag frame x = codegenExpr frame x >>= pure . (Temporary,)

codegenFuncCall :: (MonadState Env m, MonadIRBuilder m, MonadModuleBuilder m) => LLVM.Operand -> Term (T :&&: TC.Ann) FuncCall -> m LLVM.Operand
codegenFuncCall frame (FuncCall (FuncIdentifier text :&&.: TC.AnnFunction _ mlinks (FunctionType _ fts)) es :&&.: _) = do
    f <- lookupFunction $ LLVM.Name $ toShort $ encodeUtf8 text
    es' <- for es $ codegenExprWithTag frame
    es'' <- for (zip fts es') $ \(ft, (tag, e)) -> 
                                    case (ft, tag) of
                                        (Value (AType _ _), GlobalString) -> pure e
                                        (Pointer _, GlobalString) -> bitcast e (ptr i8)
                                        (Value (AType _ _), InFrame) -> pure e
                                        (Value (DType _), InFrame) -> load e 0
                                        (Ref _, InFrame) -> pure e
                                        (Pointer t, InFrame) -> bitcast e (ptr $ ttype t)
                                        (Value _, Temporary) -> pure e
                                        _ -> error "Internal compiler error on codegenFuncCall"
    let es''' = fmap (,[]) es''
    case mlinks of
        Nothing -> call f es'''
        Just links -> do
            p <- getFrame frame links
            call f ((p,[]) : es''')

codegenExprLoad :: (MonadState Env m, MonadModuleBuilder m, MonadIRBuilder m) => LLVM.Operand -> Term (T :&&: TC.Ann) Expr -> m (LLVM.Operand)
codegenExprLoad frame x@(ExprLvalue _ :&&.: _) = codegenExpr frame x >>= \o -> load o 0
codegenExprLoad frame x = codegenExpr frame x

codegenExpr :: (MonadState Env m, MonadModuleBuilder m, MonadIRBuilder m) => LLVM.Operand -> Term (T :&&: TC.Ann) Expr -> m LLVM.Operand
codegenExpr _ (ExprInt x :&&.: _) = pure $ int32 x
codegenExpr _ (ExprChar x :&&.: _) = pure $ int8 $ toInteger $ fromEnum x
codegenExpr frame (ExprLvalue x :&&.: _) = codegenLvalue frame x
codegenExpr frame (ExprFuncCall x :&&.: _) = codegenFuncCall frame x
codegenExpr frame (ExprMinus x :&&.: _) = join (xor (int32 0) <$> codegenExprLoad frame x)
codegenExpr frame (ExprAdd x y :&&.: _) = join (add <$> codegenExprLoad frame x <*> codegenExprLoad frame y)
codegenExpr frame (ExprSub x y :&&.: _) = join (sub <$> codegenExprLoad frame x <*> codegenExprLoad frame y)
codegenExpr frame (ExprMul x y :&&.: _) = join (mul <$> codegenExprLoad frame x <*> codegenExprLoad frame y)
codegenExpr frame (ExprDiv x y :&&.: _) = join (udiv <$> codegenExprLoad frame x <*> codegenExprLoad frame y)
codegenExpr frame (ExprMod x y :&&.: _) = join (urem <$> codegenExprLoad frame x <*> codegenExprLoad frame y)
codegenExpr frame (ExprNot x :&&.: TC.AnnExpr _ (Just Integ)) = join (icmp IP.EQ (int32 0) <$> codegenExprLoad frame x)
codegenExpr frame (ExprNot x :&&.: TC.AnnExpr _ (Just Byte)) = join (icmp IP.EQ (int8 0) <$> codegenExprLoad frame x)
codegenExpr _ (ExprNot _ :&&.: TC.AnnExpr _ Nothing) = error "Internal compiler error on ExprNot - missing type from typechecking stage"
codegenExpr frame (ExprAnd x y :&&.: TC.AnnExpr _ (Just Integ)) = do
    x' <- codegenExprLoad frame x
    y' <- codegenExprLoad frame y
    x'' <- icmp NE (int32 0) x'
    y'' <- icmp NE (int32 0) y'
    z <- I.and x'' y''
    icmp NE (int32 0) z
codegenExpr frame (ExprAnd x y :&&.: TC.AnnExpr _ (Just Byte)) = do
    x' <- codegenExprLoad frame x
    y' <- codegenExprLoad frame y
    x'' <- icmp NE (int8 0) x'
    y'' <- icmp NE (int8 0) y'
    z <- I.and x'' y''
    icmp NE (int8 0) z
codegenExpr _ (ExprAnd _ _ :&&.: TC.AnnExpr _ Nothing) = error "Internal compiler error on ExprAnd - missing type from typechecking stage"
codegenExpr frame (ExprOr x y :&&.: TC.AnnExpr _ (Just Integ)) = do
    x' <- codegenExprLoad frame x
    y' <- codegenExprLoad frame y
    x'' <- icmp NE (int32 0) x'
    y'' <- icmp NE (int32 0) y'
    z <- I.and x'' y''
    icmp NE (int32 0) z
codegenExpr frame (ExprOr x y :&&.: TC.AnnExpr _ (Just Byte)) = do
    x' <- codegenExprLoad frame x
    y' <- codegenExprLoad frame y
    x'' <- icmp NE (int8 0) x'
    y'' <- icmp NE (int8 0) y'
    z <- I.and x'' y''
    icmp NE (int8 0) z
codegenExpr _ (ExprOr _ _ :&&.: TC.AnnExpr _ Nothing) = error "Internal compiler error on ExprOr - missing type from typechecking stage"
codegenExpr _ (ExprTrue :&&.: _) = pure $ int8 1
codegenExpr _ (ExprFalse :&&.: _) = pure $ int8 0

codegenCond :: (MonadState Env m, MonadModuleBuilder m, MonadIRBuilder m) => LLVM.Operand -> Term (T :&&: TC.Ann) Cond -> m LLVM.Operand
codegenCond frame (CondExpr x@(_ :&&.: TC.AnnExpr _ (Just Integ)) :&&.: _) = join (icmp IP.NE (int32 0) <$> codegenExprLoad frame x)
codegenCond frame (CondExpr x@(_ :&&.: TC.AnnExpr _ (Just Byte)) :&&.: _) = join (icmp IP.NE (int8 0) <$> codegenExprLoad frame x)
codegenCond _ (CondExpr (_ :&&.: TC.AnnExpr _ Nothing) :&&.: _) = error "Internal compiler error on CondExpr - missing type from typechecking stage"
codegenCond frame (CondNot x :&&.: _) = join (I.xor (bit 1) <$> codegenCond frame x)
codegenCond frame (CondOr x y:&&.: _) = join (I.or <$> codegenCond frame x <*> codegenCond frame y)
codegenCond frame (CondAnd x y:&&.: _) = join (I.and <$> codegenCond frame x <*> codegenCond frame y)
codegenCond frame (CondEq x y:&&.: _) = join (icmp IP.EQ <$> codegenExprLoad frame x <*> codegenExprLoad frame y)
codegenCond frame (CondNe x y:&&.: _) = join (icmp IP.NE <$> codegenExprLoad frame x <*> codegenExprLoad frame y)
codegenCond frame (CondLt x y:&&.: _) = join (icmp IP.SLT <$> codegenExprLoad frame x <*> codegenExprLoad frame y)
codegenCond frame (CondGt x y:&&.: _) = join (icmp IP.SGT <$> codegenExprLoad frame x <*> codegenExprLoad frame y)
codegenCond frame (CondLe x y:&&.: _) = join (icmp IP.SLE <$> codegenExprLoad frame x <*> codegenExprLoad frame y)
codegenCond frame (CondGe x y:&&.: _) = join (icmp IP.SGE <$> codegenExprLoad frame x <*> codegenExprLoad frame y)

codegenCondStmt :: (MonadState Env m, MonadFix m, MonadModuleBuilder m, MonadIRBuilder m) => 
                   LLVM.Operand -> ShortByteString -> LLVM.Name -> Term (T :&&: TC.Ann) CondStmt -> m ()
codegenCondStmt frame label exit (CondStmt c b :&&.: _) = mdo
    br ifLabel
    ifLabel <- block `named` (label <> "-check")
    cond <- codegenCond frame c
    condBr cond ifThen ifMerge
    ifThen <- block `named` (label <> "-do")
    codegenBlock frame b
    mkTerminator $ br exit
    ifMerge <- block `named` (label <> "-merge")
    pure ()

codegenStmt :: (MonadState Env m, MonadFix m, MonadModuleBuilder m, MonadIRBuilder m) => LLVM.Operand -> Term (T :&&: TC.Ann) Stmt -> m ()
codegenStmt _ (StmtSkip :&&.: _) = pure ()
codegenStmt _ (StmtExit :&&.: _) = retVoid
codegenStmt frame (StmtAssign l e :&&.: _) = do
    l' <- codegenLvalue frame l `named` "ax"
    e' <- codegenExprLoad frame e `named` "index"
    store l' 0 e'
codegenStmt frame (StmtProcCall c :&&.: _) = codegenFuncCall frame c *> pure ()
codegenStmt frame (StmtReturn e :&&.: _) = join $ ret <$> codegenExprLoad frame e
codegenStmt frame (StmtIf i ei me :&&.: _) = mdo
    _ <- codegenCondStmt frame "if" exit i
    for_ ei $ codegenCondStmt frame "elif" exit
    for_ me $ codegenBlock frame
    mkTerminator $ br exit
    exit <- block `named` "if-exit"
    pure ()
codegenStmt frame (StmtLoop nm b :&&.: _) = mdo
    br loop
    loop <- block `named` "loop"
    s <- get
    put $ case nm of
               Just i -> s { labels = Map.insert i (loop,exit) (labels s), latestLabel = Just (loop, exit) }
               Nothing -> s { latestLabel = Just (loop, exit) } 
    codegenBlock frame b 
    mkTerminator $ br loop
    exit <- block `named` "loop-exit"
    put s
codegenStmt _ (StmtBreak nm :&&.: _) = do
    l <- case nm of
              Nothing -> fmap snd $ lookupLatestLabel
              Just n -> fmap snd $ lookupLabel n
    br l
codegenStmt _ (StmtContinue nm :&&.: _) = do
    l <- case nm of
            Nothing -> fmap fst $ lookupLatestLabel
            Just n -> fmap fst $ lookupLabel n
    br l

mkTerminator :: MonadIRBuilder m => m () -> m ()
mkTerminator instr = do
    check <- hasTerminator
    unless check instr

ensureRet :: MonadIRBuilder m => Maybe DataType -> m ()
ensureRet = mkTerminator . ensureRet'
    where ensureRet' Nothing = retVoid
          ensureRet' (Just Integ) = ret (int32 0)
          ensureRet' (Just Byte) = ret (int8 0)
