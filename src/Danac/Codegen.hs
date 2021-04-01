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
import LLVM.AST.IntegerPredicate 
import LLVM.AST.Type (i8, i64, ptr, void)

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
import Data.Foldable (for_, foldrM)
import Data.Traversable (for)
import qualified Data.Map as Map
import Control.Monad (unless, join)
import Control.Monad.State.Strict (MonadState, runStateT, put, get, gets, modify)
import Control.Monad.Fix (MonadFix)

dataType :: DataType -> LLVM.Type
dataType Byte = i8
dataType Integ = i64

maybeDataType :: Maybe DataType -> LLVM.Type
maybeDataType Nothing = void
maybeDataType (Just dt) = dataType dt

ttype :: Type -> LLVM.Type
ttype (DType dt) = dataType dt
ttype (AType t i) = LLVM.ArrayType (fromInteger i) (ttype t)

fancyType :: FancyType -> LLVM.Type
fancyType (Pointer t) = ptr $ ttype t
fancyType (Ref t) = ptr $ dataType t
fancyType (Value t@(AType _ _)) = ptr $ ttype t
fancyType (Value t@(DType _)) = ttype t

data Env = Env {
    functions :: Map.Map LLVM.Name LLVM.Operand,
    labels :: Map.Map Text (LLVM.Name, LLVM.Name),
    latestLabel :: Maybe (LLVM.Name, LLVM.Name)
}

emptyEnv = Env { functions = Map.empty, labels = Map.empty, latestLabel = Nothing }

codegen :: Text -> Term (T :&&: TC.Ann) Ast -> LLVM.Module
codegen name (Ast f :&&.: _) = buildModule (toShort $ encodeUtf8 name) $ flip runStateT emptyEnv $ do
    writeString <- extern (LLVM.Name "puts") [ptr i8] void
    modify $ \s -> s { functions = Map.insert (LLVM.Name "writeString") writeString $ functions s }
    writeChar <- extern (LLVM.Name "putchar") [i8] void
    modify $ \s -> s { functions = Map.insert (LLVM.Name "writeChar") writeChar $ functions s }
    strlen <- extern (LLVM.Name "strlen") [ptr i8] i64
    modify $ \s -> s { functions = Map.insert (LLVM.Name "strlen") strlen $ functions s }
    codegenFuncDef Nothing f

createFrame :: Maybe LLVM.Type -> Term (T :&&: TC.Ann) FuncDef -> LLVM.Type
createFrame mparent y = LLVM.StructureType False (maybeToList (fmap ptr mparent) ++ unK (cata collectAlg y))
    where collectAlg :: Alg (T :&&: TC.Ann) (K [LLVM.Type])
          collectAlg (FparDef _ t :&&: _) = K [fancyType t]
          collectAlg (VarDef _ t :&&: _) = K [ttype t]
          collectAlg (LocalDefFuncDecl _ :&&: _) = K []
          collectAlg (LocalDefFuncDef _ :&&: _) = K []
          collectAlg (x :&&: _) = K $ hfoldMap unK x

lookupFunction :: MonadState Env m => LLVM.Name -> m LLVM.Operand
lookupFunction t = do
    mop <- gets $ Map.lookup t . functions
    case mop of
        Nothing -> error "Internal compiler error on lookupFunction - Most likely unimplemented stdlib function"
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
codegenFuncDef mparent f@(FuncDef header localDefs body :&&.: _) = mdo
    let (name, paramTypes, retType) = codegenHeader mparent header
    let frameType = createFrame mparent f
    op <- function name paramTypes retType $ \params -> mdo
        modify $ (\s -> s { functions = Map.insert name op $ functions s })
        block
        for_ localDefs $ codegenLocalDef frameType
        codegenBody frameType body params
    pure ()

codegenHeader :: Maybe LLVM.Type -> Term (T :&&: TC.Ann) Header -> (LLVM.Name, [(LLVM.Type, ParameterName)], LLVM.Type)
codegenHeader mparent (Header name mdt fparDefs :&&.: _) = 
    (LLVM.Name $ toShort $ encodeUtf8 name, 
     maybeToList (fmap (\t -> (ptr t, ParameterName "parent")) mparent) ++  fmap codegenFparDef fparDefs, 
     maybeDataType mdt)

codegenFparDef :: Term (T :&&: TC.Ann) FparDef -> (LLVM.Type, ParameterName)
codegenFparDef (FparDef name ftp :&&.: _) = (fancyType ftp, ParameterName $ toShort $ encodeUtf8 name)

codegenLocalDef :: (MonadState Env m, MonadFix m, MonadModuleBuilder m) => LLVM.Type -> Term (T :&&: TC.Ann) LocalDef -> m ()
codegenLocalDef parent (LocalDefFuncDef fd :&&.: _) = codegenFuncDef (Just parent) fd
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

getFrame :: (Integral i, MonadModuleBuilder m, MonadIRBuilder m) => LLVM.Operand -> i -> m LLVM.Operand
getFrame frame 0 = pure frame
getFrame frame n = do
    p <- gep frame [int32 0, int32 0]
    f <- load p 0
    getFrame f (n-1)

getFrameElement :: (Integral i, MonadModuleBuilder m, MonadIRBuilder m) => LLVM.Operand -> i -> i -> m LLVM.Operand
getFrameElement frame n offset = do
    f <- getFrame frame n
    gep f [int32 0 , int32 $ toInteger offset]

codegenVarId :: (MonadModuleBuilder m, MonadIRBuilder m) => LLVM.Operand -> Term (T :&&: TC.Ann) VarIdentifier -> m LLVM.Operand
codegenVarId frame (VarIdentifier _ :&&.: TC.AnnVariable _ f links offset ft _) = do
    p <- getFrameElement frame links (offset + (hasParent f :: Int))
    case ft of
        Value (AType t _) -> do 
            p' <- gep p [int32 0, int32 0]
            bitcast p' (ptr $ ttype t)
        Value (DType _) -> pure p
        Ref _ -> load p 0
        Pointer _ -> load p 0
        where hasParent t = if Data.Text.any (=='.') t then 1 else 0

codegenLvalue :: (MonadState Env m, MonadIRBuilder m, MonadModuleBuilder m) => LLVM.Operand -> Term (T :&&: TC.Ann) Lvalue -> m LLVM.Operand
codegenLvalue frame (LvalueId x :&&.: _) = codegenVarId frame x
codegenLvalue _ (LvalueStr x :&&.: _) = do 
    name <- fresh
    c <- globalStringPtr (unpack x) name
    pure $ LLVM.ConstantOperand c
codegenLvalue frame (LvalueAx l e :&&.: _) = do
    l' <- codegenLvalue frame l
    e' <- codegenExprLoad frame e
    gep l' [e']

data ValueTag = GlobalString | InFrame | Temporary

codegenExprWithTag :: (MonadState Env m, MonadModuleBuilder m, MonadIRBuilder m) => LLVM.Operand -> Term (T :&&: TC.Ann) Expr -> m (ValueTag, LLVM.Operand)
codegenExprWithTag frame x@(ExprLvalue (LvalueStr _ :&&.: _) :&&.: _) = codegenExpr frame x >>= pure . (GlobalString,)
codegenExprWithTag frame x@(ExprLvalue _ :&&.: _) = codegenExpr frame x >>= pure . (InFrame,)
codegenExprWithTag frame x = codegenExpr frame x >>= pure . (Temporary,)

codegenFuncCall :: (MonadState Env m, MonadIRBuilder m, MonadModuleBuilder m) => LLVM.Operand -> Term (T :&&: TC.Ann) FuncCall -> m LLVM.Operand
codegenFuncCall frame (FuncCall (FuncIdentifier text :&&.: TC.AnnFunction _ _ mlinks (FunctionType _ fts)) es :&&.: _) = do
    f <- lookupFunction $ LLVM.Name $ toShort $ encodeUtf8 text
    es' <- for es $ codegenExprWithTag frame
    es'' <- for (zip fts es') $ \(ft, (tag, e)) -> 
                                    case (ft, tag) of
                                        (Value (AType _ _), GlobalString) -> bitcast e $ fancyType ft
                                        (Pointer _, GlobalString) -> pure e
                                        (Value _, InFrame) -> load e 0
                                        (Ref _, InFrame) -> pure e
                                        (Pointer _, InFrame) -> pure e
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
codegenExpr _ (ExprInt x :&&.: _) = pure $ int64 x
codegenExpr _ (ExprChar x :&&.: _) = pure $ int8 $ toInteger $ fromEnum x
codegenExpr frame (ExprLvalue x :&&.: _) = codegenLvalue frame x
codegenExpr frame (ExprFuncCall x :&&.: _) = codegenFuncCall frame x
codegenExpr frame (ExprMinus x :&&.: _) = join (xor (int64 0) <$> codegenExprLoad frame x)
codegenExpr frame (ExprAdd x y :&&.: _) = join (add <$> codegenExprLoad frame x <*> codegenExprLoad frame y)
codegenExpr frame (ExprSub x y :&&.: _) = join (sub <$> codegenExprLoad frame x <*> codegenExprLoad frame y)
codegenExpr frame (ExprMul x y :&&.: _) = join (mul <$> codegenExprLoad frame x <*> codegenExprLoad frame y)
codegenExpr frame (ExprDiv x y :&&.: _) = join (udiv <$> codegenExprLoad frame x <*> codegenExprLoad frame y)
codegenExpr frame (ExprMod x y :&&.: _) = join (urem <$> codegenExprLoad frame x <*> codegenExprLoad frame y)
codegenExpr frame (ExprNot x :&&.: _) = join (icmp IP.EQ (int64 0) <$> codegenExprLoad frame x)
codegenExpr frame (ExprAnd x y :&&.: _) = do
    x' <- codegenExprLoad frame x
    y' <- codegenExprLoad frame y
    x'' <- icmp NE (int64 0) x'
    y'' <- icmp NE (int64 0) y'
    z <- I.and x'' y''
    icmp NE (int64 0) z
codegenExpr frame (ExprOr x y :&&.: _) = do
    x' <- codegenExprLoad frame x
    y' <- codegenExprLoad frame y
    x'' <- icmp NE (int64 0) x'
    y'' <- icmp NE (int64 0) y'
    z <- I.or x'' y''
    icmp NE (int64 0) z
codegenExpr _ (ExprTrue :&&.: _) = pure $ int64 1
codegenExpr _ (ExprFalse :&&.: _) = pure $ int64 0

codegenCond :: (MonadState Env m, MonadModuleBuilder m, MonadIRBuilder m) => LLVM.Operand -> Term (T :&&: TC.Ann) Cond -> m LLVM.Operand
codegenCond frame (CondExpr x :&&.: _) = join (icmp NE (int64 0) <$> codegenExprLoad frame x)
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
                   LLVM.Operand -> ShortByteString -> LLVM.Name -> LLVM.Name -> Term (T :&&: TC.Ann) CondStmt -> m LLVM.Name
codegenCondStmt frame label exit next (CondStmt c b :&&.: _) = mdo
    cond <- codegenCond frame c
    condBr cond ifThen next
    ifThen <- block `named` label
    codegenBlock frame b
    mkTerminator $ br exit
    pure ifThen

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
    codegenCondStmt frame "if" exit elifs i
    elifs <- foldrM (\ic next -> codegenCondStmt frame "elif" exit next ic) elseB ei
    elseB <- case me of
                Nothing -> pure exit
                Just e -> do
                    elseB' <- block `named` "else"
                    codegenBlock frame e
                    mkTerminator $ br exit
                    pure elseB'
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
    br loop
    put s
    exit <- block `named` "loop-exit"
    pure ()
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
