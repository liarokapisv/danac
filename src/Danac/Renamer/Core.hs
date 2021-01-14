{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Danac.Renamer.Core where

import Danac.Core.Ast
import Danac.Core.XRecMap

import Data.Map
import Data.Text
import Data.Functor.Compose

import Control.Monad.Reader

import Validation (Validation (..))

data Scope = Scope {
    namespace :: Maybe Text,
    functions :: Map Text Text,
    variables :: Map Text Text,
    labels :: Map Text Text
}

emptyScope :: Scope
emptyScope = Scope { 
    namespace = Nothing, 
    functions = fromList [("writeString", "writeString"),
                          ("writeInteger", "writeInteger")],
    variables = Data.Map.empty, 
    labels = Data.Map.empty
}

data Error p = UndeclaredVariable (VarIdentifier p)
             | UndeclaredFunction (FuncIdentifier p)
             | UndeclaredLabel (LabelIdentifier p)

deriving instance forall p . (Show (VarIdentifier p),
                              Show (FuncIdentifier p),
                              Show (LabelIdentifier p)) => Show (Error p)

type Validated p = Validation [Error p]

type RenamerC p m = (CanUnwrapXRec p, CanMapXRec p, IdP p ~ Text, MonadReader Scope m)

type Renamer p m = Compose m (Validated p)

withVarIdentifier :: forall p m a . RenamerC p m => VarIdentifier p -> (VarIdentifier p -> m a) -> m a
withVarIdentifier v f = 
    case unwrap @p v of 
        VarIdentifier x i -> do
            ns <- reader namespace
            m <- reader variables
            let i' = case ns of
                        Nothing -> i
                        Just ns' -> ns' <> "." <> i
            let m' = insert i i' m
            local (\s -> s { variables = m' }) $ f $ xmap @ p v $ const $ VarIdentifier x i'

withVarDef :: forall p m a . RenamerC p m => VarDef p -> (VarDef p -> m a) -> m a
withVarDef v f = 
    case unwrap @p v of
        VarDef x y -> withVarIdentifier x $ \x' -> f (xmap @p v $ const $ VarDef x' y)

withFuncIdentifier :: forall p m a . RenamerC p m => FuncIdentifier p -> (FuncIdentifier p -> m a) -> m a
withFuncIdentifier v f = 
    case unwrap @ p v of
        FuncIdentifier x i -> do
            ns <- reader namespace
            m <- reader functions
            let i' = case ns of
                        Nothing -> i
                        Just ns' -> ns' <> "." <> i
            let m' = insert i i' m
            local (\s -> s { functions = m' }) $ f $ xmap @ p v $ const $ FuncIdentifier x i'

withScope :: forall p m a . RenamerC p m => FuncIdentifier p -> m a -> m a
withScope v f = 
    case unwrap @ p v of
        FuncIdentifier _ i -> do
            local (\s -> s { namespace = Just i }) f

withLabelIdentifier :: forall p m a . RenamerC p m => LabelIdentifier p -> (LabelIdentifier p -> m a) -> m a
withLabelIdentifier v f = 
    case unwrap @ p v of
        LabelIdentifier x i -> do
            ns <- reader namespace
            m <- reader labels
            let i' = case ns of
                        Nothing -> i
                        Just ns' -> ns' <> "." <> i
            let m' = insert i i' m
            local (\s -> s { namespace = Just i', labels = m' }) $ f $ xmap @ p v $ const $ LabelIdentifier x i'

withFparDef :: forall p m a. RenamerC p m => FparDef p -> (FparDef p -> m a) -> m a
withFparDef v f = 
    case unwrap @ p v of
        FparDef vi p -> do
            withVarIdentifier vi $ \vi' -> 
                f $ xmap @ p v $ const $ FparDef vi' p

withFparDefs :: forall p m a. RenamerC p m => [FparDef p] -> ([FparDef p] -> m a) -> m a
withFparDefs [] f = f []
withFparDefs (x : xs) f = withFparDef x $ \x' ->
    withFparDefs xs $ \xs' -> 
        f (x' : xs')

withHeader :: forall p m a b . RenamerC p m => Header p -> (Header p -> m a) -> (a -> m b) -> m b
withHeader v inner outer = 
    case unwrap @ p v of
        Header fi dt fps -> do
            withFuncIdentifier fi $ \fi' -> do
                f' <- withScope fi' $ 
                        withFparDefs fps $ \fps' -> 
                            inner $ xmap @ p v $ const $ Header fi' dt fps'
                outer f'
                                
withFuncDecl :: forall p m a. RenamerC p m => FuncDecl p -> (FuncDecl p -> m a) -> m a
withFuncDecl v f = 
    case unwrap @p v of
        FuncDecl h -> withHeader h pure (\h' -> f (xmap @p v $ const $ FuncDecl h'))

varIdentifier :: forall p m . RenamerC p m => VarIdentifier p -> Renamer p m (VarIdentifier p)
varIdentifier v = 
    case unwrap @ p v of 
        VarIdentifier x i -> Compose $ do
            i' <- reader (Data.Map.lookup i . variables)
            case i' of
                Nothing -> pure $ Failure [UndeclaredVariable v]
                Just i'' -> pure $ Success $ (xmap @ p) v $ const $ VarIdentifier x i''

funcIdentifier :: forall p m . RenamerC p m => FuncIdentifier p -> Renamer p m (FuncIdentifier p)
funcIdentifier v = 
    case unwrap @ p v of
        FuncIdentifier x i -> Compose $ do
            i' <- reader (Data.Map.lookup i . functions)
            case i' of
                Nothing -> pure $ Failure [UndeclaredFunction v]
                Just i'' -> pure $ Success $ (xmap @ p) v $ const $ FuncIdentifier x i''

labelIdentifier :: forall p m . RenamerC p m => LabelIdentifier p -> Renamer p m (LabelIdentifier p)
labelIdentifier v = 
    case unwrap @ p v of
        LabelIdentifier x i -> Compose $ do
            i' <- reader (Data.Map.lookup i . labels)
            case i' of
                Nothing -> pure $ Failure [UndeclaredLabel v]
                Just i'' -> pure $ Success $ (xmap @ p) v $ const $ LabelIdentifier x i''

rep :: forall p f a . (CanMapXRec p, Functor f) => XRec p a -> f a -> f (XRec p a)
rep x = fmap (xmap @ p x . const)

lvalue :: forall p m . RenamerC p m => Lvalue p -> Renamer p m (Lvalue p)
lvalue v = 
    case unwrap @ p v of
        LvalueId x y -> rep @p v $ LvalueId <$> pure x <*> varIdentifier y
        LvalueStr _ _ -> pure v
        LvalueAx x y z -> rep @p v $ LvalueAx <$> pure x <*> lvalue y <*> expr z

expr :: forall p m . RenamerC p m => Expr p -> Renamer p m (Expr p)
expr v = 
    case unwrap @ p v of
        ExprIntConst x y -> rep @p v $ ExprIntConst <$> pure x <*> pure y
        ExprCharConst x y -> rep @p v $ ExprCharConst <$> pure x <*> pure y
        ExprLvalue x y -> rep @p v $ ExprLvalue <$> pure x <*> lvalue y
        ExprParen x y -> rep @p v $ ExprParen <$> pure x <*> expr y
        ExprFuncCall x y -> rep @p v $ ExprFuncCall <$> pure x <*> funcCall y
        ExprSigned x y z -> rep @p v $ ExprSigned <$> pure x <*> pure y <*> expr z
        ExprAdd x y z -> rep @p v $ ExprAdd <$> pure x <*> expr y <*> expr z
        ExprSub x y z -> rep @p v $ ExprSub <$> pure x <*> expr y <*> expr z
        ExprMul x y z -> rep @p v $ ExprMul <$> pure x <*> expr y <*> expr z
        ExprDiv x y z -> rep @p v $ ExprDiv <$> pure x <*> expr y <*> expr z
        ExprMod x y z -> rep @p v $ ExprMod <$> pure x <*> expr y <*> expr z
        ExprOr  x y z -> rep @p v $ ExprOr <$> pure x <*> expr y <*> expr z
        ExprAnd x y z -> rep @p v $ ExprAnd <$> pure x <*> expr y <*> expr z
        ExprTrue x -> rep @p v $ ExprTrue <$> pure x
        ExprFalse x -> rep @p v $ ExprFalse <$> pure x
        ExprNot x y -> rep @p v $ ExprNot <$> pure x <*> expr y
        
funcCall :: forall p m . RenamerC p m => FuncCall p -> Renamer p m (FuncCall p)
funcCall v = 
    case unwrap @p v of
        FuncCall x y z -> rep @p v $ FuncCall <$> pure x <*> funcIdentifier y <*> traverse expr z

cond :: forall p m . RenamerC p m => Cond p -> Renamer p m (Cond p)
cond v = 
    case unwrap @p v of
        CondExpr x -> rep @p v $ CondExpr <$> expr x
        CondParen x  -> rep @p v $ CondParen <$> cond x
        CondNot x -> rep @p v $ CondNot <$> cond x
        CondOr x y -> rep @p v $ CondOr <$> cond x <*> cond y
        CondAnd x y -> rep @p v $ CondAnd <$> cond x <*> cond y
        CondEq x y -> rep @p v $ CondEq <$> expr x <*> expr y
        CondNe x y -> rep @p v $ CondNe <$> expr x <*> expr y
        CondLt x y -> rep @p v $ CondLt <$> expr x <*> expr y
        CondGt x y -> rep @p v $ CondGt <$> expr x <*> expr y
        CondLe x y -> rep @p v $ CondLe <$> expr x <*> expr y
        CondGe x y -> rep @p v $ CondGe <$> expr x <*> expr y


stmt :: forall p m . RenamerC p m => Stmt p -> Renamer p m (Stmt p)
stmt v = 
    case unwrap @p v of
        StmtSkip -> rep @p v $ pure StmtSkip
        StmtAssign x y -> rep @p v $ StmtAssign <$> lvalue x <*> expr y
        StmtProcCall x -> rep @p v $ StmtProcCall <$> funcCall x
        StmtExit -> rep @p v $ pure StmtExit
        StmtReturn x -> rep @p v $ StmtReturn <$> expr x
        StmtIf xy xys z -> rep @p v $ StmtIf <$> cb xy <*> traverse cb xys <*> traverse block z
            where cb (x,y) = (,) <$> cond x <*> block y
        StmtLoop Nothing b -> rep @p v $ StmtLoop <$> pure Nothing <*> block b
        StmtLoop (Just l) b -> rep @p v $ Compose $
            withLabelIdentifier l $ \l' -> getCompose $ StmtLoop <$> pure (Just l') <*> block b
        StmtBreak x -> rep @p v $ StmtBreak <$> traverse labelIdentifier x
        StmtContinue x -> rep @p v $ StmtContinue <$> traverse labelIdentifier x

block :: forall p m . RenamerC p m => Block p -> Renamer p m (Block p)
block v = 
    case unwrap @p v of
        Block xs -> rep @p v $ Block <$> traverse stmt xs


withLocalDef :: forall p m a . RenamerC p m => LocalDef p -> (Validated p (LocalDef p) -> m a) -> m a
withLocalDef v f =
    case unwrap @p v of
        LocalDefFuncDef x -> withFuncDef x $ \x' -> f (fmap (xmap @p v . const) $ LocalDefFuncDef <$> x')
        LocalDefFuncDecl x -> withFuncDecl x $ \x' -> f (Success (xmap @p v $ const $ LocalDefFuncDecl x'))
        LocalDefVarDef x -> withVarDef x $ \x' -> f (Success (xmap @p v $ const $ LocalDefVarDef x'))

withLocalDefs :: forall p m a . RenamerC p m => [LocalDef p] -> (Validated p [LocalDef p] -> m a) -> m a
withLocalDefs [] f = f $ Success []
withLocalDefs (x : xs) f = withLocalDef x $ \x' ->
    withLocalDefs xs $ \xs' -> 
        f ((:) <$> x' <*> xs')

withFuncDef :: forall p m a. RenamerC p m => FuncDef p -> (Validated p (FuncDef p) -> m a) -> m a
withFuncDef v f = 
    case unwrap @p v of
        FuncDef h ds b -> do
            withHeader h 
                (\h' -> do
                    withLocalDefs ds $ \ds' -> do
                        b' <- getCompose $ block b
                        pure (h', ds', b'))
                (\(h', ds', b') -> f (fmap (xmap @p v . const) $ FuncDef <$> (Success h') <*> ds' <*> b'))

funcDef :: forall p m . RenamerC p m => FuncDef p -> Renamer p m (FuncDef p)
funcDef v = Compose $ withFuncDef v pure

ast :: forall p m . RenamerC p m => Ast p -> Renamer p m (Ast p)
ast v = case unwrap @p v of
            Ast f -> rep @p v $ Ast <$> funcDef f
