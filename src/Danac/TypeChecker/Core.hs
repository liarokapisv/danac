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

module Danac.TypeChecker.Core where

import Danac.Core.Ast
import Danac.Core.XRecMap

import Data.Map
import Data.Functor.Compose
import Data.Coerce

import Control.Monad.Reader
import Control.Monad.Except

import Validation (Validation (..))

data FuncType p = FuncType (DataType p) [ParPassType p]

deriving instance (Eq (DataType p), Eq (ParPassType p)) => Eq (FuncType p)
deriving instance (Show (DataType p), Show (ParPassType p)) => Show (FuncType p)

data ValueType p = Rvalue (Type p)
                 | Lvalue (Type p)

deriving instance (Eq (Type p)) => Eq (ValueType p)
deriving instance (Show (Type p)) => Show (ValueType p)

data Scope p = Scope {
    functions :: Map (IdP p) (FuncType p),
    variables :: Map (IdP p) (ObjectType p)
}

emptyScope :: Scope p
emptyScope = Scope { 
    functions = Data.Map.empty, 
    variables = Data.Map.empty
}

data TypeError p = ConflictingTypes (VarIdentifier p) (ObjectType p) (ObjectType p)

data CompilerError p = FunctionLookupFailed (FuncIdentifier p) (Scope p)
                     | VariableLookupFailed (VarIdentifier p) (Scope p)

type Validated p = Validation [TypeError p]

type TypeChecker p m = Compose m (Validated p)

type TypeCheckerC p p' m = (Ord (IdP p), 
                            Ord (IdP p'),
                            IdP p ~ IdP p',
                            XRec p ~ XRec p',
                            CanMapXRec p, 
                            CanUnwrapXRec p, 
                            Coercible (LabelIdentifier p) (LabelIdentifier p'),
                            Coercible (VarIdentifier p) (VarIdentifier p'),
                            Coercible (FuncIdentifier p) (FuncIdentifier p'),
                            Coercible (IntConst p) (IntConst p'),
                            Coercible (CharConst p) (CharConst p'),
                            Coercible (StringLiteral p) (StringLiteral p'),
                            Coercible (Type p) (Type p'),
                            Coercible (ObjectType p) (ObjectType p'),
                            Coercible (DataType p) (DataType p'),
                            Coercible (ParPassType p) (ParPassType p'),
                            MonadReader (Scope p') m,
                            MonadError (CompilerError p') m)


annotateVar :: forall p p' m a . TypeCheckerC p p' m => VarIdentifier p -> ObjectType p' -> (VarIdentifier p' -> m a) -> m a
annotateVar v o f =
    case unwrap @p v of
        VarIdentifier _ i -> do
            m <- reader variables
            let m' = insert i o m
            local (\s -> s { variables = m' }) $ f $ coerce v

annotateFun :: forall p p' m a . TypeCheckerC p p' m => FuncIdentifier p -> FuncType p' -> (FuncIdentifier p' -> m a) -> m a
annotateFun v o f =
    case unwrap @p v of
        FuncIdentifier _ i -> do
            m <- reader functions
            let m' = insert i (coerce o) m
            local (\s -> s { functions = m' }) $ f $ coerce v

varType :: forall p p' m . TypeCheckerC p p' m => VarIdentifier p -> m (ObjectType p')
varType v = 
    case unwrap @p v of
        VarIdentifier _ i -> do
            m <- reader variables
            case Data.Map.lookup i m of
                Nothing -> do
                    scope <- ask 
                    throwError $ VariableLookupFailed (coerce v) scope
                Just t -> pure t
