{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Danac.Core.XRecMap where

import Danac.Core.Ast
import Data.Functor.Identity
import GHC.Generics

data XRecMap (f :: * -> *) x

type instance IdP (XRecMap f x) = IdP x
type instance CharP (XRecMap f x) = CharP x
type instance StringP (XRecMap f x) = StringP x
type instance IntP (XRecMap f x) = IntP x
type instance XLabelIdentifier (XRecMap f x) = XLabelIdentifier x
type instance XVarIdentifier (XRecMap f x) = XVarIdentifier x
type instance XFuncIdentifier (XRecMap f x) = XFuncIdentifier x
type instance XCharConst (XRecMap f x) = XCharConst x
type instance XStringLiteral (XRecMap f x) = XStringLiteral x
type instance XIntConst (XRecMap f x) = XIntConst x
type instance XFuncCall (XRecMap f x) = XFuncCall x
type instance XLvalue (XRecMap f x) = XLvalue x
type instance XExpr (XRecMap f x) = XExpr x
type instance XRec (XRecMap f x) a = f a

type Strip x = XRecMap Identity x

class CanUnwrapXRec p where
    unwrap :: XRec p a -> a

class CanWrap f where
    wrap :: a -> f a

class XRecConv t where
    conv :: forall f p. (CanUnwrapXRec p, CanWrap f) => XRec p (t p) -> f (t (XRecMap f p))
    
instance XRecConv LabelIdentifierX where
    conv :: forall f p. (CanUnwrapXRec p, CanWrap f) => LabelIdentifier p -> LabelIdentifier (XRecMap f p)
    conv (unwrap @ p -> LabelIdentifier x y) = wrap $ LabelIdentifier x y

instance XRecConv FuncIdentifierX where
    conv :: forall f p. (CanUnwrapXRec p, CanWrap f) => FuncIdentifier p -> FuncIdentifier (XRecMap f p)
    conv (unwrap @ p -> FuncIdentifier x y) = wrap $ FuncIdentifier x y

instance XRecConv VarIdentifierX where
    conv :: forall f p. (CanUnwrapXRec p, CanWrap f) => VarIdentifier p -> VarIdentifier (XRecMap f p)
    conv (unwrap @ p -> VarIdentifier x y) = wrap $ VarIdentifier x y

instance XRecConv CharConstX where
    conv :: forall f p. (CanUnwrapXRec p, CanWrap f) => CharConst p -> CharConst (XRecMap f p)
    conv (unwrap @ p -> CharConst x y) = wrap $ CharConst x y

instance XRecConv StringLiteralX where
    conv :: forall f p. (CanUnwrapXRec p, CanWrap f) => StringLiteral p -> StringLiteral (XRecMap f p)
    conv (unwrap @ p -> StringLiteral x y) = wrap $ StringLiteral x y

instance XRecConv IntConstX where
    conv :: forall f p. (CanUnwrapXRec p, CanWrap f) => IntConst p -> IntConst (XRecMap f p)
    conv (unwrap @ p -> IntConst x y) = wrap $ IntConst x y

instance XRecConv AstX where
    conv :: forall f p. (CanUnwrapXRec p, CanWrap f) => Ast p -> Ast (XRecMap f p)
    conv (unwrap @ p -> Ast x) = wrap $ Ast $ conv x

instance XRecConv FuncDefX where
    conv :: forall f p. (CanUnwrapXRec p, CanWrap f) => FuncDef p -> FuncDef (XRecMap f p)
    conv (unwrap @ p -> FuncDef x y z) = wrap $ FuncDef (conv x) (fmap conv y) (conv z)

instance XRecConv HeaderX where
    conv :: forall f p. (CanUnwrapXRec p, CanWrap f) => Header p -> Header (XRecMap f p)
    conv (unwrap @ p -> Header x y z) = wrap $ Header (conv x) (fmap conv y) (fmap conv z)

instance XRecConv DataTypeX where
    conv :: forall f p. (CanUnwrapXRec p, CanWrap f) => DataType p -> DataType (XRecMap f p)
    conv (unwrap @ p -> Integ) = wrap Integ
    conv (unwrap @ p -> Byte) = wrap Byte

instance XRecConv FparDefX where
    conv :: forall f p. (CanUnwrapXRec p, CanWrap f) => FparDef p -> FparDef (XRecMap f p)
    conv (unwrap @ p -> FparDef x y) = wrap $ FparDef (conv x) (conv y)

instance XRecConv ParPassTypeX where
    conv :: forall f p. (CanUnwrapXRec p, CanWrap f) => ParPassType p -> ParPassType (XRecMap f p)
    conv (unwrap @ p -> ByRef x) = wrap $ ByRef (conv x)
    conv (unwrap @ p -> ByDefault x) = wrap $ ByDefault (conv x)

instance XRecConv TypeX where
    conv :: forall f p. (CanUnwrapXRec p, CanWrap f) => Type p -> Type (XRecMap f p)
    conv (unwrap @ p -> OType x) = wrap $ OType (conv x)
    conv (unwrap @ p -> PType x) = wrap $ PType (conv x)

instance XRecConv ObjectTypeX where
    conv :: forall f p. (CanUnwrapXRec p, CanWrap f) => ObjectType p -> ObjectType (XRecMap f p)
    conv (unwrap @ p -> DType x) = wrap $ DType (conv x)
    conv (unwrap @ p -> AType x y) = wrap $ AType (conv x) (conv y)

instance XRecConv LocalDefX where
    conv :: forall f p. (CanUnwrapXRec p, CanWrap f) => LocalDef p -> LocalDef (XRecMap f p)
    conv (unwrap @ p -> LocalDefFuncDef x) = wrap $ LocalDefFuncDef (conv x)
    conv (unwrap @ p -> LocalDefFuncDecl x) = wrap $ LocalDefFuncDecl (conv x)
    conv (unwrap @ p -> LocalDefVarDef x) = wrap $ LocalDefVarDef (conv x)

instance XRecConv FuncDeclX where
    conv :: forall f p. (CanUnwrapXRec p, CanWrap f) => FuncDecl p -> FuncDecl (XRecMap f p)
    conv (unwrap @ p -> FuncDecl x) = wrap $ FuncDecl (conv x)

instance XRecConv VarDefX where
    conv :: forall f p. (CanUnwrapXRec p, CanWrap f) => VarDef p -> VarDef (XRecMap f p)
    conv (unwrap @ p -> VarDef x y) = wrap $ VarDef (conv x) (conv y)

instance XRecConv StmtX where
    conv :: forall f p. (CanUnwrapXRec p, CanWrap f) => Stmt p -> Stmt (XRecMap f p)
    conv (unwrap @ p -> StmtSkip) = wrap StmtSkip
    conv (unwrap @ p -> StmtAssign x y) = wrap $ StmtAssign (conv x) (conv y)
    conv (unwrap @ p -> StmtProcCall x) = wrap $ StmtProcCall (conv x)
    conv (unwrap @ p -> StmtExit) = wrap StmtSkip
    conv (unwrap @ p -> StmtReturn x) = wrap $ StmtReturn (conv x)
    conv (unwrap @ p -> StmtIf (x,y) z k) = wrap $ StmtIf (conv x, conv y)
                                                           (fmap (\(x, y) -> (conv x, conv y)) z)
                                                           (fmap conv k)
    conv (unwrap @ p -> StmtLoop x y) = wrap $ StmtLoop (fmap conv x) (conv y)
    conv (unwrap @ p -> StmtBreak x) = wrap $ StmtBreak (fmap conv x)
    conv (unwrap @ p -> StmtContinue x) = wrap $ StmtContinue (fmap conv x)

instance XRecConv BlockX where
    conv :: forall f p. (CanUnwrapXRec p, CanWrap f) => Block p -> Block (XRecMap f p)
    conv (unwrap @ p -> Block x) = wrap $ Block (fmap conv x)

instance XRecConv FuncCallX where
    conv :: forall f p. (CanUnwrapXRec p, CanWrap f) => FuncCall p -> FuncCall (XRecMap f p)
    conv (unwrap @ p -> FuncCall x y z) = wrap $ FuncCall x (conv y) (fmap conv z)

instance XRecConv LvalueX where
    conv :: forall f p. (CanUnwrapXRec p, CanWrap f) => Lvalue p -> Lvalue (XRecMap f p)
    conv (unwrap @ p -> LvalueId x y) = wrap $ LvalueId x (conv y)
    conv (unwrap @ p -> LvalueStr x y) = wrap $ LvalueStr x (conv y)
    conv (unwrap @ p -> LvalueAx x y z) = wrap $ LvalueAx x (conv y) (conv z)

instance XRecConv ExprX where
    conv :: forall f p. (CanUnwrapXRec p, CanWrap f) => Expr p -> Expr (XRecMap f p)
    conv (unwrap @ p -> ExprIntConst x y) = wrap $ ExprIntConst x (conv y)
    conv (unwrap @ p -> ExprCharConst x y) = wrap $ ExprCharConst x (conv y)
    conv (unwrap @ p -> ExprLvalue x y) = wrap $ ExprLvalue x (conv y)
    conv (unwrap @ p -> ExprParen x y) = wrap $ ExprParen x (conv y)
    conv (unwrap @ p -> ExprFuncCall x y) = wrap $ ExprFuncCall x (conv y)
    conv (unwrap @ p -> ExprSigned x y z) = wrap $ ExprSigned x y (conv z)
    conv (unwrap @ p -> ExprAdd x y z) = wrap $ ExprAdd x (conv y) (conv z)
    conv (unwrap @ p -> ExprSub x y z) = wrap $ ExprSub x (conv y) (conv z)
    conv (unwrap @ p -> ExprMul x y z) = wrap $ ExprMul x (conv y) (conv z)
    conv (unwrap @ p -> ExprDiv x y z) = wrap $ ExprDiv x (conv y) (conv z)
    conv (unwrap @ p -> ExprMod x y z) = wrap $ ExprMod x (conv y) (conv z)
    conv (unwrap @ p -> ExprOr x y z) = wrap $ ExprOr x (conv y) (conv z)
    conv (unwrap @ p -> ExprAnd x y z) = wrap $ ExprAnd x (conv y) (conv z)
    conv (unwrap @ p -> ExprTrue x) = wrap $ ExprTrue x
    conv (unwrap @ p -> ExprFalse x) = wrap $ ExprFalse x
    conv (unwrap @ p -> ExprNot x y) = wrap $ ExprNot x (conv y)

instance XRecConv CondX where
    conv :: forall f p. (CanUnwrapXRec p, CanWrap f) => Cond p -> Cond (XRecMap f p)
    conv (unwrap @ p -> CondExpr x) = wrap $ CondExpr (conv x)
    conv (unwrap @ p -> CondParen x) = wrap $ CondParen (conv x)
    conv (unwrap @ p -> CondNot x) = wrap $ CondNot (conv x)
    conv (unwrap @ p -> CondOr x y) = wrap $ CondOr (conv x) (conv y)
    conv (unwrap @ p -> CondAnd x y) = wrap $ CondAnd (conv x) (conv y)
    conv (unwrap @ p -> CondEq x y) = wrap $ CondEq (conv x) (conv y)
    conv (unwrap @ p -> CondNe x y) = wrap $ CondNe (conv x) (conv y)
    conv (unwrap @ p -> CondLt x y) = wrap $ CondLt (conv x) (conv y)
    conv (unwrap @ p -> CondGt x y) = wrap $ CondGt (conv x) (conv y)
    conv (unwrap @ p -> CondLe x y) = wrap $ CondLe (conv x) (conv y)
    conv (unwrap @ p -> CondGe x y) = wrap $ CondGe (conv x) (conv y)
