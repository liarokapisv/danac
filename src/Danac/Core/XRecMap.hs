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

class CanMapXRec p where
    xmap :: XRec p a -> (a -> b) -> XRec p b

class CanWrap f where
    wrap :: a -> f a

class XRecConv t where
    conv :: forall f p. (CanUnwrapXRec p, CanWrap f) => XRec p (t p) -> f (t (XRecMap f p))
    
instance XRecConv LabelIdentifierX where
    conv :: forall f p. (CanUnwrapXRec p, CanWrap f) => LabelIdentifier p -> LabelIdentifier (XRecMap f p)
    conv v = case unwrap @p v of 
                LabelIdentifier x y -> wrap $ LabelIdentifier x y

instance XRecConv FuncIdentifierX where
    conv :: forall f p. (CanUnwrapXRec p, CanWrap f) => FuncIdentifier p -> FuncIdentifier (XRecMap f p)
    conv v = case unwrap @p v of 
                FuncIdentifier x y -> wrap $ FuncIdentifier x y

instance XRecConv VarIdentifierX where
    conv :: forall f p. (CanUnwrapXRec p, CanWrap f) => VarIdentifier p -> VarIdentifier (XRecMap f p)
    conv v = case unwrap @p v of 
                VarIdentifier x y -> wrap $ VarIdentifier x y

instance XRecConv CharConstX where
    conv :: forall f p. (CanUnwrapXRec p, CanWrap f) => CharConst p -> CharConst (XRecMap f p)
    conv v = case unwrap @p v of 
                CharConst x y -> wrap $ CharConst x y

instance XRecConv StringLiteralX where
    conv :: forall f p. (CanUnwrapXRec p, CanWrap f) => StringLiteral p -> StringLiteral (XRecMap f p)
    conv v = case unwrap @p v of 
                StringLiteral x y -> wrap $ StringLiteral x y

instance XRecConv IntConstX where
    conv :: forall f p. (CanUnwrapXRec p, CanWrap f) => IntConst p -> IntConst (XRecMap f p)
    conv v = case unwrap @p v of 
                IntConst x y -> wrap $ IntConst x y

instance XRecConv AstX where
    conv :: forall f p. (CanUnwrapXRec p, CanWrap f) => Ast p -> Ast (XRecMap f p)
    conv v = case unwrap @p v of 
                Ast x -> wrap $ Ast $ conv x

instance XRecConv FuncDefX where
    conv :: forall f p. (CanUnwrapXRec p, CanWrap f) => FuncDef p -> FuncDef (XRecMap f p)
    conv v = case unwrap @p v of 
                FuncDef x y z -> wrap $ FuncDef (conv x) (fmap conv y) (conv z)

instance XRecConv HeaderX where
    conv :: forall f p. (CanUnwrapXRec p, CanWrap f) => Header p -> Header (XRecMap f p)
    conv v = case unwrap @p v of 
                Header x y z -> wrap $ Header (conv x) (fmap conv y) (fmap conv z)

instance XRecConv DataTypeX where
    conv :: forall f p. (CanUnwrapXRec p, CanWrap f) => DataType p -> DataType (XRecMap f p)
    conv v = case unwrap @p v of 
                Integ -> wrap Integ
                Byte -> wrap Byte

instance XRecConv FparDefX where
    conv :: forall f p. (CanUnwrapXRec p, CanWrap f) => FparDef p -> FparDef (XRecMap f p)
    conv v = case unwrap @p v of 
                FparDef x y -> wrap $ FparDef (conv x) (conv y)

instance XRecConv ParPassTypeX where
    conv :: forall f p. (CanUnwrapXRec p, CanWrap f) => ParPassType p -> ParPassType (XRecMap f p)
    conv v = case unwrap @p v of 
                ByRef x -> wrap $ ByRef (conv x)
                ByDefault x -> wrap $ ByDefault (conv x)

instance XRecConv TypeX where
    conv :: forall f p. (CanUnwrapXRec p, CanWrap f) => Type p -> Type (XRecMap f p)
    conv v = case unwrap @p v of 
                OType x -> wrap $ OType (conv x)
                PType x -> wrap $ PType (conv x)

instance XRecConv ObjectTypeX where
    conv :: forall f p. (CanUnwrapXRec p, CanWrap f) => ObjectType p -> ObjectType (XRecMap f p)
    conv v = case unwrap @p v of 
                DType x -> wrap $ DType (conv x)
                AType x y -> wrap $ AType (conv x) (conv y)

instance XRecConv LocalDefX where
    conv :: forall f p. (CanUnwrapXRec p, CanWrap f) => LocalDef p -> LocalDef (XRecMap f p)
    conv v = case unwrap @p v of 
                LocalDefFuncDef x -> wrap $ LocalDefFuncDef (conv x)
                LocalDefFuncDecl x -> wrap $ LocalDefFuncDecl (conv x)
                LocalDefVarDef x -> wrap $ LocalDefVarDef (conv x)

instance XRecConv FuncDeclX where
    conv :: forall f p. (CanUnwrapXRec p, CanWrap f) => FuncDecl p -> FuncDecl (XRecMap f p)
    conv v = case unwrap @p v of 
                FuncDecl x -> wrap $ FuncDecl (conv x)

instance XRecConv VarDefX where
    conv :: forall f p. (CanUnwrapXRec p, CanWrap f) => VarDef p -> VarDef (XRecMap f p)
    conv v = case unwrap @p v of 
                VarDef x y -> wrap $ VarDef (conv x) (conv y)

instance XRecConv StmtX where
    conv :: forall f p. (CanUnwrapXRec p, CanWrap f) => Stmt p -> Stmt (XRecMap f p)
    conv v = case unwrap @p v of 
                StmtSkip -> wrap StmtSkip
                StmtAssign x y -> wrap $ StmtAssign (conv x) (conv y)
                StmtProcCall x -> wrap $ StmtProcCall (conv x)
                StmtExit -> wrap StmtSkip
                StmtReturn x -> wrap $ StmtReturn (conv x)
                StmtIf (x,y) z k -> wrap $ StmtIf (conv x, conv y)
                                                  (fmap (\(x', y') -> (conv x', conv y')) z)
                                                  (fmap conv k)
                StmtLoop x y -> wrap $ StmtLoop (fmap conv x) (conv y)
                StmtBreak x -> wrap $ StmtBreak (fmap conv x)
                StmtContinue x -> wrap $ StmtContinue (fmap conv x)

instance XRecConv BlockX where
    conv :: forall f p. (CanUnwrapXRec p, CanWrap f) => Block p -> Block (XRecMap f p)
    conv v = case unwrap @p v of 
                Block x -> wrap $ Block (fmap conv x)

instance XRecConv FuncCallX where
    conv :: forall f p. (CanUnwrapXRec p, CanWrap f) => FuncCall p -> FuncCall (XRecMap f p)
    conv v = case unwrap @p v of 
                FuncCall x y z -> wrap $ FuncCall x (conv y) (fmap conv z)

instance XRecConv LvalueX where
    conv :: forall f p. (CanUnwrapXRec p, CanWrap f) => Lvalue p -> Lvalue (XRecMap f p)
    conv v = case unwrap @p v of 
                LvalueId x y -> wrap $ LvalueId x (conv y)
                LvalueStr x y -> wrap $ LvalueStr x (conv y)
                LvalueAx x y z -> wrap $ LvalueAx x (conv y) (conv z)

instance XRecConv ExprX where
    conv :: forall f p. (CanUnwrapXRec p, CanWrap f) => Expr p -> Expr (XRecMap f p)
    conv v = case unwrap @p v of 
                ExprIntConst x y -> wrap $ ExprIntConst x (conv y)
                ExprCharConst x y -> wrap $ ExprCharConst x (conv y)
                ExprLvalue x y -> wrap $ ExprLvalue x (conv y)
                ExprParen x y -> wrap $ ExprParen x (conv y)
                ExprFuncCall x y -> wrap $ ExprFuncCall x (conv y)
                ExprSigned x y z -> wrap $ ExprSigned x y (conv z)
                ExprAdd x y z -> wrap $ ExprAdd x (conv y) (conv z)
                ExprSub x y z -> wrap $ ExprSub x (conv y) (conv z)
                ExprMul x y z -> wrap $ ExprMul x (conv y) (conv z)
                ExprDiv x y z -> wrap $ ExprDiv x (conv y) (conv z)
                ExprMod x y z -> wrap $ ExprMod x (conv y) (conv z)
                ExprOr x y z -> wrap $ ExprOr x (conv y) (conv z)
                ExprAnd x y z -> wrap $ ExprAnd x (conv y) (conv z)
                ExprTrue x -> wrap $ ExprTrue x
                ExprFalse x -> wrap $ ExprFalse x
                ExprNot x y -> wrap $ ExprNot x (conv y)

instance XRecConv CondX where
    conv :: forall f p. (CanUnwrapXRec p, CanWrap f) => Cond p -> Cond (XRecMap f p)
    conv v = case unwrap @p v of 
                CondExpr x -> wrap $ CondExpr (conv x)
                CondParen x -> wrap $ CondParen (conv x)
                CondNot x -> wrap $ CondNot (conv x)
                CondOr x y -> wrap $ CondOr (conv x) (conv y)
                CondAnd x y -> wrap $ CondAnd (conv x) (conv y)
                CondEq x y -> wrap $ CondEq (conv x) (conv y)
                CondNe x y -> wrap $ CondNe (conv x) (conv y)
                CondLt x y -> wrap $ CondLt (conv x) (conv y)
                CondGt x y -> wrap $ CondGt (conv x) (conv y)
                CondLe x y -> wrap $ CondLe (conv x) (conv y)
                CondGe x y -> wrap $ CondGe (conv x) (conv y)
