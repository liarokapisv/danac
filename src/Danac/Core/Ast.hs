{-# LANGUAGE 
    ExplicitForAll, 
    DataKinds, 
    OverloadedLabels, 
    ScopedTypeVariables, 
    TypeOperators, 
    ConstraintKinds, 
    UndecidableInstances, 
    TypeFamilies, 
    StandaloneDeriving, 
    PatternSynonyms, 
    FlexibleContexts 
#-}

module Danac.Core.Ast where

import GHC.Exts (Constraint)

----------------------------------------------------
----------------------------------------------------

type ForAll (f :: * -> Constraint) z = 
    (
     f (IdP z)
    ,f (IntP z)
    ,f (CharP z)
    ,f (StringP z)
    ,f (XLabelIdentifier z)
    ,f (XFuncIdentifier z)
    ,f (XVarIdentifier z)
    ,f (XCharConst z)
    ,f (XStringLiteral z)
    ,f (XIntConst z)
    ,f (XFuncCall z)
    ,f (XLvalue z)
    ,f (XExpr z)
    ,f (XRec z (LabelIdentifierX z))
    ,f (XRec z (FuncIdentifierX z))
    ,f (XRec z (VarIdentifierX z))
    ,f (XRec z (CharConstX z))
    ,f (XRec z (StringLiteralX z))
    ,f (XRec z (IntConstX z))
    ,f (XRec z (TypeX))
    ,f (XRec z (LocalDefX z))
    ,f (XRec z (FuncDeclX z))
    ,f (XRec z (VarDefX z))
    ,f (XRec z (StmtX z))
    ,f (XRec z (FuncCallX z))
    ,f (XRec z (LvalueX z))
    ,f (XRec z (ExprX z))
    ,f (XRec z (CondX z))
    )

    

----------------------------------------------------
----------------------------------------------------

type family XRec z a

----------------------------------------------------
----------------------------------------------------

type family IdP z

----------------------------------------------------
----------------------------------------------------

type family XLabelIdentifier z

data LabelIdentifierX z = LabelIdentifier (XLabelIdentifier z) (IdP z)

deriving instance (ForAll Show z) => Show (LabelIdentifierX z)
deriving instance (ForAll Eq z) => Eq (LabelIdentifierX z)

type LabelIdentifier z = XRec z (LabelIdentifierX z)


----------------------------------------------------
----------------------------------------------------

type family XFuncIdentifier z

data FuncIdentifierX z = FuncIdentifier (XFuncIdentifier z) (IdP z)

deriving instance (ForAll Show z) => Show (FuncIdentifierX z)
deriving instance (ForAll Eq z) => Eq (FuncIdentifierX z)

type FuncIdentifier z = XRec z (FuncIdentifierX z)

----------------------------------------------------
----------------------------------------------------

type family XVarIdentifier z

data VarIdentifierX z = VarIdentifier (XVarIdentifier z) (IdP z)

deriving instance (ForAll Show z) => Show (VarIdentifierX z)
deriving instance (ForAll Eq z) => Eq (VarIdentifierX z)

type VarIdentifier z = XRec z (VarIdentifierX z)

---------------------------------------------------- 
----------------------------------------------------

type family CharP z

---------------------------------------------------- 
----------------------------------------------------

type family XCharConst z

data CharConstX z = CharConst (XCharConst z) (CharP z)

deriving instance (ForAll Show z) => Show (CharConstX z)
deriving instance (ForAll Eq z) => Eq (CharConstX z)

type CharConst z = XRec z (CharConstX z)

---------------------------------------------------- 
----------------------------------------------------

type family StringP z

----------------------------------------------------
----------------------------------------------------

type family XStringLiteral z

data StringLiteralX z = StringLiteral (XStringLiteral z) (StringP z)

deriving instance (ForAll Show z) => Show (StringLiteralX z)
deriving instance (ForAll Eq z) => Eq (StringLiteralX z)

type StringLiteral z = XRec z (StringLiteralX z)

----------------------------------------------------
----------------------------------------------------

type family IntP z

----------------------------------------------------
----------------------------------------------------

type family XIntConst z

data IntConstX z = IntConst (XIntConst z) (IntP z)

deriving instance (ForAll Show z) => Show (IntConstX z)
deriving instance (ForAll Eq z) => Eq (IntConstX z)

type IntConst z = XRec z (IntConstX z)

----------------------------------------------------
----------------------------------------------------

data Ast z = Ast (FuncDef z)

deriving instance (ForAll Eq z) => Eq (Ast z)
deriving instance (ForAll Show z) => Show (Ast z)

----------------------------------------------------
----------------------------------------------------

data FuncDef z = FuncDef (Header z) [LocalDef z] (Block z) 

deriving instance (ForAll Eq z) => Eq (FuncDef z)
deriving instance (ForAll Show z) => Show (FuncDef z)

----------------------------------------------------
----------------------------------------------------

data Header z = Header (FuncIdentifier z) (Maybe DataType) [FparDef z]

deriving instance (ForAll Eq z) => Eq (Header z)
deriving instance (ForAll Show z) => Show (Header z)

----------------------------------------------------
----------------------------------------------------

data FparDef z = FparDef (VarIdentifier z) (ParPassType z) 

deriving instance (ForAll Eq z) => Eq (FparDef z)
deriving instance (ForAll Show z) => Show (FparDef z)

----------------------------------------------------
----------------------------------------------------

data DataType = Integ | Byte 
    deriving (Ord, Show, Eq)

----------------------------------------------------
----------------------------------------------------

data ObjectType = DType DataType | AType (ObjectType) Integer
    deriving (Ord, Show, Eq)

----------------------------------------------------
----------------------------------------------------

data TypeX = OType ObjectType | PType ObjectType
    deriving (Ord, Show, Eq)


type Type z = XRec z (TypeX)

----------------------------------------------------
----------------------------------------------------

data ParPassType z = ByRef (Type z)
                   | ByDefault (Type z)

deriving instance (ForAll Eq z) => Eq (ParPassType z)
deriving instance (ForAll Show z) => Show (ParPassType z)

----------------------------------------------------
----------------------------------------------------

data LocalDefX z = LocalDefFuncDef (FuncDef z)
                 | LocalDefFuncDecl (FuncDecl z)
                 | LocalDefVarDef (VarDef z)

deriving instance (ForAll Eq z) => Eq (LocalDefX z)
deriving instance (ForAll Show z) => Show (LocalDefX z)

type LocalDef z = XRec z (LocalDefX z)

----------------------------------------------------
----------------------------------------------------

data FuncDeclX z = FuncDecl (Header z)

deriving instance (ForAll Eq z) => Eq (FuncDeclX z)
deriving instance (ForAll Show z) => Show (FuncDeclX z)

type FuncDecl z = XRec z (FuncDeclX z)

----------------------------------------------------
----------------------------------------------------

data VarDefX z = VarDef (VarIdentifier z) ObjectType

deriving instance (ForAll Eq z) => Eq (VarDefX z)
deriving instance (ForAll Show z) => Show (VarDefX z)

type VarDef z = XRec z (VarDefX z)

----------------------------------------------------
----------------------------------------------------

data StmtX z = StmtSkip
            | StmtAssign (Lvalue z) (Expr z)
            | StmtProcCall (FuncCall z)
            | StmtExit
            | StmtReturn (Expr z)
            | StmtIf (Cond z, Block z) [(Cond z, Block z)] (Maybe (Block z))
            | StmtLoop (Maybe (LabelIdentifier z)) (Block z)
            | StmtBreak (Maybe (LabelIdentifier z))
            | StmtContinue (Maybe (LabelIdentifier z))

deriving instance (ForAll Eq z) => Eq (StmtX z)
deriving instance (ForAll Show z) => Show (StmtX z)

type Stmt z = XRec z (StmtX z)

----------------------------------------------------
----------------------------------------------------

data Block z = Block [Stmt z]

deriving instance (ForAll Eq z) => Eq (Block z)
deriving instance (ForAll Show z) => Show (Block z)

----------------------------------------------------
----------------------------------------------------

type family XFuncCall z

data FuncCallX z = FuncCall (XFuncCall z) (FuncIdentifier z) [Expr z]

deriving instance (ForAll Eq z) => Eq (FuncCallX z)
deriving instance (ForAll Show z) => Show (FuncCallX z)

type FuncCall z = XRec z (FuncCallX z)

----------------------------------------------------
----------------------------------------------------

type family XLvalue z

data LvalueX z = LvalueId (XLvalue z) (VarIdentifier z)
               | LvalueStr (XLvalue z) (StringLiteral z)
               | LvalueAx (XLvalue z) (Lvalue z) (Expr z)

deriving instance (ForAll Eq z) => Eq (LvalueX z)
deriving instance (ForAll Show z) => Show (LvalueX z)

type Lvalue z = XRec z (LvalueX z)

-----------------------------------------------------
-----------------------------------------------------

data Sign = Plus | Minus deriving (Show, Eq)

-----------------------------------------------------
-----------------------------------------------------
 
type family XExpr z

data ExprX z = ExprIntConst (XExpr z) (IntConst z)
             | ExprCharConst (XExpr z) (CharConst z)
             | ExprLvalue (XExpr z) (Lvalue z)
             | ExprParen (XExpr z) (Expr z)
             | ExprFuncCall (XExpr z) (FuncCall z)
             | ExprSigned (XExpr z) Sign (Expr z)
             | ExprAdd (XExpr z) (Expr z) (Expr z)
             | ExprSub (XExpr z) (Expr z) (Expr z)
             | ExprMul (XExpr z) (Expr z) (Expr z)
             | ExprDiv (XExpr z) (Expr z) (Expr z)
             | ExprMod (XExpr z) (Expr z) (Expr z)
             | ExprOr  (XExpr z) (Expr z) (Expr z)
             | ExprAnd (XExpr z) (Expr z) (Expr z)
             | ExprTrue (XExpr z)
             | ExprFalse (XExpr z)
             | ExprNot (XExpr z) (Expr z)

deriving instance (ForAll Eq z) => Eq (ExprX z)
deriving instance (ForAll Show z) => Show (ExprX z)

type Expr z = XRec z (ExprX z)

-----------------------------------------------------
-----------------------------------------------------

data CondX z = CondExpr (Expr z)
             | CondParen (Cond z) 
             | CondNot (Cond z)
             | CondOr (Cond z) (Cond z)
             | CondAnd (Cond z) (Cond z)
             | CondEq (Expr z) (Expr z)
             | CondNeq (Expr z) (Expr z)
             | CondLt (Expr z) (Expr z)
             | CondGt (Expr z) (Expr z)
             | CondLe (Expr z) (Expr z)
             | CondGe (Expr z) (Expr z)

deriving instance (ForAll Eq z) => Eq (CondX z)
deriving instance (ForAll Show z) => Show (CondX z)

type Cond z = XRec z (CondX z)

