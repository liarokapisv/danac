{-# LANGUAGE 
    ExplicitForAll, 
    DataKinds, 
    OverloadedLabels, 
    ScopedTypeVariables, 
    TypeOperators, 
    ConstraintKinds, 
    UndecidableInstances, 
    TypeFamilies, 
    TypeFamilyDependencies, 
    StandaloneDeriving, 
    PatternSynonyms, 
    FlexibleContexts 
#-}

module Danac.Core.Ast where

import GHC.Exts (Constraint)

----------------------------------------------------
----------------------------------------------------

type ForAll (f :: * -> Constraint) p = 
    (
     f (IdP p)
    ,f (IntP p)
    ,f (CharP p)
    ,f (StringP p)
    ,f (XLabelIdentifier p)
    ,f (XFuncIdentifier p)
    ,f (XVarIdentifier p)
    ,f (XCharConst p)
    ,f (XStringLiteral p)
    ,f (XIntConst p)
    ,f (XFuncCall p)
    ,f (XLvalue p)
    ,f (XExpr p)
    ,f (XRec p (LabelIdentifierX p))
    ,f (XRec p (FuncIdentifierX p))
    ,f (XRec p (VarIdentifierX p))
    ,f (XRec p (CharConstX p))
    ,f (XRec p (StringLiteralX p))
    ,f (XRec p (IntConstX p))
    ,f (XRec p (AstX p))
    ,f (XRec p (FuncDefX p))
    ,f (XRec p (HeaderX p))
    ,f (XRec p (FparDefX p))
    ,f (XRec p (DataTypeX p))
    ,f (XRec p (ObjectTypeX p))
    ,f (XRec p (TypeX p))
    ,f (XRec p (ParPassTypeX p))
    ,f (XRec p (LocalDefX p))
    ,f (XRec p (FuncDeclX p))
    ,f (XRec p (VarDefX p))
    ,f (XRec p (StmtX p))
    ,f (XRec p (BlockX p))
    ,f (XRec p (FuncCallX p))
    ,f (XRec p (LvalueX p))
    ,f (XRec p (ExprX p))
    ,f (XRec p (CondX p))
    )

    

----------------------------------------------------
----------------------------------------------------

type family XRec p a = r | r -> a

----------------------------------------------------
----------------------------------------------------

type family IdP p

----------------------------------------------------
----------------------------------------------------

type family XLabelIdentifier p

data LabelIdentifierX p = LabelIdentifier (XLabelIdentifier p) (IdP p)

deriving instance (ForAll Show p) => Show (LabelIdentifierX p)
deriving instance (ForAll Eq p) => Eq (LabelIdentifierX p)

type LabelIdentifier p = XRec p (LabelIdentifierX p)


----------------------------------------------------
----------------------------------------------------

type family XFuncIdentifier p

data FuncIdentifierX p = FuncIdentifier (XFuncIdentifier p) (IdP p)

deriving instance (ForAll Show p) => Show (FuncIdentifierX p)
deriving instance (ForAll Eq p) => Eq (FuncIdentifierX p)

type FuncIdentifier p = XRec p (FuncIdentifierX p)

----------------------------------------------------
----------------------------------------------------

type family XVarIdentifier p

data VarIdentifierX p = VarIdentifier (XVarIdentifier p) (IdP p)

deriving instance (ForAll Show p) => Show (VarIdentifierX p)
deriving instance (ForAll Eq p) => Eq (VarIdentifierX p)

type VarIdentifier p = XRec p (VarIdentifierX p)

---------------------------------------------------- 
----------------------------------------------------

type family CharP p

---------------------------------------------------- 
----------------------------------------------------

type family XCharConst p

data CharConstX p = CharConst (XCharConst p) (CharP p)

deriving instance (ForAll Show p) => Show (CharConstX p)
deriving instance (ForAll Eq p) => Eq (CharConstX p)

type CharConst p = XRec p (CharConstX p)

---------------------------------------------------- 
----------------------------------------------------

type family StringP p

----------------------------------------------------
----------------------------------------------------

type family XStringLiteral p

data StringLiteralX p = StringLiteral (XStringLiteral p) (StringP p)

deriving instance (ForAll Show p) => Show (StringLiteralX p)
deriving instance (ForAll Eq p) => Eq (StringLiteralX p)

type StringLiteral p = XRec p (StringLiteralX p)

----------------------------------------------------
----------------------------------------------------

type family IntP p

----------------------------------------------------
----------------------------------------------------

type family XIntConst p

data IntConstX p = IntConst (XIntConst p) (IntP p)

deriving instance (ForAll Show p) => Show (IntConstX p)
deriving instance (ForAll Eq p) => Eq (IntConstX p)

type IntConst p = XRec p (IntConstX p)

----------------------------------------------------
----------------------------------------------------

data AstX p = Ast (FuncDef p)

deriving instance (ForAll Eq p) => Eq (AstX p)
deriving instance (ForAll Show p) => Show (AstX p)

type Ast p = XRec p (AstX p)

----------------------------------------------------
----------------------------------------------------

data FuncDefX p = FuncDef (Header p) [LocalDef p] (Block p) 

deriving instance (ForAll Eq p) => Eq (FuncDefX p)
deriving instance (ForAll Show p) => Show (FuncDefX p)

type FuncDef p = XRec p (FuncDefX p)

----------------------------------------------------
----------------------------------------------------

data HeaderX p = Header (FuncIdentifier p) (Maybe (DataType p)) [FparDef p]

deriving instance (ForAll Eq p) => Eq (HeaderX p)
deriving instance (ForAll Show p) => Show (HeaderX p)

type Header p = XRec p (HeaderX p)

----------------------------------------------------
----------------------------------------------------

data FparDefX p = FparDef (VarIdentifier p) (ParPassType p) 

deriving instance (ForAll Eq p) => Eq (FparDefX p)
deriving instance (ForAll Show p) => Show (FparDefX p)

type FparDef p = XRec p (FparDefX p)

----------------------------------------------------
----------------------------------------------------

data DataTypeX p = Integ | Byte 
    deriving (Ord, Show, Eq)

type DataType p = XRec p (DataTypeX p)

----------------------------------------------------
----------------------------------------------------

data ObjectTypeX p = DType (DataType p) | AType (ObjectType p) (IntConst p)

deriving instance (ForAll Eq p) => Eq (ObjectTypeX p)
deriving instance (ForAll Show p) => Show (ObjectTypeX p)

type ObjectType p = XRec p (ObjectTypeX p)

----------------------------------------------------
----------------------------------------------------

data TypeX p = OType (ObjectType p) | PType (ObjectType p)

deriving instance (ForAll Eq p) => Eq (TypeX p)
deriving instance (ForAll Show p) => Show (TypeX p)

type Type p = XRec p (TypeX p)

----------------------------------------------------
----------------------------------------------------

data ParPassTypeX p = ByRef (Type p)
                    | ByDefault (Type p)

deriving instance (ForAll Eq p) => Eq (ParPassTypeX p)
deriving instance (ForAll Show p) => Show (ParPassTypeX p)

type ParPassType p = XRec p (ParPassTypeX p)

----------------------------------------------------
----------------------------------------------------

data LocalDefX p = LocalDefFuncDef (FuncDef p)
                 | LocalDefFuncDecl (FuncDecl p)
                 | LocalDefVarDef (VarDef p)

deriving instance (ForAll Eq p) => Eq (LocalDefX p)
deriving instance (ForAll Show p) => Show (LocalDefX p)

type LocalDef p = XRec p (LocalDefX p)

----------------------------------------------------
----------------------------------------------------

data FuncDeclX p = FuncDecl (Header p)

deriving instance (ForAll Eq p) => Eq (FuncDeclX p)
deriving instance (ForAll Show p) => Show (FuncDeclX p)

type FuncDecl p = XRec p (FuncDeclX p)

----------------------------------------------------
----------------------------------------------------

data VarDefX p = VarDef (VarIdentifier p) (ObjectType p)

deriving instance (ForAll Eq p) => Eq (VarDefX p)
deriving instance (ForAll Show p) => Show (VarDefX p)

type VarDef p = XRec p (VarDefX p)

----------------------------------------------------
----------------------------------------------------

data StmtX p = StmtSkip
            | StmtAssign (Lvalue p) (Expr p)
            | StmtProcCall (FuncCall p)
            | StmtExit
            | StmtReturn (Expr p)
            | StmtIf (Cond p, Block p) [(Cond p, Block p)] (Maybe (Block p))
            | StmtLoop (Maybe (LabelIdentifier p)) (Block p)
            | StmtBreak (Maybe (LabelIdentifier p))
            | StmtContinue (Maybe (LabelIdentifier p))

deriving instance (ForAll Eq p) => Eq (StmtX p)
deriving instance (ForAll Show p) => Show (StmtX p)

type Stmt p = XRec p (StmtX p)

----------------------------------------------------
----------------------------------------------------

data BlockX p = Block [Stmt p]

deriving instance (ForAll Eq p) => Eq (BlockX p)
deriving instance (ForAll Show p) => Show (BlockX p)

type Block p = XRec p (BlockX p)

----------------------------------------------------
----------------------------------------------------

type family XFuncCall p

data FuncCallX p = FuncCall (XFuncCall p) (FuncIdentifier p) [Expr p]

deriving instance (ForAll Eq p) => Eq (FuncCallX p)
deriving instance (ForAll Show p) => Show (FuncCallX p)

type FuncCall p = XRec p (FuncCallX p)

----------------------------------------------------
----------------------------------------------------

type family XLvalue p

data LvalueX p = LvalueId !(XLvalue p) (VarIdentifier p)
               | LvalueStr !(XLvalue p) (StringLiteral p)
               | LvalueAx !(XLvalue p) (Lvalue p) (Expr p)

deriving instance (ForAll Eq p) => Eq (LvalueX p)
deriving instance (ForAll Show p) => Show (LvalueX p)

type Lvalue p = XRec p (LvalueX p)

-----------------------------------------------------
-----------------------------------------------------

data Sign = Plus | Minus deriving (Show, Eq)

-----------------------------------------------------
-----------------------------------------------------
 
type family XExpr p

data ExprX p = ExprIntConst (XExpr p) (IntConst p)
             | ExprCharConst (XExpr p) (CharConst p)
             | ExprLvalue (XExpr p) (Lvalue p)
             | ExprParen (XExpr p) (Expr p)
             | ExprFuncCall (XExpr p) (FuncCall p)
             | ExprSigned (XExpr p) Sign (Expr p)
             | ExprAdd (XExpr p) (Expr p) (Expr p)
             | ExprSub (XExpr p) (Expr p) (Expr p)
             | ExprMul (XExpr p) (Expr p) (Expr p)
             | ExprDiv (XExpr p) (Expr p) (Expr p)
             | ExprMod (XExpr p) (Expr p) (Expr p)
             | ExprOr  (XExpr p) (Expr p) (Expr p)
             | ExprAnd (XExpr p) (Expr p) (Expr p)
             | ExprTrue (XExpr p)
             | ExprFalse (XExpr p)
             | ExprNot (XExpr p) (Expr p)

deriving instance (ForAll Eq p) => Eq (ExprX p)
deriving instance (ForAll Show p) => Show (ExprX p)

type Expr p = XRec p (ExprX p)

-----------------------------------------------------
-----------------------------------------------------

data CondX p = CondExpr (Expr p)
             | CondParen (Cond p) 
             | CondNot (Cond p)
             | CondOr (Cond p) (Cond p)
             | CondAnd (Cond p) (Cond p)
             | CondEq (Expr p) (Expr p)
             | CondNe (Expr p) (Expr p)
             | CondLt (Expr p) (Expr p)
             | CondGt (Expr p) (Expr p)
             | CondLe (Expr p) (Expr p)
             | CondGe (Expr p) (Expr p)

deriving instance (ForAll Eq p) => Eq (CondX p)
deriving instance (ForAll Show p) => Show (CondX p)

type Cond p = XRec p (CondX p)
