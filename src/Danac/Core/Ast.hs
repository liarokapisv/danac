{-# LANGUAGE DataKinds, OverloadedLabels, ScopedTypeVariables, TypeOperators, ConstraintKinds, UndecidableInstances, TypeFamilies, StandaloneDeriving, PatternSynonyms, FlexibleContexts #-}

module Danac.Core.Ast where

import qualified Danac.Parser.Character as Lex
import GHC.Exts (Constraint)
import Data.Text

----------------------------------------------------
----------------------------------------------------

type ForAll (f :: * -> Constraint) z = 
    (f (VarIdentifier z)
    ,f (FuncIdentifier z)
    ,f (LabelIdentifier z)
    ,f (CharConst z)
    ,f (StringLiteral z)
    ,f (IntConst z)
    ,f (Ast z)
    ,f (FuncDef z)
    ,f (Header z)
    ,f (FparDef z)
    ,f (Type z)
    ,f (ParPassType z)
    ,f (LocalDef z)
    ,f (FuncDecl z)
    ,f (VarDef z)
    ,f (Stmt z)
    ,f (Block z)
    ,f (FuncCall z)
    ,f (Lvalue z)
    ,f (Expr z)
    ,f (Cond z))

----------------------------------------------------
----------------------------------------------------

type family XLabelIdentifier z

data LabelIdentifierX = LabelIdentifier Text
    deriving (Eq, Show)

type LabelIdentifier z = (LabelIdentifierX, XLabelIdentifier z)

----------------------------------------------------
----------------------------------------------------

type family XFuncIdentifier z

data FuncIdentifierX = FuncIdentifier Text
    deriving (Eq, Show)

type FuncIdentifier z = (FuncIdentifierX, XFuncIdentifier z)

----------------------------------------------------
----------------------------------------------------

type family XVarIdentifier z

data VarIdentifierX = VarIdentifier Text
    deriving (Eq, Show)

type VarIdentifier z = (VarIdentifierX, XVarIdentifier z) ---------------------------------------------------- ----------------------------------------------------

type family XCharConst z

data CharConstX = CharConst Lex.Character
    deriving (Eq, Show)

type CharConst z = (CharConstX, XCharConst z)

----------------------------------------------------
----------------------------------------------------

type family XStringLiteral z

data StringLiteralX = StringLiteral [Lex.Character]
    deriving (Eq, Show)

type StringLiteral z = (StringLiteralX, XStringLiteral z)

----------------------------------------------------
----------------------------------------------------

type family XIntConst z

data IntConstX = IntConst Integer
    deriving (Eq, Show)

type IntConst z = (IntConstX, XIntConst z)

----------------------------------------------------
----------------------------------------------------

type family XAst z

data AstX z = Ast (FuncDef z)

deriving instance (ForAll Show z) => Show (AstX z)

type Ast z = (AstX z, XAst z)

----------------------------------------------------
----------------------------------------------------

type family XFuncDef z

data FuncDefX z = FuncDef (Header z) [LocalDef z] (Block z) 

deriving instance (ForAll Show z) => Show (FuncDefX z)

type FuncDef z = (FuncDefX z, XFuncDef z)

----------------------------------------------------
----------------------------------------------------

type family XHeader z

data HeaderX z = Header (FuncIdentifier z) (Maybe DataType) [FparDef z]

deriving instance (ForAll Show z) => Show (HeaderX z)

type Header z = (HeaderX z, XHeader z)

----------------------------------------------------
----------------------------------------------------

type family XFparDef z

data FparDefX z = FparDef (VarIdentifier z) (ParPassType z) 

deriving instance (ForAll Show z) => Show (FparDefX z)

type FparDef z = (FparDefX z, XFparDef z)

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

type family XType z

data TypeX z = OType ObjectType | PType ObjectType

deriving instance (ForAll Show z) => Show (TypeX z)

type Type z = (TypeX z, XType z)

----------------------------------------------------
----------------------------------------------------

type family XParPassType z

data ParPassTypeX z = ByRef (Type z)
                   | ByDefault (Type z)

deriving instance (ForAll Show z) => Show (ParPassTypeX z)

type ParPassType z = (ParPassTypeX z, XParPassType z)

----------------------------------------------------
----------------------------------------------------

type family XLocalDef z

data LocalDefX z = LocalDefFuncDef (FuncDef z)
                 | LocalDefFuncDecl (FuncDecl z)
                 | LocalDefVarDef (VarDef z)

deriving instance (ForAll Show z) => Show (LocalDefX z)

type LocalDef z = (LocalDefX z, XLocalDef z)

----------------------------------------------------
----------------------------------------------------

type family XFuncDecl z

data FuncDeclX z = FuncDecl (Header z)

deriving instance (ForAll Show z) => Show (FuncDeclX z)

type FuncDecl z = (FuncDeclX z, XFuncDecl z)

----------------------------------------------------
----------------------------------------------------

type family XVarDef z

data VarDefX z = VarDef (VarIdentifier z) ObjectType

deriving instance (ForAll Show z) => Show (VarDefX z)

type VarDef z = (VarDefX z, XVarDef z)

----------------------------------------------------
----------------------------------------------------

type family XStmt z

data StmtX z = StmtSkip
            | StmtAssign (Lvalue z) (Expr z)
            | StmtProcCall (FuncCall z)
            | StmtExit
            | StmtReturn (Expr z)
            | StmtIf (Cond z, Block z) [(Cond z, Block z)] (Maybe (Block z))
            | StmtLoop (Maybe (LabelIdentifier z)) (Block z)
            | StmtBreak (Maybe (LabelIdentifier z))
            | StmtContinue (Maybe (LabelIdentifier z))

deriving instance (ForAll Show z) => Show (StmtX z)

type Stmt z = (StmtX z, XStmt z)

----------------------------------------------------
----------------------------------------------------

type family XBlock z

data BlockX z = Block [Stmt z]

deriving instance (ForAll Show z) => Show (BlockX z)

type Block z = (BlockX z, XBlock z)

----------------------------------------------------
----------------------------------------------------

type family XFuncCall z

data FuncCallX z = FuncCall (FuncIdentifier z) [Expr z]

deriving instance (ForAll Show z) => Show (FuncCallX z)

type FuncCall z = (FuncCallX z, XFuncCall z)

----------------------------------------------------
----------------------------------------------------

type family XLvalue z

data LvalueX z = LvalueId (VarIdentifier z)
               | LvalueStr (StringLiteral z)
               | LvalueAx (Lvalue z) (Expr z)

deriving instance (ForAll Show z) => Show (LvalueX z)

type Lvalue z = (LvalueX z, XLvalue z)

-----------------------------------------------------
-----------------------------------------------------

data Sign = Plus | Minus deriving (Show, Eq)

-----------------------------------------------------
-----------------------------------------------------
 
type family XExpr z

data ExprX z = ExprIntConst (IntConst z)
             | ExprCharConst (CharConst z)
             | ExprLvalue (Lvalue z)
             | ExprParen (Expr z)
             | ExprFuncCall (FuncCall z)
             | ExprSigned Sign (Expr z)
             | (Expr z) :+ (Expr z)
             | (Expr z) :- (Expr z)
             | (Expr z) :* (Expr z)
             | (Expr z) :/ (Expr z)
             | (Expr z) :% (Expr z)
             | (Expr z) :| (Expr z)
             | (Expr z) :& (Expr z)
             | ExprTrue
             | ExprFalse
             | ExprNot (Expr z)

deriving instance (ForAll Show z) => Show (ExprX z)

type Expr z = (ExprX z, XExpr z)

-----------------------------------------------------
-----------------------------------------------------
  
type family XCond z

data CondX z = CondExpr (Expr z)
             | CondParen (Cond z) 
             | CondNot (Cond z)
             | (Cond z) `Or` (Cond z)
             | (Cond z) `And` (Cond z)
             | (Expr z) :== (Expr z)
             | (Expr z) :<> (Expr z)
             | (Expr z) :< (Expr z)
             | (Expr z) :> (Expr z)
             | (Expr z) :<= (Expr z)
             | (Expr z) :>= (Expr z)

deriving instance (ForAll Show z) => Show (CondX z)

type Cond z = (CondX z, XCond z)
    
