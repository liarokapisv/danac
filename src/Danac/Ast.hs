{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE PatternSynonyms      #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE DeriveFunctor        #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE ViewPatterns         #-}

module Danac.Ast where

import Data.Text (Text)
import Data.Comp.Multi.Derive
import Danac.Util.Derive

data Ast 
data FuncDef 
data Header 
data FparDef 
data FuncIdentifier
data VarIdentifier
data LocalDef
data FuncDecl
data VarDef
data CondStmt
data Stmt
data Block
data FuncCall
data Lvalue
data Expr
data Cond

data DataType = Integ | Byte
    deriving Show
data Type = DType DataType | AType Type Integer
    deriving Show
data FancyType = Value Type | Ref Type | Pointer Type
    deriving Show

data T r i where
    Ast :: r FuncDef -> T r Ast

    FuncDef :: r Header -> [r LocalDef] -> r Block -> T r FuncDef

    Header :: Text -> Maybe DataType -> [r FparDef] -> T r Header

    FparDef :: Text -> FancyType -> T r FparDef

    LocalDefFuncDef :: r FuncDef -> T r LocalDef
    LocalDefFuncDecl :: r FuncDecl -> T r LocalDef
    LocalDefVarDef :: r VarDef -> T r LocalDef

    FuncDecl :: r Header -> T r FuncDecl

    VarDef :: Text -> Type -> T r VarDef

    CondStmt :: r Cond -> r Block -> T r CondStmt

    StmtSkip :: T r Stmt 
    StmtAssign :: r Lvalue -> r Expr -> T r Stmt
    StmtProcCall :: r FuncCall -> T r Stmt
    StmtExit :: T r Stmt
    StmtReturn :: r Expr -> T r Stmt

    StmtIf :: r CondStmt -> [r CondStmt] -> Maybe (r Block) -> T r Stmt
    StmtLoop :: Maybe Text -> r Block -> T r Stmt
    StmtBreak :: Maybe Text -> T r Stmt
    StmtContinue :: Maybe Text -> T r Stmt

    Block :: [r Stmt] -> T r Block

    FuncIdentifier :: Text -> T r FuncIdentifier

    FuncCall :: r FuncIdentifier -> [r Expr] -> T r FuncCall

    VarIdentifier :: Text -> T r VarIdentifier
    
    LvalueId :: r VarIdentifier -> T r Lvalue
    LvalueStr :: Text -> T r Lvalue
    LvalueAx :: r Lvalue -> r Expr -> T r Lvalue
    ExprInt :: Integer -> T r Expr
    ExprChar :: Char -> T r Expr
    ExprLvalue :: r Lvalue -> T r Expr
    ExprFuncCall :: r FuncCall -> T r Expr
    ExprMinus :: r Expr -> T r Expr
    ExprAdd :: r Expr -> r Expr -> T r Expr
    ExprSub :: r Expr -> r Expr -> T r Expr
    ExprMul :: r Expr -> r Expr -> T r Expr
    ExprDiv :: r Expr -> r Expr -> T r Expr
    ExprMod :: r Expr -> r Expr -> T r Expr
    ExprOr :: r Expr -> r Expr -> T r Expr
    ExprAnd :: r Expr -> r Expr -> T r Expr
    ExprTrue :: T r Expr
    ExprFalse :: T r Expr
    ExprNot :: r Expr -> T r Expr

    CondExpr :: r Expr -> T r Cond
    CondNot :: r Cond -> T r Cond
    CondOr :: r Cond -> r Cond -> T r Cond
    CondAnd :: r Cond -> r Cond -> T r Cond
    CondEq :: r Expr -> r Expr -> T r Cond
    CondNe :: r Expr -> r Expr -> T r Cond
    CondLt :: r Expr -> r Expr -> T r Cond
    CondGt :: r Expr -> r Expr -> T r Cond
    CondLe :: r Expr -> r Expr -> T r Cond
    CondGe :: r Expr -> r Expr -> T r Cond

$(makeHFunctor ''T)
$(makeHFoldable ''T)
$(makeHTraversable ''T)
$(makeShowHF' ''T)

data Group r i where
    GroupAst :: T r Ast -> Group r Ast 
    GroupFuncDef :: T r FuncDef -> Group r FuncDef
    GroupHeader :: T r Header -> Group r Header
    GroupFparDef :: T r FparDef -> Group r FparDef
    GroupVarIdentifier :: T r VarIdentifier -> Group r VarIdentifier
    GroupLocalDef :: T r LocalDef -> Group r LocalDef
    GroupFuncDecl :: T r FuncDecl -> Group r FuncDecl
    GroupVarDef :: T r VarDef -> Group r VarDef
    GroupCondStmt :: T r CondStmt -> Group r CondStmt
    GroupStmt :: T r Stmt -> Group r Stmt
    GroupBlock :: T r Block -> Group r Block
    GroupFuncIdentifier :: T r FuncIdentifier -> Group r FuncIdentifier
    GroupFuncCall :: T r FuncCall -> Group r FuncCall
    GroupLvalue :: T r Lvalue -> Group r Lvalue
    GroupExpr :: T r Expr -> Group r Expr
    GroupCond :: T r Cond -> Group r Cond

group :: T r i -> Group r i
group x = case x of
    (Ast _ ) -> GroupAst x
    (FuncDef _ _ _) -> GroupFuncDef x
    (Header _ _ _) -> GroupHeader x
    (FparDef _ _) -> GroupFparDef x
    (LocalDefFuncDef _) -> GroupLocalDef x
    (LocalDefFuncDecl _) -> GroupLocalDef x
    (LocalDefVarDef _) -> GroupLocalDef x
    (FuncDecl _) -> GroupFuncDecl x
    (VarDef _ _) -> GroupVarDef x
    (CondStmt _ _) -> GroupCondStmt x
    (StmtSkip) -> GroupStmt  x
    (StmtAssign _ _) -> GroupStmt x
    (StmtProcCall _) -> GroupStmt x
    (StmtExit) -> GroupStmt x
    (StmtReturn _) -> GroupStmt x
    (StmtIf _ _ _) -> GroupStmt x
    (StmtLoop _ _) -> GroupStmt x
    (StmtBreak _) -> GroupStmt x
    (StmtContinue _) -> GroupStmt x
    (Block _) -> GroupBlock x
    (FuncIdentifier _) -> GroupFuncIdentifier x
    (FuncCall _ _) -> GroupFuncCall x
    (VarIdentifier _) -> GroupVarIdentifier x
    (LvalueId _) -> GroupLvalue x
    (LvalueStr _) -> GroupLvalue x
    (LvalueAx _ _) -> GroupLvalue x
    (ExprInt _) -> GroupExpr x
    (ExprChar _) -> GroupExpr x
    (ExprLvalue _) -> GroupExpr x
    (ExprFuncCall _) -> GroupExpr x
    (ExprMinus _) -> GroupExpr x
    (ExprAdd _ _) -> GroupExpr x
    (ExprSub _ _) -> GroupExpr x
    (ExprMul _ _) -> GroupExpr x
    (ExprDiv _ _) -> GroupExpr x
    (ExprMod _ _) -> GroupExpr x
    (ExprOr _ _) -> GroupExpr x
    (ExprAnd _ _) -> GroupExpr x
    (ExprTrue) -> GroupExpr x
    (ExprFalse) -> GroupExpr x
    (ExprNot _) -> GroupExpr x
    (CondExpr _) -> GroupCond x
    (CondNot _) -> GroupCond x
    (CondOr _ _) -> GroupCond x
    (CondAnd _ _) -> GroupCond x
    (CondEq _ _) -> GroupCond x
    (CondNe _ _) -> GroupCond x
    (CondLt _ _) -> GroupCond x
    (CondGt _ _) -> GroupCond x
    (CondLe _ _) -> GroupCond x
    (CondGe _ _) -> GroupCond x
