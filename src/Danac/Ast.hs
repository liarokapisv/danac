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
{-# LANGUAGE FlexibleContexts      #-}

module Danac.Ast where

import Data.Text (Text)
import Data.Comp.Multi.Derive
import Danac.Util.Derive

data Ast 
data FuncDef 
data Header 
data FparDef 
data DataType 
data ObjectType 
data Type 
data ParPassType 
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


data T r i where
    Ast :: r FuncDef -> T r Ast

    FuncDef :: r Header -> [r LocalDef] -> r Block -> T r FuncDef

    Header :: Text -> Maybe (r DataType) -> [r FparDef] -> T r Header

    FparDef :: Text -> r ParPassType -> T r FparDef

    Integ :: T r DataType
    Byte :: T r DataType

    DType :: r DataType -> T r ObjectType
    AType :: r ObjectType -> Integer -> T r ObjectType

    OType :: r ObjectType -> T r Type
    PType :: r ObjectType -> T r Type

    ByRef :: r Type -> T r ParPassType
    ByDefault :: r Type -> T r ParPassType

    LocalDefFuncDef :: r FuncDef -> T r LocalDef
    LocalDefFuncDecl :: r FuncDecl -> T r LocalDef
    LocalDefVarDef :: r VarDef -> T r LocalDef

    FuncDecl :: r Header -> T r FuncDecl

    VarDef :: Text -> r ObjectType -> T r VarDef

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

    FuncCall :: Text -> [r Expr] -> T r FuncCall
    
    LvalueId :: Text -> T r Lvalue
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
