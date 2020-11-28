{-# LANGUAGE DataKinds, TypeOperators, FlexibleContexts, OverloadedLabels, ScopedTypeVariables, UndecidableInstances, TypeFamilies #-}

module Danac.Parser.Ast where
    
import Danac.Core.Ast 
import Text.Megaparsec.Pos

data ParserAst

data SourceSpan = SS
    { ssBegin :: !SourcePos
    , ssEnd :: !SourcePos
    } deriving Eq

instance Show SourceSpan where
    show _ = "SS"

pspan (SS (SourcePos _ line1 col1)  (SourcePos _ line2 col2)) = 
    "(" ++ show (unPos line1) ++ "," ++ show (unPos col1) 
        ++"),(" 
            ++ show (unPos line2) ++ "," ++ show (unPos col2) ++ ")"

type instance XVarIdentifier ParserAst = SourceSpan
type instance XFuncIdentifier ParserAst = SourceSpan
type instance XLabelIdentifier ParserAst = SourceSpan
type instance XCharConst ParserAst = SourceSpan
type instance XStringLiteral ParserAst = SourceSpan
type instance XIntConst ParserAst = SourceSpan
type instance XAst ParserAst = SourceSpan
type instance XFuncDef ParserAst = SourceSpan
type instance XHeader ParserAst = SourceSpan
type instance XFparDef ParserAst = SourceSpan
type instance XType ParserAst = SourceSpan
type instance XParPassType ParserAst = SourceSpan
type instance XLocalDef ParserAst = SourceSpan
type instance XFuncDecl ParserAst = SourceSpan
type instance XVarDef ParserAst = SourceSpan
type instance XStmt ParserAst = SourceSpan
type instance XBlock ParserAst = SourceSpan
type instance XFuncCall ParserAst = SourceSpan
type instance XLvalue ParserAst = SourceSpan
type instance XExpr ParserAst = SourceSpan
type instance XCond ParserAst = SourceSpan


