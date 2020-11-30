{-# LANGUAGE TypeFamilies #-}

module Danac.Core.AstSpec where

import Danac.Core.Ast
import Test.Hspec
import Text.Pretty.Simple

data ExampleTag

type instance XRec ExampleTag a = a
type instance IdP ExampleTag = ()
type instance XLabelIdentifier ExampleTag = ()
type instance XFuncIdentifier ExampleTag = ()
type instance XVarIdentifier ExampleTag = ()
type instance CharP ExampleTag = ()
type instance XCharConst ExampleTag = ()
type instance StringP ExampleTag = ()
type instance XStringLiteral ExampleTag = ()
type instance IntP ExampleTag = ()
type instance XIntConst ExampleTag = ()
type instance XLvalue ExampleTag = ()
type instance XExpr ExampleTag = ()
type instance XFuncCall ExampleTag = ()

exampleAst :: Ast ExampleTag
exampleAst = 
    Ast $
        FuncDef
            (Header 
                (FuncIdentifier () ()) 
                (Just Integ) 
                [FparDef 
                    (VarIdentifier () ())
                    (ByDefault $ OType $ DType Integ)
                ])
            []
            (Block [])

spec :: Spec
spec = pure ()
