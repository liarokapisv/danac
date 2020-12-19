{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PatternSynonyms #-}

module Danac.Core.AstSpec where

import Danac.Core.Ast
import Test.Hspec
import Text.Pretty.Simple
import Data.Functor.Identity

data ExampleTag

type instance XRec ExampleTag a = Identity a
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

pattern I x = Identity x

exampleAst :: Ast ExampleTag
exampleAst = 
    I $ Ast $ 
        I $ FuncDef
            (I $ Header 
                (I $ FuncIdentifier () ()) 
                (Just $ I Integ) 
                [I $ FparDef 
                    (I $ VarIdentifier () ())
                    (I $ ByDefault $ I $ OType $ I $ DType $ I $ Integ)
                ])
            []
            (I $ Block [])

spec :: Spec
spec = pure ()
