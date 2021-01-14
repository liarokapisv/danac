{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Danac.Renamer.Core.HeaderSpec where

import Danac.Core.Ast
import Danac.Core.XRecMap
import Danac.Renamer.Core (Scope(..), emptyScope, withHeader)
import Data.Map

import Test.Hspec
import Data.Functor.Identity
import Control.Monad.Reader
import Data.Text

data ExampleTag

type instance XRec ExampleTag a = Identity a
type instance IdP ExampleTag = Text
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

instance CanUnwrapXRec ExampleTag where
    unwrap (Identity x) = x

instance CanMapXRec ExampleTag where
    xmap f x = fmap x f

exampleHeader :: Header ExampleTag
exampleHeader = (I $ Header 
                    (I $ FuncIdentifier () "f1") 
                    (Just $ I Integ) 
                    [I $ FparDef 
                        (I $ VarIdentifier () "v1")
                        (I $ ByDefault $ I $ OType $ I $ DType $ I $ Integ),
                     I $ FparDef 
                        (I $ VarIdentifier () "v2")
                        (I $ ByDefault $ I $ OType $ I $ DType $ I $ Integ)
                    ])

header x = withHeader x pure

convertHeader :: MonadReader Scope m => m (Header ExampleTag)
convertHeader = header exampleHeader pure

spec :: Spec
spec = describe "header" $ 
    context "when given example header" $ do
        it "should rename properly without errors" $ do
            runReader (withHeader exampleHeader pure pure) emptyScope `shouldBe` 
                (I $ Header
                    (I $ FuncIdentifier () "f1")
                    (Just $ I Integ)
                    [I $ FparDef
                        (I $ VarIdentifier () "f1.v1")
                        (I $ ByDefault $ I $ OType $ I $ DType $ I $ Integ),
                     I $ FparDef
                        (I $ VarIdentifier () "f1.v2")
                        (I $ ByDefault $ I $ OType $ I $ DType $ I $ Integ)]
                )
        it "should be able to read parameters in inner continuation" $ do
            let action = (withHeader exampleHeader 
                                     (\_ -> do vs <- reader variables
                                               pure $ vs)
                                     pure)
            runReader action emptyScope `shouldBe` Data.Map.fromList[ ("v1", "f1.v1"), ("v2", "f1.v2")]
        it "should not be able to read parameters in outer continuation" $ do
            let action = (withHeader exampleHeader pure
                                     (\_ -> do vs <- reader variables
                                               pure $ vs))
            runReader action emptyScope `shouldBe` Data.Map.empty
