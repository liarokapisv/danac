{-# LANGUAGE OverloadedStrings #-}

module Danac.Parser.Core.FuncIdentifierSpec where

import Test.Danac.Parser.Identifiers

import Danac.Parser.Ast
import Danac.Parser.Core (funcIdentifier)

import Test.Hspec

spec :: Spec
spec = describe "funcIdentifier" $ identifierSpec funcIdentifier FuncIdentifierPS
