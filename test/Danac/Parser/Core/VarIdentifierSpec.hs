{-# LANGUAGE OverloadedStrings #-}

module Danac.Parser.Core.VarIdentifierSpec where

import Test.Danac.Parser.Identifiers

import Danac.Parser.Ast
import Danac.Parser.Core (varIdentifier)

import Test.Hspec

spec :: Spec
spec = describe "varIdentifier" $ identifierSpec varIdentifier VarIdentifierPS
