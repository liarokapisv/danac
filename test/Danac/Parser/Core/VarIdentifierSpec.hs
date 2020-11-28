{-# LANGUAGE OverloadedStrings #-}

module Danac.Parser.Core.VarIdentifierSpec where

import Test.Danac.Parser.Identifiers

import Danac.Core.Ast
import Danac.Parser.Core (varIdentifier)

import Test.Hspec

spec :: Spec
spec = describe "varIdentifier" $ identifierSpec varIdentifier VarIdentifier
