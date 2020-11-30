{-# LANGUAGE OverloadedStrings #-}

module Danac.Parser.Core.LabelIdentifierSpec where

import Test.Danac.Parser.Identifiers

import Danac.Parser.Ast
import Danac.Parser.Core (labelIdentifier)

import Test.Hspec

spec :: Spec
spec = describe "labelIdentifier" $ identifierSpec labelIdentifier LabelIdentifierPS
