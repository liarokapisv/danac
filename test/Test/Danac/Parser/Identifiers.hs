
{-# LANGUAGE OverloadedStrings #-}

module Test.Danac.Parser.Identifiers (identifierSpec) where

import Danac.Core.Ast
import Danac.Parser.Ast
import Danac.Parser.Core (Parser, labelIdentifier, varIdentifier, funcIdentifier)

import Data.Text
import Text.Megaparsec

import Test.Hspec
import Test.Hspec.Megaparsec

ignoringExtra :: Either (ParseErrorBundle s e) (a, SourceSpan) -> Either (ParseErrorBundle s e) a
ignoringExtra = fmap fst

failOnKeyword :: Show a => Parser a -> Text -> Expectation
failOnKeyword p keyword = parse p "" keyword `shouldFailWith` errFancy 0 (fancy $ ErrorFail ("keyword \"" ++ unpack keyword ++ "\" cannot be an identifier"))

identifierSpec :: (Eq a, Show a) => Parser (a, SourceSpan) -> (Text -> a) -> Spec
identifierSpec p wrap = do
     context "when given full identifier" $ do
        it "should parse correctly" $ do
            ignoringExtra (parse p "" "example_identifier_1_23test") `shouldParse` wrap "example_identifier_1_23test"
     context "when given def keyword" $ do
        it "should fail" $ failOnKeyword p "def"
     context "when given decl keyword" $ do
        it "should fail" $ failOnKeyword p "decl"
     context "when given if keyword" $ do
        it "should fail" $ failOnKeyword p "if"
     context "when given elif keyword" $ do
        it "should fail" $ failOnKeyword p "elif"
     context "when given else keyword" $ do
        it "should fail" $ failOnKeyword p "else"
     context "when given var keyword" $ do
        it "should fail" $ failOnKeyword p "var"
     context "when given ref keyword" $ do
        it "should fail" $ failOnKeyword p "ref"
     context "when given as keyword" $ do
        it "should fail" $ failOnKeyword p "as"
     context "when given is keyword" $ do
        it "should fail" $ failOnKeyword p "is"
     context "when given true keyword" $ do
        it "should fail" $ failOnKeyword p "true"
     context "when given false keyword" $ do
        it "should fail" $ failOnKeyword p "false"
     context "when given break keyword" $ do
        it "should fail" $ failOnKeyword p "break"
     context "when given return keyword" $ do
        it "should fail" $ failOnKeyword p "return"
     context "when given exit keyword" $ do
        it "should fail" $ failOnKeyword p "exit"
     context "when given loop keyword" $ do
        it "should fail" $ failOnKeyword p "loop"
     context "when given skip keyword" $ do
        it "should fail" $ failOnKeyword p "skip"
     context "when given continue keyword" $ do
        it "should fail" $ failOnKeyword p "continue"
     context "when given int keyword" $ do
        it "should fail" $ failOnKeyword p "int"
     context "when given byte keyword" $ do
        it "should fail" $ failOnKeyword p "byte"
     context "when given not keyword" $ do
        it "should fail" $ failOnKeyword p "not"
     context "when given and keyword" $ do
        it "should fail" $ failOnKeyword p "and"
     context "when given or keyword" $ do
        it "should fail" $ failOnKeyword p "or"
     context "when given begin keyword" $ do
        it "should fail" $ failOnKeyword p "begin"
     context "when given end keyword" $ do
        it "should fail" $ failOnKeyword p "end"

