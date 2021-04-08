{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Text (Text, pack)
import qualified Data.Text.IO as TIO
import qualified Data.Text.Lazy.IO as TLIO
import Danac.Parser (parse)
import Danac.Renamer (rename, Error(..))
import Danac.TypeChecker (typecheck)
import Danac.Codegen (codegen)
import Danac.Util.SourceSpan
import Text.Megaparsec.Error (errorBundlePretty)
import qualified Text.Megaparsec.Pos as MP
import Text.Pretty.Simple (pPrint)
import LLVM.Module
import LLVM.Context
import LLVM.AST hiding (value)
import LLVM.Analysis
import Options.Applicative
import Data.Foldable (traverse_)
import qualified Data.ByteString.Char8 as B

import System.Directory
import System.Process
import System.Posix.Temp
import Control.Exception (bracket)
import Errata

toErrataHelper :: Text -> SourceSpan -> Errata
toErrataHelper text (SS (MP.SourcePos fp l c1) (MP.SourcePos _ _ c2)) =
    errataSimple (Just text)
        (blockSimple fancyRedStyle fp Nothing
            (MP.unPos l, MP.unPos c1, MP.unPos c2, Nothing)
            Nothing
        )
        Nothing

toErrataHelper' :: Text -> Text -> SourceSpan -> Text -> SourceSpan -> Errata
toErrataHelper' text t1 (SS (MP.SourcePos fp l1 c11) (MP.SourcePos _ _ c12)) t2 (SS (MP.SourcePos _ l2 c21) (MP.SourcePos _ _ c22)) =
    errataSimple (Just text)
        (blockConnected fancyRedStyle fp Nothing
            (MP.unPos l1, MP.unPos c11, MP.unPos c12, Just t1)
            (MP.unPos l2, MP.unPos c21, MP.unPos c22, Just t2)
            Nothing
        )
        Nothing

toErrata :: Error -> Errata
toErrata (UndefinedVariable _ sp) = toErrataHelper "error: undefined variable" sp
toErrata (UndefinedFunction _ sp) = toErrataHelper "error: undefined function" sp
toErrata (UndefinedLabel _ sp) = toErrataHelper "error: undefined label" sp
toErrata (AlreadyDefinedVariable _ sp1 sp2) = toErrataHelper' "error: redefined variable" "redefined here" sp1  "variable originally defined here" sp2
toErrata (AlreadyDefinedFunction _ sp Nothing) = toErrataHelper "error: redefined standard library function" sp
toErrata (AlreadyDefinedFunction _ sp1 (Just sp2)) = toErrataHelper' "error: redefined function" "redefined here" sp1 "function originally defined here" sp2
toErrata (AlreadyDefinedLabel _ sp1 sp2) = toErrataHelper' "error: redefined label" "redefined here" sp1 "label originally defined here" sp2

data OptLevel = O0 | O1 | O2 | O3 | ON

data Options = Options {
    input :: String,
    output :: String,
    dumpParser :: Bool,
    dumpRenamer :: Bool,
    dumpTypeChecker :: Bool,
    dumpCodegen :: Bool,
    dumpASM ::Bool,
    optLevel :: OptLevel
}

options :: Parser Options
options = Options <$> argument str 
                        (metavar "FILE"
                        <> action "file"
                        <> help "Read input from FILE")
                  <*> strOption
                        (long "output"
                        <> short 'o'
                        <> metavar "FILE"
                        <> action "file"
                        <> value "a.out"
                        <> help "Write output to FILE")
                  <*> switch
                        (long "dump-parser-tree"
                        <> short 'p'
                        <> help "Dumps post-parser tree")
                  <*> switch
                        (long "dump-renamer-tree"
                        <> short 'r'
                        <> help "Dumps post-renamer tree")
                  <*> switch
                        (long "dump-typechecker-tree"
                        <> short 't'
                        <> help "Dumps post-typechecker tree")
                  <*> switch
                        (long "dump-codegen-tree"
                        <> short 'c'
                        <> help "Dumps post-codegen llvm-hs-pure tree")
                  <*> switch
                        (long "dump-llvm-asm"
                        <> short 'l'
                        <> help "Dumps post-codegen llvm assembly")
                  <*> option optLevelReader
                        (long "opt-level"
                        <> short 'O'
                        <> metavar "LEVEL"
                        <> value ON
                        <> help "Optimization level")

optLevelReader :: ReadM OptLevel
optLevelReader = maybeReader $ \s ->
    case s of
        "0" -> Just O0
        "1" -> Just O1
        "2" -> Just O2
        "3" -> Just O3
        _  -> Nothing

compile :: LLVM.AST.Module -> FilePath -> OptLevel -> IO ()
compile llvmModule outfile opt =
  bracket (mkdtemp "build") removePathForcibly $ \buildDir ->
    withCurrentDirectory buildDir $ do
      let llvm = "output.ll"
          runtime = "../src/runtime.c"
      withContext $ \context ->
            withModuleFromAST context llvmModule $ \modl -> do
                verify modl
                writeLLVMAssemblyToFile (File llvm) modl
      let optFlag = case opt of
                       O0 -> "-O0"
                       O1 -> "-O1"
                       O2 -> "-O2"
                       O3 -> "-O3"
                       ON -> ""
      callProcess "clang" ["-Wno-override-module", "-lm", llvm, runtime, optFlag, "-flto", "-o", "../" <> outfile]

main :: IO ()
main = do
    opts <- execParser $
             info (options <**> helper)
                  (fullDesc <> header "danac - Dana compiler")
    text <- TIO.readFile (input opts)
    case parse (input opts) text of
        Left err -> putStr $ errorBundlePretty err
        Right pt | dumpParser opts -> pPrint pt
                 | otherwise ->  
                    case rename pt of
                        Left errors -> TLIO.putStr $ prettyErrors text $ fmap toErrata errors
                        Right t | dumpRenamer opts -> pPrint t
                                | otherwise -> 
                                    case typecheck t of
                                        Left errs -> traverse_ print errs
                                        Right c | dumpTypeChecker opts -> pPrint c
                                                | otherwise -> do
                                                       let m = codegen (pack "test") c
                                                       if dumpCodegen opts then pPrint m
                                                       else if dumpASM opts then do
                                                            assembly <- withContext $ \context ->
                                                                  withModuleFromAST context m moduleLLVMAssembly
                                                            B.putStr assembly
                                                       else
                                                            compile m (output opts) (optLevel opts)
