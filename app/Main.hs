{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Text.IO as TIO
import qualified Data.Text.Lazy.IO as TLIO
import Danac.Parser (parse)
import qualified Danac.Renamer as RN
import qualified Danac.TypeChecker as TC
import Danac.Codegen (codegen)
import Danac.PrettyErrors (prettyRNErrors, prettyTCErrors)
import Text.Megaparsec.Error (errorBundlePretty)
import Text.Pretty.Simple (pPrint)
import LLVM.Module
import LLVM.Context
import LLVM.AST hiding (value)
import LLVM.Analysis
import Options.Applicative
import qualified Data.ByteString.Char8 as B

import System.Directory
import System.Process
import System.Posix.Temp
import System.Exit (exitFailure)
import Control.Exception (bracket)

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
        Left err -> do putStr $ errorBundlePretty err
                       exitFailure
        Right pt | dumpParser opts -> pPrint pt
                 | otherwise ->  
                    case RN.rename pt of
                        Left errors -> do TLIO.putStr $ prettyRNErrors text errors
                                          exitFailure
                        Right t | dumpRenamer opts -> pPrint t
                                | otherwise -> 
                                    case TC.typecheck t of
                                        Left errors -> do TLIO.putStr $ prettyTCErrors text errors
                                                          exitFailure
                                        Right c | dumpTypeChecker opts -> pPrint c
                                                | otherwise -> do
                                                       let m = codegen "test" c
                                                       if dumpCodegen opts then pPrint m
                                                       else if dumpASM opts then do
                                                            assembly <- withContext $ \context ->
                                                                  withModuleFromAST context m moduleLLVMAssembly
                                                            B.putStr assembly
                                                       else
                                                            compile m (output opts) (optLevel opts)
