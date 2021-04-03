{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Main where

import Data.Text (pack)
import qualified Data.Text.IO as TIO
import Danac.Parser (parse)
import Danac.Renamer (rename)
import Danac.TypeChecker (typecheck)
import Danac.Codegen (codegen)
import Text.Megaparsec.Error (errorBundlePretty)
import Text.Pretty.Simple (pPrint)
import LLVM.Module
import LLVM.Context
import LLVM.AST
import Options.Applicative
import Data.Foldable (traverse_)
import qualified Data.ByteString.Char8()

import System.IO
import System.Directory
import System.Process
import System.Posix.Temp
import Control.Exception (bracket)

data Options = Options {
    file :: String,
    dumpParser :: Bool,
    dumpRenamer :: Bool,
    dumpTypeChecker :: Bool,
    dumpCodegen :: Bool
}

options :: Parser Options
options = Options <$> argument str 
                        (metavar "FILE"
                        <> help "File to compile")
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
                        <> help "Dumps post-codegen tree")

compile :: LLVM.AST.Module -> FilePath -> IO ()
compile llvmModule outfile =
  bracket (mkdtemp "build") removePathForcibly $ \buildDir ->
    withCurrentDirectory buildDir $ do
      (llvm, llvmHandle) <- mkstemps "output" ".ll"
      let runtime = "../src/runtime.c"
      withContext $ \context ->
                      withModuleFromAST context llvmModule (writeLLVMAssemblyToFile (File llvm))
      hClose llvmHandle
      callProcess "clang" ["-Wno-override-module", "-lm", llvm, runtime, "-o", "../" <> outfile]

main :: IO ()
main = do
    opts <- execParser $
             info (options <**> helper)
                  (fullDesc <> header "danac - Dana compiler")
    text <- TIO.readFile (file opts)
    case parse (file opts) text of
        Left err -> putStr $ errorBundlePretty err
        Right pt | dumpParser opts -> pPrint pt
                 | otherwise ->  
                    case rename pt of
                        Left errors -> traverse_ print errors
                        Right t | dumpRenamer opts -> pPrint t
                                | otherwise -> 
                                    case typecheck t of
                                        Left errs -> traverse_ print errs
                                        Right c | dumpTypeChecker opts -> pPrint c
                                                | otherwise -> do
                                                       let m = codegen (pack "test") c
                                                       if dumpCodegen opts
                                                           then pPrint m
                                                       else compile m "a.out"
