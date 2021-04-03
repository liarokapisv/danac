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
import qualified Data.ByteString.Char8 as B
import Options.Applicative
import Data.Foldable (traverse_)

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
                                                       else do
                                                           assembly <- withContext $ \context ->
                                                                           withModuleFromAST context m moduleLLVMAssembly
                                                           B.putStr assembly
