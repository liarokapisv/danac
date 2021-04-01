{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.Environment (getArgs)
import qualified Data.Text.IO as TIO
import Danac.Parser (ast)
import Danac.Renamer (rename, emptyContext)
import Danac.TypeChecker (typecheck)
import Danac.Codegen (codegen)
import Text.Megaparsec (parse)
import Text.Pretty.Simple (pPrint)
import Data.Validation
import Control.Monad.Reader 
import Data.Functor.Compose
import LLVM.Module
import LLVM.Context
import qualified Data.ByteString.Char8 as B

main :: IO ()
main = do
    (file:_) <- getArgs
    text <- TIO.readFile file
    let tree = parse ast "" text
    case tree of
        Left err -> pPrint err
        Right pt -> case runReader (getCompose $ rename pt) emptyContext of
                        Failure errors -> pPrint errors
                        Success t -> case typecheck t of
                                        Right c -> do
                                            assembly <- withContext $ \context -> 
                                                            withModuleFromAST context (codegen "test" c) moduleLLVMAssembly
                                            B.putStr assembly
                                        Left errs -> pPrint errs
