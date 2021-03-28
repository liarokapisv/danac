{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import System.Environment (getArgs)
import qualified Data.Text.IO as TIO
import Danac.Parser (ast)
import Danac.Renamer (rename, emptyContext)
import Danac.TypeChecker (typecheck)
import Text.Megaparsec (parse)
import Text.Pretty.Simple (pPrint)
import Data.Validation
import Control.Monad.Reader 
import Data.Functor.Compose

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
                                        [] -> pPrint t
                                        errs -> pPrint errs
