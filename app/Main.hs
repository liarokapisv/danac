{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import System.Environment (getArgs)
import qualified Data.Text.IO as TIO
import Danac.Ast
import Danac.Util.Annotation
import Data.Comp.Multi.Algebra
import Data.Comp.Multi.Term
import Danac.Parser (ast)
import Danac.Renamer (rename, Ann(..), emptyContext)
import Text.Megaparsec (parse)
import Text.Pretty.Simple (pPrint)
import Validation
import Control.Monad.Reader 
import Data.Functor.Compose

newtype NoSpanAnn i = NoSpanAnn (Ann i)

noSpanAlg :: Alg (T :&&: Ann) (Term (T :&&: NoSpanAnn))
noSpanAlg (x :&&: a) = x :&&.: NoSpanAnn a

noSpan = cata noSpanAlg

instance Show (NoSpanAnn i) where
    show (NoSpanAnn (AnnVariable _ t)) = show t
    show (NoSpanAnn (AnnFunction _ t)) = show t
    show (NoSpanAnn (NoAnn _ _)) = ""

main :: IO ()
main = do
    (file:_) <- getArgs
    text <- TIO.readFile file
    let tree = parse ast "" text
    case tree of
        Left err -> pPrint err
        Right pt -> case runReader (getCompose $ rename pt) emptyContext of
                        Failure errors -> pPrint errors
                        Success t -> pPrint (noSpan t)
