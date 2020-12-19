{-# LANGUAGE TypeApplications #-}

module Main where

import qualified Data.Text.IO as TIO

import Danac.Core.Ast (Ast)
import qualified Danac.Core.XRecMap as X
import Danac.Parser.Ast (PS)
import Danac.Parser.Core (ast)
import Text.Megaparsec (parseTest, parse)
import Text.Pretty.Simple (pPrint)
import Data.Functor.Identity

data NoShow a = NoShow a

instance X.CanWrap NoShow where
   wrap = NoShow 

instance Show a => Show (NoShow a) where
    show (NoShow x) = show x

main :: IO ()
main = do
    text <- TIO.readFile "examples/bubblesort.da"
    let tree = parse ast "" text
    case tree of
        Left err -> pPrint err
        Right t -> pPrint (X.conv t :: Ast (X.XRecMap NoShow PS))
