module Main where

import qualified Data.Text.IO as TIO

import Danac.Parser.Core (ast)
import Text.Megaparsec (parseTest, parse)
import Text.Pretty.Simple (pPrint)

main :: IO ()
main = do
    text <- TIO.readFile "examples/hello_world.da"
    let tree = parse ast "" text
    pPrint tree
