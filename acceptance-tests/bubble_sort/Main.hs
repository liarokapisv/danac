module Main where

import qualified Data.Text.IO as TIO
import Danac.Util.Annotation (strip)
import Danac.Parser (ast)
import Text.Megaparsec (parse)
import Text.Pretty.Simple (pPrint)

main :: IO ()
main = do
    text <- TIO.readFile "examples/bubblesort.da"
    let tree = parse ast "" text
    case tree of
        Left err -> pPrint err
        Right pt -> pPrint $ strip pt --case runReader (getCompose $ R.ast pt) R.emptyScope of
                    --    Failure errors -> pPrint errors
                    --    Success t -> pPrint (X.strip t :: Ast (X.Strip NoShow))
