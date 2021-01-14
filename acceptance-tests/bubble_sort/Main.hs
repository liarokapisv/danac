module Main where

import qualified Data.Text.IO as TIO

import Danac.Core.Ast (Ast)
import qualified Danac.Parser.Core as P
import qualified Danac.Renamer.Core as R
import Danac.Parser.Ast (PS)
import qualified Danac.Core.XRecMap as X
import Text.Megaparsec (parse)
import Text.Pretty.Simple (pPrint)

import Control.Monad.Reader (runReader)
import Data.Functor.Compose (getCompose)
import Validation (Validation (..))

data NoShow a = NoShow a

instance X.CanWrap NoShow where
   wrap = NoShow 

instance Show a => Show (NoShow a) where
    show (NoShow x) = show x

main :: IO ()
main = do
    text <- TIO.readFile "examples/bubblesort.da"
    let tree = parse P.ast "" text
    case tree of
        Left err -> pPrint err
        Right pt -> case runReader (getCompose $ R.ast pt) R.emptyScope of
                        Failure errors -> pPrint errors
                        Success t -> pPrint (X.conv t :: Ast (X.XRecMap NoShow PS))
