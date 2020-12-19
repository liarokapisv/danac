module Danac.Util.Located where

import Text.Megaparsec.Pos

data SourceSpan = SS SourcePos SourcePos
    deriving (Eq, Show)

data Located a = Located { value :: a, srcSpan :: SourceSpan }
    deriving (Eq, Show)

