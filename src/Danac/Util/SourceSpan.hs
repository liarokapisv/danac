{-# LANGUAGE DeriveFunctor #-}

module Danac.Util.SourceSpan where

import Text.Megaparsec.Pos

data SourceSpan = SS SourcePos SourcePos
    deriving (Eq, Show)
