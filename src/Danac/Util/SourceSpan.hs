{-# LANGUAGE DeriveFunctor #-}

module Danac.Util.SourceSpan where

import Text.Megaparsec.Pos

data SourceSpan = SS SourcePos SourcePos

instance Show SourceSpan where
    show (SS (SourcePos {sourceLine = ll, sourceColumn = lc}) (SourcePos {sourceLine = rl, sourceColumn = rc})) = 
        show (unPos ll) <> ":" <> show (unPos lc) <> "-" <> show (unPos rl) <> ":" <> show (unPos rc)
