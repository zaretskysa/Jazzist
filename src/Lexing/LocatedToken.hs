module Lexing.LocatedToken
(
    module Lexing.Token,

    makeLocatedToken,

    LocatedToken(..),
    SourceLocation(..),
    SourceRow(..),
    SourceCol(..),
    SourceName(..)
) where

import Lexing.Token


data LocatedToken = LocatedToken Token SourceLocation
    deriving (Eq, Show)

type SourceRow = Int

type SourceCol = Int

type SourceName = String

data SourceLocation = SourceLocation SourceRow SourceCol SourceName
    deriving (Show, Eq)

defaultSrcLoc :: SourceLocation
defaultSrcLoc = SourceLocation 0 0 ""

-- TODO: remove
makeLocatedToken :: Token -> LocatedToken
makeLocatedToken token = LocatedToken token defaultSrcLoc
