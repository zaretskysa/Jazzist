module Lexing.LocatedToken
(
    module Lexing.Token,

    tokenFromLocated,

    LocatedToken(..)
) where

import Lexing.Token
import Text.ParserCombinators.Parsec.Pos

data LocatedToken = LocatedToken Token SourcePos
    deriving (Eq, Show)

tokenFromLocated :: LocatedToken -> Token
tokenFromLocated (LocatedToken token _) = token
