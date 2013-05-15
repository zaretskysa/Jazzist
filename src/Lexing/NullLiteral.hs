module Lexing.NullLiteral
(
    module Lexing.LocatedToken,

    nullLiteral,
    isNullLiteral
) where

import Text.ParserCombinators.Parsec

import Lexing.LocatedToken

nullLiteral :: Parser LocatedToken
nullLiteral = do
    pos <- getPosition
    string "null"
    return $ LocatedToken NullLiteralToken pos

isNullLiteral :: String -> Bool
isNullLiteral s = s == "null"
