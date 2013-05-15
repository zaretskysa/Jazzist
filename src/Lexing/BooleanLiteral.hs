module Lexing.BooleanLiteral
(
    module Lexing.LocatedToken,

    booleanLiteral,
    isBooleanLiteral    
) where

import Text.ParserCombinators.Parsec

import Lexing.LocatedToken

booleanLiteral :: Parser LocatedToken
booleanLiteral = do
    pos <- getPosition
    value <- string "true" <|> string "false"
    return $ LocatedToken (stringToBoolean value) pos

stringToBoolean :: String -> Token
stringToBoolean str = 
    if str == "true"
        then BooleanLiteralToken True
        else BooleanLiteralToken False

isBooleanLiteral :: String -> Bool
isBooleanLiteral s = s `elem` ["true", "false"]
