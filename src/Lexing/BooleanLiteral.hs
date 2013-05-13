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
	value <- string "true" <|> string "false"
	return $ stringToBoolean value

stringToBoolean :: String -> LocatedToken
stringToBoolean str = 
    if str == "true"
        then makeLocatedToken $ BooleanLiteralToken True
        else makeLocatedToken $ BooleanLiteralToken False

isBooleanLiteral :: String -> Bool
isBooleanLiteral s = s `elem` ["true", "false"]
