module Lexing.BooleanLiteral where

import Text.ParserCombinators.Parsec

import Lexing.Tokens

booleanLiteral :: Parser Token
booleanLiteral = do
	value <- string "true" <|> string "false"
	return $ stringToBoolean value

stringToBoolean :: String -> Token
stringToBoolean str = 
    if str == "true"
        then BooleanLiteralToken True
        else BooleanLiteralToken False

isBooleanLiteral :: String -> Bool
isBooleanLiteral s = s `elem` ["true", "false"]
