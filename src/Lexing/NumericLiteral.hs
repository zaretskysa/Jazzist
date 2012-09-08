module NumericLiteral where

import Text.ParserCombinators.Parsec

import Tokens
import Utils

numericLiteral :: Parser Token
numericLiteral = do
	num <- try hexIntegerLiteral <|> decimalLiteral
	return $ NumericLiteralToken num

decimalLiteral :: Parser Double
decimalLiteral = do
	(try $ fractionalWithLeadingIneger) 
	<|> (try $ fractionalWithoutLeadingInteger) 
	<|> integerWithoutFractional

fractionalWithLeadingIneger :: Parser Double
fractionalWithLeadingIneger = do
	intPart <- decimalIntegerLiteral
	char '.'
	fracPart <- many digit
	expPart <- option "" exponentPart
	return $ makeNumber intPart fracPart expPart

exponentPart :: Parser String
exponentPart = do
	oneOf "eE"
	signedInteger

signedInteger :: Parser String
signedInteger = do
	sign <- option '+' (oneOf "+-")
	digits <- many1 digit
	return $ sign : digits

fractionalWithoutLeadingInteger :: Parser Double
fractionalWithoutLeadingInteger = do
	char '.'
	fracPart <- many1 digit
	expPart <- option "0" exponentPart
	return $ makeNumber "0" fracPart expPart

integerWithoutFractional :: Parser Double
integerWithoutFractional = do
	intPart <- decimalIntegerLiteral
	expPart <- option "0" exponentPart
	return $ makeNumber intPart "0" expPart

decimalIntegerLiteral :: Parser String
decimalIntegerLiteral = zeroIntegerLiteral <|> nonZeroIntegerLiteral

zeroIntegerLiteral :: Parser String
zeroIntegerLiteral = char '0' >> return "0"

nonZeroIntegerLiteral :: Parser String
nonZeroIntegerLiteral = do
	begin <- nonZeroDigit
	rest <- many digit
	return $ begin : rest

nonZeroDigit :: Parser Char
nonZeroDigit = oneOf "123456789"

hexIntegerLiteral :: Parser Double
hexIntegerLiteral = do
	char '0' >> oneOf "xX"
	hs <- many1 hexDigit
	return $ fromIntegral $ intFromHex hs 

-- intPart -> fracPart -> expPart -> result
makeNumber :: String -> String -> String -> Double
makeNumber intPart fracPart expPart = (intNum + fracNum / 10.0) * 10.0 ** expNum
	where
		intNum = if null intPart then 0.0 else read intPart
		fracNum = if null fracPart then 0.0 else read fracPart
		expNum = if null expPart then 0.0 else read expPart
