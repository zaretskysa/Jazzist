module Lexing.NumericLiteral
(
	module Lexing.LocatedToken,

	numericLiteral
) where

import Text.ParserCombinators.Parsec

import Lexing.LocatedToken
import Lexing.Utils

numericLiteral :: Parser LocatedToken
numericLiteral = do
	pos <- getPosition
	num <- try hexIntegerLiteral <|> decimalLiteral
	return $ LocatedToken (NumericLiteralToken num) pos

decimalLiteral :: Parser Double
decimalLiteral = do
	(try $ fractionalWithLeadingIneger) 
	<|> (try $ fractionalWithoutLeadingInteger) 
	<|> integerWithoutFractional

fractionalWithLeadingIneger :: Parser Double
fractionalWithLeadingIneger = do
	intPart <- decimalIntegerLiteral
	fracPart <- char '.' >> (option "0" (many1 digit))
	expPart <- option "0" exponentPart
	return $ numberFromIntAndFracAndExp (read intPart) (read fracPart) (read expPart)

exponentPart :: Parser String
exponentPart = oneOf "eE" >> signedInteger

signedInteger :: Parser String
signedInteger = do
	sign <- option '+' (oneOf "+-")
	digits <- many1 digit
	return $ if sign=='+' then digits else sign : digits

fractionalWithoutLeadingInteger :: Parser Double
fractionalWithoutLeadingInteger = do
	fracPart <- char '.' >> many1 digit
	expPart <- option "0" exponentPart
	return $ numberFromIntAndFracAndExp 0  (read fracPart) (read expPart)

integerWithoutFractional :: Parser Double
integerWithoutFractional = do
	intPart <- decimalIntegerLiteral
	expPart <- option "0" exponentPart
	return $ numberFromIntAndFracAndExp (read intPart) 0 (read expPart)

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
	hs <- char '0' >> oneOf "xX" >> many1 hexDigit
	return $ fromIntegral $ intFromHex hs 

decPositions :: (Integral a) => a -> a
decPositions num = 
	if num < base 
		then 1 
		else 1 + (decPositions $ num `div` base)
	where base = 10 

numberFromIntAndFracAndExp :: (Floating b) => Integer -> Integer -> Integer -> b
numberFromIntAndFracAndExp intPart fracPart expPart = (i + f) * e
	where 
		i = fromIntegral intPart
		f = (fromIntegral fracPart) / (10 ^ (decPositions fracPart))
		e = 10 ^^ expPart
