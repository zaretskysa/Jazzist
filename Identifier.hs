module Identifier where

import Data.Char
import Text.ParserCombinators.Parsec

import Tokens
import Keyword
import NullLiteral
import BooleanLiteral
import EscapeSequence

identifier :: Parser Token
identifier = try $ do
	name <- identifierName
	if isReservedWord name
		then fail "identifier can not be a reserved word"
		else return $ IdentifierToken name

isReservedWord :: String -> Bool
isReservedWord s = isNullLiteral s || isBooleanLiteral s || isKeyword s

identifierName :: Parser String
identifierName = do
	start <- many1 identifierStart
	rest <- many identifierPart
	return $ start ++ rest

identifierPart :: Parser Char
identifierPart = do
	identifierStart 
	<|> unicodeDigit
	<|> unicodeCombiningMark
	<|> unicodeConnectorPunctuation
	-- <|> ZWNJ
	-- <|> ZWJ

unicodeCombiningMark :: Parser Char
unicodeCombiningMark = satisfy unicodeCombiningMarkCategory

unicodeCombiningMarkCategory :: Char -> Bool
unicodeCombiningMarkCategory c = 
	generalCategory c `elem` [NonSpacingMark, SpacingCombiningMark]

unicodeDigit :: Parser Char
unicodeDigit = digit

unicodeConnectorPunctuation :: Parser Char
unicodeConnectorPunctuation = satisfy unicodeConnectorPunctuationCatecory

unicodeConnectorPunctuationCatecory :: Char -> Bool
unicodeConnectorPunctuationCatecory c = 
	generalCategory c == ConnectorPunctuation

unicodeLetter :: Parser Char
unicodeLetter = satisfy unicodeLetterCategory

unicodeLetterCategory :: Char -> Bool
unicodeLetterCategory c = 
	generalCategory c `elem` categories
	where categories = [
		UppercaseLetter,
		LowercaseLetter,
		TitlecaseLetter,
		ModifierLetter,
		OtherLetter,
		LetterNumber]

identifierStart :: Parser Char
identifierStart = unicodeEscapeSequenceElement <|> unicodeLetter <|> char '$' <|> char '_'

