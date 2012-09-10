module Lexing.LineTerminator where

import Text.ParserCombinators.Parsec

import Lexing.Tokens

lineTerminatorSequence :: Parser Char
lineTerminatorSequence = do
	lineFeed
	<|> carriageReturnWithoutLineFeed
	<|> lineSeparator
	<|> paragraphSeparator
	<|> carriageReturnAndLineFeed

carriageReturnAndLineFeed :: Parser Char
carriageReturnAndLineFeed =  lineFeed 
					     <|> carriageReturnWithoutLineFeed 
					     <|> lineFeed 
					     <|> paragraphSeparator
					     <|> carriageReturnAndLineFeed

carriageReturnWithoutLineFeed :: Parser Char
carriageReturnWithoutLineFeed = do
	cr <- carriageReturn 
	notFollowedBy lineFeed
	return cr

lineTerminator :: Parser Char
lineTerminator = lineFeed <|> carriageReturn <|> lineSeparator <|> paragraphSeparator

lineFeed :: Parser Char
lineFeed = char '\x000a'

carriageReturn :: Parser Char
carriageReturn = char '\x000d'

lineSeparator :: Parser Char
lineSeparator = char '\x2028'

paragraphSeparator :: Parser Char
paragraphSeparator = char '\x2029'