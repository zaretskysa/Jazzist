module Lexing.LineTerminator
(
    module Lexing.LocatedToken,

    lineTerminator,
    lineTerminatorChar,
    lineTerminatorSequence
) where

import Text.ParserCombinators.Parsec

import Lexing.LocatedToken

lineTerminatorSequence :: Parser Char
lineTerminatorSequence = do
	lineFeed
	<|> try carriageReturnWithoutLineFeed
	<|> lineSeparator
	<|> paragraphSeparator
	<|> carriageReturnAndLineFeed

carriageReturnAndLineFeed :: Parser Char
carriageReturnAndLineFeed =  carriageReturn >> lineFeed

carriageReturnWithoutLineFeed :: Parser Char
carriageReturnWithoutLineFeed = do
	cr <- carriageReturn 
	notFollowedBy lineFeed
	return cr

lineTerminator :: Parser LocatedToken
lineTerminator = do
    pos <- getPosition
    lineTerminatorChar
    return $ LocatedToken LineTerminatorToken pos

lineTerminatorChar :: Parser Char
lineTerminatorChar = lineFeed <|> carriageReturn <|> lineSeparator <|> paragraphSeparator

lineFeed :: Parser Char
lineFeed = char '\x000a'

carriageReturn :: Parser Char
carriageReturn = char '\x000d'

lineSeparator :: Parser Char
lineSeparator = char '\x2028'

paragraphSeparator :: Parser Char
paragraphSeparator = char '\x2029'
