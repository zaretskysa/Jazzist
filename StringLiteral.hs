module StringLiteral where

import Data.Char
import Text.ParserCombinators.Parsec

import Utils
import Tokens
import LineTerminator

stringLiteral :: Parser Token
stringLiteral = doubleQuotedString -- <|> singleQuotedString

doubleQuotedString :: Parser Token
doubleQuotedString = do
    char '"'
    values <- many doubleStringCharacter
    char '"'
    return $ StringLiteralToken values

doubleStringCharacter :: Parser Char
doubleStringCharacter = do
    doubleStringCharacterWithoutDoubleQuoteAndBackSlashAndLineTerminator
    <|> escapeSequenceElement
--  <|> lineContinuation

lineContinuation :: Parser Char
lineContinuation = do
    char '\\' 
    lineTerminatorSequence

doubleStringCharacterWithoutDoubleQuoteAndBackSlashAndLineTerminator :: Parser Char
doubleStringCharacterWithoutDoubleQuoteAndBackSlashAndLineTerminator = do
    try $ notFollowedBy $ (char '"' <|> char '\\' <|> lineTerminator)
    anyChar

escapeSequenceElement :: Parser Char
escapeSequenceElement = do
    char '\\'
    escapeSequence

escapeSequence :: Parser Char
escapeSequence = do
    characterEscapeSequence
--  <|> 0 without decimal digit
    <|> hexEscapeSequence
    <|> unicodeEscapeSequence

characterEscapeSequence :: Parser Char
characterEscapeSequence = singleEscapeCharacter <|> nonEscapeCharacter

singleEscapeCharacter :: Parser Char
singleEscapeCharacter = do
    c <- oneOf "bfnrtv" <|> char '\'' <|> char '"' <|> char '\\'
    return $ case c of
        'b' -> '\b'
        'f' -> '\f'
        'n' -> '\n'
        'r' -> '\r'
        't' -> '\t'
        'v' -> '\v'
        _ -> c

nonEscapeCharacter :: Parser Char
nonEscapeCharacter = do
    notFollowedBy (escapeCharacter <|> lineTerminator)
    anyChar

escapeCharacter :: Parser Char
escapeCharacter = singleEscapeCharacter <|> digit <|> oneOf "xu"

hexEscapeSequence :: Parser Char
hexEscapeSequence = do
    char 'x'
    hs <- count 2 hexDigit
    return $ chr $ intFromHex hs

unicodeEscapeSequence :: Parser Char
unicodeEscapeSequence = do
    char 'u'
    hs <- count 4 hexDigit
    return $ chr $ intFromHex hs

singleQuotedString :: Parser Token
singleQuotedString = undefined
