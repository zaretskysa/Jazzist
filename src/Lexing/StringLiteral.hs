module Lexing.StringLiteral where

import Data.Char
import Text.ParserCombinators.Parsec

import Lexing.Utils
import Lexing.Tokens
import Lexing.LineTerminator

stringLiteral :: Parser Token
stringLiteral = do
    value <- doubleQuotedString <|> singleQuotedString
    return $ StringLiteralToken value

doubleQuotedString :: Parser String
doubleQuotedString = do
    char '"'
    values <- many doubleStringCharacter
    char '"'
    return $ concat values

doubleStringCharacter :: Parser String
doubleStringCharacter = do
    characterWithoutDoubleQuoteAndBackSlashAndLineTerminator
    <|> try escapeSequenceElement
    <|> lineContinuation

lineContinuation :: Parser String
lineContinuation = do
    char '\\'
    lineTerminatorSequence
    return []

characterWithoutDoubleQuoteAndBackSlashAndLineTerminator :: Parser String
characterWithoutDoubleQuoteAndBackSlashAndLineTerminator = do
    try $ notFollowedBy $ (char '"' <|> char '\\' <|> lineTerminator)
    c <- anyChar
    return [c]

escapeSequenceElement :: Parser String
escapeSequenceElement = do
    char '\\'
    c <- escapeSequence
    return [c]

escapeSequence :: Parser Char
escapeSequence = do
    characterEscapeSequence
    <|> try nullEscapeSequence
    <|> hexEscapeSequence
    <|> unicodeEscapeSequence

nullEscapeSequence :: Parser Char
nullEscapeSequence = char '0' >> notFollowedBy digit >> return '\0'

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

singleQuotedString :: Parser String
singleQuotedString = do
    char '\''
    values <- many singleStringCharacter
    char '\''
    return $ concat values

singleStringCharacter :: Parser String
singleStringCharacter = do
    characterWithoutSingleQuoteAndBackSlashAndLineTerminator
    <|> try escapeSequenceElement
    <|> lineContinuation

characterWithoutSingleQuoteAndBackSlashAndLineTerminator :: Parser String
characterWithoutSingleQuoteAndBackSlashAndLineTerminator = do
    try $ notFollowedBy $ (char '\'' <|> char '\\' <|> lineTerminator)
    c <- anyChar
    return [c]
