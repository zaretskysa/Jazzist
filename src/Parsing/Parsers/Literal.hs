module Parsing.Parsers.Literal where

import Parsing.Ast
import Parsing.TokenParser
import Parsing.Expression

literal :: TokenParser Literal
literal = 
    nullLiteral
    <|> booleanLiteral
    <|> numericLiteral
    <|> stringLiteral
--    <|> regexpLiteral

nullLiteral :: TokenParser Literal
nullLiteral = nullLiteralToken >> return NullLiteral

booleanLiteral :: TokenParser Literal
booleanLiteral = do
    b <- booleanLiteralToken
    return $ BooleanLiteral b

numericLiteral :: TokenParser Literal
numericLiteral = do
    num <- numericLiteralToken
    return $ NumericLiteral num

stringLiteral :: TokenParser Literal
stringLiteral = do
    str <- stringLiteralToken
    return $ StringLiteral str












