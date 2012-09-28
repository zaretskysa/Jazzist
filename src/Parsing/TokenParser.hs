module Parsing.TokenParser 
(
    module Text.ParserCombinators.Parsec.Prim,
    module Text.ParserCombinators.Parsec.Combinator,
    module Lexing.Tokens,
    TokenParser,
    maybeParse,
    identifierToken,
    punctuatorToken,
    keywordToken,
    nullLiteralToken,
    booleanLiteralToken,
    numericLiteralToken,
    stringLiteralToken,
    identifierName
) where

import Text.ParserCombinators.Parsec.Combinator
import Text.ParserCombinators.Parsec.Prim
import Text.ParserCombinators.Parsec.Pos

import Lexing.Tokens

type TokenParser a = GenParser Token () a

maybeParse :: TokenParser a -> TokenParser (Maybe a)
maybeParse p = justParse p <|> return Nothing

justParse :: TokenParser a -> TokenParser (Maybe a)
justParse p = do
    value <- try $ p
    return $ Just value

acceptAnyToken :: TokenParser Token
acceptAnyToken = token showTok posFromTok testTok
    where
        showTok t = show t
        posFromTok _ = initialPos "js tokens source"
        testTok t = Just t

identifierToken :: TokenParser String
identifierToken = try $ do
    t <- acceptAnyToken 
    case t of
        IdentifierToken ident -> return ident
        _ -> fail "IdentifierToken"

identifierName :: TokenParser String
identifierName = 
    identifierToken 
    <|> identifierNameFromKeyword
    <|> identifierNameFromNullLiteral
    <|> identifierNameFromBoleanLiteral
    <?> "IdentifierName"

-- TODO: convert keyword to string
identifierNameFromKeyword :: TokenParser String
identifierNameFromKeyword = do
    _ <- anyKeywordToken
    return "keywordIdentifierName"

identifierNameFromNullLiteral :: TokenParser String
identifierNameFromNullLiteral = nullLiteralToken >> return "null"

-- TODO: convert bool to string
identifierNameFromBoleanLiteral :: TokenParser String
identifierNameFromBoleanLiteral = do
    _ <- booleanLiteralToken
    return "boolIdentifierName"

punctuatorToken :: Punctuator -> TokenParser Punctuator
punctuatorToken p = try $ do
    tok <- acceptAnyToken 
    case tok of
        PunctuatorToken x -> if x == p 
            then return p 
            else fail $ "Punctuator " ++ show p
        _ -> fail "PunctuatoToken"

anyKeywordToken :: TokenParser Keyword
anyKeywordToken = try $ do
    tok <- acceptAnyToken 
    case tok of
        KeywordToken key -> return key
        _ -> fail "KeywordToken"

keywordToken :: Keyword -> TokenParser Keyword
keywordToken k = try $ do
    tok <- acceptAnyToken 
    case tok of
        KeywordToken x -> if x == k 
            then return k
            else fail $ "Keyword " ++ show k
        _ -> fail "KeywordToken"

nullLiteralToken :: TokenParser ()
nullLiteralToken = try $ do
    tok <- acceptAnyToken 
    case tok of
        NullLiteralToken -> return ()
        _ -> fail "NullLiteralToken"

booleanLiteralToken :: TokenParser Bool
booleanLiteralToken = try $ do
    tok <- acceptAnyToken 
    case tok of
        BooleanLiteralToken bool -> return bool
        _ -> fail "BooleanLiteralToken"

numericLiteralToken :: TokenParser Double
numericLiteralToken = try $ do
    tok <- acceptAnyToken 
    case tok of
        NumericLiteralToken num -> return num
        _ -> fail "NumericLiteralToken"

stringLiteralToken :: TokenParser String
stringLiteralToken = try $ do
    tok <- acceptAnyToken 
    case tok of
        StringLiteralToken str -> return str
        _ -> fail "StringLiteralToken"




