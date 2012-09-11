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

maybeParse p = justParse p <|> return Nothing

justParse p = try $ do
    value <- p
    return $ Just value

acceptToken :: Token -> TokenParser Token
acceptToken x = token showTok posFromTok testTok
    where
        showTok t = show t
        posFromTok t = initialPos "js tokens source"
        testTok t = if (x == t) then Just t else Nothing

acceptAnyToken :: TokenParser Token
acceptAnyToken = token showTok posFromTok testTok
    where
        showTok t = show t
        posFromTok t = initialPos "js tokens source"
        testTok t = Just t

identifierToken :: TokenParser String
identifierToken = try $ do
    t <- acceptAnyToken 
    case t of
        IdentifierToken id -> return id
        otherwise -> fail "IdentifierToken"

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
    key <- anyKeywordToken
    return "keywordIdentifierName"

identifierNameFromNullLiteral :: TokenParser String
identifierNameFromNullLiteral = nullLiteralToken >> return "null"

-- TODO: convert bool to string
identifierNameFromBoleanLiteral :: TokenParser String
identifierNameFromBoleanLiteral = do
    bool <- booleanLiteralToken
    return "boolIdentifierName"

punctuatorToken :: Punctuator -> TokenParser Punctuator
punctuatorToken p = try $ do
    tok <- acceptAnyToken 
    case tok of
        PunctuatorToken x -> if x == p 
            then return p 
            else fail $ "Punctuator " ++ show p
        otherwise -> fail "PunctuatoToken"

anyKeywordToken :: TokenParser Keyword
anyKeywordToken = try $ do
    tok <- acceptAnyToken 
    case tok of
        KeywordToken key -> return key
        otherwise -> fail "KeywordToken"

keywordToken :: Keyword -> TokenParser Keyword
keywordToken k = try $ do
    tok <- acceptAnyToken 
    case tok of
        KeywordToken x -> if x == k 
            then return k
            else fail $ "Keyword " ++ show k
        otherwise -> fail "KeywordToken"

nullLiteralToken :: TokenParser ()
nullLiteralToken = try $ do
    tok <- acceptAnyToken 
    case tok of
        NullLiteralToken -> return ()
        otherwise -> fail "NullLiteralToken"

booleanLiteralToken :: TokenParser Bool
booleanLiteralToken = try $ do
    tok <- acceptAnyToken 
    case tok of
        BooleanLiteralToken bool -> return bool
        otherwise -> fail "BooleanLiteralToken"

numericLiteralToken :: TokenParser Double
numericLiteralToken = try $ do
    tok <- acceptAnyToken 
    case tok of
        NumericLiteralToken num -> return num
        otherwise -> fail "NumericLiteralToken"

stringLiteralToken :: TokenParser String
stringLiteralToken = try $ do
    tok <- acceptAnyToken 
    case tok of
        StringLiteralToken str -> return str
        otherwise -> fail "StringLiteralToken"




