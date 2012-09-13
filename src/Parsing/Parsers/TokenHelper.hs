module Parsing.Parsers.TokenHelper where

import Parsing.TokenParser

comma :: TokenParser Punctuator
comma = punctuatorToken CommaPunctuator 

colon :: TokenParser Punctuator
colon = punctuatorToken ColonPunctuator 

dot :: TokenParser Punctuator
dot = punctuatorToken DotPunctuator 

semicolon :: TokenParser Punctuator
semicolon = punctuatorToken SemicolonPunctuator 

leftCurlyBracket :: TokenParser Punctuator
leftCurlyBracket = punctuatorToken LeftCurlyBracketPunctuator

rightCurlyBracket :: TokenParser Punctuator
rightCurlyBracket = punctuatorToken RightCurlyBracketPunctuator

leftSquareBracket :: TokenParser Punctuator
leftSquareBracket = punctuatorToken LeftSquareBracketPunctuator

rightSquareBracket :: TokenParser Punctuator
rightSquareBracket = punctuatorToken RightSquareBracketPunctuator

leftRoundBracket :: TokenParser Punctuator
leftRoundBracket = punctuatorToken LeftRoundBracketPunctuator

rightRoundBracket :: TokenParser Punctuator
rightRoundBracket = punctuatorToken RightRoundBracketPunctuator

roundBrackets :: TokenParser ()
roundBrackets = leftRoundBracket >> rightRoundBracket >> return ()

assign :: TokenParser Punctuator
assign = punctuatorToken AssignPunctuator



var :: TokenParser Keyword
var = keywordToken VarKeyword

this :: TokenParser Keyword
this = keywordToken ThisKeyword

function :: TokenParser Keyword
function = keywordToken FunctionKeyword


-- get and set are not reserved keywords (wtf??)
get :: TokenParser ()
get = concreteIdentifier "get"

set :: TokenParser ()
set = concreteIdentifier "set"

concreteIdentifier :: String -> TokenParser ()
concreteIdentifier str = do
    actual <- identifierToken
    if str == actual 
        then return ()
        else fail $ show str


