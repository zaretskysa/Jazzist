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

incrementPlus :: TokenParser Punctuator
incrementPlus = punctuatorToken IncrementPlusPunctuator

incrementMinus :: TokenParser Punctuator
incrementMinus = punctuatorToken IncrementMinusPunctuator

plus :: TokenParser Punctuator
plus = punctuatorToken PlusPunctuator

minus :: TokenParser Punctuator
minus = punctuatorToken MinusPunctuator

bitwiseNot :: TokenParser Punctuator
bitwiseNot = punctuatorToken BitwiseNotPunctuator

logicalNot :: TokenParser Punctuator
logicalNot = punctuatorToken LogicalNotPunctuator

mul :: TokenParser Punctuator
mul = punctuatorToken MulPunctuator

divOp :: TokenParser Punctuator
divOp = punctuatorToken DivPunctuator

modulus :: TokenParser Punctuator
modulus = punctuatorToken ModulusPunctuator

leftShift :: TokenParser Punctuator
leftShift = punctuatorToken LeftShiftPunctuator

rightShift :: TokenParser Punctuator
rightShift = punctuatorToken RightShiftPunctuator

unsignedRightShift :: TokenParser Punctuator
unsignedRightShift = punctuatorToken UnsignedRightShiftPunctuator

lessThan :: TokenParser Punctuator
lessThan = punctuatorToken LessThanPunctuator

greaterThan :: TokenParser Punctuator
greaterThan = punctuatorToken GreaterThanPunctuator

lessThanEquals :: TokenParser Punctuator
lessThanEquals = punctuatorToken LessThanEqualsPunctuator

greaterThanEquals :: TokenParser Punctuator
greaterThanEquals = punctuatorToken GreaterThanEqualsPunctuator


-------------------------------------------------------

var :: TokenParser Keyword
var = keywordToken VarKeyword

this :: TokenParser Keyword
this = keywordToken ThisKeyword

function :: TokenParser Keyword
function = keywordToken FunctionKeyword

new :: TokenParser Keyword
new = keywordToken NewKeyword

delete :: TokenParser Keyword
delete = keywordToken DeleteKeyword

void :: TokenParser Keyword
void = keywordToken VoidKeyword

typeOf :: TokenParser Keyword
typeOf = keywordToken TypeOfKeyword

instanceOf :: TokenParser Keyword
instanceOf = keywordToken InstanceOfKeyword

inKeyword :: TokenParser Keyword
inKeyword = keywordToken InKeyword

----------------------------------------------------

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

