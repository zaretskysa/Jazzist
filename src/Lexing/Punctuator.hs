module Lexing.Punctuator 
(
    punctuator
) where

import Text.ParserCombinators.Parsec

import Lexing.Tokens

punctuator :: Parser Token
punctuator = do
    p <- punctuator'
    return $ PunctuatorToken p

punctuator' :: Parser Punctuator
punctuator' = do
    leftCurlyBracket
    <|> rightCurlyBracket
    <|> leftRoundBracket
    <|> rightRoundBracket
    <|> leftSquareBracket
    <|> rightSquareBracket
    <|> dot
    <|> semicolon
    <|> comma
    <|> try lessThanEquals
    <|> try greaterThanEquals
    <|> try lessThan
    <|> try greaterThan
    <|> try equals
    <|> try notEquals
    <|> try strictEquals
    <|> try strictNotEquals
    <|> try incrementPlus
    <|> try plus
    <|> try minus
    <|> try mul
    <|> try modulus
    <|> try incrementMinus
    <|> minusAssign
    <|> try leftShift
    <|> try rightShift
    <|> try unsignedRightShiftAssign
    <|> try unsignedRightShift
    <|> try bitwiseAnd
    <|> try bitwiseOr
    <|> try bitwiseXor
    <|> logicalNot
    <|> try bitwiseNot
    <|> logicalAnd
    <|> logicalOr
    <|> questionMark
    <|> colon
    <|> try assign
    <|> plusAssign
    <|> mulAssign
    <|> modulusAssign
    <|> try leftShiftAssign
    <|> try rightShiftAssign
    <|> bitwiseAndAssign
    <|> bitwiseOrAssign
    <|> bitwiseXorAssign
    <|> try divOp
    <|> divAssign

leftCurlyBracket :: Parser Punctuator
leftCurlyBracket = char '{' >> return LeftCurlyBracketPunctuator

rightCurlyBracket :: Parser Punctuator
rightCurlyBracket = char '}' >> return RightCurlyBracketPunctuator

leftRoundBracket :: Parser Punctuator
leftRoundBracket = char '(' >> return LeftRoundBracketPunctuator

rightRoundBracket :: Parser Punctuator
rightRoundBracket = char ')' >> return RightRoundBracketPunctuator

leftSquareBracket :: Parser Punctuator
leftSquareBracket = char '[' >> return LeftSquareBracketPunctuator

rightSquareBracket :: Parser Punctuator
rightSquareBracket = char ']' >> return RightSquareBracketPunctuator

dot :: Parser Punctuator
dot = char '.' >> return DotPunctuator

semicolon :: Parser Punctuator
semicolon = char ';' >> return SemicolonPunctuator

comma :: Parser Punctuator
comma = char ',' >> return CommaPunctuator

lessThan :: Parser Punctuator
lessThan = do 
    char '<'
    notFollowedBy $ oneOf "<="
    return LessThanPunctuator

greaterThan :: Parser Punctuator
greaterThan = do 
    char '>'
    notFollowedBy $ oneOf ">="
    return LessThanPunctuator

lessThanEquals :: Parser Punctuator
lessThanEquals = string "<=" >> return LessThanEqualsPunctuator

greaterThanEquals :: Parser Punctuator
greaterThanEquals = string ">=" >> return GreaterThanEqualsPunctuator

equals :: Parser Punctuator
equals = do
    string "=="
    notFollowedBy $ char '='
    return EqualsPunctuator

notEquals :: Parser Punctuator
notEquals = do
    string "!="
    notFollowedBy $ char '='
    return NotEqualsPunctuator

strictEquals :: Parser Punctuator
strictEquals = string "===" >> return StrictEqualsPunctuator

strictNotEquals :: Parser Punctuator
strictNotEquals = string "!==" >> return StrictNotEqualsPunctuator

plus :: Parser Punctuator
plus = do
    char '+'
    notFollowedBy $ oneOf "+="
    return PlusPunctuator

minus :: Parser Punctuator
minus = do
    char '-'
    notFollowedBy $ oneOf "-="
    return MinusPunctuator

mul :: Parser Punctuator
mul = do
    char '*'
    notFollowedBy $ char '*'
    return MulPunctuator

modulus :: Parser Punctuator
modulus = do
    char '%'
    notFollowedBy $ char '='
    return ModulusPunctuator

incrementPlus :: Parser Punctuator
incrementPlus = string "++" >> return IncrementPlusPunctuator

incrementMinus :: Parser Punctuator
incrementMinus = string "--" >> return IncrementMinusPunctuator

leftShift :: Parser Punctuator
leftShift = do
    string "<<"
    notFollowedBy $ char '='
    return LeftShiftPunctuator

rightShift :: Parser Punctuator
rightShift = do
    string ">>"
    notFollowedBy $ oneOf ">="
    return $ RightShiftPunctuator

unsignedRightShift :: Parser Punctuator
unsignedRightShift = string ">>>" >> return UnsignedRightShiftPunctuator

bitwiseAnd :: Parser Punctuator
bitwiseAnd = do
    char '&'
    notFollowedBy $ oneOf "&="
    return BitwiseAndPunctuator

bitwiseOr :: Parser Punctuator
bitwiseOr = do
    char '|'
    notFollowedBy $ oneOf "|="
    return $ BitwiseOrPunctuator

bitwiseXor :: Parser Punctuator
bitwiseXor = do
    char '^'
    notFollowedBy $ char '='
    return BitwiseXorPunctuator

logicalNot :: Parser Punctuator
logicalNot = do
    char '!'
    notFollowedBy $ char '='
    return LogicalNotPunctuator

bitwiseNot :: Parser Punctuator
bitwiseNot = char '~' >> return BitwiseNotPunctuator

logicalAnd :: Parser Punctuator
logicalAnd = string "&&" >> return LogicalAndPunctuator

logicalOr :: Parser Punctuator
logicalOr = string "||" >> return LogicalOrPunctuator

questionMark :: Parser Punctuator
questionMark = char '?' >> return QuestionMarkPunctuator

colon :: Parser Punctuator
colon = char ':' >> return ColonPunctuator

assign :: Parser Punctuator
assign = do
    char '='
    notFollowedBy $ char '='
    return AssignPunctuator

plusAssign :: Parser Punctuator
plusAssign = string "+=" >> return PlusAssignPunctuator

minusAssign :: Parser Punctuator
minusAssign = string "-=" >> return MinusAssignPunctuator

mulAssign :: Parser Punctuator
mulAssign = string "*=" >> return MulAssignPunctuator

modulusAssign :: Parser Punctuator
modulusAssign = string "%=" >> return ModulusAssignPunctuator

leftShiftAssign :: Parser Punctuator
leftShiftAssign = string "<<=" >> return LeftShiftAssignPunctuator

rightShiftAssign :: Parser Punctuator
rightShiftAssign = string ">>=" >> return RightShiftAssignPunctuator

unsignedRightShiftAssign :: Parser Punctuator
unsignedRightShiftAssign = string ">>>=" >> return UnsignedRightShiftAssignPunctuator

bitwiseAndAssign :: Parser Punctuator
bitwiseAndAssign = string "&=" >> return BitwiseAndAssignPunctuator

bitwiseOrAssign :: Parser Punctuator
bitwiseOrAssign = string "|=" >> return BitwiseOrAssignPunctuator

bitwiseXorAssign :: Parser Punctuator
bitwiseXorAssign = string "^=" >> return BitwiseXorAssignPunctuator

divOp :: Parser Punctuator
divOp = do 
    char '/'
    notFollowedBy $ char '='
    return DivPunctuator

divAssign :: Parser Punctuator
divAssign = string "/=" >> return DivAssignPunctuator
