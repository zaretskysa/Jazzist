module Lexing.Punctuator where

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
    <|> incrementMinus
    <|> try leftShift
    <|> try rightShift
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
    <|> minusAssign
    <|> mulAssign
    <|> modulusAssign
    <|> try leftShiftAssign
    <|> try rightShiftAssign
    <|> unsignedRightShiftAssign
    <|> bitwiseAndAssign
    <|> bitwiseOrAssign
    <|> bitwiseXorAssign
    <|> try divOp
    <|> divAssign

leftCurlyBracket = char '{' >> return LeftCurlyBracketPunctuator

rightCurlyBracket = char '}' >> return RightCurlyBracketPunctuator

leftRoundBracket = char '(' >> return LeftRoundBracketPunctuator

rightRoundBracket = char ')' >> return RightRoundBracketPunctuator

leftSquareBracket = char '[' >> return LeftSquareBracketPunctuator

rightSquareBracket = char ']' >> return RightSquareBracketPunctuator

dot = char '.' >> return DotPunctuator

semicolon = char ';' >> return SemicolonPunctuator

comma = char ',' >> return CommaPunctuator

lessThan = do 
    char '<'
    notFollowedBy $ oneOf "<="
    return LessThanPunctuator

greaterThan = do 
    char '>'
    notFollowedBy $ oneOf ">="
    return LessThanPunctuator

lessThanEquals = string "<=" >> return LessThanEqualsPunctuator

greaterThanEquals = string ">=" >> return GreaterThanEqualsPunctuator

equals = do
    string "=="
    notFollowedBy $ char '='
    return EqualsPunctuator

notEquals = do
    string "!="
    notFollowedBy $ char '='
    return NotEqualsPunctuator

strictEquals = string "===" >> return StrictEqualsPunctuator

strictNotEquals = string "!==" >> return StrictNotEqualsPunctuator

plus = do
    char '+'
    notFollowedBy $ oneOf "+="
    return PlusPunctuator

minus = do
    char '-'
    notFollowedBy $ oneOf "-="
    return MinusPunctuator

mul = do
    char '*'
    notFollowedBy $ char '*'
    return MulPunctuator

modulus = do
    char '%'
    notFollowedBy $ char '='
    return ModulusPunctuator

incrementPlus = string "++" >> return IncrementPlusPunctuator

incrementMinus = string "--" >> return IncrementMinusPunctuator

leftShift = do
    string "<<"
    notFollowedBy $ char '='
    return LeftShiftPunctuator

rightShift = do
    string ">>"
    notFollowedBy $ oneOf ">="
    return $ RightShiftPunctuator

unsignedRightShift = string ">>>" >> return UnsignedRightShiftPunctuator

bitwiseAnd = do
    char '&'
    notFollowedBy $ oneOf "&="
    return BitwiseAndPunctuator

bitwiseOr = do
    char '|'
    notFollowedBy $ oneOf "|="
    return $ BitwiseOrPunctuator

bitwiseXor = do
    char '^'
    notFollowedBy $ char '='
    return BitwiseXorPunctuator

logicalNot = do
    char '!'
    notFollowedBy $ char '='
    return LogicalNotPunctuator

bitwiseNot = char '~' >> return BitwiseNotPunctuator

logicalAnd = string "&&" >> return LogicalAndPunctuator

logicalOr = string "||" >> return LogicalOrPunctuator

questionMark = char '?' >> return QuestionMarkPunctuator

colon = char ':' >> return ColonPunctuator

assign = do
    char '='
    notFollowedBy $ char '='
    return AssignPunctuator

plusAssign = string "+=" >> return PlusAssignPunctuator

minusAssign = string "-=" >> return MinusAssignPunctuator

mulAssign = string "*=" >> return MulAssignPunctuator

modulusAssign = string "%=" >> return ModulusAssignPunctuator

leftShiftAssign = string "<<=" >> return LeftShiftAssignPunctuator

rightShiftAssign = string ">>=" >> return RightShiftAssignPunctuator

unsignedRightShiftAssign = string ">>>=" >> return UnsignedRightShiftAssignPunctuator

bitwiseAndAssign = string "&=" >> return BitwiseAndAssignPunctuator

bitwiseOrAssign = string "|=" >> return BitwiseOrAssignPunctuator

bitwiseXorAssign = string "^=" >> return BitwiseXorAssignPunctuator

divOp = do 
    char '/'
    notFollowedBy $ char '='
    return DivPunctuator

divAssign = string "/=" >> return DivAssignPunctuator

punctuatorFromString :: String -> Punctuator
punctuatorFromString s = 
    case s of
        "{" -> LeftCurlyBracketPunctuator
        "}" -> RightCurlyBracketPunctuator
        "(" -> LeftRoundBracketPunctuator
        ")" -> RightRoundBracketPunctuator
        "[" -> LeftSquareBracketPunctuator
        "]" -> RightSquareBracketPunctuator
        "." -> DotPunctuator
        ";" -> SemicolonPunctuator
        "," -> CommaPunctuator
        "<" -> LessThanPunctuator
        ">" -> GreaterThanPunctuator
        "<=" -> LessThanEqualsPunctuator
        ">=" -> GreaterThanEqualsPunctuator
        "==" -> EqualsPunctuator
        "!=" -> NotEqualsPunctuator
        "===" -> StrictEqualsPunctuator
        "!==" -> StrictNotEqualsPunctuator
        "+" -> PlusPunctuator
        "-" -> MinusPunctuator
        "*" -> MulPunctuator
        "%" -> ModulusPunctuator
        "++" -> IncrementPlusPunctuator
        "--" -> IncrementMinusPunctuator
        "<<" -> LeftShiftPunctuator
        ">>" -> RightShiftPunctuator
        ">>>" -> UnsignedRightShiftPunctuator
        "&" -> BitwiseAndPunctuator
        "|" -> BitwiseOrPunctuator
        "^" -> BitwiseXorPunctuator
        "!" -> LogicalNotPunctuator
        "~" -> BitwiseNotPunctuator
        "&&" -> LogicalAndPunctuator
        "||" -> LogicalOrPunctuator
        "?" -> QuestionMarkPunctuator
        ":" -> ColonPunctuator
        "=" -> AssignPunctuator
        "+=" -> PlusAssignPunctuator
        "-=" -> MinusAssignPunctuator
        "*=" -> MulAssignPunctuator
        "%=" -> ModulusAssignPunctuator
        "<<=" -> LeftShiftAssignPunctuator
        ">>=" -> RightShiftAssignPunctuator
        ">>>=" -> UnsignedRightShiftAssignPunctuator
        "&=" -> BitwiseAndAssignPunctuator
        "|=" -> BitwiseOrAssignPunctuator
        "^=" -> BitwiseXorAssignPunctuator
        "/" ->DivPunctuator
        "/=" -> DivAssignPunctuator
        _ -> error "unknown punctuator"

