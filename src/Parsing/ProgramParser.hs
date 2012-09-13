module Parsing.ProgramParser where

import Debug.Trace

import Parsing.Ast
import Parsing.TokenParser
import Parsing.Parsers.Literal
import Parsing.Parsers.TokenHelper

program :: TokenParser Program
program = do
    srcElements <- many sourceElement
    return $ Program srcElements

sourceElement :: TokenParser SourceElement
sourceElement = statementSourceElement -- <|> functionDeclarationSourceElement

statementSourceElement :: TokenParser SourceElement
statementSourceElement = do
    stmt <- statement
    return $ StatementSourceElement stmt

functionDeclarationSourceElement :: TokenParser SourceElement
functionDeclarationSourceElement = undefined

statement :: TokenParser Statement
statement = 
    blockStatement
    <|> emptyStatement
    <|> variableStatement

blockStatement :: TokenParser Statement
blockStatement = do
    b <- block
    return $ BlockStmt b

block :: TokenParser Block
block = do
    leftCurlyBracket
    stmts <- many statement
    rightCurlyBracket
    return $ Block stmts

emptyStatement :: TokenParser Statement
emptyStatement = semicolon >> return EmptyStmt

variableStatement :: TokenParser Statement
variableStatement = do
    var
    varDeclList <- sepBy1 variableDeclaration comma
    semicolon
    return $ VariableStmt varDeclList

variableDeclaration :: TokenParser VariableDeclaration
variableDeclaration = do
    id <- identifierToken
    init <- maybeInitializer
    return $ VariableDeclaration id init

maybeInitializer :: TokenParser MaybeInitializer
maybeInitializer = maybeParse initializer

initializer :: TokenParser Initializer
initializer = do
    assign
    assignExpr <- assignmentExpression
    return $ Initializer assignExpr

assignmentExpression :: TokenParser AssignmentExpression
assignmentExpression = conditionalAssignmentExpression -- <|> assignmentOperatorExpression

conditionalAssignmentExpression :: TokenParser AssignmentExpression
conditionalAssignmentExpression = do
    cond <- conditionalExpression
    return $ ConditionalAssignmentExpression cond

assignmentOperatorExpression :: TokenParser AssignmentExpression
assignmentOperatorExpression = undefined

conditionalExpression :: TokenParser ConditionalExpression
conditionalExpression = logicalOrContionalExpression -- <|> teranaryOperatorConditionalExpression

logicalOrContionalExpression :: TokenParser ConditionalExpression
logicalOrContionalExpression = do
    logicalOr <- logicalOrExpression
    return $ LogicalOrConditionalExpression logicalOr

teranaryOperatorConditionalExpression :: TokenParser ConditionalExpression
teranaryOperatorConditionalExpression = undefined

logicalOrExpression :: TokenParser LogicalOrExpression
logicalOrExpression = unaryLogicalOrExpression -- <|> binaryLogicalOrExpression

unaryLogicalOrExpression :: TokenParser LogicalOrExpression
unaryLogicalOrExpression = do
    logicalAnd <- logicalAndExpression
    return $ UnaryLogicalOrExpression logicalAnd

binaryLogicalOrExpression :: TokenParser LogicalOrExpression
binaryLogicalOrExpression = do
    logicalOr <- logicalOrExpression
    logicalAnd <- logicalAndExpression
    return $ BinaryLogicalOrExpression logicalOr logicalAnd

logicalAndExpression :: TokenParser LogicalAndExpression
logicalAndExpression = unaryLogicalAndExpression -- <|> binaryLogicalAndExpression

unaryLogicalAndExpression :: TokenParser LogicalAndExpression
unaryLogicalAndExpression = do
    bitwiseOr <- bitwiseOrExpression
    return $ UnaryLogicalAndExpression bitwiseOr

binaryLogicalAndExpression :: TokenParser LogicalAndExpression
binaryLogicalAndExpression = do
    logicalAnd <- logicalAndExpression
    bitwiseOr <- bitwiseOrExpression
    return $ BinaryLogicalAndExpression logicalAnd bitwiseOr

bitwiseOrExpression :: TokenParser BitwiseOrExpression
bitwiseOrExpression = unaryBitwiseOrExpression -- <|> binaryBitwiseOrExpression

unaryBitwiseOrExpression :: TokenParser BitwiseOrExpression
unaryBitwiseOrExpression = do
    bitwiseXor <- bitwiseXorExpression
    return $ UnaryBitwiseOrExpression bitwiseXor

binaryBitwiseOrExpression :: TokenParser BitwiseOrExpression
binaryBitwiseOrExpression = do
    bitwiseOr <- bitwiseOrExpression
    bitwiseXor <- bitwiseXorExpression
    return $ BinaryBitwiseOrExpression bitwiseOr bitwiseXor

bitwiseXorExpression :: TokenParser BitwiseXorExpression
bitwiseXorExpression = unaryBitwiseXorExpression -- <|> binaryBitwiseXorExpression

unaryBitwiseXorExpression :: TokenParser BitwiseXorExpression
unaryBitwiseXorExpression = do
    bitwiseAnd <- bitwiseAndExpression
    return $ UnaryBitwiseXorExpression bitwiseAnd

binaryBitwiseXorExpression :: TokenParser BitwiseXorExpression
binaryBitwiseXorExpression = do
    bitwiseXor <- bitwiseXorExpression
    bitwiseAnd <- bitwiseAndExpression
    return $ BinaryBitwiseXorExpression bitwiseXor bitwiseAnd

bitwiseAndExpression :: TokenParser BitwiseAndExpression
bitwiseAndExpression = unaryBitwiseAndExpression -- <|> binaryBitwiseAndExpression

unaryBitwiseAndExpression :: TokenParser BitwiseAndExpression
unaryBitwiseAndExpression = do
    equality <- equalityExpression
    return $ UnaryBitwiseAndExpression equality

binaryBitwiseAndExpression :: TokenParser BitwiseAndExpression
binaryBitwiseAndExpression = do
    bitwiseAnd <- bitwiseAndExpression
    equality <- equalityExpression
    return $ BinaryBitwiseAndExpression bitwiseAnd equality

equalityExpression :: TokenParser EqualityExpression
equalityExpression = 
    relationalEqualityExpression
 --   <|> equalsEqualityExpression
 --   <|> notEqualsEqualityExpression
 --   <|> strictEqualsEqualityExpression
 --   <|> strictNotEqualsEqualityExpression

relationalEqualityExpression :: TokenParser EqualityExpression
relationalEqualityExpression = do
    relation <- relationalExpression
    return $ RelationalEqualityExpression relation

equalsEqualityExpression :: TokenParser EqualityExpression
equalsEqualityExpression = do
    equality <- equalityExpression
    relation <- relationalExpression
    return $ EqualsEqualityExpression equality relation

notEqualsEqualityExpression :: TokenParser EqualityExpression
notEqualsEqualityExpression = do
    equality <- equalityExpression
    relation <- relationalExpression
    return $ NotEqualsEqualityExpression equality relation

strictEqualsEqualityExpression :: TokenParser EqualityExpression
strictEqualsEqualityExpression = do
    equality <- equalityExpression
    relation <- relationalExpression
    return $ StrictEqualsEqualityExpression equality relation

strictNotEqualsEqualityExpression :: TokenParser EqualityExpression
strictNotEqualsEqualityExpression = do
    equality <- equalityExpression
    relation <- relationalExpression
    return $ StrictNotEqualsEqualityExpression equality relation

relationalExpression :: TokenParser RelationalExpression
relationalExpression = 
    shiftRelationalExpression
--    <|> lessThanRelationalExpression
--    <|> greaterThanRelationalExpression
--    <|> lessThanEqualsRelationalExpression
 --   <|> greaterThanEqualsRelationalExpression
 --   <|> instanceOfRelationalExpression
 --   <|> inRelationalExpression

shiftRelationalExpression :: TokenParser RelationalExpression
shiftRelationalExpression = do
    shift <- shiftExpression
    return $ ShiftRelationalExpression shift

lessThanRelationalExpression :: TokenParser RelationalExpression
lessThanRelationalExpression = do
    relation <- relationalExpression
    shift <- shiftExpression
    return $ LessThanRelationalExpression relation shift

greaterThanRelationalExpression :: TokenParser RelationalExpression
greaterThanRelationalExpression = do
    relation <- relationalExpression
    shift <- shiftExpression
    return $ GreaterThanRelationalExpression relation shift

lessThanEqualsRelationalExpression :: TokenParser RelationalExpression
lessThanEqualsRelationalExpression = do
    relation <- relationalExpression
    shift <- shiftExpression
    return $ LessThanEqualsRelationalExpression relation shift

greaterThanEqualsRelationalExpression :: TokenParser RelationalExpression
greaterThanEqualsRelationalExpression = do
    relation <- relationalExpression
    shift <- shiftExpression
    return $ GreaterThanEqualsRelationalExpression relation shift

instanceOfRelationalExpression :: TokenParser RelationalExpression
instanceOfRelationalExpression = do
    relation <- relationalExpression
    shift <- shiftExpression
    return $ InstanceOfRelationalExpression relation shift

inRelationalExpression :: TokenParser RelationalExpression
inRelationalExpression = do
    relation <- relationalExpression
    shift <- shiftExpression
    return $ InRelationalExpression relation shift

shiftExpression :: TokenParser ShiftExpression
shiftExpression = 
    additiveShiftExpression
--    <|> leftShiftExpression
--    <|> rightShiftExpression
--    <|> unsignedRightShiftExpression

additiveShiftExpression :: TokenParser ShiftExpression
additiveShiftExpression = do
    additive <- additiveExpression
    return $ AdditiveShiftExpression additive

leftShiftExpression :: TokenParser ShiftExpression
leftShiftExpression = do
    shift <- shiftExpression
    additive <- additiveExpression
    return $ LeftShiftExpression shift additive

rightShiftExpression :: TokenParser ShiftExpression
rightShiftExpression = do
    shift <- shiftExpression
    additive <- additiveExpression
    return $ RightShiftExpression shift additive    

unsignedRightShiftExpression :: TokenParser ShiftExpression
unsignedRightShiftExpression = do
    shift <- shiftExpression
    additive <- additiveExpression
    return $ UnsignedRightShiftExpression shift additive

additiveExpression :: TokenParser AdditiveExpression
additiveExpression = 
    multAdditiveExpression
--    <|> plusAdditiveExpression
--    <|> minusAdditiveExpression

multAdditiveExpression :: TokenParser AdditiveExpression
multAdditiveExpression = do
    mult <- multiplicativeExpression
    return $ MultAdditiveExpression mult

plusAdditiveExpression :: TokenParser AdditiveExpression
plusAdditiveExpression = do
    additive <- additiveExpression
    mult <- multiplicativeExpression
    return $ PlusAdditiveExpression additive mult

minusAdditiveExpression :: TokenParser AdditiveExpression
minusAdditiveExpression = do
    additive <- additiveExpression
    mult <- multiplicativeExpression
    return $ MinusAdditiveExpression additive mult

multiplicativeExpression :: TokenParser MultiplicativeExpression
multiplicativeExpression = 
    unaryMultiplicativeExpression
--    <|> mulMultiplicativeExpression
--    <|> divMultiplicativeExpression
--    <|> modulusMultiplicativeExpression

unaryMultiplicativeExpression :: TokenParser MultiplicativeExpression
unaryMultiplicativeExpression = do
    unary <- unaryExpression
    return $ UnaryMultiplicativeExpression unary

mulMultiplicativeExpression :: TokenParser MultiplicativeExpression
mulMultiplicativeExpression = do
    mult <- multiplicativeExpression
    unary <- unaryExpression
    return $ MulMultiplicativeExpression mult unary

divMultiplicativeExpression :: TokenParser MultiplicativeExpression
divMultiplicativeExpression = do
    mult <- multiplicativeExpression
    unary <- unaryExpression
    return $ DivMultiplicativeExpression mult unary

modulusMultiplicativeExpression :: TokenParser MultiplicativeExpression
modulusMultiplicativeExpression = do
    mult <- multiplicativeExpression
    unary <- unaryExpression
    return $ ModulusMultiplicativeExpression mult unary

unaryExpression :: TokenParser UnaryExpression
unaryExpression = 
    postfixUnaryExpression
--    <|> deleteUnaryExpression
 --   <|> voidUnaryExpression
 --   <|> typeOfUnaryExpression
 --   <|> incrementPlusUnaryExpression
--    <|> incrementMinusUnaryExpression
--    <|> plusUnaryExpression
--    <|> minusUnaryExpression
--    <|> bitwiseNotUnaryExpression
--    <|> logicalNotUnaryExpression

postfixUnaryExpression :: TokenParser UnaryExpression
postfixUnaryExpression = do
    postfix <- postfixExpression
    return $ PostfixUnaryExpression postfix

deleteUnaryExpression :: TokenParser UnaryExpression
deleteUnaryExpression = do
    unary <- unaryExpression
    return $ DeleteUnaryExpression unary

voidUnaryExpression :: TokenParser UnaryExpression
voidUnaryExpression = do
    unary <- unaryExpression
    return $ VoidUnaryExpression unary

typeOfUnaryExpression :: TokenParser UnaryExpression
typeOfUnaryExpression = do
    unary <- unaryExpression
    return $ TypeOfUnaryExpression unary

incrementPlusUnaryExpression :: TokenParser UnaryExpression
incrementPlusUnaryExpression = do
    unary <- unaryExpression
    return $ IncrementPlusUnaryExpression unary

incrementMinusUnaryExpression :: TokenParser UnaryExpression
incrementMinusUnaryExpression = do
    unary <- unaryExpression
    return $ IncrementMinusUnaryExpression unary

plusUnaryExpression :: TokenParser UnaryExpression
plusUnaryExpression = do
    unary <- unaryExpression
    return $ PlusUnaryExpression unary

minusUnaryExpression :: TokenParser UnaryExpression
minusUnaryExpression = do
    unary <- unaryExpression
    return $ MinusUnaryExpression unary

bitwiseNotUnaryExpression :: TokenParser UnaryExpression
bitwiseNotUnaryExpression = do
    unary <- unaryExpression
    return $ BitwiseNotUnaryExpression unary

logicalNotUnaryExpression :: TokenParser UnaryExpression
logicalNotUnaryExpression = do
    unary <- unaryExpression
    return $ LogicalNotUnaryExpression unary

postfixExpression :: TokenParser PostfixExpression
postfixExpression = 
    lhsPostfixExpression
--    <|> incrementPlusPostfixExpression
--    <|> incrementMinusPostfixExpression

lhsPostfixExpression :: TokenParser PostfixExpression
lhsPostfixExpression = do 
    lhs <- leftHandSideExpression
    return $ LHSPostfixExpression lhs

incrementPlusPostfixExpression :: TokenParser PostfixExpression
incrementPlusPostfixExpression = do 
    lhs <- leftHandSideExpression
    return $ IncrementPlusPostfixExpression lhs

incrementMinusPostfixExpression :: TokenParser PostfixExpression
incrementMinusPostfixExpression = do 
    lhs <- leftHandSideExpression
    return $ IncrementMinusPostfixExpression lhs

leftHandSideExpression :: TokenParser LeftHandSideExpression
leftHandSideExpression = 
    newLHSExpression
--    <|> callLHSExpression

newLHSExpression :: TokenParser LeftHandSideExpression
newLHSExpression = do
    new <- newExpression
    return $ NewLHSExpression new

callLHSExpression :: TokenParser LeftHandSideExpression
callLHSExpression = do
    call <- callExpression
    return $ CallLHSExpression call

newExpression :: TokenParser NewExpression
newExpression = 
    memberNewExpression
--    <|> newNewExpression

memberNewExpression :: TokenParser NewExpression
memberNewExpression = do
    member <- memberExpression
    return $ MemberNewExpression member

memberExpression :: TokenParser MemberExpression
memberExpression = memberExpression'
--      try primaryMemberExpression
--    <|> functionMemberExpression
--      <|> try propertyAccessByBracketsMemberExpression
--    <|> propertyAccessByDotMemberExpression
--    <|> newMemberExpression
--      <?> "MemberExpression"

-- left recursion... BURN IN HELL!!
memberExpression' :: TokenParser MemberExpression
memberExpression' = do
    primary <- primaryMemberExpression <|> functionMemberExpression
    buildFunc <- restOfMemberExpression
    return $ buildFunc primary

restOfMemberExpression :: TokenParser (MemberExpression -> MemberExpression)
restOfMemberExpression = try nonEmptyRestOfMemberExpression <|> emptyRestOfMemberExpression

nonEmptyRestOfMemberExpression :: TokenParser (MemberExpression -> MemberExpression)
nonEmptyRestOfMemberExpression = 
    restOfAccessByBracketMemberExpression
    <|> restOfAccessByDotMemberExpression

restOfAccessByBracketMemberExpression :: TokenParser (MemberExpression -> MemberExpression)
restOfAccessByBracketMemberExpression = do
    leftSquareBracket
    expr <- expression
    rightSquareBracket
    buildFunc <- restOfMemberExpression
    return $ \ memberExpr -> buildFunc $ PropertyAccessByBracketsMemberExpression memberExpr expr

restOfAccessByDotMemberExpression :: TokenParser (MemberExpression -> MemberExpression)
restOfAccessByDotMemberExpression = do
    dot
    id <- identifierToken
    buildFunc <- restOfMemberExpression
    return $ \ memberExpr -> buildFunc $ PropertyAccessByDotMemberExpression memberExpr id

emptyRestOfMemberExpression :: TokenParser (MemberExpression -> MemberExpression)
emptyRestOfMemberExpression = return (\ primary -> primary)

functionMemberExpression :: TokenParser MemberExpression
functionMemberExpression = do
    func <- functionExpression
    return $ FunctionMemberExpression func

functionExpression :: TokenParser FunctionExpression
functionExpression = do
    function
    name <- maybeParse identifierToken
    leftRoundBracket
    params <- sepBy identifierToken comma
    rightRoundBracket
    body <- between leftCurlyBracket rightCurlyBracket functionBody
    return $ FunctionExpression name params body

propertyAccessByDotMemberExpression :: TokenParser MemberExpression
propertyAccessByDotMemberExpression = undefined

newMemberExpression :: TokenParser MemberExpression
newMemberExpression = undefined

primaryMemberExpression :: TokenParser MemberExpression
primaryMemberExpression = do
    primary <- primaryExpression
    return $ PrimaryMemberExpression primary

newNewExpression :: TokenParser NewExpression
newNewExpression = undefined

callExpression :: TokenParser CallExpression
callExpression = undefined

primaryExpression :: TokenParser PrimaryExpression
primaryExpression = 
    thisPrimaryExpression
    <|> identifierPrimaryExpression
    <|> literalPrimaryExpression
    <|> arrayLiteralPrimaryExpression
    <|> objectLiteralPrimaryExpression
    <|> expressionPrimaryExpression
    <?> "PrimaryExpression"

expressionPrimaryExpression :: TokenParser PrimaryExpression
expressionPrimaryExpression = do
    expr <- between leftRoundBracket rightRoundBracket expression
    return $ ExpressionPrimaryExpression expr

expression :: TokenParser Expression
expression = do
    assigns <- sepBy1 assignmentExpression comma
    return $ Expression assigns

identifierPrimaryExpression :: TokenParser PrimaryExpression
identifierPrimaryExpression = do
    str <- identifierToken
    return $ IdentifierPrimaryExpression str

thisPrimaryExpression :: TokenParser PrimaryExpression
thisPrimaryExpression = this >> return ThisPrimaryExpression

literalPrimaryExpression :: TokenParser PrimaryExpression
literalPrimaryExpression = do
    lit <- literal
    return $ LiteralPrimaryExpression lit

arrayLiteralPrimaryExpression :: TokenParser PrimaryExpression
arrayLiteralPrimaryExpression = do
    arr <- arrayLiteral
    return $ ArrayLiteralPrimaryExpression arr

arrayLiteral :: TokenParser ArrayLiteral
arrayLiteral = do
    leftSquareBracket
    elements <- arrayLiteralElements
    rightSquareBracket
    return $ ArrayLiteral elements

arrayLiteralElements :: TokenParser [MaybeAssignmentExpression]
arrayLiteralElements = do
    assignments <- sepBy (maybeParse assignmentExpression) comma
    if null assignments
        then return []
        else case last assignments of
            Nothing -> return $ reverse $ drop 1 (reverse assignments)
            otherwise -> return assignments

objectLiteralPrimaryExpression :: TokenParser PrimaryExpression
objectLiteralPrimaryExpression = do
    leftCurlyBracket
    properties <- sepEndBy propertyAssignment comma
    rightCurlyBracket
    return $ ObjectLiteralPrimaryExpression $ ObjectLiteral properties

propertyAssignment :: TokenParser PropertyAssignment
propertyAssignment = 
    try fieldPropertyAssignment
    <|> getterPropertyAssignment
    <|> setterPropertyAssignment
    <?> "PropertyAssignment"

fieldPropertyAssignment :: TokenParser PropertyAssignment
fieldPropertyAssignment = do
    name <- propertyName
    colon
    assignment <- assignmentExpression
    return $ FieldPropertyAssignment name assignment

propertyName :: TokenParser PropertyName
propertyName = 
    stringPropertyName 
    <|> numericPropertyName
    <?> "PropertyName"

stringPropertyName :: TokenParser PropertyName
stringPropertyName = do
    id <- identifierName <|> stringLiteralToken
    return $ StringPropertyName id

numericPropertyName :: TokenParser PropertyName
numericPropertyName = do
    num <- numericLiteralToken
    return $ NumericPropertyName num

getterPropertyAssignment :: TokenParser PropertyAssignment
getterPropertyAssignment = do
    get
    name <- propertyName
    roundBrackets
    leftCurlyBracket
    body <- functionBody
    rightCurlyBracket
    return $ GetterPropertyAssignment name body

setterPropertyAssignment :: TokenParser PropertyAssignment
setterPropertyAssignment = do
    set
    name <- propertyName
    leftRoundBracket
    param <- identifierToken
    rightRoundBracket
    leftCurlyBracket
    body <- functionBody
    rightCurlyBracket
    return $ SetterPropertyAssignment name param body

functionBody :: TokenParser FunctionBody
functionBody = do
    srcElements <- many sourceElement
    return $ FunctionBody srcElements



