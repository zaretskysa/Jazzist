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
sourceElement = statementSourceElement <|> functionDeclarationSourceElement

statementSourceElement :: TokenParser SourceElement
statementSourceElement = do
    stmt <- statement
    return $ StatementSourceElement stmt

functionDeclarationSourceElement :: TokenParser SourceElement
functionDeclarationSourceElement = do
    function
    id <- identifierToken
    leftRoundBracket
    params <- sepBy identifierToken comma
    rightRoundBracket
    body <- between leftCurlyBracket rightCurlyBracket functionBody
    return $ FunctionDeclarationSourceElement id params body

statement :: TokenParser Statement
statement = 
    blockStatement
    <|> emptyStatement
    <|> variableStatement
    <|> try expressionStatement
    <|> ifStatement
    <|> iterationStatement
    <|> continueStatement
    <|> breakStatement
    <|> returnStatement
    <|> withStatement
    <|> switchStatement
    <|> labelledStatement

labelledStatement :: TokenParser Statement
labelledStatement = do
    id <- identifierToken
    colon
    stmt <- statement
    return $ LabelledStmt id stmt

switchStatement :: TokenParser Statement
switchStatement = do
    switchKeyword
    expr <- between leftRoundBracket rightRoundBracket expression
    block <- caseBlock
    return $ SwitchStmt expr block

caseBlock :: TokenParser CaseBlock
caseBlock = do
    leftCurlyBracket
    beginClauses <- many caseClause
    defaultClause <- maybeParse defaultClause
    endClauses <- many caseClause
    rightCurlyBracket
    return $ CaseBlock beginClauses defaultClause endClauses

caseClause :: TokenParser CaseClause
caseClause = do
    caseKeyword
    expr <- expression
    colon
    stmts <- many statement
    return $ CaseClause expr stmts

defaultClause :: TokenParser DefaultClause
defaultClause = do
    defaultKeyword >> colon
    stmts <- many statement
    return $ DefaultClause stmts

withStatement :: TokenParser Statement
withStatement = do
    withKeyword
    expr <- between leftRoundBracket rightRoundBracket expression
    stmt <- statement
    return $ WithStmt expr stmt

returnStatement :: TokenParser Statement
returnStatement = do
    returnKeyword
    expr <- maybeParse expression --TODO: no line termimator here
    semicolon
    return $ ReturnStmt expr

breakStatement :: TokenParser Statement
breakStatement = do
    breakKeyword
    id <- maybeParse identifierToken --TODO: no line termimator here
    semicolon
    return $ BreakStmt id

continueStatement :: TokenParser Statement
continueStatement = do
    continueKeyword
    id <- maybeParse identifierToken --TODO: no line termimator here
    semicolon
    return $ ContinueStmt id

iterationStatement :: TokenParser Statement
iterationStatement = 
    doWhileIterationStatement
    <|> whileIterationStatement
    <|> try exprTripletForIterationStatement
    <|> try varAndDoubleExprForIterationStatement
    <|> try lhsExprInExprForIterationStatement
    <|> varInExprIteratioinStatement

doWhileIterationStatement :: TokenParser Statement
doWhileIterationStatement = do
    doKeyword
    stmt <- statement
    while
    expr <- between leftRoundBracket rightRoundBracket expression
    semicolon
    return $ IterationStmt $ DoWhileIterationStatement stmt expr

whileIterationStatement :: TokenParser Statement
whileIterationStatement = do
    while
    expr <- between leftRoundBracket rightRoundBracket expression
    stmt <- statement
    return $ IterationStmt $ WhileIterationStatement expr stmt

exprTripletForIterationStatement :: TokenParser Statement
exprTripletForIterationStatement = do
    forKeyword
    leftRoundBracket
    expr1 <- maybeExpression -- TODO: no in
    semicolon
    expr2 <- maybeExpression
    semicolon
    expr3 <- maybeExpression
    rightRoundBracket
    stmt <- statement
    return $ IterationStmt $ ExprTripletForIterationStatement expr1 expr2 expr3 stmt

maybeExpression :: TokenParser MaybeExpression
maybeExpression = maybeParse expression

varAndDoubleExprForIterationStatement :: TokenParser Statement
varAndDoubleExprForIterationStatement = do
    forKeyword
    leftRoundBracket
    var
    varDecls <- variableDeclarationList --TODO: VariableDeclarationListNoIn
    semicolon
    expr1 <- maybeExpression
    semicolon
    expr2 <- maybeExpression
    rightRoundBracket
    stmt <- statement
    return $ IterationStmt $ VarAndDoubleExprForIterationStatement varDecls expr1 expr2 stmt


variableDeclarationList :: TokenParser [VariableDeclaration]
variableDeclarationList = sepBy1 variableDeclaration comma

lhsExprInExprForIterationStatement :: TokenParser Statement
lhsExprInExprForIterationStatement = do
    forKeyword
    leftRoundBracket
    lhs <- leftHandSideExpression
    inKeyword
    expr <- expression
    rightRoundBracket
    stmt <- statement
    return $ IterationStmt $ LHSExprInExprForIterationStatement lhs expr stmt

varInExprIteratioinStatement :: TokenParser Statement
varInExprIteratioinStatement = do
    forKeyword
    leftRoundBracket
    var
    varDecl <- variableDeclaration --TODO: VariableDeclarationNoIn
    inKeyword
    expr <- expression
    rightRoundBracket
    stmt <- statement
    return $ IterationStmt $ VarInExprIteratioinStatement varDecl expr stmt

ifStatement :: TokenParser Statement
ifStatement = do
    ifKeyword
    expr <- between leftRoundBracket rightRoundBracket expression
    stmt1 <- statement
    stmt2 <- maybeParse (elseKeyword >> statement)
    return $ IfStmt expr stmt1 stmt2

expressionStatement :: TokenParser Statement
expressionStatement = do
    try $ notFollowedBy leftCurlyBracket
    try $ notFollowedBy function
    expr <- expression
    semicolon
    return $ ExpressionStmt expr

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
assignmentExpression = 
    try assignmentOperatorExpression
    <|> conditionalAssignmentExpression 
    <?> "AssignmentExpression"

conditionalAssignmentExpression :: TokenParser AssignmentExpression
conditionalAssignmentExpression = do
    cond <- conditionalExpression
    return $ ConditionalAssignmentExpression cond

assignmentOperatorExpression :: TokenParser AssignmentExpression
assignmentOperatorExpression = do
    lhs <- leftHandSideExpression
    op <- assignmentOperator
    assignExpr <- assignmentExpression
    return $ AssignmentOperatorExpression lhs op assignExpr

assignmentOperator :: TokenParser AssignmentOperator
assignmentOperator = do
    op <- oneOfAssignmentOperators
    case op of 
        AssignPunctuator -> return SingleAssignOperator
        MulAssignPunctuator -> return MulAssignOperator
        DivAssignPunctuator -> return DivAssignOperator
        ModulusAssignPunctuator -> return ModulusAssignOperator
        PlusAssignPunctuator -> return PlusAssignOperator
        MinusAssignPunctuator -> return MinusAssignOperator
        LeftShiftAssignPunctuator -> return LeftShiftAssignOperator
        RightShiftAssignPunctuator -> return RightShiftAssignOperator
        UnsignedRightShiftAssignPunctuator -> return UnsignedRightShiftAssignOperator
        BitwiseAndAssignPunctuator -> return BitwiseAndAssignOperator
        BitwiseXorAssignPunctuator -> return BitwiseXorAssignOperator
        BitwiseOrAssignPunctuator -> return BitwiseOrAssignOperator
        _ -> fail "incorrect assign operator"

conditionalExpression :: TokenParser ConditionalExpression
conditionalExpression = 
    try teranaryOperatorConditionalExpression
    <|> logicalOrContionalExpression

logicalOrContionalExpression :: TokenParser ConditionalExpression
logicalOrContionalExpression = do
    logicalOr <- logicalOrExpression
    return $ LogicalOrConditionalExpression logicalOr

teranaryOperatorConditionalExpression :: TokenParser ConditionalExpression
teranaryOperatorConditionalExpression = do
    logicalOr <- logicalOrExpression
    questionMark
    assign1 <- assignmentExpression
    colon
    assign2 <- assignmentExpression
    return $ TeranaryOperatorConditionalExpression logicalOr assign1 assign2

logicalOrExpression :: TokenParser LogicalOrExpression
logicalOrExpression = do
    unary <- unaryLogicalOrExpression
    buildRestOfLogicalOrExpression unary

buildRestOfLogicalOrExpression :: LogicalOrExpression -> TokenParser LogicalOrExpression
buildRestOfLogicalOrExpression left = 
    try $ nonEmptyRestOfLogicalOrExpression left
    <|> emptyRestOfLogicalOrExpression left

nonEmptyRestOfLogicalOrExpression :: LogicalOrExpression -> TokenParser LogicalOrExpression
nonEmptyRestOfLogicalOrExpression left = do
    logicalOr
    logicalAnd <- logicalAndExpression
    buildRestOfLogicalOrExpression $ BinaryLogicalOrExpression left logicalAnd

emptyRestOfLogicalOrExpression :: LogicalOrExpression -> TokenParser LogicalOrExpression
emptyRestOfLogicalOrExpression left = return left

unaryLogicalOrExpression :: TokenParser LogicalOrExpression
unaryLogicalOrExpression = do
    logicalAnd <- logicalAndExpression
    return $ UnaryLogicalOrExpression logicalAnd

logicalAndExpression :: TokenParser LogicalAndExpression
logicalAndExpression = do
    unary <- unaryLogicalAndExpression
    buildRestOfLogicalAndExpression unary

buildRestOfLogicalAndExpression :: LogicalAndExpression -> TokenParser LogicalAndExpression
buildRestOfLogicalAndExpression left =
    try $ nonEmptyRestOfLogicalAndExpression left
    <|> emptyRestOfLogicalAndExpression left

nonEmptyRestOfLogicalAndExpression :: LogicalAndExpression -> TokenParser LogicalAndExpression
nonEmptyRestOfLogicalAndExpression left = do
    logicalAnd
    bitwiseOr <- bitwiseOrExpression
    buildRestOfLogicalAndExpression $ BinaryLogicalAndExpression left bitwiseOr

emptyRestOfLogicalAndExpression :: LogicalAndExpression -> TokenParser LogicalAndExpression
emptyRestOfLogicalAndExpression left = return left

unaryLogicalAndExpression :: TokenParser LogicalAndExpression
unaryLogicalAndExpression = do
    bitwiseOr <- bitwiseOrExpression
    return $ UnaryLogicalAndExpression bitwiseOr

bitwiseOrExpression :: TokenParser BitwiseOrExpression
bitwiseOrExpression = do 
    bitwiseXor <- unaryBitwiseOrExpression
    buildRestOfBitwiseOrExpression bitwiseXor

buildRestOfBitwiseOrExpression :: BitwiseOrExpression -> TokenParser BitwiseOrExpression
buildRestOfBitwiseOrExpression left =
    try $ nonEmptyRestOfBitwiseOrExpression left
    <|> emptyRestOfBitwiseOrExpression left

nonEmptyRestOfBitwiseOrExpression :: BitwiseOrExpression -> TokenParser BitwiseOrExpression
nonEmptyRestOfBitwiseOrExpression left = do
    bitwiseOr
    bitwiseXor <- bitwiseXorExpression
    buildRestOfBitwiseOrExpression $ BinaryBitwiseOrExpression left bitwiseXor

emptyRestOfBitwiseOrExpression :: BitwiseOrExpression -> TokenParser BitwiseOrExpression
emptyRestOfBitwiseOrExpression left = return left

unaryBitwiseOrExpression :: TokenParser BitwiseOrExpression
unaryBitwiseOrExpression = do
    bitwiseXor <- bitwiseXorExpression
    return $ UnaryBitwiseOrExpression bitwiseXor

bitwiseXorExpression :: TokenParser BitwiseXorExpression
bitwiseXorExpression = do
    bitwiseAnd <- unaryBitwiseXorExpression
    buildRestOfBitwiseXorExpression bitwiseAnd

buildRestOfBitwiseXorExpression :: BitwiseXorExpression -> TokenParser BitwiseXorExpression
buildRestOfBitwiseXorExpression left = 
    try $ nonEmptyRestOfBitwiseXorExpression left
    <|> emptyRestOfBitwiseXorExpresssion left

nonEmptyRestOfBitwiseXorExpression :: BitwiseXorExpression -> TokenParser BitwiseXorExpression
nonEmptyRestOfBitwiseXorExpression left = do
    bitwiseXor
    bitwiseAnd <- bitwiseAndExpression
    buildRestOfBitwiseXorExpression $ BinaryBitwiseXorExpression left bitwiseAnd

emptyRestOfBitwiseXorExpresssion :: BitwiseXorExpression -> TokenParser BitwiseXorExpression
emptyRestOfBitwiseXorExpresssion left = return left

unaryBitwiseXorExpression :: TokenParser BitwiseXorExpression
unaryBitwiseXorExpression = do
    bitwiseAnd <- bitwiseAndExpression
    return $ UnaryBitwiseXorExpression bitwiseAnd

bitwiseAndExpression :: TokenParser BitwiseAndExpression
bitwiseAndExpression = do
    equality <- unaryBitwiseAndExpression
    buildRestOfBitwiseAndExpresssion equality

buildRestOfBitwiseAndExpresssion :: BitwiseAndExpression -> TokenParser BitwiseAndExpression
buildRestOfBitwiseAndExpresssion left = 
    try $ nonEmptyRestOfBitwiseAndExpression left
    <|> emptyRestOfBitwiseAndExpression left

nonEmptyRestOfBitwiseAndExpression :: BitwiseAndExpression -> TokenParser BitwiseAndExpression
nonEmptyRestOfBitwiseAndExpression left = do
    bitwiseAnd
    equality <- equalityExpression
    buildRestOfBitwiseAndExpresssion $ BinaryBitwiseAndExpression left equality

emptyRestOfBitwiseAndExpression :: BitwiseAndExpression -> TokenParser BitwiseAndExpression
emptyRestOfBitwiseAndExpression left = return left

unaryBitwiseAndExpression :: TokenParser BitwiseAndExpression
unaryBitwiseAndExpression = do
    equality <- equalityExpression
    return $ UnaryBitwiseAndExpression equality

equalityExpression :: TokenParser EqualityExpression
equalityExpression = do
    relational <- relationalEqualityExpression
    buildRestOfEqualityExpression relational
 --   <|> equalsEqualityExpression
 --   <|> notEqualsEqualityExpression
 --   <|> strictEqualsEqualityExpression
 --   <|> strictNotEqualsEqualityExpression

buildRestOfEqualityExpression :: EqualityExpression -> TokenParser EqualityExpression
buildRestOfEqualityExpression left = 
    try $ nonEmptyRestOfEqualityExpression left
    <|> emptyRestOfEqualityExpression left

nonEmptyRestOfEqualityExpression :: EqualityExpression -> TokenParser EqualityExpression
nonEmptyRestOfEqualityExpression left = 
    restOfEqualsEqualityExpression left
    <|> restOfNotEqualsEqualityExpression left
    <|> restOfStrictEqualsEqualityExpression left
    <|> restOfStrictNotEqualsEqualityExpression left

restOfEqualsEqualityExpression :: EqualityExpression -> TokenParser EqualityExpression
restOfEqualsEqualityExpression left = do
    equals
    relational <- relationalExpression
    buildRestOfEqualityExpression $ EqualsEqualityExpression left relational

restOfNotEqualsEqualityExpression :: EqualityExpression -> TokenParser EqualityExpression
restOfNotEqualsEqualityExpression left = do
    notEquals
    relational <- relationalExpression
    buildRestOfEqualityExpression $ NotEqualsEqualityExpression left relational

restOfStrictEqualsEqualityExpression :: EqualityExpression -> TokenParser EqualityExpression
restOfStrictEqualsEqualityExpression left = do
    strictEquals
    relational <- relationalExpression
    buildRestOfEqualityExpression $ StrictEqualsEqualityExpression left relational

restOfStrictNotEqualsEqualityExpression :: EqualityExpression -> TokenParser EqualityExpression
restOfStrictNotEqualsEqualityExpression left = do
    strictNotEquals
    relational <- relationalExpression
    buildRestOfEqualityExpression $ StrictNotEqualsEqualityExpression left relational

emptyRestOfEqualityExpression :: EqualityExpression -> TokenParser EqualityExpression
emptyRestOfEqualityExpression left = return left

relationalEqualityExpression :: TokenParser EqualityExpression
relationalEqualityExpression = do
    relation <- relationalExpression
    return $ RelationalEqualityExpression relation

relationalExpression :: TokenParser RelationalExpression
relationalExpression = do
    shift <- shiftRelationalExpression
    buildRestOfRelationalExpression shift

buildRestOfRelationalExpression :: RelationalExpression -> TokenParser RelationalExpression
buildRestOfRelationalExpression left =
    try $ nonEmptyRestOfRelationalExpression left
    <|> emptyRestOfRelationalExpression left

nonEmptyRestOfRelationalExpression :: RelationalExpression -> TokenParser RelationalExpression
nonEmptyRestOfRelationalExpression left =
    restOfLessThanRelationalExpression left
    <|> restOfGreaterThanRelationalExpression left
    <|> restOfLessThanEqualsRelationalExpression left
    <|> restOfGreaterThanEqualsRelationalExpression left
    <|> restOfInstanceOfRelationalExpression left
    <|> restOfInRelationalExpression left

restOfLessThanRelationalExpression :: RelationalExpression -> TokenParser RelationalExpression
restOfLessThanRelationalExpression left = do
    lessThan
    shift <- shiftExpression
    buildRestOfRelationalExpression $ LessThanRelationalExpression left shift

restOfGreaterThanRelationalExpression :: RelationalExpression -> TokenParser RelationalExpression
restOfGreaterThanRelationalExpression left = do
    greaterThan
    shift <- shiftExpression
    buildRestOfRelationalExpression $ GreaterThanRelationalExpression left shift

restOfLessThanEqualsRelationalExpression :: RelationalExpression -> TokenParser RelationalExpression
restOfLessThanEqualsRelationalExpression left = do
    lessThanEquals
    shift <- shiftExpression
    buildRestOfRelationalExpression $ LessThanEqualsRelationalExpression left shift

restOfGreaterThanEqualsRelationalExpression :: RelationalExpression -> TokenParser RelationalExpression
restOfGreaterThanEqualsRelationalExpression left = do
    greaterThanEquals
    shift <- shiftExpression
    buildRestOfRelationalExpression $ GreaterThanEqualsRelationalExpression left shift

restOfInstanceOfRelationalExpression :: RelationalExpression -> TokenParser RelationalExpression
restOfInstanceOfRelationalExpression left = do
    instanceOf
    shift <- shiftExpression
    buildRestOfRelationalExpression $ InstanceOfRelationalExpression left shift

restOfInRelationalExpression :: RelationalExpression -> TokenParser RelationalExpression
restOfInRelationalExpression left = do
    inKeyword
    shift <- shiftExpression
    buildRestOfRelationalExpression $ InRelationalExpression left shift

emptyRestOfRelationalExpression :: RelationalExpression -> TokenParser RelationalExpression
emptyRestOfRelationalExpression left = return left

shiftRelationalExpression :: TokenParser RelationalExpression
shiftRelationalExpression = do
    shift <- shiftExpression
    return $ ShiftRelationalExpression shift

shiftExpression :: TokenParser ShiftExpression
shiftExpression = do
    left <- additiveShiftExpression
    buildRestOfShiftExpression left

buildRestOfShiftExpression :: ShiftExpression -> TokenParser ShiftExpression
buildRestOfShiftExpression left = 
    try $ nonEmptyRestOfShiftExpression left
    <|> emptyRestOfShiftExpression left

nonEmptyRestOfShiftExpression :: ShiftExpression -> TokenParser ShiftExpression
nonEmptyRestOfShiftExpression left = 
    restOfLeftShiftExpression left
    <|> restOfRightShiftExpression left
    <|> restOfUnsignedRightShiftExpression left

restOfLeftShiftExpression :: ShiftExpression -> TokenParser ShiftExpression
restOfLeftShiftExpression left = do
    leftShift
    additive <- additiveExpression
    buildRestOfShiftExpression $ LeftShiftExpression left additive

restOfRightShiftExpression :: ShiftExpression -> TokenParser ShiftExpression
restOfRightShiftExpression left = do
    rightShift
    additive <- additiveExpression
    buildRestOfShiftExpression $ RightShiftExpression left additive

restOfUnsignedRightShiftExpression :: ShiftExpression -> TokenParser ShiftExpression
restOfUnsignedRightShiftExpression left = do
    unsignedRightShift
    additive <- additiveExpression
    buildRestOfShiftExpression $ UnsignedRightShiftExpression left additive

emptyRestOfShiftExpression :: ShiftExpression -> TokenParser ShiftExpression
emptyRestOfShiftExpression left = return left

additiveShiftExpression :: TokenParser ShiftExpression
additiveShiftExpression = do
    additive <- additiveExpression
    return $ AdditiveShiftExpression additive

additiveExpression :: TokenParser AdditiveExpression
additiveExpression = do
    left <- multAdditiveExpression
    buildRestOfAdditiveExpression left

buildRestOfAdditiveExpression :: AdditiveExpression -> TokenParser AdditiveExpression
buildRestOfAdditiveExpression left = 
    try $ nonEmptyRestOfAdditiveExpression left
    <|> emptyRestOfAdditiveExpression left

nonEmptyRestOfAdditiveExpression :: AdditiveExpression -> TokenParser AdditiveExpression
nonEmptyRestOfAdditiveExpression left = 
    plusRestOfAdditiveExpression left
    <|> minusRestOfAdditiveExpression left

plusRestOfAdditiveExpression :: AdditiveExpression -> TokenParser AdditiveExpression
plusRestOfAdditiveExpression left = do
    plus
    multExpr <- multiplicativeExpression
    buildRestOfAdditiveExpression $ PlusAdditiveExpression left multExpr

minusRestOfAdditiveExpression :: AdditiveExpression -> TokenParser AdditiveExpression
minusRestOfAdditiveExpression left = do
    minus
    multExpr <- multiplicativeExpression
    buildRestOfAdditiveExpression $ MinusAdditiveExpression left multExpr

emptyRestOfAdditiveExpression :: AdditiveExpression -> TokenParser AdditiveExpression
emptyRestOfAdditiveExpression left = return left

multAdditiveExpression :: TokenParser AdditiveExpression
multAdditiveExpression = do
    mult <- multiplicativeExpression
    return $ MultAdditiveExpression mult

multiplicativeExpression :: TokenParser MultiplicativeExpression
multiplicativeExpression = do
    left <- unaryMultiplicativeExpression
    buildRestOfMultiplicativeExpression left

buildRestOfMultiplicativeExpression :: MultiplicativeExpression -> TokenParser MultiplicativeExpression
buildRestOfMultiplicativeExpression left = 
    try $ nonEmptyRestOfMultiplicativeExpression left
    <|> emptyRestOfMultiplicativeExpression left

emptyRestOfMultiplicativeExpression :: MultiplicativeExpression -> TokenParser MultiplicativeExpression
emptyRestOfMultiplicativeExpression left = return left

nonEmptyRestOfMultiplicativeExpression :: MultiplicativeExpression -> TokenParser MultiplicativeExpression
nonEmptyRestOfMultiplicativeExpression left = 
    mulRestOfMultiplicativeExpression left
    <|> divRestOfMultiplicativeExpression left
    <|> modulusRestOfMultiplicativeExpression left

mulRestOfMultiplicativeExpression :: MultiplicativeExpression -> TokenParser MultiplicativeExpression
mulRestOfMultiplicativeExpression left = do
    mul
    unary <- unaryExpression
    buildRestOfMultiplicativeExpression $ MulMultiplicativeExpression left unary

divRestOfMultiplicativeExpression :: MultiplicativeExpression -> TokenParser MultiplicativeExpression
divRestOfMultiplicativeExpression left = do
    divOp
    unary <- unaryExpression
    buildRestOfMultiplicativeExpression $ DivMultiplicativeExpression left unary

modulusRestOfMultiplicativeExpression :: MultiplicativeExpression -> TokenParser MultiplicativeExpression
modulusRestOfMultiplicativeExpression left = do
    modulus
    unary <- unaryExpression
    buildRestOfMultiplicativeExpression $ ModulusMultiplicativeExpression left unary

unaryMultiplicativeExpression :: TokenParser MultiplicativeExpression
unaryMultiplicativeExpression = do
    unary <- unaryExpression
    return $ UnaryMultiplicativeExpression unary

unaryExpression :: TokenParser UnaryExpression
unaryExpression = 
    postfixUnaryExpression
    <|> deleteUnaryExpression
    <|> voidUnaryExpression
    <|> typeOfUnaryExpression
    <|> incrementPlusUnaryExpression
    <|> incrementMinusUnaryExpression
    <|> plusUnaryExpression
    <|> minusUnaryExpression
    <|> bitwiseNotUnaryExpression
    <|> logicalNotUnaryExpression

postfixUnaryExpression :: TokenParser UnaryExpression
postfixUnaryExpression = do
    postfix <- postfixExpression
    return $ PostfixUnaryExpression postfix

deleteUnaryExpression :: TokenParser UnaryExpression
deleteUnaryExpression = do
    delete
    unary <- unaryExpression
    return $ VoidUnaryExpression unary

voidUnaryExpression :: TokenParser UnaryExpression
voidUnaryExpression = do
    void
    unary <- unaryExpression
    return $ VoidUnaryExpression unary

typeOfUnaryExpression :: TokenParser UnaryExpression
typeOfUnaryExpression = do
    typeOf
    unary <- unaryExpression
    return $ TypeOfUnaryExpression unary

incrementPlusUnaryExpression :: TokenParser UnaryExpression
incrementPlusUnaryExpression = do
    incrementPlus
    unary <- unaryExpression
    return $ IncrementPlusUnaryExpression unary

incrementMinusUnaryExpression :: TokenParser UnaryExpression
incrementMinusUnaryExpression = do
    incrementMinus
    unary <- unaryExpression
    return $ IncrementMinusUnaryExpression unary

plusUnaryExpression :: TokenParser UnaryExpression
plusUnaryExpression = do
    plus
    unary <- unaryExpression
    return $ PlusUnaryExpression unary

minusUnaryExpression :: TokenParser UnaryExpression
minusUnaryExpression = do
    minus
    unary <- unaryExpression
    return $ MinusUnaryExpression unary

bitwiseNotUnaryExpression :: TokenParser UnaryExpression
bitwiseNotUnaryExpression = do
    bitwiseNot
    unary <- unaryExpression
    return $ BitwiseNotUnaryExpression unary

logicalNotUnaryExpression :: TokenParser UnaryExpression
logicalNotUnaryExpression = do
    logicalNot
    unary <- unaryExpression
    return $ LogicalNotUnaryExpression unary

postfixExpression :: TokenParser PostfixExpression
postfixExpression = 
    try incrementPlusPostfixExpression
    <|> try incrementMinusPostfixExpression
    <|> lhsPostfixExpression

lhsPostfixExpression :: TokenParser PostfixExpression
lhsPostfixExpression = do 
    lhs <- leftHandSideExpression
    return $ LHSPostfixExpression lhs

incrementPlusPostfixExpression :: TokenParser PostfixExpression
incrementPlusPostfixExpression = do 
    lhs <- leftHandSideExpression
    --TODO: no line terminator here
    incrementPlus
    return $ IncrementPlusPostfixExpression lhs

incrementMinusPostfixExpression :: TokenParser PostfixExpression
incrementMinusPostfixExpression = do 
    lhs <- leftHandSideExpression
    --TODO: no line terminator here
    incrementMinus
    return $ IncrementMinusPostfixExpression lhs

leftHandSideExpression :: TokenParser LeftHandSideExpression
leftHandSideExpression = 
    try callLHSExpression
    <|> newLHSExpression

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
    try memberNewExpression
    <|> newNewExpression

memberNewExpression :: TokenParser NewExpression
memberNewExpression = do
    member <- memberExpression
    return $ MemberNewExpression member

memberExpression :: TokenParser MemberExpression
memberExpression = memberExpression'

-- left recursion in grammar... BURN IN HELL!!
memberExpression' :: TokenParser MemberExpression
memberExpression' = do
    primary <- primaryMemberExpression <|> functionMemberExpression <|> newMemberExpression
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
newMemberExpression = do
    new 
    memberExpr <- memberExpression
    args <- arguments
    return $ NewMemberExpression memberExpr args

arguments :: TokenParser [AssignmentExpression]
arguments = do
    leftRoundBracket
    args <- sepBy assignmentExpression comma
    rightRoundBracket
    return args

primaryMemberExpression :: TokenParser MemberExpression
primaryMemberExpression = do
    primary <- primaryExpression
    return $ PrimaryMemberExpression primary

newNewExpression :: TokenParser NewExpression
newNewExpression = do
    new
    newExpr <- newExpression
    return $ NewNewExpression newExpr

callExpression :: TokenParser CallExpression
callExpression = do
    base <- memberWithArgumentsCallExpression
    buildRestOfCallExpression base

buildRestOfCallExpression :: CallExpression -> TokenParser CallExpression
buildRestOfCallExpression base = 
    (try $ nonEmptyRestOfCallExpression base)
    <|> emptyRestOfCallExpression base

nonEmptyRestOfCallExpression :: CallExpression -> TokenParser CallExpression
nonEmptyRestOfCallExpression base =
    try (nonEmptyRestOfCallWithArgumentsCallExpression base)
    <|> try (nonEmptyRestOfPropertyAccessByBracketsCallExpression base)
    <|> nonEmptyRestOfPropertyAccessByDotCallExpression base

nonEmptyRestOfPropertyAccessByBracketsCallExpression :: CallExpression -> TokenParser CallExpression
nonEmptyRestOfPropertyAccessByBracketsCallExpression base = do
    leftSquareBracket
    expr <- expression
    rightSquareBracket
    buildRestOfCallExpression $ PropertyAccessByBracketsCallExpression base expr

nonEmptyRestOfPropertyAccessByDotCallExpression :: CallExpression -> TokenParser CallExpression
nonEmptyRestOfPropertyAccessByDotCallExpression base = do
    dot
    id <- identifierName
    buildRestOfCallExpression $ PropertyAccessByDotCallExpression base id

nonEmptyRestOfCallWithArgumentsCallExpression :: CallExpression -> TokenParser CallExpression
nonEmptyRestOfCallWithArgumentsCallExpression base = do
    args <- arguments
    buildRestOfCallExpression $ CallWithArgumentsCallExpression base args

emptyRestOfCallExpression :: CallExpression -> TokenParser CallExpression
emptyRestOfCallExpression base = return base

memberWithArgumentsCallExpression :: TokenParser CallExpression
memberWithArgumentsCallExpression = do
    memberExpr <- memberExpression
    args <- arguments
    return $ MemberWithArgumentsCallExpression memberExpr args

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
