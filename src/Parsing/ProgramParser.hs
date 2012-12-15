module Parsing.ProgramParser where

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
    functionKeyword
    ident <- identifierToken
    params <- betweenRoundBrackets $ sepBy identifierToken comma
    body <- betweenCurlyBrackets functionBody
    return $ FunctionDeclarationSourceElement ident params body

statement :: TokenParser (Statement (AssignmentExpression SourceElement) (LeftHandSideExpression SourceElement) (Expression SourceElement) SourceElement)
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
    <|> throwStatement
    <|> tryStatement
    <|> debuggerStatement

debuggerStatement :: TokenParser (Statement (AssignmentExpression SourceElement) (LeftHandSideExpression SourceElement) (Expression SourceElement) SourceElement)
debuggerStatement = debuggerKeyword >> semicolon >> return DebuggerStmt

tryStatement :: TokenParser (Statement (AssignmentExpression SourceElement) (LeftHandSideExpression SourceElement) (Expression SourceElement) SourceElement)
tryStatement = do
    tryStmt <- parseTryStatement
    return $ TryStmt tryStmt

parseTryStatement :: TokenParser (TryStatement (AssignmentExpression SourceElement) (LeftHandSideExpression SourceElement) (Expression SourceElement) SourceElement)
parseTryStatement = do
    try blockCatchFinallyTryStatement
    <|> try blockCatchTryStatement
    <|> try blockFinnalyTryStatement

blockCatchTryStatement :: TokenParser (TryStatement (AssignmentExpression SourceElement) (LeftHandSideExpression SourceElement) (Expression SourceElement) SourceElement)
blockCatchTryStatement = do
    tryKeyword
    b <- block
    c <- parseCatch
    return $ BlockCatchTryStatement b c

blockFinnalyTryStatement :: TokenParser (TryStatement (AssignmentExpression SourceElement) (LeftHandSideExpression SourceElement) (Expression SourceElement) SourceElement)
blockFinnalyTryStatement = do
    tryKeyword
    b <- block
    f <- finally
    return $ BlockFinallyTryStatement b f

blockCatchFinallyTryStatement :: TokenParser (TryStatement (AssignmentExpression SourceElement) (LeftHandSideExpression SourceElement) (Expression SourceElement) SourceElement)
blockCatchFinallyTryStatement = do
    tryKeyword
    b <- block
    c <- parseCatch
    f <- finally
    return $ BlockCatchFinallyTryStatement b c f

parseCatch :: TokenParser (Catch (AssignmentExpression SourceElement) (LeftHandSideExpression SourceElement) (Expression SourceElement) SourceElement)
parseCatch = do
    catchKeyword
    ident <- betweenRoundBrackets identifierToken
    b <- block
    return $ Catch ident b

finally :: TokenParser (Finally (AssignmentExpression SourceElement) (LeftHandSideExpression SourceElement) (Expression SourceElement) SourceElement)
finally = do
    finallyKeyword
    b <- block
    return $ Finally b

throwStatement :: TokenParser (Statement (AssignmentExpression SourceElement) (LeftHandSideExpression SourceElement) (Expression SourceElement) SourceElement)
throwStatement = do
    throwKeyword
    --TODO: no line terminator here
    expr <- expression
    semicolon
    return $ ThrowStmt expr

labelledStatement :: TokenParser (Statement (AssignmentExpression SourceElement) (LeftHandSideExpression SourceElement) (Expression SourceElement) SourceElement)
labelledStatement = do
    ident <- identifierToken
    colon
    stmt <- statement
    return $ LabelledStmt ident stmt

switchStatement :: TokenParser (Statement (AssignmentExpression SourceElement) (LeftHandSideExpression SourceElement) (Expression SourceElement) SourceElement)
switchStatement = do
    switchKeyword
    expr <- betweenRoundBrackets expression
    blk <- caseBlock
    return $ SwitchStmt expr blk

caseBlock :: TokenParser (CaseBlock (AssignmentExpression SourceElement) (LeftHandSideExpression SourceElement) (Expression SourceElement) SourceElement)
caseBlock = do
    leftCurlyBracket
    beginClauses <- many caseClause
    defaultPart <- maybeParse defaultClause
    endClauses <- many caseClause
    rightCurlyBracket
    return $ CaseBlock beginClauses defaultPart endClauses

caseClause :: TokenParser (CaseClause (AssignmentExpression SourceElement) (LeftHandSideExpression SourceElement) (Expression SourceElement) SourceElement)
caseClause = do
    caseKeyword
    expr <- expression
    colon
    stmts <- many statement
    return $ CaseClause expr stmts

defaultClause :: TokenParser (DefaultClause (AssignmentExpression SourceElement) (LeftHandSideExpression SourceElement) (Expression SourceElement) SourceElement)
defaultClause = do
    defaultKeyword >> colon
    stmts <- many statement
    return $ DefaultClause stmts

withStatement :: TokenParser (Statement (AssignmentExpression SourceElement) (LeftHandSideExpression SourceElement) (Expression SourceElement) SourceElement)
withStatement = do
    withKeyword
    expr <- betweenRoundBrackets expression
    stmt <- statement
    return $ WithStmt expr stmt

returnStatement :: TokenParser (Statement (AssignmentExpression SourceElement) (LeftHandSideExpression SourceElement) (Expression SourceElement) SourceElement)
returnStatement = do
    returnKeyword
    expr <- maybeParse expression --TODO: no line termimator here
    semicolon
    return $ ReturnStmt expr

breakStatement :: TokenParser (Statement (AssignmentExpression SourceElement) (LeftHandSideExpression SourceElement) (Expression SourceElement) SourceElement)
breakStatement = do
    breakKeyword
    ident <- maybeParse identifierToken --TODO: no line termimator here
    semicolon
    return $ BreakStmt ident

continueStatement :: TokenParser (Statement (AssignmentExpression SourceElement) (LeftHandSideExpression SourceElement) (Expression SourceElement) SourceElement)
continueStatement = do
    continueKeyword
    ident <- maybeParse identifierToken --TODO: no line termimator here
    semicolon
    return $ ContinueStmt ident

iterationStatement :: TokenParser (Statement (AssignmentExpression SourceElement) (LeftHandSideExpression SourceElement) (Expression SourceElement) SourceElement)
iterationStatement = 
    doWhileIterationStatement
    <|> whileIterationStatement
    <|> try exprTripletForIterationStatement
    <|> try varAndDoubleExprForIterationStatement
    <|> try lhsExprInExprForIterationStatement
    <|> varInExprIteratioinStatement

doWhileIterationStatement :: TokenParser  (Statement (AssignmentExpression SourceElement) (LeftHandSideExpression SourceElement) (Expression SourceElement) SourceElement)
doWhileIterationStatement = do
    doKeyword
    stmt <- statement
    whileKeyword
    expr <- betweenRoundBrackets expression
    semicolon
    return $ IterationStmt $ DoWhileIterationStatement stmt expr

whileIterationStatement :: TokenParser (Statement (AssignmentExpression SourceElement) (LeftHandSideExpression SourceElement) (Expression SourceElement) SourceElement)
whileIterationStatement = do
    whileKeyword
    expr <- betweenRoundBrackets expression
    stmt <- statement
    return $ IterationStmt $ WhileIterationStatement expr stmt

exprTripletForIterationStatement :: TokenParser (Statement (AssignmentExpression SourceElement) (LeftHandSideExpression SourceElement) (Expression SourceElement) SourceElement)
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

maybeExpression :: TokenParser (MaybeExpression SourceElement)
maybeExpression = maybeParse expression

varAndDoubleExprForIterationStatement :: TokenParser (Statement (AssignmentExpression SourceElement) (LeftHandSideExpression SourceElement) (Expression SourceElement) SourceElement)
varAndDoubleExprForIterationStatement = do
    forKeyword
    leftRoundBracket
    varKeyword
    varDecls <- variableDeclarationList --TODO: VariableDeclarationListNoIn
    semicolon
    expr1 <- maybeExpression
    semicolon
    expr2 <- maybeExpression
    rightRoundBracket
    stmt <- statement
    return $ IterationStmt $ VarAndDoubleExprForIterationStatement varDecls expr1 expr2 stmt

variableDeclarationList :: TokenParser [(VariableDeclaration (AssignmentExpression SourceElement) SourceElement)]
variableDeclarationList = sepBy1 variableDeclaration comma

lhsExprInExprForIterationStatement :: TokenParser (Statement (AssignmentExpression SourceElement) (LeftHandSideExpression SourceElement) (Expression SourceElement) SourceElement)
lhsExprInExprForIterationStatement = do
    forKeyword
    leftRoundBracket
    lhs <- leftHandSideExpression
    inKeyword
    expr <- expression
    rightRoundBracket
    stmt <- statement
    return $ IterationStmt $ LHSExprInExprForIterationStatement lhs expr stmt

varInExprIteratioinStatement :: TokenParser (Statement (AssignmentExpression SourceElement) (LeftHandSideExpression SourceElement) (Expression SourceElement) SourceElement)
varInExprIteratioinStatement = do
    forKeyword
    leftRoundBracket
    varKeyword
    varDecl <- variableDeclaration --TODO: VariableDeclarationNoIn
    inKeyword
    expr <- expression
    rightRoundBracket
    stmt <- statement
    return $ IterationStmt $ VarInExprIteratioinStatement varDecl expr stmt

ifStatement :: TokenParser (Statement (AssignmentExpression SourceElement) (LeftHandSideExpression SourceElement) (Expression SourceElement) SourceElement)
ifStatement = do
    ifKeyword
    expr <- betweenRoundBrackets expression
    stmt1 <- statement
    stmt2 <- maybeParse (elseKeyword >> statement)
    return $ IfStmt expr stmt1 stmt2

expressionStatement :: TokenParser (Statement (AssignmentExpression SourceElement) (LeftHandSideExpression SourceElement) (Expression SourceElement) SourceElement)
expressionStatement = do
    try $ notFollowedBy leftCurlyBracket
    try $ notFollowedBy functionKeyword
    expr <- expression
    semicolon
    return $ ExpressionStmt expr

blockStatement :: TokenParser (Statement (AssignmentExpression SourceElement) (LeftHandSideExpression SourceElement) (Expression SourceElement) SourceElement)
blockStatement = do
    b <- block
    return $ BlockStmt b

block :: TokenParser (Block (AssignmentExpression SourceElement) (LeftHandSideExpression SourceElement) (Expression SourceElement) SourceElement)
block = do
    stmts <- betweenCurlyBrackets $ many statement
    return $ Block stmts

emptyStatement :: TokenParser (Statement (AssignmentExpression SourceElement) (LeftHandSideExpression SourceElement) (Expression SourceElement) SourceElement)
emptyStatement = semicolon >> return EmptyStmt

variableStatement :: TokenParser (Statement (AssignmentExpression SourceElement) (LeftHandSideExpression SourceElement) (Expression SourceElement) SourceElement)
variableStatement = do
    varKeyword
    varDeclList <- sepBy1 variableDeclaration comma
    semicolon
    return $ VariableStmt varDeclList

variableDeclaration :: TokenParser (VariableDeclaration (AssignmentExpression SourceElement) SourceElement)
variableDeclaration = do
    ident <- identifierToken
    initial <- maybeInitializer
    return $ VariableDeclaration ident initial

maybeInitializer :: TokenParser (MaybeInitializer (AssignmentExpression SourceElement))
maybeInitializer = maybeParse initializer

initializer :: TokenParser (Initializer (AssignmentExpression SourceElement))
initializer = do
    assign
    assignExpr <- assignmentExpression
    return $ Initializer assignExpr

assignmentExpression :: TokenParser (AssignmentExpression SourceElement)
assignmentExpression = 
    try assignmentOperatorExpression
    <|> conditionalAssignmentExpression 
    <?> "AssignmentExpression"

conditionalAssignmentExpression :: TokenParser (AssignmentExpression SourceElement)
conditionalAssignmentExpression = do
    cond <- conditionalExpression
    return $ ConditionalAssignmentExpression cond

assignmentOperatorExpression :: TokenParser (AssignmentExpression SourceElement)
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

conditionalExpression :: TokenParser (ConditionalExpression SourceElement)
conditionalExpression = 
    try teranaryOperatorConditionalExpression
    <|> logicalOrContionalExpression

logicalOrContionalExpression :: TokenParser (ConditionalExpression SourceElement)
logicalOrContionalExpression = do
    logOr <- logicalOrExpression
    return $ LogicalOrConditionalExpression logOr

teranaryOperatorConditionalExpression :: TokenParser (ConditionalExpression SourceElement)
teranaryOperatorConditionalExpression = do
    logOr <- logicalOrExpression
    questionMark
    assign1 <- assignmentExpression
    colon
    assign2 <- assignmentExpression
    return $ TeranaryOperatorConditionalExpression logOr assign1 assign2

logicalOrExpression :: TokenParser (LogicalOrExpression SourceElement)
logicalOrExpression = do
    unary <- unaryLogicalOrExpression
    buildRestOfLogicalOrExpression unary

buildRestOfLogicalOrExpression :: (LogicalOrExpression SourceElement) -> TokenParser (LogicalOrExpression SourceElement)
buildRestOfLogicalOrExpression left = 
    try $ nonEmptyRestOfLogicalOrExpression left
    <|> emptyRestOfLogicalOrExpression left

nonEmptyRestOfLogicalOrExpression :: (LogicalOrExpression SourceElement) -> TokenParser (LogicalOrExpression SourceElement)
nonEmptyRestOfLogicalOrExpression left = do
    logicalOr
    logAnd <- logicalAndExpression
    buildRestOfLogicalOrExpression $ BinaryLogicalOrExpression left logAnd

emptyRestOfLogicalOrExpression :: (LogicalOrExpression SourceElement) -> TokenParser (LogicalOrExpression SourceElement)
emptyRestOfLogicalOrExpression left = return left

unaryLogicalOrExpression :: TokenParser (LogicalOrExpression SourceElement)
unaryLogicalOrExpression = do
    logAnd <- logicalAndExpression
    return $ UnaryLogicalOrExpression logAnd

logicalAndExpression :: TokenParser (LogicalAndExpression SourceElement)
logicalAndExpression = do
    unary <- unaryLogicalAndExpression
    buildRestOfLogicalAndExpression unary

buildRestOfLogicalAndExpression :: (LogicalAndExpression SourceElement) -> TokenParser (LogicalAndExpression SourceElement)
buildRestOfLogicalAndExpression left =
    try $ nonEmptyRestOfLogicalAndExpression left
    <|> emptyRestOfLogicalAndExpression left

nonEmptyRestOfLogicalAndExpression :: (LogicalAndExpression SourceElement) -> TokenParser (LogicalAndExpression SourceElement)
nonEmptyRestOfLogicalAndExpression left = do
    logicalAnd
    bitOr <- bitwiseOrExpression
    buildRestOfLogicalAndExpression $ BinaryLogicalAndExpression left bitOr

emptyRestOfLogicalAndExpression :: (LogicalAndExpression SourceElement) -> TokenParser (LogicalAndExpression SourceElement)
emptyRestOfLogicalAndExpression left = return left

unaryLogicalAndExpression :: TokenParser (LogicalAndExpression SourceElement)
unaryLogicalAndExpression = do
    bitOr <- bitwiseOrExpression
    return $ UnaryLogicalAndExpression bitOr

bitwiseOrExpression :: TokenParser (BitwiseOrExpression SourceElement)
bitwiseOrExpression = do 
    bitXor <- unaryBitwiseOrExpression
    buildRestOfBitwiseOrExpression bitXor

buildRestOfBitwiseOrExpression :: (BitwiseOrExpression SourceElement) -> TokenParser (BitwiseOrExpression SourceElement)
buildRestOfBitwiseOrExpression left =
    try $ nonEmptyRestOfBitwiseOrExpression left
    <|> emptyRestOfBitwiseOrExpression left

nonEmptyRestOfBitwiseOrExpression :: (BitwiseOrExpression SourceElement) -> TokenParser (BitwiseOrExpression SourceElement)
nonEmptyRestOfBitwiseOrExpression left = do
    bitwiseOr
    bitXor <- bitwiseXorExpression
    buildRestOfBitwiseOrExpression $ BinaryBitwiseOrExpression left bitXor

emptyRestOfBitwiseOrExpression :: (BitwiseOrExpression SourceElement) -> TokenParser (BitwiseOrExpression SourceElement)
emptyRestOfBitwiseOrExpression left = return left

unaryBitwiseOrExpression :: TokenParser (BitwiseOrExpression SourceElement)
unaryBitwiseOrExpression = do
    bitXor <- bitwiseXorExpression
    return $ UnaryBitwiseOrExpression bitXor

bitwiseXorExpression :: TokenParser (BitwiseXorExpression SourceElement)
bitwiseXorExpression = do
    bitAnd <- unaryBitwiseXorExpression
    buildRestOfBitwiseXorExpression bitAnd

buildRestOfBitwiseXorExpression :: (BitwiseXorExpression SourceElement) -> TokenParser (BitwiseXorExpression SourceElement)
buildRestOfBitwiseXorExpression left = 
    try $ nonEmptyRestOfBitwiseXorExpression left
    <|> emptyRestOfBitwiseXorExpresssion left

nonEmptyRestOfBitwiseXorExpression :: (BitwiseXorExpression SourceElement) -> TokenParser (BitwiseXorExpression SourceElement)
nonEmptyRestOfBitwiseXorExpression left = do
    bitwiseXor
    bitAnd <- bitwiseAndExpression
    buildRestOfBitwiseXorExpression $ BinaryBitwiseXorExpression left bitAnd

emptyRestOfBitwiseXorExpresssion :: (BitwiseXorExpression SourceElement) -> TokenParser (BitwiseXorExpression SourceElement)
emptyRestOfBitwiseXorExpresssion left = return left

unaryBitwiseXorExpression :: TokenParser (BitwiseXorExpression SourceElement)
unaryBitwiseXorExpression = do
    bitAnd <- bitwiseAndExpression
    return $ UnaryBitwiseXorExpression bitAnd

bitwiseAndExpression :: TokenParser (BitwiseAndExpression SourceElement)
bitwiseAndExpression = do
    equality <- unaryBitwiseAndExpression
    buildRestOfBitwiseAndExpresssion equality

buildRestOfBitwiseAndExpresssion :: (BitwiseAndExpression SourceElement) -> TokenParser (BitwiseAndExpression SourceElement)
buildRestOfBitwiseAndExpresssion left = 
    try $ nonEmptyRestOfBitwiseAndExpression left
    <|> emptyRestOfBitwiseAndExpression left

nonEmptyRestOfBitwiseAndExpression :: (BitwiseAndExpression SourceElement) -> TokenParser (BitwiseAndExpression SourceElement)
nonEmptyRestOfBitwiseAndExpression left = do
    bitwiseAnd
    equality <- equalityExpression
    buildRestOfBitwiseAndExpresssion $ BinaryBitwiseAndExpression left equality

emptyRestOfBitwiseAndExpression :: (BitwiseAndExpression SourceElement) -> TokenParser (BitwiseAndExpression SourceElement)
emptyRestOfBitwiseAndExpression left = return left

unaryBitwiseAndExpression :: TokenParser (BitwiseAndExpression SourceElement)
unaryBitwiseAndExpression = do
    equality <- equalityExpression
    return $ UnaryBitwiseAndExpression equality

equalityExpression :: TokenParser (EqualityExpression SourceElement)
equalityExpression = do
    relational <- relationalEqualityExpression
    buildRestOfEqualityExpression relational

buildRestOfEqualityExpression :: (EqualityExpression SourceElement) -> TokenParser (EqualityExpression SourceElement)
buildRestOfEqualityExpression left = 
    try $ nonEmptyRestOfEqualityExpression left
    <|> emptyRestOfEqualityExpression left

nonEmptyRestOfEqualityExpression :: (EqualityExpression SourceElement) -> TokenParser (EqualityExpression SourceElement)
nonEmptyRestOfEqualityExpression left = 
    restOfEqualsEqualityExpression left
    <|> restOfNotEqualsEqualityExpression left
    <|> restOfStrictEqualsEqualityExpression left
    <|> restOfStrictNotEqualsEqualityExpression left

restOfEqualsEqualityExpression :: (EqualityExpression SourceElement) -> TokenParser (EqualityExpression SourceElement)
restOfEqualsEqualityExpression left = do
    equals
    relational <- relationalExpression
    buildRestOfEqualityExpression $ EqualsEqualityExpression left relational

restOfNotEqualsEqualityExpression :: (EqualityExpression SourceElement) -> TokenParser (EqualityExpression SourceElement)
restOfNotEqualsEqualityExpression left = do
    notEquals
    relational <- relationalExpression
    buildRestOfEqualityExpression $ NotEqualsEqualityExpression left relational

restOfStrictEqualsEqualityExpression :: (EqualityExpression SourceElement) -> TokenParser (EqualityExpression SourceElement)
restOfStrictEqualsEqualityExpression left = do
    strictEquals
    relational <- relationalExpression
    buildRestOfEqualityExpression $ StrictEqualsEqualityExpression left relational

restOfStrictNotEqualsEqualityExpression :: (EqualityExpression SourceElement) -> TokenParser (EqualityExpression SourceElement)
restOfStrictNotEqualsEqualityExpression left = do
    strictNotEquals
    relational <- relationalExpression
    buildRestOfEqualityExpression $ StrictNotEqualsEqualityExpression left relational

emptyRestOfEqualityExpression :: (EqualityExpression SourceElement) -> TokenParser (EqualityExpression SourceElement)
emptyRestOfEqualityExpression left = return left

relationalEqualityExpression :: TokenParser (EqualityExpression SourceElement)
relationalEqualityExpression = do
    relation <- relationalExpression
    return $ RelationalEqualityExpression relation

relationalExpression :: TokenParser (RelationalExpression SourceElement)
relationalExpression = do
    shift <- shiftRelationalExpression
    buildRestOfRelationalExpression shift

buildRestOfRelationalExpression :: (RelationalExpression SourceElement) -> TokenParser (RelationalExpression SourceElement)
buildRestOfRelationalExpression left =
    try $ nonEmptyRestOfRelationalExpression left
    <|> emptyRestOfRelationalExpression left

nonEmptyRestOfRelationalExpression :: (RelationalExpression SourceElement) -> TokenParser (RelationalExpression SourceElement)
nonEmptyRestOfRelationalExpression left =
    restOfLessThanRelationalExpression left
    <|> restOfGreaterThanRelationalExpression left
    <|> restOfLessThanEqualsRelationalExpression left
    <|> restOfGreaterThanEqualsRelationalExpression left
    <|> restOfInstanceOfRelationalExpression left
    <|> restOfInRelationalExpression left

restOfLessThanRelationalExpression :: (RelationalExpression SourceElement) -> TokenParser (RelationalExpression SourceElement)
restOfLessThanRelationalExpression left = do
    lessThan
    shift <- shiftExpression
    buildRestOfRelationalExpression $ LessThanRelationalExpression left shift

restOfGreaterThanRelationalExpression :: (RelationalExpression SourceElement) -> TokenParser (RelationalExpression SourceElement)
restOfGreaterThanRelationalExpression left = do
    greaterThan
    shift <- shiftExpression
    buildRestOfRelationalExpression $ GreaterThanRelationalExpression left shift

restOfLessThanEqualsRelationalExpression :: (RelationalExpression SourceElement) -> TokenParser (RelationalExpression SourceElement)
restOfLessThanEqualsRelationalExpression left = do
    lessThanEquals
    shift <- shiftExpression
    buildRestOfRelationalExpression $ LessThanEqualsRelationalExpression left shift

restOfGreaterThanEqualsRelationalExpression :: (RelationalExpression SourceElement) -> TokenParser (RelationalExpression SourceElement)
restOfGreaterThanEqualsRelationalExpression left = do
    greaterThanEquals
    shift <- shiftExpression
    buildRestOfRelationalExpression $ GreaterThanEqualsRelationalExpression left shift

restOfInstanceOfRelationalExpression :: (RelationalExpression SourceElement) -> TokenParser (RelationalExpression SourceElement)
restOfInstanceOfRelationalExpression left = do
    instanceOfKeyword
    shift <- shiftExpression
    buildRestOfRelationalExpression $ InstanceOfRelationalExpression left shift

restOfInRelationalExpression :: (RelationalExpression SourceElement) -> TokenParser (RelationalExpression SourceElement)
restOfInRelationalExpression left = do
    inKeyword
    shift <- shiftExpression
    buildRestOfRelationalExpression $ InRelationalExpression left shift

emptyRestOfRelationalExpression :: (RelationalExpression SourceElement) -> TokenParser (RelationalExpression SourceElement)
emptyRestOfRelationalExpression left = return left

shiftRelationalExpression :: TokenParser (RelationalExpression SourceElement)
shiftRelationalExpression = do
    shift <- shiftExpression
    return $ ShiftRelationalExpression shift

shiftExpression :: TokenParser (ShiftExpression SourceElement)
shiftExpression = do
    left <- additiveShiftExpression
    buildRestOfShiftExpression left

buildRestOfShiftExpression :: (ShiftExpression SourceElement) -> TokenParser (ShiftExpression SourceElement)
buildRestOfShiftExpression left = 
    try $ nonEmptyRestOfShiftExpression left
    <|> emptyRestOfShiftExpression left

nonEmptyRestOfShiftExpression :: (ShiftExpression SourceElement) -> TokenParser (ShiftExpression SourceElement)
nonEmptyRestOfShiftExpression left = 
    restOfLeftShiftExpression left
    <|> restOfRightShiftExpression left
    <|> restOfUnsignedRightShiftExpression left

restOfLeftShiftExpression :: (ShiftExpression SourceElement) -> TokenParser (ShiftExpression SourceElement)
restOfLeftShiftExpression left = do
    leftShift
    additive <- additiveExpression
    buildRestOfShiftExpression $ LeftShiftExpression left additive

restOfRightShiftExpression :: (ShiftExpression SourceElement) -> TokenParser (ShiftExpression SourceElement)
restOfRightShiftExpression left = do
    rightShift
    additive <- additiveExpression
    buildRestOfShiftExpression $ RightShiftExpression left additive

restOfUnsignedRightShiftExpression :: (ShiftExpression SourceElement) -> TokenParser (ShiftExpression SourceElement)
restOfUnsignedRightShiftExpression left = do
    unsignedRightShift
    additive <- additiveExpression
    buildRestOfShiftExpression $ UnsignedRightShiftExpression left additive

emptyRestOfShiftExpression :: (ShiftExpression SourceElement) -> TokenParser (ShiftExpression SourceElement)
emptyRestOfShiftExpression left = return left

additiveShiftExpression :: TokenParser (ShiftExpression SourceElement)
additiveShiftExpression = do
    additive <- additiveExpression
    return $ AdditiveShiftExpression additive

additiveExpression :: TokenParser (AdditiveExpression SourceElement)
additiveExpression = do
    left <- multAdditiveExpression
    buildRestOfAdditiveExpression left

buildRestOfAdditiveExpression :: (AdditiveExpression SourceElement) -> TokenParser (AdditiveExpression SourceElement)
buildRestOfAdditiveExpression left = 
    try $ nonEmptyRestOfAdditiveExpression left
    <|> emptyRestOfAdditiveExpression left

nonEmptyRestOfAdditiveExpression :: (AdditiveExpression SourceElement) -> TokenParser (AdditiveExpression SourceElement)
nonEmptyRestOfAdditiveExpression left = 
    plusRestOfAdditiveExpression left
    <|> minusRestOfAdditiveExpression left

plusRestOfAdditiveExpression :: (AdditiveExpression SourceElement) -> TokenParser (AdditiveExpression SourceElement)
plusRestOfAdditiveExpression left = do
    plus
    multExpr <- multiplicativeExpression
    buildRestOfAdditiveExpression $ PlusAdditiveExpression left multExpr

minusRestOfAdditiveExpression :: (AdditiveExpression SourceElement) -> TokenParser (AdditiveExpression SourceElement)
minusRestOfAdditiveExpression left = do
    minus
    multExpr <- multiplicativeExpression
    buildRestOfAdditiveExpression $ MinusAdditiveExpression left multExpr

emptyRestOfAdditiveExpression :: (AdditiveExpression SourceElement) -> TokenParser (AdditiveExpression SourceElement)
emptyRestOfAdditiveExpression left = return left

multAdditiveExpression :: TokenParser (AdditiveExpression SourceElement)
multAdditiveExpression = do
    mult <- multiplicativeExpression
    return $ MultAdditiveExpression mult

multiplicativeExpression :: TokenParser (MultiplicativeExpression SourceElement)
multiplicativeExpression = do
    left <- unaryMultiplicativeExpression
    buildRestOfMultiplicativeExpression left

buildRestOfMultiplicativeExpression :: (MultiplicativeExpression SourceElement) -> TokenParser (MultiplicativeExpression SourceElement)
buildRestOfMultiplicativeExpression left = 
    try $ nonEmptyRestOfMultiplicativeExpression left
    <|> emptyRestOfMultiplicativeExpression left

emptyRestOfMultiplicativeExpression :: (MultiplicativeExpression SourceElement) -> TokenParser (MultiplicativeExpression SourceElement)
emptyRestOfMultiplicativeExpression left = return left

nonEmptyRestOfMultiplicativeExpression :: (MultiplicativeExpression SourceElement) -> TokenParser (MultiplicativeExpression SourceElement)
nonEmptyRestOfMultiplicativeExpression left = 
    mulRestOfMultiplicativeExpression left
    <|> divRestOfMultiplicativeExpression left
    <|> modulusRestOfMultiplicativeExpression left

mulRestOfMultiplicativeExpression :: (MultiplicativeExpression SourceElement) -> TokenParser (MultiplicativeExpression SourceElement)
mulRestOfMultiplicativeExpression left = do
    mul
    unary <- unaryExpression
    buildRestOfMultiplicativeExpression $ MulMultiplicativeExpression left unary

divRestOfMultiplicativeExpression :: (MultiplicativeExpression SourceElement) -> TokenParser (MultiplicativeExpression SourceElement)
divRestOfMultiplicativeExpression left = do
    divOp
    unary <- unaryExpression
    buildRestOfMultiplicativeExpression $ DivMultiplicativeExpression left unary

modulusRestOfMultiplicativeExpression :: (MultiplicativeExpression SourceElement) -> TokenParser (MultiplicativeExpression SourceElement)
modulusRestOfMultiplicativeExpression left = do
    modulus
    unary <- unaryExpression
    buildRestOfMultiplicativeExpression $ ModulusMultiplicativeExpression left unary

unaryMultiplicativeExpression :: TokenParser (MultiplicativeExpression SourceElement)
unaryMultiplicativeExpression = do
    unary <- unaryExpression
    return $ UnaryMultiplicativeExpression unary

unaryExpression :: TokenParser (UnaryExpression SourceElement)
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

postfixUnaryExpression :: TokenParser (UnaryExpression SourceElement)
postfixUnaryExpression = do
    postfix <- postfixExpression
    return $ PostfixUnaryExpression postfix

deleteUnaryExpression :: TokenParser (UnaryExpression SourceElement)
deleteUnaryExpression = do
    deleteKeyword
    unary <- unaryExpression
    return $ VoidUnaryExpression unary

voidUnaryExpression :: TokenParser (UnaryExpression SourceElement)
voidUnaryExpression = do
    voidKeyword
    unary <- unaryExpression
    return $ VoidUnaryExpression unary

typeOfUnaryExpression :: TokenParser (UnaryExpression SourceElement)
typeOfUnaryExpression = do
    typeOfKeyword
    unary <- unaryExpression
    return $ TypeOfUnaryExpression unary

incrementPlusUnaryExpression :: TokenParser (UnaryExpression SourceElement)
incrementPlusUnaryExpression = do
    incrementPlus
    unary <- unaryExpression
    return $ IncrementPlusUnaryExpression unary

incrementMinusUnaryExpression :: TokenParser (UnaryExpression SourceElement)
incrementMinusUnaryExpression = do
    incrementMinus
    unary <- unaryExpression
    return $ IncrementMinusUnaryExpression unary

plusUnaryExpression :: TokenParser (UnaryExpression SourceElement)
plusUnaryExpression = do
    plus
    unary <- unaryExpression
    return $ PlusUnaryExpression unary

minusUnaryExpression :: TokenParser (UnaryExpression SourceElement)
minusUnaryExpression = do
    minus
    unary <- unaryExpression
    return $ MinusUnaryExpression unary

bitwiseNotUnaryExpression :: TokenParser (UnaryExpression SourceElement)
bitwiseNotUnaryExpression = do
    bitwiseNot
    unary <- unaryExpression
    return $ BitwiseNotUnaryExpression unary

logicalNotUnaryExpression :: TokenParser (UnaryExpression SourceElement)
logicalNotUnaryExpression = do
    logicalNot
    unary <- unaryExpression
    return $ LogicalNotUnaryExpression unary

postfixExpression :: TokenParser (PostfixExpression SourceElement)
postfixExpression = 
    try incrementPlusPostfixExpression
    <|> try incrementMinusPostfixExpression
    <|> lhsPostfixExpression

lhsPostfixExpression :: TokenParser (PostfixExpression SourceElement)
lhsPostfixExpression = do 
    lhs <- leftHandSideExpression
    return $ LHSPostfixExpression lhs

incrementPlusPostfixExpression :: TokenParser (PostfixExpression SourceElement)
incrementPlusPostfixExpression = do 
    lhs <- leftHandSideExpression
    --TODO: no line terminator here
    incrementPlus
    return $ IncrementPlusPostfixExpression lhs

incrementMinusPostfixExpression :: TokenParser (PostfixExpression SourceElement)
incrementMinusPostfixExpression = do 
    lhs <- leftHandSideExpression
    --TODO: no line terminator here
    incrementMinus
    return $ IncrementMinusPostfixExpression lhs

leftHandSideExpression :: TokenParser (LeftHandSideExpression SourceElement)
leftHandSideExpression = 
    try callLHSExpression
    <|> newLHSExpression

newLHSExpression :: TokenParser (LeftHandSideExpression SourceElement)
newLHSExpression = do
    new <- newExpression
    return $ NewLHSExpression new

callLHSExpression :: TokenParser (LeftHandSideExpression SourceElement)
callLHSExpression = do
    call <- callExpression
    return $ CallLHSExpression call

newExpression :: TokenParser (NewExpression SourceElement)
newExpression = 
    try memberNewExpression
    <|> newNewExpression

memberNewExpression :: TokenParser (NewExpression SourceElement)
memberNewExpression = do
    member <- memberExpression
    return $ MemberNewExpression member

memberExpression :: TokenParser (MemberExpression SourceElement)
memberExpression = memberExpression'

-- left recursion in grammar... BURN IN HELL!!
memberExpression' :: TokenParser (MemberExpression SourceElement)
memberExpression' = do
    primary <- primaryMemberExpression <|> functionMemberExpression <|> newMemberExpression
    buildFunc <- restOfMemberExpression
    return $ buildFunc primary

restOfMemberExpression :: TokenParser ((MemberExpression SourceElement) -> (MemberExpression SourceElement))
restOfMemberExpression = try nonEmptyRestOfMemberExpression <|> emptyRestOfMemberExpression

nonEmptyRestOfMemberExpression :: TokenParser ((MemberExpression SourceElement) -> (MemberExpression SourceElement))
nonEmptyRestOfMemberExpression = 
    restOfAccessByBracketMemberExpression
    <|> restOfAccessByDotMemberExpression

restOfAccessByBracketMemberExpression :: TokenParser ((MemberExpression SourceElement) -> (MemberExpression SourceElement))
restOfAccessByBracketMemberExpression = do
    leftSquareBracket
    expr <- expression
    rightSquareBracket
    buildFunc <- restOfMemberExpression
    return $ \ memberExpr -> buildFunc $ PropertyAccessByBracketsMemberExpression memberExpr expr

restOfAccessByDotMemberExpression :: TokenParser ((MemberExpression SourceElement) -> (MemberExpression SourceElement))
restOfAccessByDotMemberExpression = do
    dot
    ident <- identifierToken
    buildFunc <- restOfMemberExpression
    return $ \ memberExpr -> buildFunc $ PropertyAccessByDotMemberExpression memberExpr ident

emptyRestOfMemberExpression :: TokenParser ((MemberExpression SourceElement) -> (MemberExpression SourceElement))
emptyRestOfMemberExpression = return (\ primary -> primary)

functionMemberExpression :: TokenParser (MemberExpression SourceElement)
functionMemberExpression = do
    func <- functionExpression
    return $ FunctionMemberExpression func

functionExpression :: TokenParser (FunctionExpression SourceElement)
functionExpression = do
    functionKeyword
    name <- maybeParse identifierToken
    params <- betweenRoundBrackets $ sepBy identifierToken comma
    body <- betweenCurlyBrackets functionBody
    return $ FunctionExpression name params body

propertyAccessByDotMemberExpression :: TokenParser (MemberExpression SourceElement)
propertyAccessByDotMemberExpression = undefined

newMemberExpression :: TokenParser (MemberExpression SourceElement)
newMemberExpression = do
    newKeyword
    memberExpr <- memberExpression
    args <- arguments
    return $ NewMemberExpression memberExpr args

arguments :: TokenParser [(AssignmentExpression SourceElement)]
arguments = betweenRoundBrackets $ sepBy assignmentExpression comma

primaryMemberExpression :: TokenParser (MemberExpression SourceElement)
primaryMemberExpression = do
    primary <- primaryExpression
    return $ PrimaryMemberExpression primary

newNewExpression :: TokenParser (NewExpression SourceElement)
newNewExpression = do
    newKeyword
    newExpr <- newExpression
    return $ NewNewExpression newExpr

callExpression :: TokenParser (CallExpression SourceElement)
callExpression = do
    base <- memberWithArgumentsCallExpression
    buildRestOfCallExpression base

buildRestOfCallExpression :: (CallExpression SourceElement) -> TokenParser (CallExpression SourceElement)
buildRestOfCallExpression base = 
    (try $ nonEmptyRestOfCallExpression base)
    <|> emptyRestOfCallExpression base

nonEmptyRestOfCallExpression :: (CallExpression SourceElement) -> TokenParser (CallExpression SourceElement)
nonEmptyRestOfCallExpression base =
    try (nonEmptyRestOfCallWithArgumentsCallExpression base)
    <|> try (nonEmptyRestOfPropertyAccessByBracketsCallExpression base)
    <|> nonEmptyRestOfPropertyAccessByDotCallExpression base

nonEmptyRestOfPropertyAccessByBracketsCallExpression :: (CallExpression SourceElement) -> TokenParser (CallExpression SourceElement)
nonEmptyRestOfPropertyAccessByBracketsCallExpression base = do
    leftSquareBracket
    expr <- expression
    rightSquareBracket
    buildRestOfCallExpression $ PropertyAccessByBracketsCallExpression base expr

nonEmptyRestOfPropertyAccessByDotCallExpression :: (CallExpression SourceElement) -> TokenParser (CallExpression SourceElement)
nonEmptyRestOfPropertyAccessByDotCallExpression base = do
    dot
    ident <- identifierName
    buildRestOfCallExpression $ PropertyAccessByDotCallExpression base ident

nonEmptyRestOfCallWithArgumentsCallExpression :: (CallExpression SourceElement) -> TokenParser (CallExpression SourceElement)
nonEmptyRestOfCallWithArgumentsCallExpression base = do
    args <- arguments
    buildRestOfCallExpression $ CallWithArgumentsCallExpression base args

emptyRestOfCallExpression :: (CallExpression SourceElement) -> TokenParser (CallExpression SourceElement)
emptyRestOfCallExpression base = return base

memberWithArgumentsCallExpression :: TokenParser (CallExpression SourceElement)
memberWithArgumentsCallExpression = do
    memberExpr <- memberExpression
    args <- arguments
    return $ MemberWithArgumentsCallExpression memberExpr args

primaryExpression :: TokenParser (PrimaryExpression SourceElement)
primaryExpression = 
    thisPrimaryExpression
    <|> identifierPrimaryExpression
    <|> literalPrimaryExpression
    <|> arrayLiteralPrimaryExpression
    <|> objectLiteralPrimaryExpression
    <|> expressionPrimaryExpression
    <?> "PrimaryExpression"

expressionPrimaryExpression :: TokenParser (PrimaryExpression SourceElement)
expressionPrimaryExpression = do
    expr <- betweenRoundBrackets expression
    return $ ExpressionPrimaryExpression expr

expression :: TokenParser (Expression SourceElement)
expression = do
    assigns <- sepBy1 assignmentExpression comma
    return $ Expression assigns

identifierPrimaryExpression :: TokenParser (PrimaryExpression SourceElement)
identifierPrimaryExpression = do
    str <- identifierToken
    return $ IdentifierPrimaryExpression str

thisPrimaryExpression :: TokenParser (PrimaryExpression SourceElement)
thisPrimaryExpression = thisKeyword >> return ThisPrimaryExpression

literalPrimaryExpression :: TokenParser (PrimaryExpression SourceElement)
literalPrimaryExpression = do
    lit <- literal
    return $ LiteralPrimaryExpression lit

arrayLiteralPrimaryExpression :: TokenParser (PrimaryExpression SourceElement)
arrayLiteralPrimaryExpression = do
    arr <- arrayLiteral
    return $ ArrayLiteralPrimaryExpression arr

arrayLiteral :: TokenParser (ArrayLiteral SourceElement)
arrayLiteral = do
    leftSquareBracket
    elements <- arrayLiteralElements
    rightSquareBracket
    return $ ArrayLiteral elements

arrayLiteralElements :: TokenParser [(MaybeAssignmentExpression SourceElement)]
arrayLiteralElements = do
    assignments <- sepBy (maybeParse assignmentExpression) comma
    if null assignments
        then return []
        else case last assignments of
            Nothing -> return $ reverse $ drop 1 (reverse assignments)
            _ -> return assignments

objectLiteralPrimaryExpression :: TokenParser (PrimaryExpression SourceElement)
objectLiteralPrimaryExpression = do
    properties <- betweenCurlyBrackets $ sepEndBy propertyAssignment comma
    return $ ObjectLiteralPrimaryExpression $ ObjectLiteral properties

propertyAssignment :: TokenParser (PropertyAssignment SourceElement)
propertyAssignment = 
    try fieldPropertyAssignment
    <|> getterPropertyAssignment
    <|> setterPropertyAssignment
    <?> "PropertyAssignment"

fieldPropertyAssignment :: TokenParser (PropertyAssignment SourceElement)
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
    ident <- identifierName <|> stringLiteralToken
    return $ StringPropertyName ident

numericPropertyName :: TokenParser PropertyName
numericPropertyName = do
    num <- numericLiteralToken
    return $ NumericPropertyName num

getterPropertyAssignment :: TokenParser (PropertyAssignment SourceElement)
getterPropertyAssignment = do
    getKeyword
    name <- propertyName
    roundBrackets
    body <- betweenCurlyBrackets functionBody
    return $ GetterPropertyAssignment name body

setterPropertyAssignment :: TokenParser (PropertyAssignment SourceElement)
setterPropertyAssignment = do
    setKeyword
    name <- propertyName
    param <- betweenRoundBrackets identifierToken
    body <- betweenCurlyBrackets functionBody
    return $ SetterPropertyAssignment name param body

functionBody :: TokenParser (FunctionBody SourceElement)
functionBody = do
    srcElements <- many sourceElement
    return $ FunctionBody srcElements

