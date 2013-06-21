module Evaluating.ExpressionEvaluator
(
    evalExpression
) where

import Parsing.Ast

import Evaluating.Eval
import Evaluating.Reference
import Evaluating.InternalValue
import Evaluating.LexicalEnvironment
import Evaluating.ExecutionContext

evalLiteral :: Literal -> Eval InternalValue
evalLiteral (NumericLiteral value) = return $ DoubleValue value
evalLiteral (BooleanLiteral _) = undefined
evalLiteral (NullLiteral) = undefined
evalLiteral (StringLiteral _) = undefined

evalPrimaryExpression :: PrimaryExpression -> Eval InternalValue
evalPrimaryExpression (LiteralPrimaryExpression literal) = evalLiteral literal

evalPrimaryExpression (IdentifierPrimaryExpression identifier) = do
    cx <- activeContext
    return $ RefValue $ getIdentifierReference (lexicalEnvironment cx) identifier

evalPrimaryExpression _ = undefined

evalMemberExpression :: MemberExpression -> Eval InternalValue
evalMemberExpression (PrimaryMemberExpression primExpr) = evalPrimaryExpression primExpr
evalMemberExpression _ = undefined

evalNewExpression :: NewExpression -> Eval InternalValue
evalNewExpression (MemberNewExpression memberExpr) = evalMemberExpression memberExpr
evalNewExpression (NewNewExpression _) = undefined

evalLeftHandSideExpression :: LeftHandSideExpression -> Eval InternalValue
evalLeftHandSideExpression (NewLHSExpression newExpr) = evalNewExpression newExpr
evalLeftHandSideExpression (CallLHSExpression _) = undefined

evalPostfixExpression :: PostfixExpression -> Eval InternalValue
evalPostfixExpression (LHSPostfixExpression lhsExpr) = evalLeftHandSideExpression lhsExpr
evalPostfixExpression _ = undefined

evalUnaryExpression :: UnaryExpression -> Eval InternalValue
evalUnaryExpression (PostfixUnaryExpression postfixExpr) = evalPostfixExpression postfixExpr
evalUnaryExpression _ = undefined

evalMultiplicativeExpression :: MultiplicativeExpression -> Eval InternalValue
evalMultiplicativeExpression (UnaryMultiplicativeExpression unaryExpr) =
    evalUnaryExpression unaryExpr

evalMultiplicativeExpression (MulMultiplicativeExpression multExpr unaryExpr) = do
    DoubleValue multVal <- evalMultiplicativeExpression multExpr
    DoubleValue unaryVal <- evalUnaryExpression unaryExpr
    return $ DoubleValue $ multVal * unaryVal

evalMultiplicativeExpression (DivMultiplicativeExpression multExpr unaryExpr) = do
    DoubleValue multVal <- evalMultiplicativeExpression multExpr
    DoubleValue unaryVal <- evalUnaryExpression unaryExpr
    return $ DoubleValue $ multVal / unaryVal

evalMultiplicativeExpression _ = undefined

evalAdditiveExpression :: AdditiveExpression -> Eval InternalValue
evalAdditiveExpression (MultAdditiveExpression multExpr) =
    evalMultiplicativeExpression multExpr

evalAdditiveExpression (PlusAdditiveExpression addExpr multExpr) = do
    DoubleValue addVal <- evalAdditiveExpression addExpr
    DoubleValue multVal <- evalMultiplicativeExpression multExpr
    return $ DoubleValue $ addVal + multVal

evalAdditiveExpression (MinusAdditiveExpression addExpr multExpr) = do
    DoubleValue addVal <- evalAdditiveExpression addExpr
    DoubleValue multVal <- evalMultiplicativeExpression multExpr
    return $ DoubleValue $ addVal - multVal

evalShiftExpression :: ShiftExpression -> Eval InternalValue
evalShiftExpression (AdditiveShiftExpression addExpr) = evalAdditiveExpression addExpr
evalShiftExpression _ = undefined

evalRelationalExpression :: RelationalExpression -> Eval InternalValue
evalRelationalExpression (ShiftRelationalExpression shiftExpr) = evalShiftExpression shiftExpr
evalRelationalExpression _ = undefined

evalEqualityExpression :: EqualityExpression -> Eval InternalValue
evalEqualityExpression (RelationalEqualityExpression relExpr) = evalRelationalExpression relExpr
evalEqualityExpression _ = undefined

evalBitwiseAndExpression :: BitwiseAndExpression -> Eval InternalValue
evalBitwiseAndExpression (UnaryBitwiseAndExpression eqExpr) = evalEqualityExpression eqExpr
evalBitwiseAndExpression _ = undefined

evalBitwiseXorExpression :: BitwiseXorExpression -> Eval InternalValue
evalBitwiseXorExpression (UnaryBitwiseXorExpression bitAndExpr) = evalBitwiseAndExpression bitAndExpr
evalBitwiseXorExpression _ = undefined

evalBitwiseOrExpression :: BitwiseOrExpression -> Eval InternalValue
evalBitwiseOrExpression (UnaryBitwiseOrExpression bitXorExpr) = evalBitwiseXorExpression bitXorExpr
evalBitwiseOrExpression _ = undefined

evalLogicalAndExpression :: LogicalAndExpression -> Eval InternalValue
evalLogicalAndExpression (UnaryLogicalAndExpression bitOrExpr) = evalBitwiseOrExpression bitOrExpr
evalLogicalAndExpression _ = undefined

evalLogicalOrExpression :: LogicalOrExpression -> Eval InternalValue
evalLogicalOrExpression (UnaryLogicalOrExpression logicAndExpr) = evalLogicalAndExpression logicAndExpr
evalLogicalOrExpression _ = undefined

evalConditionalExpression :: ConditionalExpression -> Eval InternalValue
evalConditionalExpression (LogicalOrConditionalExpression logicOrExpr) = evalLogicalOrExpression logicOrExpr
evalConditionalExpression _ = undefined

evalAssignmentExpression :: AssignmentExpression -> Eval InternalValue
evalAssignmentExpression (ConditionalAssignmentExpression condExpr) =
    evalConditionalExpression condExpr

--evalAssignmentExpression (AssignmentOperatorExpression lhsExpr op assignExpr) = do
--    evalConditionalExpression condExpr

evalAssignmentExpression _ = undefined

evalExpression :: Expression -> Eval Double
evalExpression (Expression [assignExpr]) = do
    DoubleValue value <- evalAssignmentExpression assignExpr
    return value

evalExpression _ = undefined
