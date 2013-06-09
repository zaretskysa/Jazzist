module Evaluating.ExpressionEvaluator
(
    evalExpression
) where

import Parsing.Ast

import Evaluating.Eval

evalLiteral :: Literal -> Eval Double
evalLiteral (NumericLiteral value) = return value
evalLiteral (BooleanLiteral _) = undefined
evalLiteral (NullLiteral) = undefined
evalLiteral (StringLiteral _) = undefined

evalPrimaryExpression :: PrimaryExpression -> Eval Double
evalPrimaryExpression (LiteralPrimaryExpression literal) = evalLiteral literal
evalPrimaryExpression _ = undefined

evalMemberExpression :: MemberExpression -> Eval Double
evalMemberExpression (PrimaryMemberExpression primExpr) = evalPrimaryExpression primExpr
evalMemberExpression _ = undefined

evalNewExpression :: NewExpression -> Eval Double
evalNewExpression (MemberNewExpression memberExpr) = evalMemberExpression memberExpr
evalNewExpression (NewNewExpression _) = undefined

evalLeftHandSideExpression :: LeftHandSideExpression -> Eval Double
evalLeftHandSideExpression (NewLHSExpression newExpr) = evalNewExpression newExpr
evalLeftHandSideExpression (CallLHSExpression _) = undefined

evalPostfixExpression :: PostfixExpression -> Eval Double
evalPostfixExpression (LHSPostfixExpression lhsExpr) = evalLeftHandSideExpression lhsExpr
evalPostfixExpression _ = undefined

evalUnaryExpression :: UnaryExpression -> Eval Double
evalUnaryExpression (PostfixUnaryExpression postfixExpr) = evalPostfixExpression postfixExpr
evalUnaryExpression _ = undefined

evalMultiplicativeExpression :: MultiplicativeExpression -> Eval Double
evalMultiplicativeExpression (UnaryMultiplicativeExpression unaryExpr) =
    evalUnaryExpression unaryExpr

evalMultiplicativeExpression (MulMultiplicativeExpression multExpr unaryExpr) = do
    multVal <- evalMultiplicativeExpression multExpr
    unaryVal <- evalUnaryExpression unaryExpr
    return $ multVal * unaryVal

evalMultiplicativeExpression (DivMultiplicativeExpression multExpr unaryExpr) = do
    multVal <- evalMultiplicativeExpression multExpr
    unaryVal <- evalUnaryExpression unaryExpr
    return $ multVal / unaryVal

evalMultiplicativeExpression _ = undefined

evalAdditiveExpression :: AdditiveExpression -> Eval Double
evalAdditiveExpression (MultAdditiveExpression multExpr) =
    evalMultiplicativeExpression multExpr

evalAdditiveExpression (PlusAdditiveExpression addExpr multExpr) = do
    addVal <- evalAdditiveExpression addExpr
    multVal <- evalMultiplicativeExpression multExpr
    return $ addVal + multVal

evalAdditiveExpression (MinusAdditiveExpression addExpr multExpr) = do
    addVal <- evalAdditiveExpression addExpr
    multVal <- evalMultiplicativeExpression multExpr
    return $ addVal - multVal

evalShiftExpression :: ShiftExpression -> Eval Double
evalShiftExpression (AdditiveShiftExpression addExpr) = evalAdditiveExpression addExpr
evalShiftExpression _ = undefined

evalRelationalExpression :: RelationalExpression -> Eval Double
evalRelationalExpression (ShiftRelationalExpression shiftExpr) = evalShiftExpression shiftExpr
evalRelationalExpression _ = undefined

evalEqualityExpression :: EqualityExpression -> Eval Double
evalEqualityExpression (RelationalEqualityExpression relExpr) = evalRelationalExpression relExpr
evalEqualityExpression _ = undefined

evalBitwiseAndExpression :: BitwiseAndExpression -> Eval Double
evalBitwiseAndExpression (UnaryBitwiseAndExpression eqExpr) = evalEqualityExpression eqExpr
evalBitwiseAndExpression _ = undefined

evalBitwiseXorExpression :: BitwiseXorExpression -> Eval Double
evalBitwiseXorExpression (UnaryBitwiseXorExpression bitAndExpr) = evalBitwiseAndExpression bitAndExpr
evalBitwiseXorExpression _ = undefined

evalBitwiseOrExpression :: BitwiseOrExpression -> Eval Double
evalBitwiseOrExpression (UnaryBitwiseOrExpression bitXorExpr) = evalBitwiseXorExpression bitXorExpr
evalBitwiseOrExpression _ = undefined

evalLogicalAndExpression :: LogicalAndExpression -> Eval Double
evalLogicalAndExpression (UnaryLogicalAndExpression bitOrExpr) = evalBitwiseOrExpression bitOrExpr
evalLogicalAndExpression _ = undefined

evalLogicalOrExpression :: LogicalOrExpression -> Eval Double
evalLogicalOrExpression (UnaryLogicalOrExpression logicAndExpr) = evalLogicalAndExpression logicAndExpr
evalLogicalOrExpression _ = undefined

evalConditionalExpression :: ConditionalExpression -> Eval Double
evalConditionalExpression (LogicalOrConditionalExpression logicOrExpr) = evalLogicalOrExpression logicOrExpr
evalConditionalExpression _ = undefined

evalAssignmentExpression :: AssignmentExpression -> Eval Double
evalAssignmentExpression (ConditionalAssignmentExpression condExpr) = evalConditionalExpression condExpr
evalAssignmentExpression _ = undefined

evalExpression :: Expression -> Eval Double
evalExpression (Expression [assignExpr]) = evalAssignmentExpression assignExpr
evalExpression _ = undefined



