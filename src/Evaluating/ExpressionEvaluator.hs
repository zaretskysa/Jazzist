{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}


module Evaluating.ExpressionEvaluator
(
    evalExpression
) where

import Parsing.Ast

evalLiteral :: Literal -> Double
evalLiteral (NumericLiteral value) = value

evalPrimaryExpression :: PrimaryExpression -> Double
evalPrimaryExpression (LiteralPrimaryExpression literal) = evalLiteral literal

evalMemberExpression :: MemberExpression -> Double
evalMemberExpression (PrimaryMemberExpression primExpr) = evalPrimaryExpression primExpr

evalNewExpression :: NewExpression -> Double
evalNewExpression (MemberNewExpression memberExpr) = evalMemberExpression memberExpr

evalLeftHandSideExpression :: LeftHandSideExpression -> Double
evalLeftHandSideExpression (NewLHSExpression newExpr) = evalNewExpression newExpr

evalPostfixExpression :: PostfixExpression -> Double
evalPostfixExpression (LHSPostfixExpression lhsExpr) = evalLeftHandSideExpression lhsExpr

evalUnaryExpression :: UnaryExpression -> Double
evalUnaryExpression (PostfixUnaryExpression postfixExpr) = evalPostfixExpression postfixExpr

evalMultiplicativeExpression :: MultiplicativeExpression -> Double
evalMultiplicativeExpression (UnaryMultiplicativeExpression unaryExpr) =
    evalUnaryExpression unaryExpr

evalMultiplicativeExpression (MulMultiplicativeExpression multExpr unaryExpr) =
    multVal * unaryVal
    where
        multVal = evalMultiplicativeExpression multExpr
        unaryVal = evalUnaryExpression unaryExpr

evalMultiplicativeExpression (DivMultiplicativeExpression multExpr unaryExpr) =
    multVal / unaryVal
    where
        multVal = evalMultiplicativeExpression multExpr
        unaryVal = evalUnaryExpression unaryExpr

evalAdditiveExpression :: AdditiveExpression -> Double
evalAdditiveExpression (MultAdditiveExpression multExpr) =
    evalMultiplicativeExpression multExpr

evalAdditiveExpression (PlusAdditiveExpression addExpr multExpr) =
    addVal + multVal
    where
        addVal = evalAdditiveExpression addExpr
        multVal = evalMultiplicativeExpression multExpr

evalAdditiveExpression (MinusAdditiveExpression addExpr multExpr) =
    addVal - multVal
    where
        addVal = evalAdditiveExpression addExpr
        multVal = evalMultiplicativeExpression multExpr


evalShiftExpression :: ShiftExpression -> Double
evalShiftExpression (AdditiveShiftExpression addExpr) = evalAdditiveExpression addExpr

evalRelationalExpression :: RelationalExpression -> Double
evalRelationalExpression (ShiftRelationalExpression shiftExpr) = evalShiftExpression shiftExpr

evalEqualityExpression :: EqualityExpression -> Double
evalEqualityExpression (RelationalEqualityExpression relExpr) = evalRelationalExpression relExpr

evalBitwiseAndExpression :: BitwiseAndExpression -> Double
evalBitwiseAndExpression (UnaryBitwiseAndExpression eqExpr) = evalEqualityExpression eqExpr

evalBitwiseXorExpression :: BitwiseXorExpression -> Double
evalBitwiseXorExpression (UnaryBitwiseXorExpression bitAndExpr) = evalBitwiseAndExpression bitAndExpr

evalBitwiseOrExpression :: BitwiseOrExpression -> Double
evalBitwiseOrExpression (UnaryBitwiseOrExpression bitXorExpr) = evalBitwiseXorExpression bitXorExpr

evalLogicalAndExpression :: LogicalAndExpression -> Double
evalLogicalAndExpression (UnaryLogicalAndExpression bitOrExpr) = evalBitwiseOrExpression bitOrExpr

evalLogicalOrExpression :: LogicalOrExpression -> Double
evalLogicalOrExpression (UnaryLogicalOrExpression logicAndExpr) = evalLogicalAndExpression logicAndExpr

evalConditionalExpression :: ConditionalExpression -> Double
evalConditionalExpression (LogicalOrConditionalExpression logicOrExpr) = evalLogicalOrExpression logicOrExpr

evalAssignmentExpression :: AssignmentExpression -> Double
evalAssignmentExpression (ConditionalAssignmentExpression condExpr) = evalConditionalExpression condExpr

evalExpression :: Expression -> Double
evalExpression (Expression [assignExpr]) = evalAssignmentExpression assignExpr



