module Evaluating.ExpressionEvaluator
(
    evalExpression
) where

import Parsing.Ast

evalLiteral :: Literal -> Double
evalLiteral (NumericLiteral value) = value
evalLiteral (BooleanLiteral _) = undefined
evalLiteral (NullLiteral) = undefined
evalLiteral (StringLiteral _) = undefined

evalPrimaryExpression :: PrimaryExpression -> Double
evalPrimaryExpression (LiteralPrimaryExpression literal) = evalLiteral literal
evalPrimaryExpression (ThisPrimaryExpression) = undefined
evalPrimaryExpression (IdentifierPrimaryExpression _) = undefined
evalPrimaryExpression (ArrayLiteralPrimaryExpression _) = undefined
evalPrimaryExpression (ObjectLiteralPrimaryExpression _) = undefined
evalPrimaryExpression (ExpressionPrimaryExpression _) = undefined

evalMemberExpression :: MemberExpression -> Double
evalMemberExpression (PrimaryMemberExpression primExpr) = evalPrimaryExpression primExpr
evalMemberExpression (FunctionMemberExpression _) = undefined
evalMemberExpression (PropertyAccessByBracketsMemberExpression _ _) = undefined
evalMemberExpression (PropertyAccessByDotMemberExpression _ _) = undefined
evalMemberExpression (NewMemberExpression _ _) = undefined

evalNewExpression :: NewExpression -> Double
evalNewExpression (MemberNewExpression memberExpr) = evalMemberExpression memberExpr
evalNewExpression (NewNewExpression _) = undefined

evalLeftHandSideExpression :: LeftHandSideExpression -> Double
evalLeftHandSideExpression (NewLHSExpression newExpr) = evalNewExpression newExpr
evalLeftHandSideExpression (CallLHSExpression _) = undefined

evalPostfixExpression :: PostfixExpression -> Double
evalPostfixExpression (LHSPostfixExpression lhsExpr) = evalLeftHandSideExpression lhsExpr
evalPostfixExpression _ = undefined

evalUnaryExpression :: UnaryExpression -> Double
evalUnaryExpression (PostfixUnaryExpression postfixExpr) = evalPostfixExpression postfixExpr
evalUnaryExpression _ = undefined

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

evalMultiplicativeExpression _ = undefined

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
evalShiftExpression _ = undefined

evalRelationalExpression :: RelationalExpression -> Double
evalRelationalExpression (ShiftRelationalExpression shiftExpr) = evalShiftExpression shiftExpr
evalRelationalExpression _ = undefined

evalEqualityExpression :: EqualityExpression -> Double
evalEqualityExpression (RelationalEqualityExpression relExpr) = evalRelationalExpression relExpr
evalEqualityExpression _ = undefined

evalBitwiseAndExpression :: BitwiseAndExpression -> Double
evalBitwiseAndExpression (UnaryBitwiseAndExpression eqExpr) = evalEqualityExpression eqExpr
evalBitwiseAndExpression _ = undefined

evalBitwiseXorExpression :: BitwiseXorExpression -> Double
evalBitwiseXorExpression (UnaryBitwiseXorExpression bitAndExpr) = evalBitwiseAndExpression bitAndExpr
evalBitwiseXorExpression _ = undefined

evalBitwiseOrExpression :: BitwiseOrExpression -> Double
evalBitwiseOrExpression (UnaryBitwiseOrExpression bitXorExpr) = evalBitwiseXorExpression bitXorExpr
evalBitwiseOrExpression _ = undefined

evalLogicalAndExpression :: LogicalAndExpression -> Double
evalLogicalAndExpression (UnaryLogicalAndExpression bitOrExpr) = evalBitwiseOrExpression bitOrExpr
evalLogicalAndExpression _ = undefined

evalLogicalOrExpression :: LogicalOrExpression -> Double
evalLogicalOrExpression (UnaryLogicalOrExpression logicAndExpr) = evalLogicalAndExpression logicAndExpr
evalLogicalOrExpression _ = undefined

evalConditionalExpression :: ConditionalExpression -> Double
evalConditionalExpression (LogicalOrConditionalExpression logicOrExpr) = evalLogicalOrExpression logicOrExpr
evalConditionalExpression _ = undefined

evalAssignmentExpression :: AssignmentExpression -> Double
evalAssignmentExpression (ConditionalAssignmentExpression condExpr) = evalConditionalExpression condExpr
evalAssignmentExpression _ = undefined

evalExpression :: Expression -> Double
evalExpression (Expression [assignExpr]) = evalAssignmentExpression assignExpr
evalExpression _ = undefined



