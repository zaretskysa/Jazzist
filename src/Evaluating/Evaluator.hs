module Evaluating.Evaluator
(
    module Parsing.Ast,
    module Evaluating.Value,

    evalProgram,
    evalString
) where

import Parsing.Ast
import Evaluating.Value
import Parsing.Parser


evalString :: String -> Double
evalString input = 
    case programFromString input of
        Left _ -> 666
        Right program -> evalProgram program

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
evalMultiplicativeExpression (UnaryMultiplicativeExpression unaryExpr) = evalUnaryExpression unaryExpr

evalAdditiveExpression :: AdditiveExpression -> Double
evalAdditiveExpression (MultAdditiveExpression multExpr) = evalMultiplicativeExpression multExpr

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

evalStatement :: Statement -> Double
evalStatement (ExpressionStmt expr) = evalExpression expr

evalSourceElement :: SourceElement -> Double
evalSourceElement (StatementSourceElement statement) = evalStatement statement

evalProgram :: Program -> Double
evalProgram (Program [sourceElement]) = evalSourceElement sourceElement

