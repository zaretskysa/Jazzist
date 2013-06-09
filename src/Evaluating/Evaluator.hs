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

import Evaluating.ExpressionEvaluator

evalString :: String -> Double
evalString input = 
    case programFromString input of
        Left _ -> 666
        Right program -> evalProgram program

evalStatement :: Statement -> Double
evalStatement (ExpressionStmt expr) = evalExpression expr

evalSourceElement :: SourceElement -> Double
evalSourceElement (StatementSourceElement statement) = evalStatement statement

evalProgram :: Program -> Double
evalProgram (Program [sourceElement]) = evalSourceElement sourceElement

