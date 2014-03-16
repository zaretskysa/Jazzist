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

import Evaluating.Eval
import Evaluating.ExpressionEvaluator
import Evaluating.ExecutionContext as Ctx
import qualified Evaluating.Environment as Env

evalString :: String -> Double
evalString input = 
    case programFromString input of
        Left _ -> 666
        Right program -> fst $ runEval $ evalProgram program

evalProgram :: Program -> Eval Double
evalProgram (Program [sourceElement]) = do
    --let ctx = Ctx.newContxt
    evalSourceElement sourceElement

evalProgram _ = undefined

evalSourceElement :: SourceElement -> Eval Double
evalSourceElement (StatementSourceElement statement) = evalStatement statement
evalSourceElement _ = undefined

evalStatement :: Statement -> Eval Double
evalStatement (ExpressionStmt expr) = evalExpression expr
evalStatement _ = undefined
