module Evaluating.Evaluator
(
    module Parsing.Ast,
    module Evaluating.Value,

    evalProgram,
    evalString
) where

import Common.Debug
import Debug.Trace

import Parsing.Ast
import Evaluating.Value
import Parsing.Parser

import Evaluating.Eval
import Evaluating.ExpressionEvaluator
import qualified Evaluating.EnvironmentM as Env

evalString :: String -> Double
evalString input = 
    case programFromString input of
        Left _ -> 666
        Right program -> fst $ runEval $ evalProgram program

evalProgram :: Program -> Eval Double
evalProgram (Program [sourceElement]) = do
    Env.enterGlobalContext
    evalSourceElement sourceElement

evalProgram _ = $stub

evalSourceElement :: SourceElement -> Eval Double
evalSourceElement (StatementSourceElement statement) = evalStatement statement
evalSourceElement _ = $stub

evalStatement :: Statement -> Eval Double
evalStatement (ExpressionStmt expr) = evalExpression expr
evalStatement (VariableStmt [varDecl]) = $stub
evalStatement st = traceShow st $stub
