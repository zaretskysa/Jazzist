module Evaluating.ExecutionContext
(
    module Evaluating.LexicalEnvironment,

    ExecutionContext,
    
    makeExecutionContext,
    lexicalEnvironment,
    variableEnvironment
) where

import Evaluating.LexicalEnvironment

data ExecutionContext = ExecutionContext 
    {
        lexicalEnvironment :: Maybe LexicalEnvironment,
        variableEnvironment :: LexicalEnvironment
    }
    deriving (Show)

makeExecutionContext :: LexicalEnvironment -> LexicalEnvironment -> ExecutionContext
makeExecutionContext lexicalEnv variableEnv = 
    ExecutionContext {lexicalEnvironment = Just lexicalEnv, variableEnvironment = variableEnv}

