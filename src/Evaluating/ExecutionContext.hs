module Evaluating.ExecutionContext
(
    module Evaluating.LexicalEnvironment,

    ExecutionContext,
    
    newContext,
    lexicalEnvironment,
    variableEnvironment
) where

import Evaluating.LexicalEnvironment
import Evaluating.Object (ObjectId)

data ExecutionContext = ExecutionContext 
    {
        lexicalEnvironment :: LexicalEnvironment,
        variableEnvironment :: LexicalEnvironment,
        thisBinding :: Maybe ObjectId
    }
    deriving (Show)

newContext :: LexicalEnvironment -> LexicalEnvironment -> Maybe ObjectId -> ExecutionContext
newContext lexicalEnv variableEnv this = 
    ExecutionContext {
        lexicalEnvironment = lexicalEnv, 
        variableEnvironment = variableEnv,
        thisBinding = this
    }

