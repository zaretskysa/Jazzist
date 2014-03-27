module Evaluating.ExecutionContext
(
    module Evaluating.LexicalEnvironment,

    ExecutionContext,
    
    new,
    lexicalEnv,
    variableEnv,
    thisBinding,
) where

import Evaluating.LexicalEnvironment
import Evaluating.Object (ObjectId)

data ExecutionContext = ExecutionContext 
    {
        lexicalEnv :: LexicalEnvironment,
        variableEnv :: LexicalEnvironment,
        thisBinding :: Maybe ObjectId
    }
    deriving (Show)

new :: LexicalEnvironment -> LexicalEnvironment -> Maybe ObjectId -> ExecutionContext
new lexEnv varEnv this = 
    ExecutionContext {
        lexicalEnv = lexEnv, 
        variableEnv = varEnv,
        thisBinding = this
    }
