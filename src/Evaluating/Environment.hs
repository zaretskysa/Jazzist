-- TODO: rename to JsState
module Evaluating.Environment 
(
    Environment,

    newEnvironment,
    activeContext
) where

import Data.Map
import qualified Evaluating.Stack as Stack

import Evaluating.ExecutionContext

type ContextsStack = Stack.Stack ExecutionContext

data Environment = Environment
    {
        contexts :: ContextsStack
    }
    deriving (Show)

newEnvironment :: Environment
newEnvironment = Environment {contexts = Stack.empty}

activeContext :: Environment -> ExecutionContext
activeContext env = Stack.top $ contexts env
