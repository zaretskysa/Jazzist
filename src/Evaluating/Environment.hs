-- TODO: rename to JsState
module Evaluating.Environment 
(
    Environment,

    objectsHeap,

    newEnvironment,
    activeContext,

    putObject,
    getObject,
    modifyObject,
) where

import Data.Map
import qualified Evaluating.Stack as Stack

import Evaluating.ExecutionContext
import qualified Evaluating.ObjectsHeap as Heap
import Evaluating.Object

type ContextsStack = Stack.Stack ExecutionContext

data Environment = Environment
    {
        contexts :: ContextsStack,
        objectsHeap :: Heap.ObjectsHeap,
        lastObjectId :: ObjectId,  -- TODO: Maybe Object
        globalEnv :: LexicalEnvironment,
        globalObj :: Object
    }
    deriving (Show)

newEnvironment :: Environment
newEnvironment = Environment {
    contexts = Stack.empty,
    lastObjectId = 0
    }

activeContext :: Environment -> ExecutionContext
activeContext env = Stack.top $ contexts env

putObject :: Environment -> Object -> (Environment, ObjectId)
putObject env obj =
    let newObjectId = (lastObjectId env) + 1
        newHeap = Heap.put (objectsHeap env) newObjectId obj
        newEnv = env {lastObjectId = newObjectId, objectsHeap = newHeap}
    in (newEnv, newObjectId)

getObject :: Environment -> ObjectId -> MaybeObject
getObject env objId = Heap.lookup (objectsHeap env) objId

modifyObject :: Environment -> ObjectId -> Object -> Environment
modifyObject env objId obj =
    let heap = objectsHeap env
        newHeap = Heap.modifyObject heap objId obj
    in env {objectsHeap = newHeap}





