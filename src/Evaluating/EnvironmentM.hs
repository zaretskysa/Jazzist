module Evaluating.EnvironmentM
(
    putObject,
    getObject,
    modifyObject,

    objectsHeap,
) where

import Evaluating.Eval
import qualified Evaluating.Environment as Env
import Evaluating.ExecutionContext
import Evaluating.Object
import Evaluating.ObjectsHeap (ObjectsHeap)
import qualified Evaluating.ObjectsHeap as Heap

pushContext :: ExecutionContext -> Eval ()
pushContext cx = undefined

popContext :: Eval ExecutionContext
popContext = undefined

putObject :: Object -> Eval ObjectId
putObject obj = do
    env <- get
    (newEnv, objId) <- return $ Env.putObject env obj
    put newEnv
    return objId

getObject :: ObjectId -> Eval MaybeObject
getObject objId = do
    env <- get
    return $ Env.getObject env objId

objectsHeap :: Eval ObjectsHeap
objectsHeap = do
    env <- get
    return $ Env.objectsHeap env

modifyObject :: ObjectId -> Object -> Eval ()
modifyObject objId obj = do
    env <- get
    put $ Env.modifyObject env objId obj







