module Evaluating.ObjectM
(
    getOwnProperty,
    getProperty,
    get,
    canPut,
    hasProperty,
    deleteProperty,
) where

import qualified Evaluating.ObjectAlgo as ObjAlgo
import Evaluating.Eval (Eval)
import Evaluating.Value
import qualified Evaluating.EnvironmentM as EnvM
import Evaluating.Object (ObjectId)
import Evaluating.PropertyDescriptor (MaybePropertyDescriptor)

getOwnProperty :: ObjectId -> String -> Eval MaybePropertyDescriptor
getOwnProperty objId propName = do
    Just obj <- EnvM.getObject objId
    return $ ObjAlgo.getOwnProperty obj propName

getProperty :: ObjectId -> String -> Eval MaybePropertyDescriptor
getProperty objId propName = do
    heap <- EnvM.objectsHeap
    Just obj <- EnvM.getObject objId
    return $ ObjAlgo.getProperty obj propName heap

get :: ObjectId -> String -> Eval MaybeValue
get objId propName = do
    heap <- EnvM.objectsHeap
    Just obj <- EnvM.getObject objId
    return $ ObjAlgo.get obj propName heap

canPut :: ObjectId -> String -> Eval Bool
canPut objId propName = do
    heap <- EnvM.objectsHeap
    Just obj <- EnvM.getObject objId
    return $ ObjAlgo.canPut obj propName heap

hasProperty :: ObjectId -> String -> Eval Bool
hasProperty objId propName = do
    heap <- EnvM.objectsHeap
    Just obj <- EnvM.getObject objId
    return $ ObjAlgo.hasProperty obj propName heap

deleteProperty :: ObjectId -> String -> Bool -> Eval Bool
deleteProperty objId propName throwFlag = do
    Just obj <- EnvM.getObject objId
    case ObjAlgo.deleteProperty obj propName of
        (_newObj, False) -> if throwFlag 
            then error "js exception" 
            else return False
        (newObj, True) -> do
            EnvM.modifyObject objId newObj
            return True


