module Evaluating.ObjectM
() where

import Evaluating.Eval
import Evaluating.Value
import qualified Evaluating.EnvironmentM as EnvM
import qualified Evaluating.Object as Obj
import Evaluating.Object (Object, ObjectId, MaybeObjectId)
import Evaluating.PropertyDescriptor (MaybePropertyDescriptor)

getOwnProperty :: ObjectId -> String -> Eval MaybePropertyDescriptor
getOwnProperty objId propName = do
    Just obj <- EnvM.getObject objId
    return $ Obj.getOwnProperty obj propName

getProperty :: ObjectId -> String -> Eval MaybePropertyDescriptor
getProperty objId propName = do
    heap <- EnvM.objectsHeap
    Just obj <- EnvM.getObject objId
    return $ Obj.getProperty obj propName heap

get :: ObjectId -> String -> Eval MaybeValue
get objId propName = do
    heap <- EnvM.objectsHeap
    return $ Obj.getForId objId propName heap

canPut :: ObjectId -> String -> Eval Bool
canPut objId propName = do
    heap <- EnvM.objectsHeap
    return $ Obj.canPutForId objId propName heap

hasProperty :: ObjectId -> String -> Eval Bool
hasProperty objId propName = do
    heap <- EnvM.objectsHeap
    return $ Obj.hasPropertyForId objId propName heap

deleteProperty :: ObjectId -> String -> Bool -> Eval Bool
deleteProperty objId propName throwFlag = do
    Just obj <- EnvM.getObject objId
    case Obj.deleteProperty obj propName of
        (_newObj, False) -> if throwFlag 
            then error "js exception" 
            else return False
        (newObj, True) -> do
            EnvM.modifyObject objId newObj
            return True


