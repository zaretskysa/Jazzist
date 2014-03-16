module Evaluating.ObjEnvRecord
(
    ObjEnvRecord,
    hasBinding,
    createMutableBinding,
    setMutableBinding,
    getBindingValue,
    deleteBinding,
    implicitThisValue
) where

import Common.Result
import Evaluating.Object (Object)
import qualified Evaluating.ObjectAlgo as Algo
import Evaluating.ObjectsHeap (ObjectsHeap)
import Evaluating.PropertyDescriptor (PropertyDescriptor(..))
import qualified Evaluating.PropertyDescriptor as PDesc
import Evaluating.Value


data ObjEnvRecord = ObjEnvRecord
    { object :: Object
    , provideThis :: Bool
    } deriving (Show)


hasBinding :: ObjEnvRecord -> String -> ObjectsHeap -> Bool
hasBinding record name heap = 
    Algo.hasProperty (object record) name heap

createMutableBinding :: ObjEnvRecord -> String -> Bool -> Result ObjEnvRecord
createMutableBinding record name deletable =
    let prop = PDesc.new {writable = Just True, enumerable = Just True, configurable = Just deletable}
    in case Algo.defineOwnProperty (object record) name prop of
        Algo.Updated newObj -> Success $ record {object = newObj}
        Algo.Rejected -> Fail

setMutableBinding :: ObjEnvRecord -> ObjectsHeap -> String -> Value -> Bool -> Result ObjEnvRecord
setMutableBinding record heap name value _strict =
    case Algo.put (object record) name value heap of
        (newObj, True) -> Success $ record {object = newObj}
        (_, False) -> Fail

getBindingValue :: ObjEnvRecord -> ObjectsHeap -> String -> Bool -> Result Value
getBindingValue record heap name strict = 
    let obj = object record
    in case Algo.hasProperty obj name heap of
        False -> case strict of
            False -> Success UndefinedValue
            True -> Fail
        True -> case Algo.get obj name heap of
            Just value -> Success value
            Nothing -> Fail

deleteBinding :: ObjEnvRecord -> String -> Result ObjEnvRecord
deleteBinding record name = 
    case Algo.deleteProperty (object record) name of
        (newObj, True) -> Success $ record {object = newObj}
        (_, False) -> Fail

implicitThisValue :: ObjEnvRecord -> Value
implicitThisValue record = 
    case provideThis record of
        True -> ObjectValue $ object record
        False -> UndefinedValue
