module Evaluating.ObjectAlgo
(
    Hint(..),
    UpdateResult(..),

    getOwnProperty,
    getProperty,
    get,
    put,
    canPut,
    hasProperty,
    hasPropertyForId,
    deleteProperty,
    defineOwnProperty
) where

--TODO: remove this import
import qualified Data.Map as Map

import Data.Maybe

import Common.BoolUtils
import Evaluating.Value
import Evaluating.NamedDataProperty (NamedDataProperty)
import Evaluating.NamedAccessorProperty (NamedAccessorProperty)
import Evaluating.PropertyDescriptor (PropertyDescriptor, MaybePropertyDescriptor)
import qualified Evaluating.PropertyDescriptor as PDesc
import Evaluating.Property (Property)
import qualified Evaluating.Property as Prop
import Evaluating.ObjectsHeap (ObjectsHeap)
import qualified Evaluating.ObjectsHeap as Heap
import Evaluating.Object (Object, ObjectId)
import qualified Evaluating.Object as Obj
import qualified Evaluating.TypeTesting as Testing


data Hint = StringHint | NumberHint | NoHint deriving (Show)

data UpdateResult = Updated Object | Rejected deriving (Show)

getOwnProperty :: Object -> String -> Maybe PropertyDescriptor
getOwnProperty obj propName = 
    case Obj.property obj propName of
        Nothing -> Nothing
        Just prop -> Just $ Prop.toDescriptor prop

hasOwnProperty :: Object -> String -> Bool
hasOwnProperty obj prop = isJust $ getOwnProperty obj prop

getProperty :: Object -> String -> ObjectsHeap -> MaybePropertyDescriptor
getProperty obj propName heap =
    let desc = getOwnProperty obj propName
    in case desc of
        Just _ -> desc
        Nothing -> case Obj.prototype obj of
            Nothing -> Nothing
            Just protoId -> getPropertyForId protoId propName heap

getPropertyForId :: ObjectId -> String -> ObjectsHeap -> MaybePropertyDescriptor
getPropertyForId objId prop heap =
    getProperty (fromJust $ Map.lookup objId heap) prop heap

get :: Object -> String -> ObjectsHeap -> Maybe Value
get obj propName heap =
    case getProperty obj propName heap of
        Nothing -> Nothing
        Just desc -> case PDesc.isData desc of
            True -> PDesc.value desc
            False -> case PDesc.get desc of
                Nothing -> Nothing
                Just getter -> undefined -- call getter

getForId :: ObjectId -> String -> ObjectsHeap -> Maybe Value
getForId objId prop heap = get (fromJust $ Map.lookup objId heap) prop heap

canPut :: Object -> String -> ObjectsHeap -> Bool
canPut obj prop heap =
    case getOwnProperty obj prop of
        Just desc -> case PDesc.isAccessor desc of
            True -> case PDesc.set desc of
                Nothing -> False
                Just _ -> True
            False -> fromJust $ PDesc.writable desc
        Nothing -> case Obj.prototype obj of
            Nothing -> Obj.extensible obj
            Just protoId -> case getPropertyForId protoId prop heap of
                Nothing -> Obj.extensible obj
                Just inheritedDesc -> case PDesc.isAccessor inheritedDesc of
                    True -> PDesc.hasSetter inheritedDesc
                    False -> case Obj.extensible obj of
                        False -> False
                        True -> fromJust $ PDesc.writable inheritedDesc

canPutForId :: ObjectId -> String -> ObjectsHeap -> Bool
canPutForId objId prop heap = canPut (fromJust $ Map.lookup objId heap) prop heap

put :: Object -> String -> Value -> ObjectsHeap -> (Object, Bool)
put obj prop value heap =
    case canPut obj prop heap of
        False -> (obj, False)
        True -> case getOwnProperty obj prop of
            Nothing -> error "Own property does not exist"
            Just ownDesc -> case PDesc.isData ownDesc of
                True -> undefined --FIX ME
                False -> case getProperty obj prop heap of
                    Nothing -> error "Given property does not exist"
                    Just desc -> case PDesc.isAccessor desc of
                        True -> case PDesc.set desc of
                            Nothing -> error "Property descriptor setter is undefined"
                            Just setterId -> 
                                let newValue = callId setterId -- FIX ME
                                in (obj, True)
                        False -> 
                            let newDesc =  PDesc.new {PDesc.value = Just value, PDesc.writable = Just True, PDesc.enumerable = Just True, PDesc.configurable = Just True}
                                newValue = defineOwnProperty obj prop newDesc
                            in (obj, True) -- FIX ME

hasProperty :: Object -> String -> ObjectsHeap -> Bool
hasProperty obj prop heap =
    case getProperty obj prop heap of
        Nothing -> False
        Just _ -> True

hasPropertyForId :: ObjectId -> String -> ObjectsHeap -> Bool
hasPropertyForId objId prop heap = hasProperty (fromJust $ Map.lookup objId heap) prop heap

deleteProperty :: Object -> String -> (Object, Bool)
deleteProperty obj prop =
    case getOwnProperty obj prop of
        Nothing -> (obj, True)
        Just desc -> case (fromJust $ PDesc.configurable desc) of
            True -> (Obj.deleteProperty obj prop, True)
            False -> (obj, False)

defineOwnProperty :: Object -> String -> PropertyDescriptor -> UpdateResult
defineOwnProperty obj propName desc 
    | PDesc.allFieldsAreAbsent desc = Updated obj
    | otherwise = case getOwnProperty obj propName of
        Nothing
            | Obj.notExtensible obj -> Rejected
            | Obj.extensible obj ->
                if PDesc.isGenericOrData desc
                    then Updated $ Obj.putDataProperty obj propName desc
                    else Updated $ Obj.putAccessorProperty obj propName desc
        Just current
            | PDesc.isSame desc current -> Updated obj
            | PDesc.bothNotConfigurable current desc -> Rejected
            | PDesc.isNotConfigurable current, PDesc.hasEnumerable desc,
              PDesc.onlyOneIsEnumerable desc current -> Rejected
            | PDesc.isGeneric desc -> Updated $ putPropertyToObj obj propName desc
            | PDesc.onlyOneIsData current desc ->
                case PDesc.isConfigurable current of
                    False -> Rejected
                    True -> if PDesc.isData current 
                        then let convertedObj = Obj.convertPropertyToAccessor obj propName
                             in Updated $ putPropertyToObj convertedObj propName desc
                        else let convertedObj = Obj.convertPropertyToData obj propName
                             in Updated $ putPropertyToObj convertedObj propName desc
            | PDesc.bothAreData current desc ->
                case PDesc.isConfigurable current of
                    False 
                        | PDesc.isNotWritable current, PDesc.isWritable desc -> Rejected
                        | PDesc.isNotWritable current, PDesc.hasValue desc, 
                          PDesc.haveDifferentValues desc current -> Rejected
                        | otherwise -> Updated $ putPropertyToObj obj propName desc
                    True -> Updated $ putPropertyToObj obj propName desc
            | PDesc.bothAreAccessors current desc ->
                case PDesc.isConfigurable current of
                    False
                        | PDesc.hasSetter desc, PDesc.haveDifferentSetters desc current -> Rejected
                        | PDesc.hasGetter desc, PDesc.haveDifferentGetters desc current -> Rejected
                        | otherwise -> Updated $ putPropertyToObj obj propName desc
                    True -> Updated $ putPropertyToObj obj propName desc
            | otherwise -> Updated $ putPropertyToObj obj propName desc

putPropertyToObj :: Object -> String -> PropertyDescriptor -> Object
putPropertyToObj obj name desc =
    let prop = Prop.fromDescriptor desc
    in Obj.putProperty obj name prop

defaultValue :: Object -> Hint -> ObjectsHeap -> MaybeValue
defaultValue obj StringHint heap
    | value@(Just _) <- primitiveValue obj "toString" heap = value
    | value@(Just _) <- primitiveValue obj "valueOf" heap = value
    | otherwise = Nothing
defaultValue obj NumberHint heap
    | value@(Just _) <- primitiveValue obj "valueOf" heap = value
    | value@(Just _) <- primitiveValue obj "toString" heap = value
    | otherwise = Nothing
defaultValue obj NoHint heap = defaultValue obj NumberHint heap


primitiveValue :: Object -> String -> ObjectsHeap -> MaybeValue
primitiveValue obj propName heap
    | Just (ObjectIdValue value1) <- get obj propName heap, 
      Obj.isCallableId value1 heap, result <- callId value1, 
      Testing.isPrimitive result = Just $ result
    | otherwise = Nothing


call :: Object -> Value
call = undefined

callId :: ObjectId -> Value
callId = undefined

