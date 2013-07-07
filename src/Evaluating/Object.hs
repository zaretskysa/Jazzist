module Evaluating.Object
(
    Object(..),
    MaybeObject,
    ObjectId,
    MaybeObjectId,
    Property,
    MaybeProperty,

    property,
    newObject,
    prototype,

    getOwnProperty,
    getProperty,
    getForId,
    canPut,
    canPutForId,
    hasPropertyForId,
    deleteProperty,
) where

import qualified Data.Map as Map
import Data.Maybe

import Evaluating.Value
import Evaluating.NamedDataProperty (NamedDataProperty)
import Evaluating.NamedAccessorProperty (NamedAccessorProperty)
import Evaluating.PropertyDescriptor (PropertyDescriptor, MaybePropertyDescriptor)
import qualified Evaluating.PropertyDescriptor as PDesc
import Evaluating.Property
import Evaluating.ObjectsHeap

type ObjectId = Integer

type MaybeObjectId = Maybe ObjectId

type MaybeObject = Maybe Object

data Hint = StringHint | NumberString | NoHint deriving (Show)

data Object = Object
    { prototype :: Maybe ObjectId
    , class_ :: String
    , extensible :: Bool
    , properties :: Properties
    } deriving (Show)

property :: Object -> String -> MaybeProperty
property obj prop = Map.lookup prop (properties obj)

newObject :: Object
newObject = undefined

getOwnProperty :: Object -> String -> Maybe PropertyDescriptor
getOwnProperty obj propName = 
    case property obj propName of
        Nothing -> Nothing
        Just prop -> Just $ PDesc.fromProperty prop

getProperty :: Object -> String -> ObjectsHeap -> MaybePropertyDescriptor
getProperty obj propName heap =
    let desc = getOwnProperty obj propName
    in case desc of
        Just _ -> desc
        Nothing -> case prototype obj of
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
        Nothing -> case prototype obj of
            Nothing -> extensible obj
            Just protoId -> case getPropertyForId protoId prop heap of
                Nothing -> extensible obj
                Just inheritedDesc -> case PDesc.isAccessor inheritedDesc of
                    True -> PDesc.hasSetter inheritedDesc
                    False -> case extensible obj of
                        False -> False
                        True -> fromJust $ PDesc.writable inheritedDesc

canPutForId :: ObjectId -> String -> ObjectsHeap -> Bool
canPutForId objId prop heap = canPut (fromJust $ Map.lookup objId heap) prop heap

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
            True -> (deleteOwnProperty obj prop, True)
            False -> (obj, False)

deleteOwnProperty :: Object -> String -> Object
deleteOwnProperty obj prop =
    let newProps = Map.delete prop (properties obj)
    in obj {properties = newProps}

hasOwnProperty :: Object -> String -> Bool
hasOwnProperty obj prop = isJust $ getOwnProperty obj prop

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
                                let newValue = callForId setterId -- FIX ME
                                in (obj, True)
                        False -> 
                            let newDesc =  PDesc.new {PDesc.value = Just value, PDesc.writable = Just True, PDesc.enumerable = Just True, PDesc.configurable = Just True}
                                newValue = defineOwnProperty obj prop newDesc
                            in (obj, True) -- FIX ME


defaultValue :: Object -> Hint -> ObjectsHeap -> Value
defaultValue obj hint heap = 
    case get obj "toString" heap of
        Nothing -> error "Object has no toString property"
        Just toStringProp -> undefined

call :: Object -> Value
call = undefined

callForId :: ObjectId -> Value
callForId = undefined

-- primitive
putProperty :: Object -> Property -> Object
putProperty obj prop = undefined

-- TODO: use special type UpdateResult = Successful Object | Rejected
defineOwnProperty :: Object -> String -> PropertyDescriptor -> (Object, Bool)
defineOwnProperty obj propName desc 
    | PDesc.allFieldsAreAbsent desc = (obj, True)
    | otherwise = case getOwnProperty obj propName of
        Nothing
            | not $ extensible obj -> (obj, False)
            | extensible obj ->
                if (PDesc.isGeneric desc) || (PDesc.isData desc) 
                    then let prop = dataPropertyFromDescriptor desc
                             newObj = putProperty obj prop
                         in (newObj, True)
                    else let prop = accessorPropertyFromDescriptor desc
                             newObj = putProperty obj prop
                         in (newObj, True)
        Just current
            | PDesc.isSame desc current -> (obj, True)
            | PDesc.isNotConfigurable current, PDesc.isNotConfigurable desc -> (obj, False)
            | PDesc.isNotConfigurable current, PDesc.hasEnumerable desc,
                xor (PDesc.isEnumerable desc) (PDesc.isEnumerable current) -> (obj, False)
            | PDesc.isGeneric desc -> undefined
            | xor (PDesc.isData current) (PDesc.isData desc) ->
                case PDesc.isConfigurable current of
                    False -> (obj, False)
                    True
                        | PDesc.isData current -> error "not implemented yet: see definedOwnProperty item 9.b"
                        | otherwise -> error "not implemented yet: see definedOwnProperty item 9.c"
            | (PDesc.isData current) && (PDesc.isData desc) ->
                case PDesc.isConfigurable current of
                    False -> error "not implemented yet: see definedOwnProperty item 10.a"
                    True -> error "not implemented yet: see definedOwnProperty item 10.b"
            | (PDesc.isAccessor current) && (PDesc.isAccessor desc) ->
                error "not implemented yet: see item 11.a of algorithm"



xor :: Bool -> Bool -> Bool
xor True True = False
xor False False = False
xor _ _ = True
