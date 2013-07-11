module Evaluating.Object
(
    Object(..),
    MaybeObject,
    ObjectId,
    MaybeObjectId,
    Property,
    MaybeProperty,

    newObject,
    prototype,
    extensible,
    notExtensible,

    property,
    deleteProperty,
    putProperty,

    convertPropertyToAccessor,
    convertPropertyToData,
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

data Object = Object
    { prototype :: Maybe ObjectId
    , class_ :: String
    , extensible :: Bool
    , properties :: Properties
    } deriving (Show)

property :: Object -> String -> MaybeProperty
property obj prop = Map.lookup prop (properties obj)

-- primitive
putProperty :: Object -> Property -> Object
putProperty obj prop = undefined

deleteProperty :: Object -> String -> Object
deleteProperty obj prop =
    let newProps = Map.delete prop (properties obj)
    in obj {properties = newProps}

newObject :: Object
newObject = undefined

notExtensible :: Object -> Bool
notExtensible = not . extensible

convertPropertyToAccessor :: Object -> String -> Object
convertPropertyToAccessor = undefined

convertPropertyToData :: Object -> String -> Object
convertPropertyToData = undefined


