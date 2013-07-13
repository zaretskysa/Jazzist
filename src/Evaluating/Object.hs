module Evaluating.Object
(
    Object(..),
    MaybeObject,
    ObjectId,
    MaybeObjectId,
    Property,
    MaybeProperty,

    new,
    prototype,
    extensible,
    notExtensible,

    property,
    deleteProperty,
    putProperty,

    convertPropertyToAccessor,
    convertPropertyToData,

    putDataProperty,
    putAccessorProperty,
) where

import Data.Maybe

import Evaluating.Value
import Evaluating.NamedDataProperty (NamedDataProperty)
import Evaluating.NamedAccessorProperty (NamedAccessorProperty)
import Evaluating.PropertyDescriptor (PropertyDescriptor, MaybePropertyDescriptor)
import qualified Evaluating.PropertyDescriptor as PDesc
import Evaluating.Property
import Evaluating.ObjectsHeap
import Evaluating.Properties (Properties)
import qualified Evaluating.Properties as Props
import qualified Evaluating.Property as Prop


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
property obj prop = Props.lookup (properties obj) prop

putProperty :: Object -> String -> Property -> Object
putProperty obj name prop = 
    let newProps = Props.put (properties obj) name prop
    in obj {properties = newProps}

deleteProperty :: Object -> String -> Object
deleteProperty obj prop = 
    let newProps = Props.delete newProps prop
    in obj {properties = newProps}

new :: Object
new = Object
    { prototype = Nothing
    , class_ = ""
    , extensible = False
    , properties = Props.empty
    }

notExtensible :: Object -> Bool
notExtensible = not . extensible

convertPropertyToAccessor :: Object -> String -> Object
convertPropertyToAccessor = undefined

convertPropertyToData :: Object -> String -> Object
convertPropertyToData = undefined

putDataProperty :: Object -> String -> PropertyDescriptor -> Object
putDataProperty obj name desc = 
    let prop = Prop.dataPropertyFromDescriptor desc
    in putProperty obj name prop

putAccessorProperty :: Object -> String -> PropertyDescriptor -> Object
putAccessorProperty obj name desc =
    let prop = Prop.accessorPropertyFromDescriptor desc
    in putProperty obj name prop
