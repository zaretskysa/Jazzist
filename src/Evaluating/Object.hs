module Evaluating.Object
(
    Object,
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

import Evaluating.PropertyDescriptor (PropertyDescriptor)
import Evaluating.Property
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
convertPropertyToAccessor obj propName = 
    let newProps = Props.convertToAccessor (properties obj) propName
    in obj {properties = newProps}

convertPropertyToData :: Object -> String -> Object
convertPropertyToData obj propName = 
    let newProps = Props.convertToData (properties obj) propName
    in obj {properties = newProps}

putDataProperty :: Object -> String -> PropertyDescriptor -> Object
putDataProperty obj name desc = 
    let prop = Prop.dataPropertyFromDescriptor desc
    in putProperty obj name prop

putAccessorProperty :: Object -> String -> PropertyDescriptor -> Object
putAccessorProperty obj name desc =
    let prop = Prop.accessorPropertyFromDescriptor desc
    in putProperty obj name prop
