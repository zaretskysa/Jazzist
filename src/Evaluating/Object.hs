module Evaluating.Object
(
    Object(..)
) where

import Evaluating.Value
import Evaluating.NamedDataProperty
import Evaluating.NamedAccessorProperty

data Object = Object
    { prototype :: Maybe Object
    , class_ :: String
    , extensible :: Bool
    } deriving (Show)

type PropertyDescriptor = String

get :: Object -> String -> Value
get obj propertyName = undefined

getOwnProperty :: Object -> String -> Maybe PropertyDescriptor
getOwnProperty obj propName = undefined

getProperty :: Object -> String -> Maybe PropertyDescriptor
getProperty obj propName = undefined

put :: Object -> String -> Value -> Bool -> Object
put obj propName value failFlag = undefined

canPut :: Object -> String -> Bool
canPut obj propName = undefined

hasProperty :: Object -> String -> Bool
hasProperty obj propName = undefined

delete :: Object -> String -> Bool -> Object
delete obj propName flag = undefined

defaultValue :: Object -> String -> Primitive
defaultValue obj hint = undefined

defineOwnProperty :: Object -> String -> PropertyDescriptor -> Bool -> Object
defineOwnProperty obj propName propDesc flag = undefined
