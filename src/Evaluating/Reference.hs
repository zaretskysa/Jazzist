module Evaluating.Reference
(
    Reference(..),
    RefBaseValue(..),

    refName,
    refBaseValue,
    isStrictRef,
    hasPrimitiveBase,
    isPropertyRef,
    isUnresolvableRef,
    getRefValue,
    putRefValue
) where

import Data.Maybe

import Evaluating.Value
import Evaluating.Object
import Evaluating.EnvironmentRecord

data RefBaseValue = 
    UndefinedRefVal
    | ObjectRefVal Object
    | BoolRefVal Bool
    | StringRefVal String
    | NumberRefVal Double
    | EnvRecordRefVal EnvironmentRecord
    deriving (Show)

data Reference = Reference String RefBaseValue
    deriving (Show)

refName :: Reference -> String
refName (Reference name _) = name

refBaseValue :: Reference -> RefBaseValue
refBaseValue (Reference _ value) = value

isStrictRef :: Reference -> Bool
isStrictRef _ = False

hasPrimitiveBase :: Reference -> Bool
hasPrimitiveBase (Reference _ value) = undefined
hasPrimitiveBase _ = False

isPropertyRef :: Reference -> Bool
isPropertyRef (Reference _ _) = undefined
isPropertyRef _ = undefined

isUnresolvableRef :: Reference -> Bool
isUnresolvableRef = not . isPropertyRef

getRefValue :: Reference -> RefBaseValue
getRefValue = refBaseValue

putRefValue :: Reference -> ()
putRefValue _ = ()

baseToValue :: RefBaseValue -> Value
baseToValue UndefinedRefVal = UndefinedValue
baseToValue (ObjectRefVal obj) = ObjectValue obj
baseToValue (BoolRefVal bool) = BooleanValue bool
baseToValue (StringRefVal str) = StringValue str
baseToValue (NumberRefVal num) = NumberValue num
baseToValue _ = error "Error conversion to js value"


