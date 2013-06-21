module Evaluating.Reference
(
    Reference(..),
    RefBaseValue(..),

    refName,
    refBaseValue,
    isStrictRef,
    refHasPrimitiveBase,
    isPropertyRef,
    isUnresolvableRef,
    getRefValue,
    putRefValue
) where

import Data.Maybe

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

refHasPrimitiveBase :: Reference -> Bool
refHasPrimitiveBase (Reference _ value) = undefined
refHasPrimitiveBase _ = False

isPropertyRef :: Reference -> Bool
isPropertyRef (Reference _ _) = undefined
isPropertyRef _ = undefined

isUnresolvableRef :: Reference -> Bool
isUnresolvableRef = not . isPropertyRef

getRefValue :: Reference -> RefBaseValue
getRefValue = refBaseValue

putRefValue :: Reference -> ()
putRefValue _ = ()
 