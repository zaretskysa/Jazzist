module Evaluating.Properties
(
    Properties,

    empty,

    put,
    get,
    delete,
    lookup,

    convertToData,
    convertToAccessor,
) where

import Prelude hiding (lookup)
import qualified Data.Map as Map

import Evaluating.Property (Property)
import qualified Evaluating.Property as Prop

type Properties = Map.Map String Property

empty :: Properties
empty = Map.empty

put :: Properties -> String -> Property -> Properties
put = undefined

get :: Properties -> String -> Property
get props name = case lookup props name of
    Just prop -> prop
    Nothing -> error $ "Property does not exist: " ++ name

delete :: Properties -> String -> Properties
delete props name = Map.delete name props

lookup :: Properties -> String -> Maybe Property
lookup props name = Map.lookup name props

type PropConvertor = Property -> Property

convertProperty :: Properties -> String -> PropConvertor -> Properties
convertProperty props name convertor =
    case lookup props name of
        Just prop -> put props name (convertor prop)
        Nothing -> error $ "Property does not exist: " ++ name

convertToData :: Properties -> String -> Properties
convertToData props name =
    convertProperty props name Prop.convertToData

convertToAccessor :: Properties -> String -> Properties
convertToAccessor props name =
    convertProperty props name Prop.convertToAccessor
