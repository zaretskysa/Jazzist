module Evaluating.DeclEnvRecordBinding
(
    Binding,
    newBinding,
    setValue,
    uninitialized,
    immutable,
    mutable,
    deletable,
    value,

    Bindings,
    lookup,
    member,
    insert,
    delete
) where

import Prelude hiding (lookup)
import Evaluating.Value
import qualified Data.Map as Map
import Data.Maybe (isJust)

type Bindings = Map.Map String Binding

data Binding = Binding 
    { deletable :: Bool
    , mutable :: Bool
    , value :: MaybeValue
    } deriving (Show)

lookup :: Bindings -> String -> Maybe Binding
lookup bindings name = Map.lookup name bindings

member :: Bindings -> String -> Bool
member bindings name = Map.member name bindings

insert :: Bindings -> String -> Binding -> Bindings
insert bindings name binding = Map.insert name binding bindings

delete :: Bindings -> String -> Bindings
delete bindings name = Map.delete name bindings



initialized :: Binding -> Bool
initialized binding = isJust $ value binding

uninitialized :: Binding -> Bool
uninitialized = not . initialized

immutable :: Binding -> Bool
immutable = not . mutable

newBinding :: MaybeValue -> Bool -> Bool -> Binding
newBinding val isMutable isDeletable =
    Binding {value = val, mutable = isMutable, deletable = isDeletable}

setValue :: Binding -> Value -> Binding
setValue binding val =
    binding {value = Just val}