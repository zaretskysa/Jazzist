module Evaluating.DeclEnvRecord
(
    DeclEnvRecord,

    hasBinding,
    createMutableBinding,
    setMutableBinding,
    getBindingValue,
    deleteBinding,
    implicitThisValue,

    createImmutableBinding,
    initializeImmutableBinding
) where

--import Evaluating.EnvRecord
import Evaluating.Value
import Prelude hiding (lookup)
import Common.Result
import Data.Maybe (fromJust)
import qualified Evaluating.DeclEnvRecordBinding as B
import Evaluating.DeclEnvRecordBinding (Binding, Bindings)

data DeclEnvRecord = DeclEnvRecord 
    { bindings :: Bindings
    } deriving (Show)

--instance EnvRecord DeclEnvRecord where
--    hasBinding = undefined


hasBinding :: DeclEnvRecord -> String -> Bool
hasBinding record name = 
    B.member (bindings record) name

createMutableBinding :: DeclEnvRecord -> String -> Bool -> DeclEnvRecord
createMutableBinding record name deletable = 
    let binding = B.newBinding Nothing True deletable
    in insert record name binding

setMutableBinding :: DeclEnvRecord -> String -> Value -> Bool -> Result DeclEnvRecord
setMutableBinding record name value strict =
    case lookup record name of
        Just binding
            | B.mutable binding ->
                let newBinding = B.setValue binding value
                in Success $ insert record name newBinding
            | otherwise -> if strict then Fail else Success record
        Nothing -> error $ "Record must contain binding " ++ name

getBindingValue :: DeclEnvRecord -> String -> Bool -> Result Value
getBindingValue record name strict = 
    case lookup record name of
        Just binding
            | B.uninitialized binding, B.immutable binding -> 
                if strict then Fail else Success UndefinedValue
            | otherwise -> Success $ fromJust $ B.value binding
        Nothing -> error $ "Record must contains binding " ++ name

deleteBinding :: DeclEnvRecord -> String -> Result DeclEnvRecord
deleteBinding record name = 
    case lookup record name of
        Nothing -> Success record
        Just binding
            | B.deletable binding -> Success $ delete record name
            | otherwise -> Fail

implicitThisValue :: DeclEnvRecord -> Value
implicitThisValue _ = UndefinedValue

createImmutableBinding :: DeclEnvRecord -> String -> DeclEnvRecord
createImmutableBinding record name = 
    case lookup record name of
        Just _ -> error $ "Record must not contain binding " ++ name
        Nothing ->
            let binding = B.newBinding Nothing False False
            in insert record name binding

initializeImmutableBinding :: DeclEnvRecord -> String -> Value -> DeclEnvRecord
initializeImmutableBinding record name value =
    case lookup record name of
        Just binding
            | B.uninitialized binding, B.immutable binding -> 
                let newBinding = B.setValue binding value
                in insert record name newBinding
            | otherwise -> error "Binding must be uninitialized and immutable"
        Nothing -> error "Binding must present in record"

-- helpers

insert :: DeclEnvRecord -> String -> Binding -> DeclEnvRecord
insert record name binding = 
    let newBindings = B.insert (bindings record) name binding
    in record {bindings = newBindings}

lookup :: DeclEnvRecord -> String -> Maybe Binding
lookup record name = B.lookup (bindings record) name

delete :: DeclEnvRecord -> String -> DeclEnvRecord
delete record name =
    let newBindings = B.delete (bindings record) name
    in record {bindings = newBindings}
