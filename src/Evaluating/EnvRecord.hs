module Evaluating.EnvRecord
(
    EnvRecord(..),
    hasBinding
) where

data EnvRecord = EnvRecord deriving (Show)

hasBinding :: EnvRecord -> String -> Bool
hasBinding envRec identifier = undefined

createMutableBinding :: String -> Bool -> ()
createMutableBinding identifier delete = undefined

setMutableBinding :: ()
setMutableBinding = undefined

getBindingValue :: ()
getBindingValue = undefined

deleteBinding :: String -> ()
deleteBinding identifier = undefined

implicitThisValue :: ()
implicitThisValue = undefined


data DeclEnvRecord = DeclEnvRecord deriving (Show)


