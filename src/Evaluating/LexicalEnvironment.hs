module Evaluating.LexicalEnvironment
(
    LexicalEnvironment,

    makeLexicalEnvironment,
    getIdentifierReference
) where

import Evaluating.Reference
import Evaluating.EnvRecord
import Evaluating.DeclEnvRecord

data LexicalEnvironment = LexicalEnvironment
    {
        --envRecord :: EnvRecord
    }
    deriving (Show)

makeLexicalEnvironment :: LexicalEnvironment
makeLexicalEnvironment = undefined

getIdentifierReference :: Maybe LexicalEnvironment -> String -> Reference
getIdentifierReference Nothing name = Reference name UndefinedRefVal
getIdentifierReference (Just lexEnv) name = undefined
--    let envRec = envRecord lexEnv
--        exists = hasBinding envRec name
--    in case exists of
--        True -> Reference name (EnvRecordRefVal envRec)
--        False -> undefined






