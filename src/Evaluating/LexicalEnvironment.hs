module Evaluating.LexicalEnvironment
(
    LexicalEnvironment,

    makeLexicalEnvironment,
    getIdentifierReference,
    newDeclarativeEnvironment,
    newObjectEnvironment
) where

import Evaluating.Reference
import qualified Evaluating.DeclEnvRecord as Decl
import Evaluating.Object (Object)

data LexicalEnvironment = LexicalEnvironment
    {
        envRecord :: Decl.DeclEnvRecord,
        outer :: Maybe LexicalEnvironment
    }
    deriving (Show)

makeLexicalEnvironment :: LexicalEnvironment
makeLexicalEnvironment = undefined

getIdentifierReference :: Maybe LexicalEnvironment -> String -> Reference
getIdentifierReference Nothing name = Reference name UndefinedRefVal
getIdentifierReference (Just lexEnv) name =
    let envRec = envRecord lexEnv
    in case Decl.hasBinding envRec name of
        True -> Reference name (EnvRecordRefVal envRec)
        False -> getIdentifierReference (outer lexEnv) name

newDeclarativeEnvironment :: Maybe LexicalEnvironment -> LexicalEnvironment
newDeclarativeEnvironment _outerEnv = undefined

newObjectEnvironment :: Maybe LexicalEnvironment -> Object -> LexicalEnvironment
newObjectEnvironment _outerEnv _obj = undefined
