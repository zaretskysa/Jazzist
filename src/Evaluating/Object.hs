module Object
(
) where


type PropertyName = String

data PropertyDescriptor = PropertyDescriptor deriving (Show)

data Any = Any deriving (Show)

data Hint = Hint deriving (Show)

data Primitive = Primitive deriving (Show)

data AnyOrReference = AnyOrReference deriving (Show)

data LexicalEnvironment = LexicalEnvironment deriving (Show)

data Code = Code deriving (Show)

type Index = Integer

data MatchResult = MatchResult deriving (Show)

data Object = Object {
    } deriving (Show)


-- all objects internal properties

prototype :: Object -> Maybe Object
prototype obj = undefined

class_ :: Object -> String
class_ obj = undefined

extensible :: Object -> Bool
extensible obj = undefined

get :: Object -> PropertyName -> Any
get obj propName = undefined

getOwnProperty :: Object -> PropertyName -> Maybe PropertyDescriptor
getOwnProperty obj = undefined

getProperty :: Object -> PropertyName -> Maybe PropertyDescriptor
getProperty obj propName = undefined

put :: Object -> PropertyName -> Any -> Bool -> ()
put obj propName = undefined

hasPropery :: Object -> PropertyName -> Bool
hasPropery obj propName = undefined

delete :: Object -> PropertyName -> Bool -> Bool
delete obj propName flag = undefined


defaultValue :: Object -> Hint -> Primitive
defaultValue obj hint = undefined

defineOwnProperty :: Object -> PropertyName -> PropertyDescriptor -> Bool -> Bool
defineOwnProperty obj propName propDesc flag = undefined


-- special objects internal properties

primitiveValue :: Object -> Primitive
primitiveValue obj = undefined

construct :: Object -> [Any] -> Object
construct obj anys = undefined

call :: Object -> Any -> [Any] -> AnyOrReference
call obj anyVal anys = undefined

hasInstance :: Object -> Any -> Bool
hasInstance obj anyVal = undefined

scope :: Object -> LexicalEnvironment
scope obj = undefined

formalParameters :: Object -> [String] 
formalParameters obj = undefined

code :: Object -> Code
code obj = undefined

targetFunction :: Object -> Object
targetFunction obj = undefined

boundThis :: Object -> Any
boundThis obj = undefined

boundArguments :: Object -> [Any]
boundArguments obj = undefined

match :: Object -> String -> Index -> MatchResult
match obj str index = undefined

parameterMap :: Object -> Object
parameterMap obj = undefined


--data NamedDataProperty = NamedDataProperty {
--        value :: Object,
--        writable :: Bool,
--        enumerable :: Bool,
--        configurable :: Bool
--    } deriving (Show)
--
--data NamedAccessorPropery = NamedAccessorPropery {
--        get ::String, -- Object or Undefined must be here
--        set ::String, -- Object or Undefined must be here
--        enumerable :: Bool,
--        configurable :: Bool
--    } deriving (Show)


