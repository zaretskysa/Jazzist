module Evaluating.EnvironmentM
(
    putObject,
    getObject,
    modifyObject,

    objectsHeap,

    enterContext,
    enterGlobalContext,
    leaveContext
) where

import Common.Debug
import Evaluating.Eval
import qualified Evaluating.Environment as Env
import qualified Evaluating.ExecutionContext as Ctx
import Evaluating.ExecutionContext (ExecutionContext)
import qualified Evaluating.LexicalEnvironment as LexEnv
import Evaluating.Object
import Evaluating.ObjectsHeap (ObjectsHeap)


enterGlobalContext :: Eval ()
enterGlobalContext = 
    let lexEnv = LexEnv.makeLexicalEnvironment
        varEnv = LexEnv.makeLexicalEnvironment
        globalObj = Just 1
        ctx = Ctx.new lexEnv varEnv globalObj
    in enterContext ctx

enterContext :: ExecutionContext -> Eval ()
enterContext ctx = do
    env <- get
    let newEnv = Env.pushContext env ctx
    put newEnv

leaveContext :: Eval ()
leaveContext = $stub

putObject :: Object -> Eval ObjectId
putObject obj = do
    env <- get
    (newEnv, objId) <- return $ Env.putObject env obj
    put newEnv
    return objId

getObject :: ObjectId -> Eval MaybeObject
getObject objId = do
    env <- get
    return $ Env.getObject env objId

objectsHeap :: Eval ObjectsHeap
objectsHeap = do
    env <- get
    return $ Env.objectsHeap env

modifyObject :: ObjectId -> Object -> Eval ()
modifyObject objId obj = do
    env <- get
    put $ Env.modifyObject env objId obj
