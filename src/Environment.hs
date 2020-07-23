module Environment where

import Types
import Data.IORef
import Control.Monad.Except

type Env = IORef [(String, IORef LispVal)]

-- Making a new environment --
nullEnv :: IO Env
nullEnv = newIORef []

-- Does this variable already exist --
isBound :: Env -> String -> IO Bool
isBound envRef var = readIORef envRef >>= return . maybe False (const True) . lookup var

-- Get the variable from the environment --
getVar :: Env -> String -> IOThrowsError LispVal
getVar envRef var = do env <- liftIO $ readIORef envRef
                       maybe (throwError $ UnboundVar "Getting an unbound variable" var)
                             (liftIO . readIORef) (lookup var env)
                             

-- Set the variable in the environment, if it is already bound --
setVar :: Env -> String -> LispVal -> IOThrowsError LispVal
setVar envRef var val = do env <- liftIO $ readIORef envRef 
                           maybe (throwError $ UnboundVar "Setting an unbound variable" var)
                                 (liftIO . (flip writeIORef val))
                                 (lookup var env)
                           return val

-- Bind a new variable --
defineVar :: Env -> String -> LispVal -> IOThrowsError LispVal
defineVar envRef var value = do
  alreadyDefined <- liftIO $ isBound envRef var
  if alreadyDefined
     then setVar envRef var value >> return value
     else liftIO $ do
       valueRef <- newIORef value
       env <- readIORef envRef
       writeIORef envRef ((var, valueRef) : env)
       return value

-- Bind multiple variables at once --
bindVars :: Env -> [(String, LispVal)] -> IO Env
bindVars envRef bindings = readIORef envRef >>= extendEnv bindings >>= newIORef
  where extendEnv bindings env = liftM (++ env) (mapM addBinding bindings)
        addBinding (var, value) = do ref <- newIORef value
                                     return (var, ref)


