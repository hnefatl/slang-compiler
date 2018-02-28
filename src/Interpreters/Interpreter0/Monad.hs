{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Interpreters.Interpreter0.Monad
(
    Interpreter0,
    getEnvironment,
    getValue,
    getValueE,
    setValue,
    removeValue,
    local,
    runtimeError,
    runInterpreter0Monad,

    liftIO -- Export liftIO from the Control.Monad.IO.Class module, as it's useful for doing IO actions in this monad
) where

import Control.Monad.Except
import Control.Monad.State
import qualified Data.Map as M

import Interpreters.Error
import Interpreters.Interpreter0.Values
import Interpreters.Ast (VariableName)

type Environment = M.Map VariableName Value

newtype Interpreter0 a = Interpreter0
    {
        run :: StateT Environment (ExceptT Error IO) a
    }
    deriving (Functor, Applicative, Monad, MonadIO, MonadState Environment, MonadError Error)

getEnvironment :: Interpreter0 Environment
getEnvironment = get

getValue :: VariableName -> Interpreter0 (Maybe Value)
getValue k = gets (M.lookup k)

getValueE :: VariableName -> Error -> Interpreter0 Value
getValueE k e = do
                    val <- getValue k
                    case val of
                        Just v  -> return v
                        Nothing -> throwError e

setValue :: VariableName -> Value -> Interpreter0 ()
setValue k v = modify (M.insert k v)

removeValue :: VariableName -> Interpreter0 ()
removeValue k = modify (M.delete k)

local :: VariableName -> Value -> Interpreter0 a -> Interpreter0 a
local k v a = do
                existingValue <- getValue k
                setValue k v
                res <- a
                case existingValue of
                    Just oldV -> setValue k oldV
                    Nothing   -> removeValue k
                return res

runtimeError :: Error -> Interpreter0 a
runtimeError = throwError

runInterpreter0Monad :: Interpreter0 a -> IO (Either Error a)
runInterpreter0Monad a = runExceptT $ evalStateT (run a) M.empty