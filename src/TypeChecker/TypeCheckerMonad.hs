{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module TypeChecker.TypeCheckerMonad
(
    TypeChecker,
    Environment,
    Error,
    inModifiedEnv,
    fromEnv,
    typeError,
    runTypeChecker
) where

import Control.Monad.Reader
import Control.Monad.Except
import Data.Map.Lazy as M

import qualified Parser.Types as T
import qualified Parser.Expressions as E

type Environment = M.Map E.Variable T.Type
type Error = String

newtype TypeChecker a = TypeChecker
    {
        run :: ReaderT Environment (Except Error ) a
    }
    deriving (Functor, Applicative, Monad, MonadReader Environment, MonadError Error)

inModifiedEnv :: (Environment -> Environment) -> TypeChecker a -> TypeChecker a
inModifiedEnv = local

fromEnv :: (Environment -> a) -> TypeChecker a
fromEnv = asks

typeError :: String -> TypeChecker a
typeError = throwError

runTypeChecker :: TypeChecker a -> Either Error a
runTypeChecker t = runExcept $ runReaderT (run t) M.empty