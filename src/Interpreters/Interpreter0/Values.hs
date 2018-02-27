module Interpreters.Interpreter0.Values where

import Control.Monad (liftM, liftM2)
import qualified Interpreters as I (Result(..), ResultConvertible, convertResult)
import Interpreters.Ast (VariableName, Ast)
import Common

data Value = Unit
           | Integer Integer
           | Boolean Bool
           | Ref VariableName
           | Pair Value Value
           | Inl Value
           | Inr Value
           | Fun VariableName (Ast Position)
           deriving (Eq, Show)

instance I.ResultConvertible Value where
    convertResult Unit        = Just $ I.Unit
    convertResult (Integer i) = Just $ I.Integer i
    convertResult (Boolean b) = Just $ I.Boolean b
    convertResult (Ref _)     = Nothing
    convertResult (Pair l r)  = liftM2 I.Pair (I.convertResult l) (I.convertResult r)
    convertResult (Inl v)     = liftM I.Inl (I.convertResult v)
    convertResult (Inr v)     = liftM I.Inr (I.convertResult v)
    convertResult (Fun _ _)   = Nothing