module Interpreters.Error
(
    Error(..),
    ErrorType(..),
    convertError
) where

import qualified Interpreters.Ast as A
import Common

data Error = Error Position ErrorType

instance ErrorConvertible Error where
    convertError (Error (Position a r c) err) = 
        "Runtime Error at (row " ++ show r ++ ", col " ++ show c ++ ", abs " ++ show a ++ "): " ++ show err

data ErrorType = DivisionByZero
               | MissingVariable A.Variable
               | FATAL_InlInrMismatch (A.Ast Position)
               | FATAL_InvalidUOpArguments A.UOp
               | FATAL_InvalidBOpArguments A.BOp
               | FATAL_InvalidApplication
               | FATAL_RefMissing

instance Show ErrorType where
    show DivisionByZero = "Division by zero"
    show (MissingVariable name) = "Variable not defined: " ++ show name
    show (FATAL_InlInrMismatch expr) = "TYPECHECKER BUG: Should be Inl or Inr: " ++ show expr
    show (FATAL_InvalidUOpArguments op) = "TYPECHECKER BUG: Invalid arguments to unary operator: " ++ show op
    show (FATAL_InvalidBOpArguments op) = "TYPECHECKER BUG: Invalid arguments to binary operator: " ++ show op
    show FATAL_InvalidApplication = "TYPECHECKER BUG: Invalid application"
    show FATAL_RefMissing = "COMPILER BUG: Reference not found"