module Interpreters.Error
(
    Error(..),
    ErrorType(..),
    convertError
) where

import Interpreters.Ast
import Common

data Error = Error Position ErrorType

instance ErrorConvertible Error where
    convertError (Error (Position a r c) err) = 
        "Runtime Error at (row " ++ show r ++ ", col " ++ show c ++ ", abs " ++ show a ++ "): " ++ show err

data ErrorType = DivisionByZero Integer
               | MissingVariable VariableName
               | FATAL_InlInrMismatch (Ast Position)
               | FATAL_InvalidUOpArguments UOp
               | FATAL_InvalidBOpArguments BOp
               | FATAL_InvalidApplication
               | FATAL_RefMissing

instance Show ErrorType where
    show (DivisionByZero i) = "Division by zero: " ++ show i ++ "/0"
    show (MissingVariable name) = "Variable not defined: " ++ show name
    show (FATAL_InlInrMismatch expr) = "TYPECHECKER BUG: Should be Inl or Inr:\n" ++ show expr
    show (FATAL_InvalidUOpArguments op) = "TYPECHECKER BUG: Invalid arguments to unary operator: " ++ show op
    show (FATAL_InvalidBOpArguments op) = "TYPECHECKER BUG: Invalid arguments to binary operator: " ++ show op
    show FATAL_InvalidApplication = "TYPECHECKER BUG: Invalid application"
    show FATAL_RefMissing = "COMPILER BUG: Reference not found"