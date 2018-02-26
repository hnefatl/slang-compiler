module TypeChecker.Error
(
    Error(..),
    ErrorType(..),
    convertError
) where

import Common
import Parser.Types
import Parser.Expressions (Variable)

data Error = Error Position ErrorType

instance ErrorConvertible Error where
    convertError (Error (Position a r c) err) =
        "Type Error at (row " ++ show r ++ ", col " ++ show c ++ ", abs " ++ show a ++ "): " ++ show err


data ErrorType = NegationNonInteger
               | NotNonBoolean
               | ArithmeticOpBadRHS
               | ArithmeticOpBadLHS
               | BooleanOpBadRHS
               | BooleanOpBadLHS
               | EqualOpMismatch
               | LessOpNonIntegerRHS
               | LessOpNonIntegerLHS
               | AssignmentMismatch
               | IfCondMismatch
               | IfBranchMismatch
               | CaseNonUnion
               | CaseLeftMismatch Type
               | CaseRightMismatch Type Type
               | FstNonProduct
               | SndNonProduct
               | WhileCondMismatch
               | LetMismatch
               | LetFunMismatch Type
               | LetFunNonFunction
               | ApplicationMismatch
               | ApplicationNonFunction
               | NoSuchVariable Variable
               | DereferencingNonReference

instance Show ErrorType where
    show NegationNonInteger             = "Negation of a non-integer expression"
    show NotNonBoolean                  = "\"Not\"-ing of a non-boolean expression"
    show ArithmeticOpBadRHS             = "RHS of arithmetic operator is non-integer"
    show ArithmeticOpBadLHS             = "LHS of arithmetic operator is non-integer"
    show BooleanOpBadRHS                = "RHS of boolean operator is non-boolean"
    show BooleanOpBadLHS                = "LHS of boolean operator is non-boolean"
    show EqualOpMismatch                = "Type of LHS doesn't match type of RHS in equality"
    show LessOpNonIntegerRHS            = "RHS of < must have type integer"
    show LessOpNonIntegerLHS            = "LHS of < must have type integer"
    show AssignmentMismatch             = "Type of LHS doesn't match type of RHS in assignment"
    show IfCondMismatch                 = "Condition of if-statement is non-boolean"
    show IfBranchMismatch               = "Branches of if-statement don't have the same type"
    show CaseNonUnion                   = "Expression in case statement must have type union"
    show (CaseLeftMismatch larg)        = "Expression in inl branch of case statement must have type = " ++ show larg ++ " -> *"
    show (CaseRightMismatch rarg lret)  = "Expression in inr branch of case statement must have type = " ++ show rarg ++ " -> = " ++ show lret
    show FstNonProduct                  = "Expression in fst statement must have product type"
    show SndNonProduct                  = "Expression in snd statement must have product type"
    show WhileCondMismatch              = "Condition in while loop must have boolean type"
    show LetMismatch                    = "Variable's initialiser must have the same type as the variable"
    show (LetFunMismatch funType)       = "Expression in let fun statement isn't of type = " ++ show funType
    show LetFunNonFunction              = "Expected fun type in let fun statement."
    show ApplicationMismatch            = "Expected function in application"
    show ApplicationNonFunction         = "Function applied to value of wrong type"
    show (NoSuchVariable name)          = "Missing variable in environment " ++ name
    show DereferencingNonReference      = "Dereferencing a non-reference"