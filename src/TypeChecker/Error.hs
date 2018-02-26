module TypeChecker.Error
(
    Error(..),
    ErrorType(..),
    convertError
) where

import Common
import Parser.Types
import qualified Parser.Expressions as E

data Error = Error Position ErrorType

instance ErrorConvertible Error where
    convertError (Error (Position a r c) err) =
        "Type Error at (row " ++ show r ++ ", col " ++ show c ++ ", abs " ++ show a ++ "): " ++ show err


data ErrorType = OpNegNonInteger (E.Expr Position)
               | OpNotNonBoolean (E.Expr Position)
               | ArithmeticOpBadRHS (E.Expr Position)
               | ArithmeticOpBadLHS (E.Expr Position)
               | BooleanOpBadRHS (E.Expr Position)
               | BooleanOpBadLHS (E.Expr Position)
               | EqualOpMismatch (E.Expr Position) (E.Expr Position)
               | LessOpNonIntegerRHS (E.Expr Position)
               | LessOpNonIntegerLHS (E.Expr Position)
               | AssignmentMismatch (E.Expr Position) (E.Expr Position)
               | IfCondMismatch (E.Expr Position)
               | IfBranchMismatch (E.Expr Position) (E.Expr Position)
               | CaseNonUnion (E.Expr Position)
               | CaseLeftMismatch Type (E.Expr Position)
               | CaseRightMismatch Type (E.Expr Position)
               | FstNonProduct (E.Expr Position)
               | SndNonProduct (E.Expr Position)
               | WhileCondMismatch (E.Expr Position)
               | LetMismatch Type (E.Expr Position)
               | LetFunMismatch Type (E.Expr Position)
               | LetFunNonFunction (E.Expr Position)
               | ApplicationMismatch (E.Expr Position) (E.SimpleExpr Position)
               | ApplicationBadArg Type (E.SimpleExpr Position)
               | NoSuchVariable E.Variable
               | DereferencingNonReference (E.SimpleExpr Position)

instance Show ErrorType where
    show (OpNegNonInteger e)            = "Negation of a non-integer expression:\n" ++ show e
    show (OpNotNonBoolean e)            = "\"Not\"-ing of a non-boolean expression:\n" ++ show e
    show (ArithmeticOpBadRHS e)         = "RHS of arithmetic operator is non-integer:\n" ++ show e
    show (ArithmeticOpBadLHS e)         = "LHS of arithmetic operator is non-integer:\n" ++ show e
    show (BooleanOpBadRHS e)            = "RHS of boolean operator is non-boolean:\n" ++ show e
    show (BooleanOpBadLHS e)            = "LHS of boolean operator is non-boolean:\n" ++ show e
    show (EqualOpMismatch l r)          = "Type of LHS doesn't match type of RHS in equality:\nLeft: " ++ show l ++ "\nRight: " ++ show r
    show (LessOpNonIntegerRHS e)        = "RHS of < must have type integer:\n" ++ show e
    show (LessOpNonIntegerLHS e)        = "LHS of < must have type integer:\n" ++ show e
    show (AssignmentMismatch l r)       = "Type of LHS doesn't match type of RHS in assignment:\nLeft: " ++ show l ++ "\nRight: " ++ show r
    show (IfCondMismatch e)             = "Condition of if-statement is non-boolean:\n" ++ show e
    show (IfBranchMismatch l r)         = "Branches of if-statement don't have the same type:\nLeft: " ++ show l ++ "\nRight: " ++ show r
    show (CaseNonUnion e)               = "Expression in case statement must have type union:\n" ++ show e
    show (CaseLeftMismatch t e)         = "Expression in inl branch of case statement must take type: " ++ show t ++ "\n" ++ show e
    show (CaseRightMismatch t e)        = "Expression in inr branch of case statement must take type: " ++ show t ++ "\n" ++ show e
    show (FstNonProduct e)              = "Expression in fst statement must have product type:\n" ++ show e
    show (SndNonProduct e)              = "Expression in snd statement must have product type:\n" ++ show e
    show (WhileCondMismatch e)          = "Condition in while loop must have boolean type:\n" ++ show e
    show (LetMismatch t e)              = "Variable's initialiser must have type: " ++ show t ++ "\n" ++ show e
    show (LetFunMismatch funType e)     = "Expression in let fun statement must be of type: " ++ show funType ++ "\n" ++ show e
    show (LetFunNonFunction e)          = "Expected fun type in let fun statement:\n" ++ show e
    show (ApplicationMismatch e1 e2)    = "Expected function in application:\nFunction: " ++ show e1 ++ "\nArgument: " ++ show e2
    show (ApplicationBadArg t e)        = "Function not applied to value of type: " ++ show t ++ "\n" ++ show e
    show (NoSuchVariable name)          = "Missing variable in environment " ++ name
    show (DereferencingNonReference e)  = "Dereferencing a non-reference:\n" ++ show e