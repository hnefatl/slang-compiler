module Common
(
    ifexpr,
    Position(..),
    FrontEndError,
    ErrorConvertible,
    convertError
) where

-- Just a utility "if expression"
ifexpr :: Bool -> a -> a -> a
ifexpr True v _ = v
ifexpr False _ v = v

data Position = Position
    {
        absCharPos  :: Int, -- Absolute character position in the file
        row         :: Int, -- Line number
        col         :: Int  -- Column number
    }
    deriving (Eq, Show)

type FrontEndError = String

class ErrorConvertible t where
    convertError :: t -> FrontEndError