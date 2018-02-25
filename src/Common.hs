module Common
(
    ifexpr,
    Error
) where

-- Just a utility "if expression"
ifexpr :: Bool -> a -> a -> a
ifexpr True v _ = v
ifexpr False _ v = v

type Error = String