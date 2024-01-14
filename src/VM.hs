{-
-- EPITECH PROJECT, 2024
-- GLaDOS
-- File description:
-- VM.hs
-}

module VM (exec) where

import Types

exec :: Insts -> Stack -> Value
exec ((Ret):_) ((IntVM i):_) = (IntVM i)
exec _ _ = (IntVM 0)
