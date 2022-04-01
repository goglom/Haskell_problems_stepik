module Task_3'6'10 where

import Data.List (unfoldr)

revRange :: (Char,Char) -> [Char]
revRange (a,z) = unfoldr g z
  where g = \x -> if x < a then Nothing else Just (x, pred x)

