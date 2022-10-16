module Task_1'6'6 where

seqA :: Integer -> Integer
seqA n 
    | n >= 0 = let
            helper :: Integer -> Integer -> Integer -> Integer 
                -> Integer 
            helper a0 a1 a2 0 = a0
            helper a0 a1 a2 1 = a1
            helper a0 a1 a2 2 = a2
            helper a0 a1 a2 n = helper a1 a2 (a2 + a1 - 2 * a0) (n - 1)
        in helper 1 2 3 n
    | otherwise = undefined