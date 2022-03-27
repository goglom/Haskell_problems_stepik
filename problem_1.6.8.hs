module Task_1'6'8 where

sum'n'count :: Integer -> (Integer, Integer)
sum'n'count x = helper 0 0 x
    where
        helper 0 0 0 = (0, 1)
        helper count sum 0 = (sum, count)
        helper count sum x 
            | x > 0 = helper (count + 1) (sum + mod x 10) (div x 10)
            | otherwise = helper count sum (-x)

