module Task_1'5'10 where

fibonacci :: Integer -> Integer
helper :: Integer -> Integer -> Integer -> Integer

fibonacci = helper 0 1 
           
helper acc1 acc2 n | n == 0 = acc1
                   | n > 0 = helper acc2 (acc1 + acc2) (n - 1)
                   | otherwise = helper (acc2 - acc1) acc1 (n + 1)