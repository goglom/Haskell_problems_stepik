module Task_3'5'7 where

meanList :: [Double] -> Double
meanList = (\(n,s) -> s / n) . foldr (\x (n, s) -> (n + 1, x + s)) (0, 0)
