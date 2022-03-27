module Task_3'3'9 where

coins = [2,3,7]

--change :: (Ord a, Num a) => a -> [[a]]
change 0 = [[]]
change sum = [coin : xs | coin <- coins, sum >= coin, xs <- change (sum - coin)]
{-
    change 7 ~> [coin : xs | coin <- [2,3,7], 7 >= coin, xs <- change (7 - coin)] ] ~>
    [
        2 : xs | xs <- change(5),
        3 : xs | xs <- change(4),
        7 : xs | xs <- change(0),
        
    ]
-}