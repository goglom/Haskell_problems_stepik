module Task_3'1'12 where

sum3 :: Num a => [a] -> [a] -> [a] -> [a]
sum3 xs ys zs = reverse $ sum3Impl [] xs ys zs 
    where
        sum3Impl acc [] [] [] = acc
        sum3Impl acc x1s x2s x3s = 
            sum3Impl ((myHead x1s + myHead x2s + myHead x3s) : acc) 
            (myTail x1s) 
            (myTail x2s)
            (myTail x3s)

        myHead :: Num a => [a] -> a
        myHead (x:xs) = x
        myHead _ = 0

        myTail [] = []
        myTail (_:xs) = xs