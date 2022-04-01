module Task_4'1'7 where

dist :: Char -> Char -> Int
dist a b | a < b     = 1 + dist (succ a) b 
         | a == b    = 0
         | otherwise = dist (pred a) b - 1

charToInt :: Char -> Int
charToInt c | c >= '0' && c <= '9' = dist '0' c
            | otherwise            = undefined

