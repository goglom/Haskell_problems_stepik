module Task_3'6'3 where

lastElem :: [a] -> a
lastElem = foldl1 (flip const)


