module Task_3'2'6 where

qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort [x] = [x]
qsort lst = qsort p1 ++ p2 ++ qsort p3
    where
        baseElem = head lst
        p1 = filter (< baseElem)  lst
        p2 = filter (== baseElem) lst
        p3 = filter (> baseElem)  lst