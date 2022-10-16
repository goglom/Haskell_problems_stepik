module Task_3'1'13 where

groupElems :: Eq a => [a] -> [[a]]
groupElems [] = []
groupElems lst =
    let (gp, tl) = span (== head lst) lst 
    in gp : groupElems tl