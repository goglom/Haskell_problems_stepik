module Task_3'2'8 where

foo :: a -> [a] -> [[a]]
foo elem [] = [[elem]]
foo elem lst = impl elem [] lst []
    where
        impl :: a -> [a] -> [a] -> [[a]] -> [[a]]
        impl elem headPart [] res = (headPart ++ [elem]) : res
        impl elem headPart tailPart@(x:xs) res =
            impl elem (headPart ++ [x]) xs ((headPart ++ [elem] ++ tailPart) : res)


perms :: [a] -> [[a]]
perms [] = [[]]
perms [x] = [[x]]
perms [x, y] = [[x, y], [y, x]]
perms (x:xs) = concatMap (foo x) (perms xs)