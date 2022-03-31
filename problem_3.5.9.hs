module Task_3'5'8 where

evenOnly :: [a] -> [a]
evenOnly = reverse . snd . foldl (\(pos,lst) x  -> if even pos then (pos + 1, x:lst) else (pos + 1, lst)) (1, [])

