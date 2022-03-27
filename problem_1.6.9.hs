module Task_1'6'9 where

integration :: (Double -> Double) -> Double -> Double -> Double
integration f a b = helper 0 a iters
    where
    iters :: Int
    h :: Double
    helper :: Double -> Double -> Int
        -> Double
    iters = 1000
    h = (b - a) / fromIntegral iters
    helper sum x 0 = sum
    helper sum x n = helper (sum + h * (f x + f (x + h)) / 2) (x + h) (n - 1)