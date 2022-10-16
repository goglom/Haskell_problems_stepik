module Probelm_4'4'3 where

data Coord a = Coord a a

diff :: (Num a) => Coord a -> Coord a -> Coord a
diff (Coord x1 y1) (Coord x2 y2) = Coord (x2 - x1) (y2 - y1)

distance :: Coord Double -> Coord Double -> Double
distance p1 p2 = sqrt(x*x + y*y) where (Coord x y) = diff p1 p2

manhDistance :: Coord Int -> Coord Int -> Int
manhDistance p1 p2 = abs x + abs y where (Coord x y) = diff p1 p2
