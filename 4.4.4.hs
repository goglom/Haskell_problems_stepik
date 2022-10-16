module Problem_4'4'4 where

data Coord a = Coord a a deriving (Eq, Show)

getCenter :: Double -> Coord Int -> Coord Double
getCenter size (Coord x y) = Coord (fromIntegral x * size) (fromIntegral y * size)

getCell :: Double -> Coord Double -> Coord Int
getCell size (Coord x y) = Coord (floor $ x / size) (floor $ y / size)
