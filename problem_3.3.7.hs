module Task_3'3'3 where

data Odd = Odd Integer deriving (Eq, Show)

instance Enum Odd where
    toEnum x    | odd x = Odd $ toEnum(x) 
                | otherwise = undefined
    fromEnum (Odd x) = fromEnum x 
    succ (Odd x) = Odd(x + 2)
    pred (Odd x) = Odd(x - 2)
    enumFrom (Odd x) = map Odd [x, x + 2..]
    enumFromThen (Odd x1) (Odd x2) = map Odd [x1, x2..]
    enumFromThenTo (Odd x1) (Odd x2) (Odd lim) = map Odd [x1, x2 .. lim]
    enumFromTo (Odd x) (Odd lim)
        | x <= lim = map Odd [x, x + 2.. lim]
        | otherwise = []
