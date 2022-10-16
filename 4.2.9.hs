
module Task_4'2'9 where

import Data.Function (on)

data Bit = Zero | One
data Sign = Minus | Plus
data Z = Z Sign [Bit]

instance Show Sign where
    show Plus = "+"
    show Minus = "-"

instance Show Bit where
    show Zero = "0"
    show One  = "1"

instance Show Z where
    show (Z s bs) = show s ++ " " ++ show bs

bitToInteger :: Bit -> Integer
bitToInteger One  = 1
bitToInteger Zero = 0

zToInteger (Z s bits) = sign * impl 1 bits
    where
        impl :: Integer -> [Bit] -> Integer
        impl _ [] = 0
        impl base (b:bs) = base * bitToInteger b + impl (base * 2) bs
        sign = case s of
            Plus  -> 1
            Minus -> -1

integerToZ x = Z sign $ impl (abs x)
    where
        sign = if x > 0 then Plus else Minus
        impl :: Integer -> [Bit]
        impl 0 = []
        impl x = (if even x then Zero else One) : impl (div x 2)


add :: Z -> Z -> Z
add x y =  integerToZ (on (+) zToInteger  x y)

mul :: Z -> Z -> Z
mul x y = integerToZ (on (*) zToInteger  x y)