module Problem_4'5'4 where


data Nat = Zero | Suc Nat

instance Show Nat where
  show Zero = ""
  show (Suc x) = '|' : show x

fromNat :: Nat -> Integer
fromNat Zero = 0
fromNat (Suc n) = fromNat n + 1

add :: Nat -> Nat -> Nat
add Zero y     = y
add (Suc xs) y = add xs (Suc y)

mul :: Nat -> Nat -> Nat
mul (Suc a) b    = add b (mul a b)
mul Zero b       = Zero   

fac :: Nat -> Nat
fac Zero = Suc Zero
fac x@(Suc n)= mul x (fac n)