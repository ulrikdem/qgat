module Shor where

-- Example

shor :: Integer -> Integer -> Integer -> Integer
shor n a r = qft [ (x , f x) | x <- [0..d] , f(x) == r ]
  where f x = (a ^ x) `mod` n
        d = floor (logBase 2 (fromInteger n ^ 2))
        qft ps = fst $ head (tail ps)

data Exp = 
    QFT
  | Generate Integer
  | Apply (Integer -> Integer)
  | Test Integer
  | Exp `Then` Exp


shorE :: Integer -> Integer -> Integer -> Exp
shorE n a r =
  Generate d `Then`
  Apply (\x -> a ^ x `mod` n) `Then`
  Test r `Then`
  QFT
  where d = floor (logBase 2 (fromInteger n ^ 2))
