module Examples where

import Data.Bits


simon :: Integer -> Integer -> Integer -> [Integer]
simon d b r = hadamard d [ x | x <- generate d, f x == r ]
  where f x = min x (x `xor` b)

deutschJozsa :: Integer -> (Integer -> Integer) -> [Integer]
deutschJozsa d f = phaseHadamard d [ (x, f x) | x <- generate d ]

bernsteinVazirani :: Integer -> Integer -> [Integer]
bernsteinVazirani d s = phaseHadamard d [ (x, f x) | x <- generate d ]
  where f x = s `dot` x

grover :: Integer -> Integer -> [Integer]
grover d t = diffusion [ (x, f x) | x <- generate d ]
  where f x = if x == t then 1 else 0


data Exp =
    QFT
  | Hadamard
  | Diffusion
  | Generate Integer
  | Apply (Integer -> Integer)
  | ApplyPhase (Integer -> Bool)
  | Test Integer
  | Exp `Then` Exp


generate :: Integer -> [Integer]
generate d = [0..2^d - 1]

dot :: Integer -> Integer -> Integer
x `dot` y = toInteger $ popCount (x .&. y) `mod` 2

hadamard :: Integer -> [Integer] -> [Integer]
hadamard d xs = [ z | z <- generate d, sum [ (-1)^(x `dot` z) | x <- xs ] /= 0 ]

phaseHadamard :: Integer -> [(Integer, Integer)] -> [Integer]
phaseHadamard d xys = [ z | z <- generate d, sum [ (-1)^(x `dot` z + y) | (x, y) <- xys ] /= 0 ]

diffusion :: [(Integer, Integer)] -> [Integer]
diffusion xys = [ x | (x, y) <- xys, y /= 0 ]
