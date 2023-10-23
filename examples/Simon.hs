import Data.List

import QuantumDsl
import Quipper
import Quipper.Algorithms.BF.Hex
import Quipper.Utils.Auxiliary

build_circuit
f :: Int -> [BoolParam] -> [Bool] -> [Bool]
f i b x = if x !! i then zipWith bool_xor x $ newBools b else x where

oracle :: [BoolParam] -> Qulist -> Circ Qulist
oracle b = case elemIndex PTrue b of
  Just i -> apply template_f i b
  Nothing -> return

simon :: [BoolParam] -> Circ Qulist
simon b = do
  x <- generate $ length b
  y <- oracle b x
  test y
  approx_qft x

circuit :: Circ Qulist
circuit = simon [PTrue, PFalse, PTrue]

-- main = preview_circuit circuit
main = simulate_circuit circuit
