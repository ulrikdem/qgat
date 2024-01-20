import Data.List

import QuantumDsl
import Quipper
import Quipper.Algorithms.BF.Hex
import Quipper.Utils.Auxiliary

buildOracle [d|
  f :: Param [Bool] -> [Bool] -> [Bool]
  f b x = case liftP (elemIndex True) b of
    Param (Just i) -> if x !! i then zipWith bool_xor x $ getParam b else x
    _ -> x
  |]

simon :: [Bool] -> Program ([Bool] -> [Bool]) (Qulist -> Circ Qulist) BitwisePeriod
simon b = Program
  { generateBits = length b
  , applyOracle = oracle_f `applyParam` b
  , query = BitwisePeriod
  }

circuit :: Circ Qulist
circuit = toCircuit $ simon [True, False, True]

-- main = previewCircuit circuit
-- main = countGates circuit
main = simulateCircuit circuit
