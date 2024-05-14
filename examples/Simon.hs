import Data.List

import QGAT
import Quipper
import Quipper.Algorithms.BF.Hex
import Quipper.Utils.Auxiliary

buildOracle [d|
  f :: Param [Bool] -> [Bool] -> [Bool]
  f b x = case liftP (elemIndex True) b of
    Param (Just i) -> if x !! i then zipWith bool_xor x $ getParam b else x
    _ -> x
  |]

simon :: [Bool] -> Program BitwisePeriod
simon b = Program
  { generateBits = length b
  , applyOracle = oracle_f `applyParam` b
  , query = BitwisePeriod
  }

program :: Program BitwisePeriod
program = simon [True, False, True]

-- main = previewCircuit program
-- main = countGates program
main = print $ simulateCircuit program
