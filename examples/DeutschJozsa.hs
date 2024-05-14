import QGAT
import Quipper
import Quipper.Utils.Auxiliary

deutschJozsa :: Int -> OracleFor FourierExpansion -> Program FourierExpansion
deutschJozsa d f = Program
  { generateBits = d
  , applyOracle = f
  , query = FourierExpansion
  }

buildOracle [d|
  constant :: [Bool] -> Bool
  constant x = True
  |]

buildOracle [d|
  balanced :: [Bool] -> Bool
  balanced x = foldl bool_xor False x
  |]

program :: Program FourierExpansion
-- program = deutschJozsa 4 oracle_constant
program = deutschJozsa 4 oracle_balanced

-- main = previewCircuit program
-- main = countGates program
main = print $ simulateCircuit program
