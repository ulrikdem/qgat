import QGAT
import Quipper
import Quipper.Utils.Auxiliary

buildOracle [d|
  dot :: Param [Bool] -> [Bool] -> Bool
  dot s x = foldl bool_xor False $ zipWith (&&) x $ getParam s
  |]

bernsteinVazirani :: [Bool] -> Program FourierExpansion
bernsteinVazirani s = Program
  { generateBits = length s
  , applyOracle = oracle_dot `applyParam` s
  , query = FourierExpansion
  }

program :: Program FourierExpansion
program = bernsteinVazirani [True, False, True, False]

-- main = previewCircuit program
-- main = countGates program
main = print $ simulateCircuit program
