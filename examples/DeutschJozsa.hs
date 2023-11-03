import QuantumDsl
import Quipper
import Quipper.Utils.Auxiliary

deutschJozsa :: Int -> Oracle a a' -> Program a a' FourierExpansion
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

circuit :: Circ Qulist
-- circuit = toCircuit $ deutschJozsa 4 oracle_constant
circuit = toCircuit $ deutschJozsa 4 oracle_balanced

-- main = previewCircuit circuit
main = simulateCircuit circuit
