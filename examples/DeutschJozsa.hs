import QuantumDsl
import Quipper
import Quipper.Utils.Auxiliary

deutsch_jozsa :: Int -> PredicateCirc Qulist -> Circ Qulist
deutsch_jozsa d f = do
  x <- generate d
  apply_phase f x
  approx_qft x

build_circuit
constant :: [Bool] -> Bool
constant x = True

build_circuit
balanced :: [Bool] -> Bool
balanced x = foldl bool_xor False x

circuit :: Circ Qulist
-- circuit = deutsch_jozsa 4 template_constant
circuit = deutsch_jozsa 4 template_balanced

-- main = preview_circuit circuit
main = simulate_circuit circuit
