import QuantumDsl
import Quipper
import Quipper.Algorithms.BF.Hex
import Quipper.Utils.Auxiliary

build_circuit
dot :: [BoolParam] -> [Bool] -> Bool
dot s x = foldl bool_xor False $ zipWith (&&) x $ newBools s

bernstein_vazirani :: [BoolParam] -> Circ Qulist
bernstein_vazirani s = do
  x <- generate $ length s
  apply_phase template_dot s x
  approx_qft x

circuit :: Circ Qulist
circuit = bernstein_vazirani [PTrue, PFalse, PTrue, PFalse]

-- main = preview_circuit circuit
main = simulate_circuit circuit
