import QuantumDsl
import Quipper
import Quipper.Utils.Auxiliary

param_and :: BoolParam -> Bool -> Bool
param_and PTrue y = y
param_and PFalse y = False

template_param_and :: Circ (BoolParam -> Circ (Qubit -> Circ Qubit))
template_param_and = pack $ \x y -> do
  q <- qinit False
  qnot q `controlled` x == PTrue .&&. y

build_circuit
dot :: [BoolParam] -> [Bool] -> Bool
dot x y = foldl bool_xor False $ zipWith param_and x y

bernstein_vazirani :: [BoolParam] -> Circ Qulist
bernstein_vazirani s = do
  let d = length s
  x <- generate d
  apply_phase (template_dot >>= \f -> f s) x
  approx_qft x

circuit :: Circ Qulist
circuit = bernstein_vazirani [PTrue, PFalse, PTrue, PFalse]

-- main = preview_circuit circuit
main = simulate_circuit circuit
