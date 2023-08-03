import QuantumDsl
import Quipper

simon :: [Bool] -> Circ Qulist
simon b = do
  x <- generate $ length b
  (x, y) <- apply_f b x
  test y
  approx_qft x

apply_f :: [Bool] -> Qulist -> Circ (Qulist, Qulist)
apply_f b x = do
  y <- qinit $ replicate (length b) False
  (y, x) <- controlled_not y x
  y <- bool_controlled_not y b `controlled` head x
  return (x, y)

circuit :: Circ Qulist
circuit = simon [True, False, True]

-- main = preview_circuit circuit
main = simulate_circuit circuit
