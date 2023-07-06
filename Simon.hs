import Quipper
import Quipper.Libraries.Simulation

simon :: [Bool] -> Circ (Qulist)
simon b = do
  x <- generate $ length b
  (x, y) <- apply_f b x
  test y
  approx_qft x

generate :: Int -> Circ Qulist
generate d = do
  q <- qinit $ replicate d False
  map_hadamard q

apply_f :: [Bool] -> Qulist -> Circ (Qulist, Qulist)
apply_f b x = do
  y <- qinit $ replicate (length b) False
  (y, x) <- controlled_not y x
  y <- bool_controlled_not y b `controlled` head x
  return (x, y)

test :: Qulist -> Circ ()
test = qdiscard

approx_qft :: Qulist -> Circ (Qulist)
approx_qft = map_hadamard

bitstring :: [Bool]
bitstring = [True, False, True]

-- main = print_generic Preview $ simon bitstring
main = run_clifford_generic (simon bitstring) >>= putStrLn . show
