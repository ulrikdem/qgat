import Control.Monad

import QGAT
import Quipper
import Quipper.Libraries.Arith

template_mod :: Circ (QDInt -> Circ (QDInt -> Circ QDInt))
template_mod = return $ \x -> return $ \y -> do (x, y, z) <- q_mod_unsigned x y; return z

buildOracle [d|
  mulMod :: IntM -> IntM -> IntM -> IntM
  mulMod n x y = x * y `mod` n
  |]

buildOracle [d|
  powMod :: Param Int -> Param IntM -> Param IntM -> IntM -> IntM
  powMod d n a x = snd $ foldl f (a, y) $ list_of_xint_lh x where
    y = getParam $ liftP2 intm d $ liftP0 1
    n' = getParam n
    f (a, y) b = (liftP3 mulMod n a a, if b then mulMod n' y (getParam a) else y)
  |]

shor :: Integer -> Integer -> Program (IntM -> IntM) (QDInt -> Circ QDInt) Period
shor n a = Program
  { generateBits = d
  , applyOracle = oracle_powMod `applyParam` d `applyParam` intm d n `applyParam` intm d a
  , query = Period
  } where d = ceiling $ 2 * logBase 2 (fromInteger n)

circuit :: Circ Qulist
circuit = optimize $ toCircuit $ shor 15 2

-- main = previewCircuit circuit
main = countGates circuit
-- main = simulateCircuit circuit
