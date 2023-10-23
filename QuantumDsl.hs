{-# LANGUAGE FlexibleContexts #-}

module QuantumDsl where

import Quipper
import Quipper.Internal.Generic
import Quipper.Libraries.Simulation

type PredicateCirc a = Circ (a -> Circ Qubit)

generate :: Int -> Circ Qulist
generate d = do
  q <- qinit $ replicate d False
  map_hadamard q

apply :: (CircLiftingUnpack packed fun, QCurry fun args qc, QCData qc) => packed -> fun
apply f = qcurry $ \x -> with_computed (quncurry (unpack f) x) qc_copy

apply_phase :: (CircLiftingUnpack packed fun, QCurry fun args Qubit, QCurry fun' args ()) => packed -> fun'
apply_phase f = qcurry $ \x -> do
  a <- qinit_plusminus True
  with_computed (quncurry (unpack f) x) (controlled_not_at a)
  qdiscard a

test :: (QShape ba qa ca) => qa -> Circ ca
test = measure

approx_qft :: (QData qa) => qa -> Circ qa
approx_qft = map_hadamard

preview_circuit :: Circ a -> IO ()
preview_circuit = print_simple Preview

simulate_circuit circuit = putStrLn $ show $ sim_generic (0 :: Double) circuit
