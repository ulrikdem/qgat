{-# LANGUAGE FlexibleContexts #-}

module QuantumDsl where

import Quipper
import Quipper.Libraries.Simulation

type PredicateCirc a = Circ (a -> Circ Qubit)

generate :: Int -> Circ Qulist
generate d = do
  q <- qinit $ replicate d False
  map_hadamard q

apply_phase :: PredicateCirc a -> a -> Circ ()
apply_phase f x = do
  a <- qinit True
  hadamard_at a
  with_computed (unpack f x) (controlled_not_at a)
  qdiscard a

test :: (QData qa) => qa -> Circ ()
test = qdiscard

approx_qft :: (QData qa) => qa -> Circ qa
approx_qft = map_hadamard

preview_circuit :: Circ a -> IO ()
preview_circuit = print_simple Preview

simulate_circuit circuit = putStrLn $ show $ sim_generic (0 :: Double) circuit
