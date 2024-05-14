{-# LANGUAGE FlexibleContexts, TypeFamilies #-}

module QGAT where

import Data.Maybe
import Language.Haskell.TH

import Quipper
import Quipper.Internal.Generic
import Quipper.Libraries.Arith
import Quipper.Libraries.QFT
import Quipper.Libraries.Simulation

data Oracle a a' = Oracle a (Circ a')

buildOracle :: Q [Dec] -> Q [Dec]
buildOracle decs = do
  decs' <- decs
  templates <- decToCircMonad decs
  let getName (FunD n _) = Just n
      getName (ValD (VarP n) _ _) = Just n
      getName _ = Nothing
      names = mapMaybe getName decs'
      f n = [d| $(varP $ mkName $ "oracle_" ++ nameBase n) = Oracle $(varE n) $(varE $ mkName $ "template_" ++ nameBase n) |]
  oracles <- mapM f names
  return $ decs' ++ templates ++ concat oracles

applyParam :: (Oracle (Param a -> b) (Param a -> Circ b')) -> a -> Oracle b b'
applyParam (Oracle f f') x = Oracle (f $ Param x) (f' >>= ($ Param x))

data (Query q) => Program q = Program
  { generateBits :: Int
  , applyOracle :: OracleFor q
  , query :: q
  }

class Query q where
  type OracleFor q
  type CircOutput q
  toCircuit :: Program q -> Circ (CircOutput q)

data FourierExpansion = FourierExpansion
instance Query FourierExpansion where
  type OracleFor _ = Oracle ([Bool] -> Bool) (Qulist -> Circ Qubit)
  type CircOutput _ = Qulist

  toCircuit (Program d (Oracle _ f) _) = do
    x <- generate d
    applyPhase f x
    map_hadamard x

data Period = Period
instance Query Period where
  type OracleFor _ = Oracle (IntM -> IntM) (QDInt -> Circ QDInt)
  type CircOutput _ = QDInt

  toCircuit (Program d (Oracle _ f) _) = do
    x <- generate d
    let x' = xint_of_list_lh x
    y <- apply f x'
    qdiscard y
    qft_int x'

data BitwisePeriod = BitwisePeriod
instance Query BitwisePeriod where
  type OracleFor _ = Oracle ([Bool] -> [Bool]) (Qulist -> Circ Qulist)
  type CircOutput _ = Qulist

  toCircuit (Program d (Oracle _ f) _) = do
    x <- generate d
    y <- apply f x
    qdiscard y
    map_hadamard x

generate :: Int -> Circ Qulist
generate d = do
  q <- qinit $ replicate d False
  map_hadamard q

apply :: (CircLiftingUnpack packed fun, QCurry fun args qc, QCData qc) => packed -> fun
apply f = qcurry $ \x -> with_computed (quncurry (unpack f) x) qc_copy

applyPhase :: (CircLiftingUnpack packed fun, QCurry fun args Qubit, QCurry fun' args ()) => packed -> fun'
applyPhase f = qcurry $ \x -> do
  a <- qinit_plusminus True
  with_computed (quncurry (unpack f) x) $ controlled_not_at a
  qdiscard a

previewCircuit, countGates :: Circ a -> IO ()
previewCircuit = print_simple Preview
countGates = print_simple GateCount

simulateCircuit circuit = print $ sim_generic (0 :: Double) circuit
