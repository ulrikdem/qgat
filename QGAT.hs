{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, TupleSections, TypeOperators #-}

module QGAT where

import Data.Maybe
import Language.Haskell.TH

import Quipper
import Quipper.Internal.Generic
import Quipper.Internal.QData
import Quipper.Libraries.Arith
import Quipper.Libraries.ClassicalOptim
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

data Program a a' q = Program
  { generateBits :: Int
  , applyOracle :: Oracle a a'
  , query :: q
  }

class Query q a b c where
  toCircuit :: Program f (a -> Circ b) q -> Circ c

data FourierExpansion = FourierExpansion
instance Query FourierExpansion Qulist Qubit Qulist where
  toCircuit (Program d (Oracle _ f) _) = do
    x <- generate d
    applyPhase f x
    map_hadamard x

data Period = Period
instance Query Period QDInt QDInt Qulist where
  toCircuit (Program d (Oracle _ f) _) = do
    x <- generate d
    let x' = xint_of_list_lh x
    y <- apply f x'
    qdiscard y
    z <- qft_int x'
    return $ list_of_xint_lh z

data BitwisePeriod = BitwisePeriod
instance Query BitwisePeriod Qulist Qulist Qulist where
  toCircuit (Program d (Oracle _ f) _) = do
    x <- generate d
    y <- apply f x
    qdiscard y
    map_hadamard x

generate :: Int -> Circ Qulist
generate d = do
  q <- qinit $ replicate d False
  map_hadamard q

apply :: (CircLiftingUnpack packed fun, QCurry fun args qc, QData args, QData qc) => packed -> fun
apply f = qcurry $ \x -> with_computed (quncurry (unpack f) x) qc_copy
-- apply f = qcurry $ \x -> with_computed (simplify_classical (quncurry (unpack f)) x) qc_copy

applyPhase :: (CircLiftingUnpack packed fun, QCurry fun args Qubit, QCurry fun' args (), QData args) => packed -> fun'
applyPhase f = qcurry $ \x -> do
  a <- qinit_plusminus True
  with_computed (quncurry (unpack f) x) $ controlled_not_at a
  -- with_computed (simplify_classical (quncurry (unpack f)) x) $ controlled_not_at a
  qdiscard a

previewCircuit, countGates :: Circ a -> IO ()
previewCircuit = print_simple Preview
countGates = print_simple GateCount

simulateCircuit circuit = print $ sim_generic (0 :: Double) circuit

optimize :: Circ [Qubit] -> Circ [Qubit]
optimize circuit = mapM toUnknown =<< transform_generic_shape optimizer circuit

data MaybeKnown a = Known (BType a) | Unknown a

toUnknown :: (QCData qc) => MaybeKnown qc -> Circ qc
toUnknown (Known b) = qc_init b
toUnknown (Unknown q) = return q

toUnknown' :: B_Endpoint (MaybeKnown Qubit) (MaybeKnown Bit) -> Circ Endpoint
toUnknown' (Endpoint_Qubit (Known b)) = fmap Endpoint_Qubit $ qc_init b
toUnknown' (Endpoint_Qubit (Unknown q)) = fmap Endpoint_Qubit $ return q
toUnknown' (Endpoint_Bit (Known b)) = fmap Endpoint_Bit $ qc_init b
toUnknown' (Endpoint_Bit (Unknown q)) = fmap Endpoint_Bit $ return q

fromUnknown :: B_Endpoint (MaybeKnown Qubit) (MaybeKnown Bit) -> Maybe Endpoint
fromUnknown (Endpoint_Qubit (Known a)) = Nothing
fromUnknown (Endpoint_Qubit (Unknown a)) = Just $ Endpoint_Qubit a
fromUnknown (Endpoint_Bit (Known a)) = Nothing
fromUnknown (Endpoint_Bit (Unknown a)) = Just $ Endpoint_Bit a

knownFalse :: Ctrls (MaybeKnown Qubit) (MaybeKnown Bit) -> Bool
knownFalse = any f where
  f (Signed (Endpoint_Qubit (Known b)) a) = a /= b
  f (Signed (Endpoint_Bit (Known b)) a) = a /= b
  f _ = False

filterControls :: Ctrls (MaybeKnown Qubit) (MaybeKnown Bit) -> Ctrls Qubit Bit
filterControls = mapMaybe f where
  f (Signed (Endpoint_Qubit (Known b)) a) = Nothing
  f (Signed (Endpoint_Qubit (Unknown q)) a) = Just $ Signed (Endpoint_Qubit q) a
  f (Signed (Endpoint_Bit (Known b)) a) = Nothing
  f (Signed (Endpoint_Bit (Unknown c)) a) = Just $ Signed (Endpoint_Bit c) a

toUnknownCtrls :: Ctrls Qubit Bit -> Ctrls (MaybeKnown Qubit) (MaybeKnown Bit)
toUnknownCtrls = map f where
  f (Signed (Endpoint_Qubit q) a) = Signed (Endpoint_Qubit $ Unknown q) a
  f (Signed (Endpoint_Bit c) a) = Signed (Endpoint_Bit $ Unknown c) a

optimizer :: Transformer Circ (MaybeKnown Qubit) (MaybeKnown Bit)
optimizer (T_QGate name n m inv ncf f) = f $ \ws vs c ->
  let f' g = do
        ws' <- mapM toUnknown ws
        vs' <- mapM toUnknown vs
        (ws'', vs'', _) <- g ws' vs' $ filterControls c
        return (map Unknown ws'', map Unknown vs'', c)
   in if knownFalse c then return (ws, vs, c) else identity_transformer (T_QGate name n m inv ncf f')
optimizer (T_QRot name n m inv t ncf f) = f $ \ws vs c ->
  let f' g = do
        ws' <- mapM toUnknown ws
        vs' <- mapM toUnknown vs
        (ws'', vs'', _) <- g ws' vs' $ filterControls c
        return (map Unknown ws'', map Unknown vs'', c)
   in if knownFalse c then return (ws, vs, c) else identity_transformer (T_QRot name n m inv t ncf f')
optimizer (T_GPhase t ncf f) = f $ \qs c ->
  let f' g = do
        g (mapMaybe fromUnknown qs) $ filterControls c
        return c
   in if knownFalse c then return c else identity_transformer (T_GPhase t ncf f')
optimizer (T_CNot ncf f) = f $ \q c ->
  let f' g = do
        q' <- toUnknown q
        (q'', _) <- g q' $ filterControls c
        return (Unknown q'', c)
   in if knownFalse c then return (q, c) else identity_transformer (T_CNot ncf f')
optimizer (T_CGate name ncf f) = f $ \ws ->
  let f' g = do
        ws' <- mapM toUnknown ws
        (v, ws'') <- g ws'
        return (Unknown v, map Unknown ws'')
   in identity_transformer (T_CGate name ncf f')
optimizer (T_CGateInv name ncf f) = f $ \v ws ->
  let f' g = do
        v' <- toUnknown v
        ws' <- mapM toUnknown ws
        ws'' <- g v' ws'
        return $ map Unknown ws''
   in identity_transformer (T_CGateInv name ncf f')
optimizer (T_CSwap ncf f) = f $ \w v c ->
  let f' g = do
        w' <- toUnknown w
        v' <- toUnknown v
        (w'', v'', _) <- g w' v' $ filterControls c
        return (Unknown w'', Unknown v'', c)
   in if knownFalse c then return (w, v, c) else identity_transformer (T_CSwap ncf f')
optimizer (T_QPrep ncf f) = f $ \w -> case w of
  Known a -> return $ Known a
  Unknown a -> fmap Unknown $ prepare a
optimizer (T_QUnprep ncf f) = f $ \w ->
  let f' g = do
        -- TODO: skip for Known
        w' <- toUnknown w
        v <- g w'
        return $ Unknown v
    -- TODO: check that ncf is not applicable for prep, init and term
   in identity_transformer (T_QUnprep ncf f')
optimizer (T_QInit b ncf f) = f $ return $ Known b
optimizer (T_CInit b ncf f) = f $ return $ Known b
optimizer (T_QTerm b ncf f) = f $ \w -> case w of
  Known a -> return () -- TODO: assert a == b
  Unknown q -> qterm b q
optimizer (T_CTerm b ncf f) = f $ \w -> case w of
  Known a -> return () -- TODO: assert a == b
  Unknown q -> cterm b q
optimizer (T_QMeas f) = f $ \w -> case w of
  Known a -> return $ Known a
  Unknown a -> fmap Unknown $ measure a
optimizer (T_QDiscard f) = f $ \w -> case w of
  Known a -> return ()
  Unknown q -> qdiscard q
optimizer (T_CDiscard f) = f $ \w -> case w of
  Known a -> return ()
  Unknown q -> cdiscard q
optimizer (T_DTerm b f) = f $ \w ->
  let f' g = do
        -- TODO: skip for Known
        w' <- toUnknown w
        g w'
   in identity_transformer (T_DTerm b f')
optimizer (T_Subroutine n inv ncf scf ws_pat a1 vs_pat a2 rep f) = undefined -- TODO: implement
optimizer (T_Comment s inv f) = f $ \ws ->
  let f' g = do
        g $ mapMaybe (\(e, s) -> fmap (, s) $ fromUnknown e) ws
   in identity_transformer (T_Comment s inv f')
