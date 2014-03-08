{-# LANGUAGE MultiParamTypeClasses, TypeSynonymInstances, FlexibleContexts,
FlexibleInstances, GADTs, UndecidableInstances, DataKinds #-}

module Measuring where

import GHC.Prim
import Data.FingerTree (Measured, measure)
import Data.Monoid hiding (Any)
import System.IO.Unsafe
import System.Random

import LexJavaletteLight hiding (One)
import CnfTablesJavaletteLight
import Data.Matrix.Quad
import Algebra.RingUtils

instance Monoid (SomeTri [(CATEGORY,Any)]) where
    mempty = T Leaf' (Zero :/: Zero)
    t0 `mappend` t1 = merge True t0 t1
    -- FIXME: Change to (unsafePerformIO randomIO)

instance Measured (SomeTri [(CATEGORY,Any)]) IntToken where
    -- Note: place the token just above the diagonal
    measure tok = T (Bin' 0 Leaf' Leaf') (q True :/: q False)
      where 
        q b = quad Zero (t b) Zero Zero
        t b = case intToToken tok of
            Nothing  -> Zero
            Just tok -> One $ (if b then leftOf else rightOf) $ tokenToCats b tok

-- None and Skip are just discarded, as seen in measureToTokens in LexGen
intToToken :: IntToken -> Maybe Token
intToToken (Token lex acc) = case acc of
    AlexAccNone -> Nothing
    AlexAccSkip -> Nothing
    AlexAcc f   -> Just $ f (Pn 0 1 1) lex -- dummy position for now


