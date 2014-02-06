{-# LANGUAGE MultiParamTypeClasses, TypeSynonymInstances, FlexibleInstances,
UndecidableInstances, DataKinds #-}

module Measuring where

import Data.FingerTree (Measured, measure)
import Data.Monoid

import Java hiding (One)
import CnfTablesJava
import Data.Matrix.Quad
import Algebra.RingUtils

instance Monoid (Mat x y a) where
    mempty = Zero
    t0 `mappend` t1 = undefined -- chop!

instance Measured (Mat x x IntToken) IntToken where
    measure tok = One tok


--
-- lexer input >>= \tree -> parser tree >>= \abssyn
--
