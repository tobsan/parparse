{-# LANGUAGE MultiParamTypeClasses, TypeSynonymInstances, FlexibleInstances, UndecidableInstances, DataKinds #-}

module Measuring where

import Data.FingerTree (Measured, measure)
import Data.Monoid
import System.IO.Unsafe
import System.Random

import LexJavaletteLight hiding (One)
import CnfTablesJavaletteLight
import Data.Matrix.Quad
import Algebra.RingUtils

instance Monoid (SomeTri a) where
    mempty = T Leaf' (Zero :/: Zero)
    t0 `mappend` t1 = merge True t0 t1
    -- FIXME: Change to (unsafePerformIO randomIO)

instance Measured (SomeTri IntToken) IntToken where
    -- Note: place the token just above the diagonal
    measure tok = T s (q :/: q)
      where s = Bin' 0 Leaf' Leaf'
            q = Quad Zero (One tok) Zero Zero

-- This is the progress:
-- Lexer input is Char, measured to IntToken
-- IntToken has to be converted into Tokens
-- 
-- Tokens is the input to CnfTables, which can convert them to a Pair of
-- category and AST-ADT.
--
-- The category/AST-ADT-pairs can then be input to be converted to matricies in
-- the Quad module of BNFC.
--
-- Char -> IntToken -> Token -> Pair [(Category, Any *)]

