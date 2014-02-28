{-# LANGUAGE MultiParamTypeClasses, TypeSynonymInstances, FlexibleInstances,
UndecidableInstances, DataKinds #-}

module Measuring where

import Data.FingerTree (Measured, measure)
import Data.Monoid

import Java hiding (One)
import CnfTablesJava
import Data.Matrix.Quad
import Algebra.RingUtils

-- 
-- Should it be Mat x y or SomeTri? SomeTri seems more reasonable in a way. 
-- 

instance Monoid (SomeTri a) where
    mempty = Zero
    t0 `mappend` t1 = \b -> merge b t0 t1

instance Measured (SomeTri IntToken) IntToken where
    measure tok = One tok

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

