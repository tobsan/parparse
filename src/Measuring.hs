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

-- 
-- TODO: Change Token to [(Category,Any)] or something along those lines. 
--

instance Monoid (SomeTri Token) where
    mempty = T Leaf' (Zero :/: Zero)
    t0 `mappend` t1 = merge True t0 t1
    -- FIXME: Change to (unsafePerformIO randomIO)

instance Measured (SomeTri Token) IntToken where
    -- Note: place the token just above the diagonal
    measure tok = T s (q :/: q)
      where s = Bin' 0 Leaf' Leaf'
            q = quad Zero t Zero Zero
            t = case intToToken tok of
                Nothing  -> Zero
                Just tok -> One tok


intToToken :: IntToken -> Maybe Token
intToToken (Token lex acc) = case acc of
    AlexAccNone -> Nothing
    AlexAccSkip -> Nothing
    AlexAcc f   -> Just $ f (Pn 0 1 1) lex -- dummy position for now


-- This is the progress:
-- Lexer input is Char, measured to IntToken
-- IntToken has to be converted into Tokens - HOW?
-- 
-- Tokens is the input to CnfTables, which can convert them to a Pair of
-- category and AST-ADT.
--
-- The category/AST-ADT-pairs can then be input to be converted to matricies in
-- the Quad module of BNFC.
--
-- Char -> IntToken -> Token -> Pair [(Category, Any *)]

