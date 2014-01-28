{-# LANGUAGE MultiParamTypeClasses, TypeSynonymInstances, FlexibleInstances #-}

module Measuring where

import Data.FingerTree (Measured, measure)
import Data.Sequence
import Data.Monoid

import Java

data AST = AST

-- This is where the matrix stuff should go
data Parse a = Q (Parse a) (Parse a) (Parse a) (Parse a)
             | One a
             | Z

instance Monoid (Parse a) where
    mempty = Z
    Z `mappend` a1 = Z 
    a1 `mappend` Z = Z


instance Measured (Parse AST) (Table State Tokens, Size) where
    measure tab = undefined


--
-- lexer input >>= \tree -> parser tree >>= \abssyn
--
