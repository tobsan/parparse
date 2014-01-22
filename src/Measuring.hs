{-# LANGUAGE MultiParamTypeClasses, TypeSynonymInstances, FlexibleInstances #-}

module Measuring where

import Data.FingerTree (Measured, measure)
import Data.Sequence
import Data.Monoid

import Java

-- This is where the matrix stuff should go
data Parser = A

instance Monoid Parser where
    mempty = A -- empty
    a0 `mappend` a1 = A -- Do we have a0a1 \elem P?

-- Which one of these would be most reasonable? Let's find out!

instance Measured Parser (Seq Token) where
    measure seq = undefined
    
instance Measured Parser (Table State Tokens, Size) where
    measure tab = undefined

--
-- lexer input >>= \tree -> parser tree >>= \abssyn
--
