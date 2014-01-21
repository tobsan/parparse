{-# LANGUAGE MultiParamTypeClasses, TypeSynonymInstances, FlexibleInstances #-}

module Measuring where

import Data.FingerTree
import Data.Monoid
import Java

-- This is where the matrix stuff should go
data Parser = A

instance Monoid Parser where
    mempty = A
    mappend = \_ -> const A

-- Largely borrowed from LexGen
instance Measured Parser (Table State Tokens, Size) where
    measure tab = case access (fst tab) startState of
        InvalidTokens s -> error "Unacceptable token"
        NoTokens -> undefined
        Tokens seq suff out_state -> undefined
    

--
-- lexer input >>= \tree -> parser tree >>= \abssyn
--
