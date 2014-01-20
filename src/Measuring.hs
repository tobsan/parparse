{-# LANGUAGE MultiParamTypeClasses, TypeSynonymInstances #-}

module Measuring where


import Data.FingerTree
import Data.Monoid

type Parser a = [a] -- for now

{-
instance Monoid (Parser a) where
    mempty = []
    mappend = (++)
-}

instance Measured (Parser a) (FingerTree v a) where
    measure tree = toList $ viewl tree
      where
        toList = undefined 
        {-
        toList EmptyL     = []
        toList (a :< seq) = a : toList $ viewl seq
        -}
