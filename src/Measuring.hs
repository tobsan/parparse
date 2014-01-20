{-# LANGUAGE MultiParamTypeClasses #-}
module Measuring where


import Data.FingerTree
import Data.Monoid

data List a = Empty | Head a (List a)
    deriving Show

concatenate :: List a -> List a -> List a
concatenate Empty l2 = l2
concatenate l1 Empty = l1
concatenate (Head a Empty) l2 = Head a l2
concatenate (Head a l1) l2 = Head a (concatenate l1 l2)

list1 = Head "hej" $ Head "på" $ Head "dig" Empty
list2 = Head "hur" $ Head "mår" $ Head "du" Empty

instance Monoid (List a) where
    mempty = Empty
    mappend = concatenate

-- Additive monoid
instance Monoid Int where
    mempty = 1
    mappend = (+)

instance Measured Int (List a) where
    measure Empty      = 1
    measure (Head _ t) = 1 + measure t
