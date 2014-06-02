{-# LANGUAGE MagicHash, FlexibleInstances, MultiParamTypeClasses #-}
module CYKParser(getProgram, getAllProgram,
                 getStm, getAllStm,
                 getExp, getAllExp,CATEGORY, Any, parse) where
import GHC.Prim
import GHC.Exts
import Control.Applicative
import Data.Monoid hiding (Any)
import System.IO.Unsafe (unsafePerformIO)
import System.Random
import Data.FingerTree

import Algebra.RingUtils
import Parsing.Chart ()
import Data.Matrix.Quad
import AbsC
import LexC hiding (One)
import CnfTablesC

instance RingP a => Monoid (SomeTri a) where
  mempty = T Leaf' (Zero :/: Zero)
  t0 `mappend` t1 = unsafePerformIO $ do
      b <- randomIO
      return $ merge b t0 t1

instance Measured (SomeTri [(CATEGORY,Any)]) Token where
    -- Note: place the token just above the diagonal
    measure tok = T (bin' Leaf' Leaf') (q True :/: q False)
      where q b = quad zero (t b) zero zero
            select b = if b then leftOf else rightOf
            t b = One $ select b $ tokenToCats b tok

type ParseState = SomeTri [(CATEGORY,Any)]
parse :: FingerTree ParseState Token -> [(Int,[(CATEGORY,Any)],Int)]
parse tree = results $ measure tree

{-
intToToken :: IntToken -> Maybe Token
intToToken (Token lexeme acc) = case acc of
    AlexAcc f   -> Just $ f (Pn 0 1 1) lexeme
    _           -> Nothing
-}

--use these functions to convert from parser-internal state to AST
getProgram :: (CATEGORY,Any) -> Maybe Program
getProgram (CAT_Program,ast) = Just $ ((unsafeCoerce# ast)::Program)
getProgram _ = Nothing

getAllProgram :: [(a,[(CATEGORY,Any)],a)] -> [Maybe Program]
getAllProgram = concatMap (\(_,cs,_) -> map getProgram cs)

getStm :: (CATEGORY,Any) -> Maybe Stm
getStm (CAT_Stm,ast) = Just $ ((unsafeCoerce# ast)::Stm)
getStm _ = Nothing

getAllStm :: [(a,[(CATEGORY,Any)],a)] -> [Maybe Stm]
getAllStm = concatMap (\(_,cs,_) -> map getStm cs)

getExp :: (CATEGORY,Any) -> Maybe Exp
getExp (CAT_Exp,ast) = Just $ ((unsafeCoerce# ast)::Exp)
getExp _ = Nothing

getAllExp :: [(a,[(CATEGORY,Any)],a)] -> [Maybe Exp]
getAllExp = concatMap (\(_,cs,_) -> map getExp cs)
