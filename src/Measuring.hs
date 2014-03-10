{-# LANGUAGE MultiParamTypeClasses, TypeSynonymInstances, FlexibleContexts,
FlexibleInstances, GADTs, UndecidableInstances, DataKinds #-}

module Measuring where

import GHC.Prim
import Data.FingerTree (Measured, measure)
import Data.Monoid hiding (Any)
import Control.Monad (forM_)
import System.IO.Unsafe
import System.Random

import LexJavaletteLight hiding (One)
import CnfTablesJavaletteLight
import Data.Matrix.Quad
import Algebra.RingUtils
import Data.FingerTree

instance Monoid (SomeTri [(CATEGORY,Any)]) where
    mempty = T Leaf' (Zero :/: Zero)
    t0 `mappend` t1 = merge True t0 t1
    -- FIXME: Change to (unsafePerformIO randomIO)

instance Measured (SomeTri [(CATEGORY,Any)]) IntToken where
    -- Note: place the token just above the diagonal
    measure tok = T (Bin' 0 Leaf' Leaf') (q True :/: q False)
      where 
        q b = quad Zero (t b) Zero Zero
        select b = if b then leftOf else rightOf
        t b = case intToToken tok of
            Nothing  -> Zero
            Just tok -> One $ (select b) $ tokenToCats b tok

instance Show (SomeTri [(CATEGORY,Any)]) where
    show (T s (ml :/: mr)) = undefined

-- None and Skip are just discarded, as seen in measureToTokens in LexGen
intToToken :: IntToken -> Maybe Token
intToToken (Token lex acc) = case acc of
    AlexAccNone -> Nothing
    AlexAccSkip -> Nothing
    AlexAcc f   -> Just $ f (Pn 0 1 1) lex -- dummy position for now

type LexState = (Table State (Tokens ParseState),Size)
type ParseState = SomeTri [(CATEGORY,Any)]
type Result = [(Int,[(CATEGORY,Any)],Int)]

showResults :: [(CATEGORY,Any)] -> IO ()
showResults x = do
    putStrLn $ show (length x) ++ " results"
    forM_ x $ \(cat,ast) -> do
      putStrLn $ describe cat
      putStrLn $ showAst (cat,ast)

test :: FilePath -> IO ()
test filename = do
    file <- readFile filename
    let result = runTest $ makeTree file
    case result of -- borrowed from TestProgram.hs
        [(_,x,_)] -> showResults x
        _         -> print "nope :("
  where
    runTest :: FingerTree LexState Char -> Result
    runTest tree = results $ measure $ stateToTree $ fst $ measure tree


