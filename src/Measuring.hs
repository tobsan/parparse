{-# LANGUAGE MultiParamTypeClasses, TypeSynonymInstances, FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances, GADTs, UndecidableInstances, DataKinds #-}
{-# LANGUAGE MagicHash #-}

module Measuring where

import GHC.Prim
import GHC.Exts
import Data.FingerTree (Measured, measure)
import Data.Monoid hiding (Any)
import Control.Monad (forM_, replicateM)
import System.IO.Unsafe
import System.Random

import AbsJavaletteLight
import LexJavaletteLight hiding (One)
import CnfTablesJavaletteLight
import Data.Matrix.Quad
import Parsing.Chart hiding (fingerprint)
import Algebra.RingUtils
import Data.FingerTree
import Data.List (nub)

instance Monoid (SomeTri [(CATEGORY,Any)]) where
    mempty = T Leaf' (Zero :/: Zero)
    t0 `mappend` t1 = unsafePerformIO $ do
      b <- randomIO
      return $ merge b t0 t1

instance Measured (SomeTri [(CATEGORY,Any)]) IntToken where
    -- Note: place the token just above the diagonal
    measure tok = T (Bin' 0 Leaf' Leaf') (q True :/: q False)
      where 
        q b = quad Zero (t b) Zero Zero
        select b = if b then leftOf else rightOf
        t b = case intToToken tok of
            Nothing  -> Zero
            Just tok -> One $ (select b) $ tokenToCats b tok

-- None and Skip are just discarded, as seen in measureToTokens in LexGen
intToToken :: IntToken -> Maybe Token
intToToken (Token lex acc) = case acc of
    AlexAccNone -> Nothing
    AlexAccSkip -> Nothing
    AlexAcc f   -> Just $ f (Pn 0 1 1) lex -- dummy position for now

type LexState = (Table State (Tokens ParseState),Size)
type ParseState = SomeTri [(CATEGORY,Any)]
type Result = [(Int,[(CATEGORY,Any)],Int)]

showResults :: (Int,[(CATEGORY,Any)],Int) -> IO ()
showResults (px,x,py) = do
    let xs = nub $ map toAst x
    mapM_ putStrLn xs
    putStrLn $ show (length x) ++ " results"

-- Slightly modified from CnfTables
toAst :: (CATEGORY,Any) -> String
toAst (cat,ast) = case cat of 
      CAT_Prog -> show $ ((unsafeCoerce# ast)::Prog)
      CAT_Stm -> show $ ((unsafeCoerce# ast)::Stm)
      CAT_Exp -> show $ ((unsafeCoerce# ast)::Exp)
      CAT_Typ -> show $ ((unsafeCoerce# ast)::Typ)
      _       -> describe cat

testMany :: FilePath -> IO ()
testMany filename = do 
    sizes <- replicateM 100 (do
        file <- readFile filename
        let tri = measure $ stateToTree $ fst $ measure $ makeTree file
            res = results tri
            mid (a,b,c) = b
        return $ length $ mid $ head $ res)
    print sizes
    print $ Prelude.sum sizes `div` 100

test :: FilePath -> IO ()
test filename = do
    file <- readFile filename
    let tri = getTri $ makeTree file
        res = results tri
        fing = fingerprint tri
    case tri of
      T s _ -> print s
    mapM_ putStrLn fing
    writeFile (filename ++ ".xpm") $ genXPM fing
    case res of -- borrowed from TestProgram.hs
        [] -> print "No results!"
        xs -> mapM_ showResults xs
  where
    getTri :: FingerTree LexState Char -> SomeTri [(CATEGORY,Any)]
    getTri tree = measure $ stateToTree $ fst $ measure tree


