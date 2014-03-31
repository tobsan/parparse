{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, MagicHash #-}

module Main where

import GHC.Prim
import GHC.Exts
import Data.Monoid hiding (Any)
import Control.Monad (forM_, replicateM)
import System.IO.Unsafe
import System.Random
import System.Environment

import Control.Applicative

import Data.Matrix.Quad
import Parsing.Chart hiding (fingerprint)
import Algebra.RingUtils
import Data.FingerTree
import Data.List (nub)

import AbsJavaletteLight
import LexJavaletteLight hiding (One)
import CnfTablesJavaletteLight

instance RingP a => Monoid (SomeTri a) where
    mempty = T Leaf' (Zero :/: Zero)
    t0 `mappend` t1 = unsafePerformIO $ do
      b <- randomIO
      return $ merge b t0 t1

instance Measured (SomeTri [(CATEGORY,Any,Range)]) IntToken where
    -- Note: place the token just above the diagonal
    measure tok = T (bin' Leaf' Leaf') (q True :/: q False)
      where q b = quad zero (t b) zero zero
            select b = if b then leftOf else rightOf
            t b = case intToToken tok of
                    Nothing    -> Zero
                    Just token -> 
                        let cats = select b $ tokenToCats b token
                        in One $ map (\(c,a) -> (c,a, Nil)) cats

data Range = Nil | Branch Range Range
    deriving Show

range :: Range -> Int
range Nil = 1
range (Branch r1 r2) = range r1 Prelude.+ range r2

instance Monoid Range where
    mempty = Nil
    mappend r1 r2 = Branch r1 r2

instance RingP [(CATEGORY,Any,Range)] where 
    -- Modified from CNFTables module
    mul p a b = trav [map (app tx ty rx ry) l :/: 
                      map (app tx ty rx ry) r
                     | (x,tx,rx) <- a, (y,ty,ry) <- b
                     , let l :/: r = combine p x y]
      where 
        trav :: [Pair [a]] -> Pair [a]
        trav [] = pure []
        trav (x:xs) = (++) <$> x <*> trav xs
        app tx ty rx ry (c,f) = (c, f tx ty, rx <> ry)

-- None and Skip are just discarded, as seen in measureToTokens in LexGen
intToToken :: IntToken -> Maybe Token
intToToken (Token lex acc) = case acc of
    AlexAccNone -> Nothing
    AlexAccSkip -> Nothing
    AlexAcc f   -> Just $ f (Pn 0 1 1) lex -- dummy position for now

type LexState = (Table State (Tokens ParseState),Size)
type ParseState = SomeTri [(CATEGORY,Any,Range)]
type Result = [(Int,[(CATEGORY,Any)],Int)]

--showResults :: (Int,[(CATEGORY,Any,Range)],Int) -> IO ()
--showResults (px,xs,py) = do
--    let x = nub $ map toAst xs
--    forM_ x $ \r -> do
--        putStrLn "Result: "
--        putStrLn r
--    putStrLn $ "Total number of results: " ++ (show $ length xs) ++ ", but only " ++ (show $ length x) ++ " unique results"
--    putStrLn "*************"

-- Slightly modified from CnfTables
toAst :: (CATEGORY,Any,Range) -> String
toAst (cat,ast,_) = case cat of 
      CAT_Prog -> show $ ((unsafeCoerce# ast)::Prog)
      CAT_Stm -> show $ ((unsafeCoerce# ast)::Stm)
      CAT_Exp -> show $ ((unsafeCoerce# ast)::Exp)
      CAT_Typ -> show $ ((unsafeCoerce# ast)::Typ)
      _       -> describe cat

main = getArgs >>= \[filename] -> test filename

test :: FilePath -> IO ()
test filename = do
    file <- readFile filename
    let tri = getTri $ makeTree file
        res = results tri
        fing = fingerprint tri
    mapM_ putStrLn fing
    -- writeFile (filename ++ ".xpm") $ genXPM fing
    case res of -- borrowed from TestProgram.hs
        [(_,[x],_)] -> putStrLn $ toAst x
        _           -> print "Something is wrong"
  where
    getTri :: FingerTree LexState Char -> SomeTri [(CATEGORY,Any,Range)]
    getTri tree = measure $ stateToTree $ fst $ measure tree

