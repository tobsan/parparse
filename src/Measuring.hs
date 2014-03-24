{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, MagicHash #-}

module Main where

import GHC.Prim (Any)
import GHC.Exts
import Data.Monoid hiding (Any)
import System.IO.Unsafe (unsafePerformIO)
import System.Random (randomIO)
import System.Environment
import Control.Applicative
import Control.Monad

import Data.Matrix.Quad
import Parsing.Chart hiding (fingerprint)
import Algebra.RingUtils
import Data.FingerTree

import AbsJavaletteLight
import LexJavaletteLight hiding (One)
import CnfTablesJavaletteLight

type LexState = (Table State (Tokens ParseState),Size)
type ParseState = SomeTri [(CATEGORY,Any)]

-- For testing having range embedded
type ExtendedLexState = (Table State (Tokens ExtendedState),Size)
type ExtendedState = SomeTri [(CATEGORY,Any,Range)]

-- Generic instance for upper-triangular matrices
instance RingP a => Monoid (SomeTri a) where
    mempty = T Leaf' (Zero :/: Zero)
    t0 `mappend` t1 = unsafePerformIO $ do
        b <- randomIO
        return $ merge b t0 t1

instance Measured ParseState IntToken where
    measure tok = T (Bin' 0 Leaf' Leaf') (q True :/: q False)
      where 
        q b = quad Zero (t b) Zero Zero
        select b = if b then leftOf else rightOf
        t b = case intToToken tok of
            Nothing    -> Zero
            Just token -> one $ select b $ tokenToCats b token

--instance Measured ExtendedState IntToken where
--    measure tok = T (Bin' 0 Leaf' Leaf') (q True :/: q False)
--      where
--        q b = quad Zero (t b) Zero Zero
--        select b = if b then leftOf else rightOf
--        t b = case intToToken tok of
--            Nothing  -> Zero
--            Just tok -> let cats = select b $ tokenToCats b tok
--                        in one $ map (\(c,a) -> (c,a,Nil)) cats

data Range = Nil | Branch Range Range
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
    AlexAcc f   -> Just $ f (Pn 0 1 1) lex -- dummy position

toAst :: (CATEGORY,Any) -> String
toAst (cat,ast) = case cat of 
      CAT_Prog -> show $ ((unsafeCoerce# ast)::Prog)
      CAT_Stm -> show $ ((unsafeCoerce# ast)::Stm)
      CAT_Exp -> show $ ((unsafeCoerce# ast)::Exp)
      CAT_Typ -> show $ ((unsafeCoerce# ast)::Typ)
      _       -> describe cat

main :: IO ()
main = getArgs >>= \[filename] -> test filename

test :: FilePath -> IO ()
test filename = do
    file <- readFile filename
    let tri = getTri $ makeTree file
        res = results tri
        fing = fingerprint tri
    mapM_ putStrLn fing
--    writeFile (filename ++ ".xpm") $ genXPM fing
    forM_ res $ \(_,x,_) -> mapM_ (putStrLn . toAst) x
  where
    getTri :: FingerTree LexState Char -> ParseState
    getTri tree = measure $ stateToTree $ fst $ measure tree

