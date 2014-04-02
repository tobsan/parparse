{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, MagicHash #-}

--
--
--
module Main where

import GHC.Prim
import GHC.Exts
import Data.Monoid hiding (Any)
import Control.Monad (forM_, replicateM)
import System.IO.Unsafe
import System.Random
import System.Environment

import Control.Applicative
import Prelude as P

import Data.Matrix.Quad
import Parsing.Chart hiding (fingerprint)
import Algebra.RingUtils
import Data.FingerTree
import qualified Data.Foldable as F
import qualified Data.Sequence as S

import AbsJavaletteLight
import LexJavaletteLight hiding (One)
import CnfTablesJavaletteLight

-- | Lex some (sub)fingertree into another fingertree that can be measured into
-- an abstract syntax tree. 
lex :: FingerTree LexState Char -> FingerTree ParseState IntToken
lex tree = stateToTree $ measure tree
-- lex tree = stateToTree $ fst $ measure tree

-- TODO: How to solve grammars with multiple entrypoints?
parse :: FingerTree ParseState IntToken -> Maybe Prog
parse tree = case results $ measure tree of
    [(_,[(cat,ast,r)],_)] -> unsafeCoerce# ast
    _                     -> Nothing

type LexState = (Table State (Tokens ParseState),Size)
type ParseState = SomeTri [(CATEGORY,Any,Range)]
type Result = [(Int,[(CATEGORY,Any)],Int)]

instance RingP a => Monoid (SomeTri a) where
    mempty = T Leaf' (Zero :/: Zero)
    t0 `mappend` t1 = unsafePerformIO $ do
      b <- randomIO
      return $ merge b t0 t1

data Range = R (Int -> Int) (Int -> Int) (Int -> Int)
runRange :: Range -> Int -> Int -> Int -> (Int,Int,Int)
runRange (R lf cf tf) l c t = (lf l, cf c, tf t)

toRange :: Char -> Range
toRange '\n' = R (P.+1) (const 0) (P.+1)
toRange c    = R id (P.+1) (P.+1)

instance Monoid Range where
    mempty = R id id id
    (R l1 c1 t1) `mappend` (R l2 c2 t2) = R (l2 . l1) (c2 . c1) (t2 . t1)

-- | Measure (parse) some token
instance Measured (SomeTri [(CATEGORY,Any,Range)]) IntToken where
    -- Note: place the token just above the diagonal
    measure tok = T (bin' Leaf' Leaf') (q True :/: q False)
      where q b = quad zero (t b) zero zero
            select b = if b then leftOf else rightOf
            t b = case intToToken tok of
                Nothing    -> Zero
                Just token -> 
                    let cats = select b $ tokenToCats b token
                        lrange = F.foldMap toRange (lexeme tok)
                    in One $ map (\(c,a) -> (c,a,lrange)) cats

-- r for Range
instance Monoid r => RingP [(CATEGORY,Any,r)] where 
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
intToToken (Token lexeme acc) = case acc of
    AlexAcc f   -> Just $ f (Pn 0 1 1) lexeme -- dummy position for now
    _           -> Nothing

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
toAst (cat,ast,range) = case cat of 
      CAT_Prog -> "Range: " ++ show (runRange range 1 0 0) ++ show ((unsafeCoerce# ast)::Prog)
      CAT_Stm -> show $ ((unsafeCoerce# ast)::Stm)
      CAT_Exp -> show $ ((unsafeCoerce# ast)::Exp)
      CAT_Typ -> show $ ((unsafeCoerce# ast)::Typ)
      _       -> describe cat

main :: IO ()
main = getArgs >>= \[filename] -> test filename

test :: FilePath -> IO ()
test filename = do
    file <- readFile filename
    let mes = measure $ makeTree file
        tri = measure $ stateToTree mes
        res = results tri
        fing = fingerprint tri
    mapM_ putStrLn fing
    -- writeFile (filename ++ ".xpm") $ genXPM fing
    case res of -- borrowed from TestProgram.hs
        [(_,[x],_)] -> putStrLn $ toAst x
        _           -> print "Something is wrong"
  where
    getTri :: FingerTree LexState Char -> SomeTri [(CATEGORY,Any,Range)]
    getTri tree = measure $ stateToTree $ measure tree


testSmall :: IO ()
testSmall = do
    cont <- readFile "Small.jl"
    let tree = makeTree cont
        smaller = takeUntil pred tree
        ptree = stateToTree $ measure smaller
    putStrLn $ show $ F.toList $ ptree
  where pred (tab,range) = getSum range > 100

instance Show IntToken where
    show (Token lex acc) = F.foldr (:) "" lex
