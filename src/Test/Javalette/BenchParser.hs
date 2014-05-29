{-# LANGUAGE FlexibleContexts #-}
module Main where

import System.Environment ( getArgs )
import Data.Monoid
import Control.DeepSeq
import Control.Monad
import Criterion.Main
import Criterion.Config
import Data.FingerTree (FingerTree, Measured, singleton, measure, viewr, (><), ViewR((:>)))
import qualified Measuring as M
import qualified Data.Matrix.Quad as Q
import qualified LexJavalette as L
import Data.Sequence (empty)
import AbsJavalette
import CnfTablesJavalette

testFiles = ["Enormous1.jl", "Enormous8.jl", "Enormous16.jl","Enormous24.jl",
             "Enormous32.jl","Enormous40.jl","Enormous48.jl", "Enormous56.jl",
             "Enormous64.jl","Enormous72.jl", "Enormous80.jl","Enormous88.jl",
             "Enormous96.jl", "Enormous104.jl","Enormous112.jl","Enormous120.jl", 
             "Enormous128.jl"]

myConfig = defaultConfig {
    cfgSamples = Last $ Just 1000,
    cfgReport = Last $ Just $ "report.html",
    cfgSummaryFile = Last $ Just $ "summary.csv"
}

dummy :: Measured v L.IntToken => FingerTree v L.IntToken
dummy = singleton $ L.Token empty L.AlexAccNone

runTest :: [FilePath] -> IO ()
runTest fs = do
    ss <- mapM readFile fs
    mapM_ (print . length) ss
    let tokens = map (L.stateToTree . measure . L.makeTree) ss
        work [ts,tok] = case viewr (ts >< tok) of
            (tree :> _) -> show $ map fst $ Q.root $ measure tree
    defaultMainWith myConfig (return ()) $
        zipWith (\f ts -> bench f $ nf work [ts,dummy]) fs tokens

main :: IO ()
main = runTest testFiles

