module Main where

import System.Environment ( getArgs )
import Data.Monoid
import Control.DeepSeq
import Control.Monad
import Criterion.Main
import Criterion.Config
import qualified Measuring as M
import qualified LexJavalette as L
import AbsJavalette

instance NFData Prog where

test :: String -> Maybe Prog
test = M.parse . M.lex . L.makeTree

testFiles = ["Enormous1.jl","Enormous2.jl","Enormous4.jl", 
             "Enormous8.jl", "Enormous16.jl","Enormous32.jl",
             "Enormous64.jl", "Enormous128.jl"]

myConfig = defaultConfig {
    cfgSamples = Last $ Just 1000,
    cfgReport = Last $ Just $ "report.html",
    cfgSummaryFile = Last $ Just $ "summary.csv"
}

runTest :: [FilePath] -> IO ()
runTest fs = do
    ss <- mapM readFile fs
    let tokens = map (\s -> M.lex (L.makeTree s)) ss
        parses = map M.measure tokens
    defaultMainWith myConfig (return ()) $
        zipWith (\f ps -> bench f $ nf M.merge ps ps) fs parses

-- TODO: Design this better
main :: IO ()
main = runTest testFiles

{-
main :: IO ()
main = do
    fs <- mapM readFile testFiles
    defaultMainWith myConfig (return ()) $ map (\(f,d) -> bench f $ nf test d) $ zip testLarger fs
-}
