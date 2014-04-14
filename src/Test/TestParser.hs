module Main where

import Data.Monoid
import Control.DeepSeq
import Control.Monad
import Criterion.Main
import Criterion.Config
import qualified Measuring as M
import qualified LexJavaletteLight as L
import AbsJavaletteLight

instance NFData Prog where

test :: String -> Maybe Prog
test = M.parse . M.lex . L.makeTree

testFiles = ["Medium.jl","Large.jl","Enormous.jl"]

myConfig = defaultConfig {
    cfgSamples = Last $ Just 1000
}

main :: IO ()
main = do
    fs <- mapM readFile testFiles
    defaultMainWith myConfig (return ()) $ map (\(f,d) -> bench f $ nf test d) $ zip testFiles fs

