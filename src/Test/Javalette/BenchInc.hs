import System.Environment ( getArgs )
import LexJavalette as Lexer
import CnfTablesJavalette as Parser
import GHC.Exts
import Criterion.Main
import Criterion.Config
import Algebra.RingUtils hiding (lex)
import Control.Applicative
import Measuring hiding (main)
import Data.FingerTree (measure, FingerTree)
import Data.Monoid hiding (Any)
import Control.DeepSeq
import Data.Matrix.Quad (SomeTri, root)

instance NFData CATEGORY where

main :: IO ()
main = runTest testFiles

runTest :: [FilePath] -> IO ()
runTest fs = do
    ss <- mapM readFile fs
    let lss = map toLexed ss
        work :: [FingerTree ParseState IntToken] -> [CATEGORY]
        work [c1,c2] = map fst $ root $ mappend (measure c1) (measure c2)
    defaultMainWith myConfig (return ()) $
        zipWith (\f (l1,l2) -> bench f (nf work [l1,l2])) fs lss
  where
    toLexed :: String -> (FingerTree ParseState IntToken,FingerTree ParseState IntToken)
    toLexed s = let (cs1, cs2) = splitAt (length s `div` 2) s
                    lex = stateToTree . measure . makeTree
                in (lex cs1, lex cs2)

myConfig = defaultConfig {
    cfgSamples = Last $ Just 100,
    cfgReport = Last $ Just "benchInc.html",
    cfgSummaryFile = Last $ Just "benchInc.csv",
    cfgVerbosity = Last $ Just Verbose
}

testFiles = ["Enormous1.jl","Enormous2.jl","Enormous4.jl", 
             "Enormous8.jl", "Enormous16.jl","Enormous24.jl",
             "Enormous32.jl", "Enormous64.jl"]

