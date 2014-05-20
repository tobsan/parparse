import System.Environment ( getArgs )
import LexJavalette as Lexer
import CnfTablesJavalette as Parser
import GHC.Exts
import Criterion.Main
import Criterion.Config
import Algebra.RingUtils
import Control.Applicative
import Measuring hiding (main)
import Data.FingerTree (measure, FingerTree)
import Data.Monoid hiding (Any)
import Control.DeepSeq
import Data.Matrix.Quad (SomeTri, root)

instance NFData CATEGORY where

main = do
  f:_ <- getArgs
  s <- readFile f
  let (cs1,cs2) = splitAt (length s `div` 2) s
      lex = stateToTree . measure . makeTree
      ts = [lex cs1,lex cs2]
      work :: [FingerTree ParseState IntToken] -> [CATEGORY]
      work [c1,c2] = map fst $ root $ mappend (measure c1) (measure c2)
  defaultMainWith myConfig (return ()) [bench f (nf work ts)]

myConfig = defaultConfig {
    cfgSamples = Last $ Just 1000,
    cfgReport = Last $ Just "benchCNF.html",
    cfgSummaryFile = Last $ Just "benchCNF.csv",
    cfgVerbosity = Last $ Just Verbose
}

