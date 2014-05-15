import System.Environment ( getArgs )
import LexJavalette as Lexer
import CnfTablesJavalette as Parser
import GHC.Exts
import Criterion.Main
import Criterion.Config
import Algebra.RingUtils
import Control.Applicative
import Measuring ()
import Data.FingerTree (measure)
import Data.Monoid hiding (Any)
import Control.DeepSeq
import Data.Matrix.Quad (SomeTri, root)

type T = [(CATEGORY,Any)]
instance NFData (SomeTri a) where

main = do
  f:_ <- getArgs
  s <- readFile f
  let (cs1,cs2) = splitAt (length s `div` 2) s
      lex = stateToTree . measure . makeTree
      ts = [lex cs1,lex cs2]
      work [c1,c2] = mappend (measure c1) (measure c2)
--  putStrLn $ show $ map fst $ root $ work ts
  defaultMainWith myConfig (return ()) [bench f $ nf work ts]

myConfig = defaultConfig {
    cfgSamples = Last $ Just 1000,
    cfgReport = Last $ Just "benchCNF.html",
    cfgSummaryFile = Last $ Just "benchCNF.csv",
    cfgVerbosity = Last $ Just Verbose
}

{-
  let ts = zipWith tokenToCats (cycle [False,True]) (Lexer.tokens s)
      (ts1,x:ts2) = splitAt (length ts `div` 2) ts
      cs = [mkTree ts1,mkTree' ts2]
      work [c1,c2] = show $ map fst $ root $ mergein False c1 x c2
  defaultMain [bench f $ nf work cs] -- note the hack!!!
-}
