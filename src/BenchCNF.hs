import System.Environment ( getArgs )
import LexJava as Lexer
import CnfTablesJava as Parser
import GHC.Exts
import Parsing.Chart
import Criterion.Main
import Algebra.RingUtils
import Control.Applicative
type T = [(CATEGORY,Any)]
pLGrammar :: [Pair T] -> MT2 T
pLGrammar = mkTree
main = do
  f:_ <- getArgs
  s <- readFile f
  let ts = zipWith tokenToCats (cycle [False,True]) (Lexer.tokens s)
      (ts1,x:ts2) = splitAt (length ts `div` 2) ts
      cs = [mkTree ts1,mkTree' ts2]
      work [c1,c2] = show $ map fst $ root $ mergein False c1 x c2
  defaultMain [bench f $ nf work cs] -- note the hack!!!