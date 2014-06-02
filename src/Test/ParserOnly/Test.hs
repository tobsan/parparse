module Main where

import System.Environment
import Data.FingerTree (fromList)
import Data.Maybe
import Control.Monad (forM_)
import PrintC
import AbsC
import LexC as L
import CYKParser as P

main :: IO ()
main = do
    f:_ <- getArgs
    cs <- readFile f
    let ts = L.tokens cs
        ast = P.parse $ fromList ts
    forM_ ast $ \(_,as,_) -> do
        forM_ as $ \a ->
            print $ printTree $ fromJust $ getProgram $ a

