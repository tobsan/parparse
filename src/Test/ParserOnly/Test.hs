module Main where

import System.Environment
import Data.FingerTree (fromList)
import Data.Maybe
import Control.Monad (forM_)
-- Non-incremental lexing
import qualified PrintC as Print
import qualified AbsC as Abc
import qualified LexC as L
import qualified CYKParser as P
-- Incremental lexing
import qualified PrintCInc as PrintInc
import qualified AbsCInc as AbcInc
import qualified LexCInc as LI
import qualified CYKParserInc as PI

testInc :: String -> IO ()
testInc str = 
    forM_ ast $ \(_,as,_) -> do
        forM_ as $ \a ->
            print $ PrintInc.printTree $ fromJust $ PI.getProgram $ a
        putStrLn $ "Parse results: " ++ show (length as)
  where 
    ts = LI.tokens str
    ast = PI.parse ts

testReg :: String -> IO ()
testReg str = 
    forM_ ast $ \(_,as,_) -> do
        forM_ as $ \a ->
            print $ Print.printTree $ fromJust $ P.getProgram $ a
        putStrLn $ "Parse results: " ++ show (length as)
  where 
    ts = L.tokens str
    ast = P.parse $ fromList ts

main :: IO ()
main = do
    f:_ <- getArgs
    cs <- readFile f
    putStrLn "*** Testing with incremental lexer ***"
    testInc cs
    putStrLn "*** Testing with regular lexer ***"
    testReg cs

