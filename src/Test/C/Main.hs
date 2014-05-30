module Main where

-- System libraries
import System.Environment
import Data.Maybe
import Control.Monad

-- Language specific
import AbsC
import LexC
import PrintC
import CYKParser

main :: IO ()
main = do
    f:_ <- getArgs
    ast <- liftM (catMaybes . lexAndParse) (readFile f)
    print $ typecheck $ head ast

-- Look at CYKParser.hs for functions supporting other entrypoints
lexAndParse :: String -> [Maybe Program]
lexAndParse = getAllProgram . parse . tokens

-- Add your own code here
typecheck :: Program -> Bool
typecheck prg = True
