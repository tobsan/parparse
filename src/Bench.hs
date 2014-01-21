{-# LANGUAGE FlexibleInstances #-}
module Main where

import Prelude as P hiding (mapM_,foldl1)
import System.Environment
import System.IO
import Criterion.Main
import Criterion.Config
import Java as J
import Lexjava as A
import Control.DeepSeq
import Data.FingerTree as F
import Data.Map as M (foldrWithKey',insert,empty)
import Data.Foldable (mapM_,foldl1)
import Data.Sequence as S hiding (zip)
import Data.Monoid
import Data.Char
import Data.Array

instance NFData A.Token where
  rnf = rnf . A.prToken

instance (NFData v,NFData a,Measured v a) => NFData (FingerTree v a) where
  rnf tree = foldl1 (seq . rnf) tree `seq` rnf (measure tree)

--instance NFData a => NFData (Seq a) where
--  rnf = flip seq () . foldl1 (seq . rnf)
   
instance NFData a => NFData (Sum a) where
  rnf (Sum a) = rnf a

instance NFData Tokens where
  rnf (Tokens toks suff _) = rnf toks `seq` rnf suff
  rnf _ = ()

instance NFData IntToken where
  rnf (Token lex _) = rnf lex

instance NFData Suffix where
  rnf (Str s) = rnf s
  rnf (One t) = rnf t
  rnf (Multi toks) = rnf toks

allTest :: [String]
allTest = ["Alex","Update"]

testSizes :: [Int]
testSizes = 10:[100,200..1900]

main = do
  args <- getArgs
  let (n,inFile,tests',arg) = sortArgs args
  (file,code) <- case inFile of
    [] -> return ("internal",core002)
    _  -> do lazyCode <- openFile inFile ReadMode >>= hGetContents
             return (inFile,lazyCode)
  let tests = case tests' of
        [] -> allTest
        _  -> tests'
      codes = map (\i -> (concat . P.take i $ repeat code,show i)) testSizes
      trees = do
        (code,name) <- codes
        let tree = J.lexCode code
            halfSize = size tree `div` 2
            (left,right) = splitTreeAt halfSize tree
        return ((left, right),name)
      testFuns =
        [ ("Alex",map (benchStuff (last . alexScanTokens)) codes)
        , ("IncLex",map (benchStuff (J.lexCode)) codes)
        , ("Update",map (benchStuff (getOutState . measure . uncurry mappend)) trees)
        ]
--      testBig = map (\i -> bgroup (show i) [bgroup tests | (name,tests) <- testFuns])
  withArgs (tests ++ arg) $
    defaultMain [ bgroup name tests | (name,tests) <- testFuns ]

repeatTree :: Int -> LexTree -> LexTree
repeatTree 0 _ = mempty
repeatTree 1 tree = tree
repeatTree i tree = tree <> repeatTree (i-1) tree

helperIncBuilder :: LexTree -> [(LexTree,String)] -> Int -> [(LexTree,String)]
helperIncBuilder tree sizeTrees i = sizeTrees ++ [(repeatTree i tree, show i)]

benchStuff :: (NFData b, NFData a) => (a -> b) -> (a,String) -> Benchmark
benchStuff f (x,name) = x `deepseq` bench name $ nf f x

sortArgs :: [String] -> (Int,String,[String],[String])
sortArgs args = foldl sortArg (10,[],[],[]) args
  where sortArg (num,files,tests,args) arg@('-':_) =
          (num,files,tests,(arg):args)
        sortArg (num,files,tests,args) ('+':test)  =
          (num,files,if test `elem` "IncLex":allTest
                     then test:tests
                     else tests
                               ,args)
        sortArg (num,files,tests,args) arg | and $ map isDigit arg = (read arg,files,tests,args)
                                           | otherwise = (num,arg,tests,args)

getOutState :: (Table State Tokens,Size) -> Bool
getOutState (table,_) = case access table 0 of
  Tokens _ _ s -> s == 0
  NoTokens -> 0 == 0
  _ -> False

core002 :: String
core002 = "/*@ @@*/\n\n" ++
          "// hej\n" ++
          "int foo.thur() {\n" ++
          "printString(\"foo\");\n" ++
          "return if true then else if 1 else null;\n" ++
          "}"
          
simpleTest = mapM_ putStrLn $ fmap (J.prToken) $ treeToTokens $ makeTree core002

