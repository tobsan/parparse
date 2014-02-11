{-# LANGUAGE CPP, FlexibleInstances, MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances, FlexibleContexts #-}

{-# OPTIONS -fno-warn-incomplete-patterns #-}
module JavaLette where

import Data.Array

import Prelude hiding (foldl,foldr,null)
import Data.FingerTree 
import qualified Data.Sequence as S 
import Data.Foldable (foldl,foldr,toList)
import Data.Monoid

-- Wrapper template
import Data.Word
import Data.Bits

lexCode ::  Measured v IntToken => String -> LexTree v 
lexCode = makeTree

tokens :: Measured v IntToken => LexTree v -> FingerTree v IntToken
tokens = undefined
-- tokens = treeToTokens

printTree = undefined


alex_base :: Array Int Int
alex_base = listArray (0,29) [-8,-36,74,288,416,544,800,-70,0,785,999,0,145,-35,140,353,914,1255,1191,0,1437,0,1651,18,-34,0,1868,1060,1400,1410]

alex_table :: Array Int Int
alex_table = listArray (0,2123) [0,23,23,23,23,23,10,0,0,25,15,20,0,29,29,29,29,29,29,29,29,29,29,0,23,0,0,23,23,23,23,23,25,25,25,24,0,0,0,1,27,27,27,27,27,27,27,27,27,27,23,25,25,25,0,0,0,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,0,0,0,0,0,0,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,25,3,25,0,0,0,21,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,0,16,28,28,28,28,28,28,28,28,28,28,0,0,0,0,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,6,7,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,2,0,0,0,0,22,18,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,29,29,29,29,29,29,29,29,29,29,0,0,0,0,0,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,6,7,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,17,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,4,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,12,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,2,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,0,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,0,26,26,26,26,26,26,26,26,14,0,27,27,27,27,27,27,27,27,27,27,0,0,0,0,0,0,0,0,0,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,6,7,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,28,28,28,28,28,28,28,28,28,28,29,29,29,29,29,29,29,29,29,29,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,13,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,17,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,18,4,11,11,11,11,11,11,11,11,11,11,11,11,11,11,12,5,8,8,8,9,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,2,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,6,7,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,26,0,0,0,0,0,0,0,0,26,26,26,26,26,26,26,26,26,26,0,0,0,0,0,0,0,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,0,0,0,0,26,0,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,16,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0]

alex_check :: Array Int Int
alex_check = listArray (0,2123) [-1,9,10,11,12,13,42,-1,-1,43,45,47,-1,48,49,50,51,52,53,54,55,56,57,-1,32,-1,-1,9,10,11,12,13,40,41,42,43,-1,-1,-1,47,48,49,50,51,52,53,54,55,56,57,32,59,60,61,-1,-1,-1,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,84,85,86,87,88,89,90,-1,-1,-1,-1,-1,-1,97,98,99,100,101,102,103,104,105,106,107,108,109,110,111,112,113,114,115,116,117,118,119,120,121,122,123,42,125,-1,-1,-1,47,192,193,194,195,196,197,198,199,200,201,202,203,204,205,206,207,208,209,210,211,212,213,214,215,216,217,218,219,220,221,222,223,224,225,226,227,228,229,230,231,232,233,234,235,236,237,238,239,240,241,242,243,244,245,246,247,248,249,250,251,252,253,254,255,-1,195,48,49,50,51,52,53,54,55,56,57,-1,-1,-1,-1,128,129,130,131,132,133,134,135,136,137,138,139,140,141,142,143,144,145,146,147,148,149,150,151,152,153,154,155,156,157,158,159,160,161,162,163,164,165,166,167,168,169,170,171,172,173,174,175,176,177,178,179,180,181,182,183,184,185,186,187,188,189,190,191,192,193,194,195,196,197,198,199,200,201,202,203,204,205,206,207,208,209,210,211,212,213,214,215,216,217,218,219,220,221,222,223,224,225,226,227,228,229,230,231,232,233,234,235,236,237,238,239,240,241,242,243,244,245,246,247,248,249,250,251,252,253,254,255,42,-1,-1,-1,-1,47,191,192,193,194,195,196,197,198,199,200,201,202,203,204,205,206,207,208,209,210,211,212,213,214,215,216,217,218,219,220,221,222,223,224,225,226,227,228,229,230,231,232,233,234,235,236,237,238,239,240,241,242,243,244,245,246,247,248,249,250,251,252,253,254,255,48,49,50,51,52,53,54,55,56,57,-1,-1,-1,-1,-1,128,129,130,131,132,133,134,135,136,137,138,139,140,141,142,143,144,145,146,147,148,149,150,151,152,153,154,155,156,157,158,159,160,161,162,163,164,165,166,167,168,169,170,171,172,173,174,175,176,177,178,179,180,181,182,183,184,185,186,187,188,189,190,191,192,193,194,195,196,197,198,199,200,201,202,203,204,205,206,207,208,209,210,211,212,213,214,215,216,217,218,219,220,221,222,223,224,225,226,227,228,229,230,231,232,233,234,235,236,237,238,239,240,241,242,243,244,245,246,247,248,249,250,251,252,253,254,255,128,129,130,131,132,133,134,135,136,137,138,139,140,141,142,143,144,145,146,147,148,149,150,151,152,153,154,155,156,157,158,159,160,161,162,163,164,165,166,167,168,169,170,171,172,173,174,175,176,177,178,179,180,181,182,183,184,185,186,187,188,189,190,191,192,193,194,195,196,197,198,199,200,201,202,203,204,205,206,207,208,209,210,211,212,213,214,215,216,217,218,219,220,221,222,223,224,225,226,227,228,229,230,231,232,233,234,235,236,237,238,239,240,241,242,243,244,245,246,247,248,249,250,251,252,253,254,255,128,129,130,131,132,133,134,135,136,137,138,139,140,141,142,143,144,145,146,147,148,149,150,151,152,153,154,155,156,157,158,159,160,161,162,163,164,165,166,167,168,169,170,171,172,173,174,175,176,177,178,179,180,181,182,183,184,185,186,187,188,189,190,191,192,193,194,195,196,197,198,199,200,201,202,203,204,205,206,207,208,209,210,211,212,213,214,215,216,217,218,219,220,221,222,223,224,225,226,227,228,229,230,231,232,233,234,235,236,237,238,239,240,241,242,243,244,245,246,247,248,249,250,251,252,253,254,255,0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63,64,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,84,85,86,87,88,89,90,91,92,93,94,95,96,97,98,99,100,101,102,103,104,105,106,107,108,109,110,111,112,113,114,115,116,117,118,119,120,121,122,123,124,125,126,127,143,144,145,146,147,148,149,150,151,152,153,154,155,156,157,158,159,160,161,162,163,164,165,166,167,168,169,170,171,172,173,174,175,176,177,178,179,180,181,182,183,184,185,186,187,188,189,190,191,192,193,194,195,196,197,198,199,200,201,202,203,204,205,206,207,208,209,210,211,212,213,214,215,216,217,218,219,220,221,222,223,224,225,226,227,228,229,230,231,232,233,234,235,236,237,238,239,240,241,242,243,244,245,246,247,248,249,250,251,252,253,254,255,42,128,129,130,131,132,133,134,135,136,137,138,139,140,141,142,143,144,145,146,147,148,149,150,-1,152,153,154,155,156,157,158,159,160,161,162,163,164,165,166,167,168,169,170,171,172,173,174,175,176,177,178,179,180,181,182,-1,184,185,186,187,188,189,190,191,46,-1,48,49,50,51,52,53,54,55,56,57,-1,-1,-1,-1,-1,-1,-1,-1,-1,128,129,130,131,132,133,134,135,136,137,138,139,140,141,142,143,144,145,146,147,148,149,150,151,152,153,154,155,156,157,158,159,160,161,162,163,164,165,166,167,168,169,170,171,172,173,174,175,176,177,178,179,180,181,182,183,184,185,186,187,188,189,190,191,192,193,194,195,196,197,198,199,200,201,202,203,204,205,206,207,208,209,210,211,212,213,214,215,216,217,218,219,220,221,222,223,224,225,226,227,228,229,230,231,232,233,234,235,236,237,238,239,240,241,242,243,244,245,246,247,248,249,250,251,252,253,254,255,0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63,64,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,84,85,86,87,88,89,90,91,92,93,94,95,96,97,98,99,100,101,102,103,104,105,106,107,108,109,110,111,112,113,114,115,116,117,118,119,120,121,122,123,124,125,126,127,192,193,194,195,196,197,198,199,200,201,202,203,204,205,206,207,208,209,210,211,212,213,214,215,216,217,218,219,220,221,222,223,224,225,226,227,228,229,230,231,232,233,234,235,236,237,238,239,240,241,242,243,244,245,246,247,248,249,250,251,252,253,254,255,10,48,49,50,51,52,53,54,55,56,57,48,49,50,51,52,53,54,55,56,57,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,101,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,128,129,130,131,132,133,134,135,136,137,138,139,140,141,142,143,144,145,146,147,148,149,150,151,152,153,154,155,156,157,158,159,160,161,162,163,164,165,166,167,168,169,170,171,172,173,174,175,176,177,178,179,180,181,182,183,184,185,186,187,188,189,190,191,192,193,194,195,196,197,198,199,200,201,202,203,204,205,206,207,208,209,210,211,212,213,214,215,216,217,218,219,220,221,222,223,224,225,226,227,228,229,230,231,232,233,234,235,236,237,238,239,240,241,242,243,244,245,246,247,248,249,250,251,252,253,254,255,42,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,128,129,130,131,132,133,134,135,136,137,138,139,140,141,142,143,144,145,146,147,148,149,150,151,152,153,154,155,156,157,158,159,160,161,162,163,164,165,166,167,168,169,170,171,172,173,174,175,176,177,178,179,180,181,182,183,184,185,186,187,188,189,190,191,192,193,194,195,196,197,198,199,200,201,202,203,204,205,206,207,208,209,210,211,212,213,214,215,216,217,218,219,220,221,222,223,224,225,226,227,228,229,230,231,232,233,234,235,236,237,238,239,240,241,242,243,244,245,246,247,248,249,250,251,252,253,254,255,39,-1,-1,-1,-1,-1,-1,-1,-1,48,49,50,51,52,53,54,55,56,57,-1,-1,-1,-1,-1,-1,-1,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,84,85,86,87,88,89,90,-1,-1,-1,-1,95,-1,97,98,99,100,101,102,103,104,105,106,107,108,109,110,111,112,113,114,115,116,117,118,119,120,121,122,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,195,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1]

alex_deflt :: Array Int Int
alex_deflt = listArray (0,29) [-1,-1,10,10,-1,-1,10,10,11,11,10,19,19,-1,-1,-1,-1,20,20,20,20,-1,10,-1,-1,-1,-1,-1,-1,-1]

-- NOTE: [] used to be AlexAccNone from BNFC generation
alex_accept :: Array Int [AlexAcc (Posn -> S.Seq Char -> Token)]
alex_accept = listArray (0::Int,29) [[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[AlexAccSkip],[AlexAccSkip],[AlexAccSkip],[AlexAccSkip],[AlexAcc (alex_action_3)],[AlexAcc (alex_action_3)],[AlexAcc (alex_action_4)],[AlexAcc (alex_action_5)],[AlexAcc (alex_action_6)],[AlexAcc (alex_action_6)]]

tok f p s = f p s

--share :: S.Seq Char -> S.Seq Char
share = id

data Tok =
   TS !(S.Seq Char) !Int    -- reserved words and symbols
 | TL !(S.Seq Char)         -- string literals
 | TI !(S.Seq Char)         -- integer literals
 | TV !(S.Seq Char)         -- identifiers
 | TD !(S.Seq Char)         -- double precision float literals
 | TC !(S.Seq Char)         -- character literals
 deriving (Eq,Show,Ord)

data Token = 
   PT  Posn Tok
 | Err Posn
  deriving (Eq,Show,Ord)

data Posn = Pn !Int !Int !Int
        deriving (Eq,Show,Ord)

tokenPos (PT (Pn _ l _) _ :_) = "line " ++ show l
tokenPos (Err (Pn _ l _) :_) = "line " ++ show l
tokenPos _ = "end of file"

posLineCol :: Posn -> (Int,Int)
posLineCol (Pn _ l c) = (l,c)

mkPosToken :: Token -> ((Int,Int), String)
mkPosToken t@(PT p _) = (posLineCol p, prToken t)

prToken :: Token -> String
prToken t = case t of
  PT _ (TS s _) -> toList s
  PT _ (TI s) -> toList s
  PT _ (TV s) -> toList s
  PT _ (TD s) -> toList s
  PT _ (TC s) -> toList s
  PT _ (TL s) -> toList s
--  _ -> S.fromList $ show t

data BTree = N | B (S.Seq Char) Tok BTree BTree deriving (Show)

eitherResIdent :: (S.Seq Char -> Tok) -> S.Seq Char -> Tok
eitherResIdent tv s = treeFind resWords
  where
  treeFind N = tv s
  treeFind (B a t left right) | s < a  = treeFind left
                              | s > a  = treeFind right
                              | s == a = t

resWords :: BTree
resWords = b "<" 7 (b "+" 4 (b ")" 2 (b "(" 1 N N) (b "*" 3 N N)) (b ";" 6 (b "++" 5 N N) N)) (b "while" 11 (b "double" 9 (b "=" 8 N N) (b "int" 10 N N)) (b "}" 13 (b "{" 12 N N) N))
   where b s n = let bs = S.fromList s
                 in B bs (TS bs n)

unescapeInitTail :: S.Seq Char -> S.Seq Char
unescapeInitTail = unesc . tail . toList where
  unesc s = case s of
    '\\':c:cs | elem c ['\"', '\\', '\''] -> S.singleton c <> unesc cs
    '\\':'n':cs  -> S.singleton '\n' <> unesc cs
    '\\':'t':cs  -> S.singleton '\t' <> unesc cs
    '"':[]    -> mempty
    c:cs      -> S.singleton c <> unesc cs
    _         -> mempty

-------------------------------------------------------------------
-- Alex wrapper code.
-- A divide and conquer wrapper.
-------------------------------------------------------------------
-- Generic template
type State = Int
type Transition v = State -> (Tokens v) -- Transition from in state to Tokens
-- Wrapper template?
data Tokens v = NoTokens
              | InvalidTokens !(S.Seq Char)
              | Tokens { currentSeq  :: !(FingerTree v IntToken)
                        , lastToken  :: !(Suffix v)
                        , outState   :: !State}
-- The suffix is the the sequence of as long as possible accepting tokens.
-- It can itself contain a suffix for the last token.
                 -- deriving Show
--This is either a Sequence of tokens or one token if the it hits an accepting state with later characters
-- Generic template
data Suffix v = Str !(S.Seq Char)
              | One !IntToken
              | Multi !(Tokens v)
                 -- deriving Show
type Size     = Sum Int
--Wrapper
type LexTree v = FingerTree (Table State (Tokens v),Size) Char
data IntToken  = Token { lexeme   :: !(S.Seq Char)
--                      , prev     :: Char
                      , token_id :: Accepts}
--Wrapper template
type Accepts   = [AlexAcc (Posn -> S.Seq Char -> Token)]
type Table a b = Array State b

tabulate :: (State,State) -> (State -> b) -> Table State b
tabulate range f = listArray range [f i | i <- [fst range..snd range]]

access :: Table State b -> (State -> b)
access a x = a ! x

-- debug stuffs
instance Show IntToken where
  show token = case map showAcc (token_id token) of
      [] -> "No Token:" ++ show (lexeme token) ++ "\n"
      toks -> unlines toks
    where showAcc acc = case acc of 
            AlexAcc f -> show $ f (Pn 0 0 0) (lexeme token)
            AlexAccSkip -> "Skip:" ++ show (lexeme token)

-- Generic template
instance (Measured v IntToken) => Monoid (Table State (Tokens v)) where
  mempty = tabulate stateRange (\_ -> emptyTokens)
  f `mappend` g = tabulate stateRange $ combineTokens (access f) (access g)

-- Wrapper template
-- The base case for when one character is lexed.
instance (Measured v IntToken) => Measured (Table State (Tokens v),Size) Char where
  measure c =
    let bytes = encode c
        sing = singleton c
        cSeq = S.singleton c
        baseCase in_state | in_state == -1 = InvalidTokens cSeq
                          | otherwise = case foldl automata in_state bytes of
          -1 -> InvalidTokens cSeq
          os -> case alex_accept ! os of
            []  -> Tokens empty (Str cSeq) os
            acc -> Tokens empty (One (createToken cSeq acc)) os
    in (tabulate stateRange $ baseCase, Sum 1)

createToken :: S.Seq Char -> Accepts -> IntToken
createToken lex acc = Token lex acc

createTokens :: Measured v IntToken => FingerTree v IntToken -> Suffix v -> State -> Tokens v
createTokens seq suf state = if null seq
                             then NoTokens
                             else Tokens seq suf state

-- Wrapper template
invalidTokens :: S.Seq Char -> Tokens v
invalidTokens s = InvalidTokens s

-- Wrapper template
emptyTokens :: Tokens v
emptyTokens = NoTokens

--------- Combination functions, the conquer step

-- Generic template
-- Combines two transition maps
combineTokens :: Measured v IntToken => Transition v -> Transition v -> Transition v
combineTokens trans1 trans2 in_state | isInvalid toks1 = toks1
                                     | isEmpty toks1   = trans2 in_state
                                     | otherwise = combineWithRHS toks1 trans2
  where toks1 = trans1 in_state

-- Generic template
-- Tries to merge tokens first, if it can't it either appends the token or calls
-- itself if the suffix contains Tokens instaed of a single token.
combineWithRHS :: Measured v IntToken => Tokens v -> Transition v -> Tokens v
combineWithRHS toks1 trans2 | isEmpty toks2 = toks1
                            | isValid toks2 =
    let toks2' = mergeTokens (lastToken toks1) toks2 trans2
    in appendTokens seq1 toks2'                           
                            | otherwise     = case lastToken toks1 of
    Multi suffToks ->
      let toks2' = combineWithRHS suffToks trans2 -- try to merge suffix
      in appendTokens seq1 toks2'
    One tok -> appendTokens (seq1 |> tok) (trans2 startState)
    Str s -> invalidTokens s
  where toks2 = trans2 $ outState toks1
        seq1 = currentSeq toks1

-- Generic template
-- Creates one token from the last token of the first sequence and and the first
-- token of the second sequence and inserts it between the init of the first
-- sequence and the tail of the second sequence
mergeTokens :: Measured v IntToken => Suffix v -> Tokens v -> Transition v -> Tokens v
mergeTokens suff1 toks2 trans2 = case viewl (currentSeq toks2) of
  token2 :< seq2' -> let newToken = mergeToken suff1 token2
                     in toks2 {currentSeq = newToken <| seq2'}
  EmptyL -> case alex_accept ! out_state of
    [] -> toks2 {lastToken = mergeSuff suff1 (lastToken toks2) trans2}
    acc -> let lex = suffToStr suff1 <> suffToStr (lastToken toks2)
           in toks2 {lastToken = One $ createToken lex acc}
  where out_state = outState toks2

-- Generic template
-- Creates on token from a suffix and a token
mergeToken :: Suffix v -> IntToken -> IntToken
mergeToken suff1 token2 = token2 {lexeme = suffToStr suff1 <> lexeme token2}

-- Generic template
-- Creates the apropiet new suffix from two suffixes
mergeSuff :: Measured v IntToken => Suffix v -> Suffix v -> Transition v -> Suffix v
mergeSuff (Multi toks1) suff2 trans2 = Multi $ -- O(n^2)
  let newToks = combineWithRHS toks1 trans2
  in if isValid $ newToks
     then newToks
     else toks1 {lastToken = mergeSuff (lastToken toks1) suff2 trans2}
mergeSuff (Str s1) suff2 _ = Str $ s1 <> suffToStr suff2
mergeSuff (One token1) (Str s) trans2 =
  let toks2 = trans2 startState
  in if isValid toks2
     then Multi $ toks2 {currentSeq = token1 <| currentSeq toks2}
     else Multi $ createTokens (singleton token1) (Str s) (-1)
mergeSuff suff1 (One token2) _ = One $ mergeToken suff1 token2 -- O(n)
mergeSuff suff1 (Multi toks2) trans2 = Multi $ mergeTokens suff1 toks2 trans2 -- O(n^2)

-- Generic template
-- Prepends a sequence of tokens on the sequence in Tokens
appendTokens :: Measured v IntToken => FingerTree v IntToken -> Tokens v -> Tokens v
appendTokens seq1 toks2 | isValid toks2 = toks2 {currentSeq = seq1 <> currentSeq toks2}
                        | otherwise = toks2

---------- Constructors

-- Generic template
makeTree :: Measured v IntToken => String -> LexTree v
makeTree str = fromList str

{-
-- Wrapper template
measureToTokens :: Measured v IntToken => (Table State (Tokens v),Size) -> FingerTree v Token
measureToTokens m = case access (fst $ m) startState of
  InvalidTokens s -> error $ "Unacceptable token: " ++ toList s
  NoTokens -> empty
  Tokens seq suff out_state -> 
    snd $ foldlWithIndex showToken (Pn 0 1 1,empty) $ intToks seq suff
  where 
    -- Reconstructing the one from Data.Sequence
    foldlWithIndex :: (b -> Int -> a -> b) -> b -> FingerTree v a -> b
    foldlWithIndex f z xs = foldl (\ g x i -> i `seq` f (g (i - 1)) i x) (const z) xs (length xs - 1)

    showToken (pos,toks) _ (Token lex accs) =
      let pos' = foldl alexMove pos lex
      in case accs of
        [] -> (pos',toks)
        AlexAcc f:_   -> (pos',toks |> f pos lex)
        AlexAccSkip:_ -> (pos',toks)

    intToks :: Measured v IntToken => FingerTree v IntToken -> Suffix v -> FingerTree v IntToken
    intToks seq (Str str) = error $ "Unacceptable token: " ++ toList str
    intToks seq (One token) = seq |> token
    intToks seq (Multi (Tokens seq' suff' _)) = intToks (seq <> seq') suff'
-}
{-
-- Generic template
treeToTokens :: Measured v IntToken => LexTree v -> FingerTree v IntToken
treeToTokens = measureToTokens . measure
-}

------------- Util funs

-- Wrapper template
isValid :: Tokens v -> Bool
isValid (Tokens _ _ _) = True
isValid _ = False

-- Wrapper template
isEmpty :: Tokens v -> Bool
isEmpty NoTokens = True
isEmpty _        = False

-- Wrapper template
isInvalid :: Tokens v -> Bool
isInvalid (InvalidTokens _) = True
isInvalid _ = False

-- Generic template
suffToStr :: Suffix v -> S.Seq Char
suffToStr (Str s) = s
suffToStr (One token) = lexeme token
suffToStr (Multi toks) =
  concatLexemes (currentSeq toks) <> suffToStr (lastToken toks)

isAccepting :: Tokens v -> Bool
isAccepting (Tokens _ suff _) = case suff of
  Str _ -> False
  One _ -> True
  Multi toks -> isAccepting toks
isAccepting NoTokens = True
isAccepting _ = False

-- Genereic template
concatLexemes :: FingerTree v IntToken -> S.Seq Char
concatLexemes = foldr ((<>) . lexeme) mempty

insertAtIndex :: Measured v IntToken => String -> Int -> LexTree v -> LexTree v
insertAtIndex str i tree = 
  if i < 0
  then error "index must be >= 0"
  else l <> (makeTree str) <> r
     where (l,r) = splitTreeAt i tree

splitTreeAt :: Measured v IntToken => Int -> LexTree v -> (LexTree v,LexTree v)
splitTreeAt i tree = split (\(_,s) -> getSum s>i) tree

size :: Measured v IntToken => LexTree v -> Int
size tree = getSum . snd $ measure tree

suffSize :: Suffix v -> Int
suffSize (Multi toks) = toksSize toks
suffSize (Str s) = S.length s
suffSize (One (Token lex _)) = S.length lex

toksSize :: Tokens v -> Int
toksSize (Tokens seq suff _) = suffSize suff + foldl bla 0 seq
  where bla _ (Token lex _) = S.length lex
toksSize _ = 0

-- wrapper template
alexMove :: Posn -> Char -> Posn
alexMove (Pn a l c) '\t' = Pn (a+1)  l     (((c+7) `div` 8)*8+1)
alexMove (Pn a l _) '\n' = Pn (a+1) (l+1)   1
alexMove (Pn a l c) _    = Pn (a+1)  l     (c+1)

-- Starting state
startState :: Int
startState = 0
-- A tuple that says how many states there are
stateRange :: (Int,Int)
stateRange = let (start,end) = bounds alex_accept
             in (start-1,end)

-- Generic
-- Takes an in state and a byte and returns the corresponding out state using the DFA
automata :: Int -> Word8 -> Int
automata (-1) _ = -1
automata s c = let base   = alex_base ! s
                   ord_c  = fromEnum c
                   offset = base + ord_c
                   check  =  alex_check ! offset
               in if (offset >= (0)) && (check == ord_c)
                  then alex_table ! offset
                  else alex_deflt ! s

-- wrapper (Not byteString but others)
-- Converts an UTF8 character to a list of bytes
encode :: Char -> [Word8]
encode  = map fromIntegral . go . fromEnum
 where
  go oc
   | oc <= 0x7f       = [oc]
   | oc <= 0x7ff      = [ 0xc0 + (oc `shiftR` 6)
                        , 0x80 + oc .&. 0x3f
                        ]
   | oc <= 0xffff     = [ 0xe0 + (oc `shiftR` 12)
                        , 0x80 + ((oc `shiftR` 6) .&. 0x3f)
                        , 0x80 + oc .&. 0x3f
                        ]
   | otherwise        = [ 0xf0 + (oc `shiftR` 18)
                        , 0x80 + ((oc `shiftR` 12) .&. 0x3f)
                        , 0x80 + ((oc `shiftR` 6) .&. 0x3f)
                        , 0x80 + oc .&. 0x3f
                        ]

alex_action_3 =  tok (\p s -> PT p (eitherResIdent (TV . share) s)) 
alex_action_4 =  tok (\p s -> PT p (eitherResIdent (TV . share) s)) 
alex_action_5 =  tok (\p s -> PT p (TI $ share s))    
alex_action_6 =  tok (\p s -> PT p (TD $ share s)) 
-- -----------------------------------------------------------------------------
-- ALEX TEMPLATE
--
-- This code is in the PUBLIC DOMAIN; you may copy it freely and use
-- it for any purpose whatsoever.

data AlexAcc a
  = AlexAcc a
  | AlexAccSkip

--
-- Tobsan deleted lots of alex code that was commented out here
--

