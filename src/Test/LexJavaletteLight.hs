{-# LANGUAGE CPP #-}

{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts #-}
module LexJavaletteLight where

import qualified Data.Bits
import Data.Word (Word8)

import Prelude hiding (null, foldl, foldr)
import Data.Monoid
import Data.FingerTree
import qualified Data.Sequence as S
import Data.Foldable (toList, foldl, foldr, foldMap)

#if __GLASGOW_HASKELL__ >= 603
#include "ghcconfig.h"
#elif defined(__GLASGOW_HASKELL__)
#include "config.h"
#endif
#if __GLASGOW_HASKELL__ >= 503
import Data.Array
import Data.Char (ord)
import Data.Array.Base (unsafeAt)
#else
import Array
import Char (ord)
#endif
alex_base :: Array Int Int
alex_base = listArray (0,29) [-8,-36,74,288,416,544,800,-70,0,785,999,0,145,-35,140,353,914,1255,1191,0,1437,0,1651,18,-34,0,1868,1060,1400,1410]

alex_table :: Array Int Int
alex_table = listArray (0,2123) [0,23,23,23,23,23,10,0,0,25,15,20,0,29,29,29,29,29,29,29,29,29,29,0,23,0,0,23,23,23,23,23,25,25,25,24,0,0,0,1,27,27,27,27,27,27,27,27,27,27,23,25,25,25,0,0,0,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,0,0,0,0,0,0,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,25,3,25,0,0,0,21,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,0,16,28,28,28,28,28,28,28,28,28,28,0,0,0,0,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,6,7,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,2,0,0,0,0,22,18,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,29,29,29,29,29,29,29,29,29,29,0,0,0,0,0,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,6,7,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,17,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,4,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,12,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,2,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,0,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,0,26,26,26,26,26,26,26,26,14,0,27,27,27,27,27,27,27,27,27,27,0,0,0,0,0,0,0,0,0,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,6,7,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,28,28,28,28,28,28,28,28,28,28,29,29,29,29,29,29,29,29,29,29,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,13,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,17,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,18,4,11,11,11,11,11,11,11,11,11,11,11,11,11,11,12,5,8,8,8,9,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,2,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,6,7,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,26,0,0,0,0,0,0,0,0,26,26,26,26,26,26,26,26,26,26,0,0,0,0,0,0,0,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,0,0,0,0,26,0,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,16,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0]

alex_check :: Array Int Int
alex_check = listArray (0,2123) [-1,9,10,11,12,13,42,-1,-1,43,45,47,-1,48,49,50,51,52,53,54,55,56,57,-1,32,-1,-1,9,10,11,12,13,40,41,42,43,-1,-1,-1,47,48,49,50,51,52,53,54,55,56,57,32,59,60,61,-1,-1,-1,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,84,85,86,87,88,89,90,-1,-1,-1,-1,-1,-1,97,98,99,100,101,102,103,104,105,106,107,108,109,110,111,112,113,114,115,116,117,118,119,120,121,122,123,42,125,-1,-1,-1,47,192,193,194,195,196,197,198,199,200,201,202,203,204,205,206,207,208,209,210,211,212,213,214,215,216,217,218,219,220,221,222,223,224,225,226,227,228,229,230,231,232,233,234,235,236,237,238,239,240,241,242,243,244,245,246,247,248,249,250,251,252,253,254,255,-1,195,48,49,50,51,52,53,54,55,56,57,-1,-1,-1,-1,128,129,130,131,132,133,134,135,136,137,138,139,140,141,142,143,144,145,146,147,148,149,150,151,152,153,154,155,156,157,158,159,160,161,162,163,164,165,166,167,168,169,170,171,172,173,174,175,176,177,178,179,180,181,182,183,184,185,186,187,188,189,190,191,192,193,194,195,196,197,198,199,200,201,202,203,204,205,206,207,208,209,210,211,212,213,214,215,216,217,218,219,220,221,222,223,224,225,226,227,228,229,230,231,232,233,234,235,236,237,238,239,240,241,242,243,244,245,246,247,248,249,250,251,252,253,254,255,42,-1,-1,-1,-1,47,191,192,193,194,195,196,197,198,199,200,201,202,203,204,205,206,207,208,209,210,211,212,213,214,215,216,217,218,219,220,221,222,223,224,225,226,227,228,229,230,231,232,233,234,235,236,237,238,239,240,241,242,243,244,245,246,247,248,249,250,251,252,253,254,255,48,49,50,51,52,53,54,55,56,57,-1,-1,-1,-1,-1,128,129,130,131,132,133,134,135,136,137,138,139,140,141,142,143,144,145,146,147,148,149,150,151,152,153,154,155,156,157,158,159,160,161,162,163,164,165,166,167,168,169,170,171,172,173,174,175,176,177,178,179,180,181,182,183,184,185,186,187,188,189,190,191,192,193,194,195,196,197,198,199,200,201,202,203,204,205,206,207,208,209,210,211,212,213,214,215,216,217,218,219,220,221,222,223,224,225,226,227,228,229,230,231,232,233,234,235,236,237,238,239,240,241,242,243,244,245,246,247,248,249,250,251,252,253,254,255,128,129,130,131,132,133,134,135,136,137,138,139,140,141,142,143,144,145,146,147,148,149,150,151,152,153,154,155,156,157,158,159,160,161,162,163,164,165,166,167,168,169,170,171,172,173,174,175,176,177,178,179,180,181,182,183,184,185,186,187,188,189,190,191,192,193,194,195,196,197,198,199,200,201,202,203,204,205,206,207,208,209,210,211,212,213,214,215,216,217,218,219,220,221,222,223,224,225,226,227,228,229,230,231,232,233,234,235,236,237,238,239,240,241,242,243,244,245,246,247,248,249,250,251,252,253,254,255,128,129,130,131,132,133,134,135,136,137,138,139,140,141,142,143,144,145,146,147,148,149,150,151,152,153,154,155,156,157,158,159,160,161,162,163,164,165,166,167,168,169,170,171,172,173,174,175,176,177,178,179,180,181,182,183,184,185,186,187,188,189,190,191,192,193,194,195,196,197,198,199,200,201,202,203,204,205,206,207,208,209,210,211,212,213,214,215,216,217,218,219,220,221,222,223,224,225,226,227,228,229,230,231,232,233,234,235,236,237,238,239,240,241,242,243,244,245,246,247,248,249,250,251,252,253,254,255,0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63,64,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,84,85,86,87,88,89,90,91,92,93,94,95,96,97,98,99,100,101,102,103,104,105,106,107,108,109,110,111,112,113,114,115,116,117,118,119,120,121,122,123,124,125,126,127,143,144,145,146,147,148,149,150,151,152,153,154,155,156,157,158,159,160,161,162,163,164,165,166,167,168,169,170,171,172,173,174,175,176,177,178,179,180,181,182,183,184,185,186,187,188,189,190,191,192,193,194,195,196,197,198,199,200,201,202,203,204,205,206,207,208,209,210,211,212,213,214,215,216,217,218,219,220,221,222,223,224,225,226,227,228,229,230,231,232,233,234,235,236,237,238,239,240,241,242,243,244,245,246,247,248,249,250,251,252,253,254,255,42,128,129,130,131,132,133,134,135,136,137,138,139,140,141,142,143,144,145,146,147,148,149,150,-1,152,153,154,155,156,157,158,159,160,161,162,163,164,165,166,167,168,169,170,171,172,173,174,175,176,177,178,179,180,181,182,-1,184,185,186,187,188,189,190,191,46,-1,48,49,50,51,52,53,54,55,56,57,-1,-1,-1,-1,-1,-1,-1,-1,-1,128,129,130,131,132,133,134,135,136,137,138,139,140,141,142,143,144,145,146,147,148,149,150,151,152,153,154,155,156,157,158,159,160,161,162,163,164,165,166,167,168,169,170,171,172,173,174,175,176,177,178,179,180,181,182,183,184,185,186,187,188,189,190,191,192,193,194,195,196,197,198,199,200,201,202,203,204,205,206,207,208,209,210,211,212,213,214,215,216,217,218,219,220,221,222,223,224,225,226,227,228,229,230,231,232,233,234,235,236,237,238,239,240,241,242,243,244,245,246,247,248,249,250,251,252,253,254,255,0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63,64,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,84,85,86,87,88,89,90,91,92,93,94,95,96,97,98,99,100,101,102,103,104,105,106,107,108,109,110,111,112,113,114,115,116,117,118,119,120,121,122,123,124,125,126,127,192,193,194,195,196,197,198,199,200,201,202,203,204,205,206,207,208,209,210,211,212,213,214,215,216,217,218,219,220,221,222,223,224,225,226,227,228,229,230,231,232,233,234,235,236,237,238,239,240,241,242,243,244,245,246,247,248,249,250,251,252,253,254,255,10,48,49,50,51,52,53,54,55,56,57,48,49,50,51,52,53,54,55,56,57,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,101,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,128,129,130,131,132,133,134,135,136,137,138,139,140,141,142,143,144,145,146,147,148,149,150,151,152,153,154,155,156,157,158,159,160,161,162,163,164,165,166,167,168,169,170,171,172,173,174,175,176,177,178,179,180,181,182,183,184,185,186,187,188,189,190,191,192,193,194,195,196,197,198,199,200,201,202,203,204,205,206,207,208,209,210,211,212,213,214,215,216,217,218,219,220,221,222,223,224,225,226,227,228,229,230,231,232,233,234,235,236,237,238,239,240,241,242,243,244,245,246,247,248,249,250,251,252,253,254,255,42,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,128,129,130,131,132,133,134,135,136,137,138,139,140,141,142,143,144,145,146,147,148,149,150,151,152,153,154,155,156,157,158,159,160,161,162,163,164,165,166,167,168,169,170,171,172,173,174,175,176,177,178,179,180,181,182,183,184,185,186,187,188,189,190,191,192,193,194,195,196,197,198,199,200,201,202,203,204,205,206,207,208,209,210,211,212,213,214,215,216,217,218,219,220,221,222,223,224,225,226,227,228,229,230,231,232,233,234,235,236,237,238,239,240,241,242,243,244,245,246,247,248,249,250,251,252,253,254,255,39,-1,-1,-1,-1,-1,-1,-1,-1,48,49,50,51,52,53,54,55,56,57,-1,-1,-1,-1,-1,-1,-1,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,84,85,86,87,88,89,90,-1,-1,-1,-1,95,-1,97,98,99,100,101,102,103,104,105,106,107,108,109,110,111,112,113,114,115,116,117,118,119,120,121,122,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,195,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1]

alex_deflt :: Array Int Int
alex_deflt = listArray (0,29) [-1,-1,10,10,-1,-1,10,10,11,11,10,19,19,-1,-1,-1,-1,20,20,20,20,-1,10,-1,-1,-1,-1,-1,-1,-1]

alex_accept = listArray (0::Int,29) [AlexAccNone,AlexAccNone,AlexAccNone,AlexAccNone,AlexAccNone,AlexAccNone,AlexAccNone,AlexAccNone,AlexAccNone,AlexAccNone,AlexAccNone,AlexAccNone,AlexAccNone,AlexAccNone,AlexAccNone,AlexAccNone,AlexAccNone,AlexAccNone,AlexAccNone,AlexAccNone,AlexAccSkip,AlexAccSkip,AlexAccSkip,AlexAccSkip,AlexAcc (alex_action_3),AlexAcc (alex_action_3),AlexAcc (alex_action_4),AlexAcc (alex_action_5),AlexAcc (alex_action_6),AlexAcc (alex_action_6)]


tok f p s = f p s

share :: S.Seq Char -> S.Seq Char
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

tokenPos (PT (Pn _ l _) _ :_) = "line " ++ show l
tokenPos (Err (Pn _ l _) :_) = "line " ++ show l
tokenPos _ = "end of file"

tokenPosn (PT p _) = p
tokenPosn (Err p) = p
tokenLineCol = posLineCol . tokenPosn
posLineCol (Pn _ l c) = (l,c)
mkPosToken t@(PT p _) = (posLineCol p, prToken t)

prToken t = case t of
  PT _ (TS s _) -> s
  PT _ (TL s)   -> s
  PT _ (TI s)   -> s
  PT _ (TV s)   -> s
  PT _ (TD s)   -> s
  PT _ (TC s)   -> s

data BTree = N | B (S.Seq Char) Tok BTree BTree deriving (Show)

eitherResIdent :: (S.Seq Char -> Tok) -> S.Seq Char -> Tok
eitherResIdent tv s = treeFind resWords
  where
  treeFind N = tv s
  treeFind (B a t left right) | s < a  = treeFind left
                              | s > a  = treeFind right
                              | s == a = t

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
-- A modified "posn" wrapper.
-------------------------------------------------------------------

data Posn = Pn !Int !Int !Int
      deriving (Eq, Show,Ord)

alexStartPos :: Posn
alexStartPos = Pn 0 1 1

alexMove :: Posn -> Char -> Posn
alexMove (Pn a l c) '\t' = Pn (a+1)  l     (((c+7) `div` 8)*8+1)
alexMove (Pn a l c) '\n' = Pn (a+1) (l+1)   1
alexMove (Pn a l c) _    = Pn (a+1)  l     (c+1)

  -- | Encode a Haskell String to a list of Word8 values, in UTF8 format.
utf8Encode :: Char -> [Word8]
utf8Encode = map fromIntegral . go . ord
 where
  go oc
   | oc <= 0x7f       = [oc]

   | oc <= 0x7ff      = [ 0xc0 + (oc `Data.Bits.shiftR` 6)
                        , 0x80 + oc Data.Bits..&. 0x3f
                        ]

   | oc <= 0xffff     = [ 0xe0 + (oc `Data.Bits.shiftR` 12)
                        , 0x80 + ((oc `Data.Bits.shiftR` 6) Data.Bits..&. 0x3f)
                        , 0x80 + oc Data.Bits..&. 0x3f
                        ]
   | otherwise        = [ 0xf0 + (oc `Data.Bits.shiftR` 18)
                        , 0x80 + ((oc `Data.Bits.shiftR` 12) Data.Bits..&. 0x3f)
                        , 0x80 + ((oc `Data.Bits.shiftR` 6) Data.Bits..&. 0x3f)
                        , 0x80 + oc Data.Bits..&. 0x3f
                        ]

newtype RangedTokens v = Ranged { unRange :: (Table State (Tokens v), Range) }
-- TODO: inline combineTokens and have the range in there
instance Measured v IntToken => Monoid (RangedTokens v) where
    mempty = Ranged (tabulate stateRange (const emptyTokens), R id id id)
    rt0 `mappend` rt1 = 
        let t2 = tabulate stateRange $ combineTokens (access t0) (access t1)
        in Ranged (t2, r2)
      where (t0,r0) = unRange rt0
            (t1,r1) = unRange rt1
            r2 = r0 <> r1
    

-- Generic template
type State = Int
type Transition v = State -> (Tokens v) -- Transition from in state to Tokens
-- Wrapper template?
data Tokens v = NoTokens
            | InvalidTokens !(S.Seq Char) !Range
            | Tokens { currentSeq  :: !(FingerTree v IntToken)
                      , lastToken  :: !(Suffix v)
                      , outState   :: !State}
-- The suffix is the the sequence of as long as possible accepting tokens.
-- It can itself contain a suffix for the last token.
--This is either a Sequence of tokens or one token if the it hits an accepting state with later characters
-- Generic template
data Suffix v = Str !(S.Seq Char)
              | One !IntToken
              | Multi !(Tokens v)
type Size     = Sum Int
--Wrapper
type LexTree v = FingerTree (Table State (Tokens v),Size) Char
data IntToken  = Token { lexeme   :: !(S.Seq Char)
                       , token_id :: Accepts
                       , range :: Range}
--Wrapper template
type Accepts   = AlexAcc (Posn -> S.Seq Char -> Token) ()
type Table a b = Array State b

tabulate :: (State,State) -> (State -> b) -> Table State b
tabulate range f = listArray range [f i | i <- [fst range..snd range]]

access :: Table State b -> (State -> b)
access a x = a ! x
-- Generic template
makeTree :: Measured v IntToken => String -> LexTree v
makeTree str = fromList str

data Range = R (Int -> Int) (Int -> Int) (Int -> Int)
runRange :: Range -> Int -> Int -> Int -> (Int,Int,Int)
runRange (R lf cf tf) l c t = (lf l, cf c, tf t)

toRange :: Char -> Range
toRange '\n' = R (+1) (const 0) (+1)
toRange c    = R id (+1) (+1)

instance Monoid Range where
    mempty = R id id id
    (R l1 c1 t1) `mappend` (R l2 c2 t2) = R (l2 . l1) (c2 . c1) (t2 . t1)

-- Generic template
instance (Measured v IntToken) => Monoid (Table State (Tokens v)) where
  mempty = tabulate stateRange (\_ -> emptyTokens)
  f `mappend` g = tabulate stateRange $ combineTokens (access f) (access g)

-- Wrapper template
-- The base case for when one character is lexed.
instance (Measured v IntToken) => Measured (Table State (Tokens v),Size) Char where
  measure c =
    let bytes = utf8Encode c
        sing = singleton c
        cSeq = S.singleton c
        baseCase in_state | in_state == -1 = InvalidTokens cSeq (foldMap toRange cSeq)
                          | otherwise = case foldl automata in_state bytes of
          -1 -> InvalidTokens cSeq (foldMap toRange cSeq)
          os -> case alex_accept ! os of
            AlexAccNone -> Tokens empty (Str cSeq) os
            acc -> Tokens empty (One (createToken cSeq acc)) os
    in (tabulate stateRange $ baseCase, Sum 1)

-- TODO: add range info if unacceptable?
intToks :: Measured v IntToken => FingerTree v IntToken -> Suffix v -> FingerTree v IntToken
intToks seq (Str str) = error $ "Unacceptable token: " ++ toList str
intToks seq (One token) = seq |> token
intToks seq (Multi (Tokens seq' suff' _)) = intToks (seq <> seq') suff'

-- TODO: add range info if unnacceptable?
stateToTree :: Measured v IntToken => (Table State (Tokens v),Size) -> FingerTree v IntToken
stateToTree (tab,size) = case access tab startState of
    InvalidTokens s r        -> error $ "Unacceptable token: " ++ toList s ++ " at " ++ (show $ runRange r 0 0 0)
    NoTokens                 -> empty
    Tokens seq suff outstate -> intToks seq suff

createToken :: S.Seq Char -> Accepts -> IntToken
createToken lex acc = Token lex acc (foldMap toRange lex)

createTokens :: Measured v IntToken => FingerTree v IntToken -> Suffix v -> State -> Tokens v
createTokens seq suf state = if null seq
                             then NoTokens
                             else Tokens seq suf state
-- Wrapper template
invalidTokens :: S.Seq Char -> Range -> Tokens v
invalidTokens s r = InvalidTokens s r

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
combineWithRHS toks1 trans2 
    | isEmpty toks2 = toks1
    | isValid toks2 = let toks2' = mergeTokens (lastToken toks1) toks2 trans2
                      in appendTokens seq1 toks2'
    | otherwise     = case lastToken toks1 of
        Multi suffToks ->
          let toks2' = combineWithRHS suffToks trans2 -- try to merge suffix
          in appendTokens seq1 toks2'
        One tok -> appendTokens (seq1 |> tok) (trans2 startState)
        Str s -> invalidTokens s (foldMap toRange s)
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
    AlexAccNone -> toks2 {lastToken = mergeSuff suff1 (lastToken toks2) trans2}
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
isInvalid (InvalidTokens _ _) = True
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

-- Not used, therefore undefined
alexGetByte :: AlexInput -> Maybe (Byte,AlexInput)
alexGetByte (p, c, (b:bs), s) = Just (b, (p, c, bs, s))
alexGetByte (p, _, [], s) = undefined

alexInputPrevChar :: AlexInput -> Char
alexInputPrevChar (p, c, bs, s) = c

-- Not used, but needed for Alex templates
type Byte = Word8
type AlexInput = (Posn,     -- current position,
                  Char,     -- previous char
                  [Byte],   -- pending bytes on the current char
                  S.Seq Char)   -- current input string


alex_action_3 =  tok (\p s -> PT p (eitherResIdent (TV . share) s)) 
alex_action_4 =  tok (\p s -> PT p (eitherResIdent (TV . share) s)) 
alex_action_5 =  tok (\p s -> PT p (TI $ share s))    
alex_action_6 =  tok (\p s -> PT p (TD $ share s)) 

-- -----------------------------------------------------------------------------
-- ALEX TEMPLATE
--
-- This code is in the PUBLIC DOMAIN; you may copy it freely and use
-- it for any purpose whatsoever.

-- -----------------------------------------------------------------------------
-- INTERNALS and main scanner engine

alexIndexInt16OffAddr arr off = arr ! off
alexIndexInt32OffAddr arr off = arr ! off
quickIndex arr i = arr ! i

-- -----------------------------------------------------------------------------
-- Main lexing routines

data AlexReturn a
  = AlexEOF
  | AlexError  !AlexInput
  | AlexSkip   !AlexInput !Int
  | AlexToken  !AlexInput !Int a

-- alexScan :: AlexInput -> StartCode -> AlexReturn a
alexScan input (sc)
  = alexScanUser undefined input (sc)

alexScanUser user input (sc)
  = case alex_scan_tkn user input (0) input sc AlexNone of
	(AlexNone, input') ->
		case alexGetByte input of
			Nothing -> AlexEOF
			Just _ -> AlexError input'
	(AlexLastSkip input'' len, _) -> AlexSkip input'' len
	(AlexLastAcc k input''' len, _) -> AlexToken input''' len k

-- Push the input through the DFA, remembering the most recent accepting
-- state it encountered.

alex_scan_tkn user orig_input len input s last_acc =
  input `seq` -- strict in the input
  let 
	new_acc = (check_accs (alex_accept `quickIndex` (s)))
  in
  new_acc `seq`
  case alexGetByte input of
     Nothing -> (new_acc, input)
     Just (c, new_input) -> 
      case fromIntegral c of { (ord_c) ->
        let
                base   = alexIndexInt32OffAddr alex_base s
                offset = (base + ord_c)
                check  = alexIndexInt16OffAddr alex_check offset
		
                new_s = if (offset >= (0)) && (check == ord_c)
			  then alexIndexInt16OffAddr alex_table offset
			  else alexIndexInt16OffAddr alex_deflt s
	in
        case new_s of
	    (-1) -> (new_acc, input)
		-- on an error, we want to keep the input *before* the
		-- character that failed, not after.
    	    _ -> alex_scan_tkn user orig_input (if c < 0x80 || c >= 0xC0 then (len + (1)) else len)
                                                -- note that the length is increased ONLY if this is the 1st byte in a char encoding)
			new_input new_s new_acc
      }
  where
	check_accs (AlexAccNone) = last_acc
	check_accs (AlexAcc a  ) = AlexLastAcc a input (len)
	check_accs (AlexAccSkip) = AlexLastSkip  input (len)

	check_accs (AlexAccPred a predx rest)
	   | predx user orig_input (len) input
	   = AlexLastAcc a input (len)
	   | otherwise
	   = check_accs rest
	check_accs (AlexAccSkipPred predx rest)
	   | predx user orig_input (len) input
	   = AlexLastSkip input (len)
	   | otherwise
	   = check_accs rest


data AlexLastAcc a
  = AlexNone
  | AlexLastAcc a !AlexInput !Int
  | AlexLastSkip  !AlexInput !Int

instance Functor AlexLastAcc where
    fmap f AlexNone = AlexNone
    fmap f (AlexLastAcc x y z) = AlexLastAcc (f x) y z
    fmap f (AlexLastSkip x y) = AlexLastSkip x y

data AlexAcc a user
  = AlexAccNone
  | AlexAcc a
  | AlexAccSkip

  | AlexAccPred a   (AlexAccPred user) (AlexAcc a user)
  | AlexAccSkipPred (AlexAccPred user) (AlexAcc a user)

type AlexAccPred user = user -> AlexInput -> Int -> AlexInput -> Bool

-- -----------------------------------------------------------------------------
-- Predicates on a rule

alexAndPred p1 p2 user in1 len in2
  = p1 user in1 len in2 && p2 user in1 len in2

--alexPrevCharIsPred :: Char -> AlexAccPred _ 
alexPrevCharIs c _ input _ _ = c == alexInputPrevChar input

alexPrevCharMatches f _ input _ _ = f (alexInputPrevChar input)

--alexPrevCharIsOneOfPred :: Array Char Bool -> AlexAccPred _ 
alexPrevCharIsOneOf arr _ input _ _ = arr ! alexInputPrevChar input

--alexRightContext :: Int -> AlexAccPred _
alexRightContext (sc) user _ _ input = 
     case alex_scan_tkn user input (0) input sc AlexNone of
	  (AlexNone, _) -> False
	  _ -> True
	-- TODO: there's no need to find the longest
	-- match when checking the right context, just
	-- the first match will do.


-- used by wrappers
iUnbox (i) = i
