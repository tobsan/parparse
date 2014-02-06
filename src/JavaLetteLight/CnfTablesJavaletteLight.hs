{-# LANGUAGE MagicHash, FlexibleInstances #-}
module CnfTablesJavaletteLight where
import GHC.Prim
import GHC.Exts
import Control.Applicative hiding (Const)
import Algebra.RingUtils
import Parsing.Chart ()
import AbsJavaletteLight
import LexJavaletteLight
import PrintJavaletteLight
readInteger :: String -> Integer
readInteger = read
readDouble :: String -> Double
readDouble = read
instance RingP [(CATEGORY,Any)] where
  mul p a b = trav [map (app tx ty) l :/: map (app tx ty) r | (x,tx) <- a, (y,ty) <- b, let l:/:r = combine p x y]
    where trav :: [Pair [a]] -> Pair [a]
          trav [] = pure []
          trav (x:xs) = (++) <$> x <*> trav xs
          app tx ty (z,f)  = (z, f tx ty)
showAst (cat,ast) = case cat of 
      CAT_Prog -> printTree ((unsafeCoerce# ast)::Prog)
      CAT_Stm -> printTree ((unsafeCoerce# ast)::Stm)
      CAT_Exp -> printTree ((unsafeCoerce# ast)::Exp)
      CAT_Typ -> printTree ((unsafeCoerce# ast)::Typ)
      _ -> "Unprintable category"
data CATEGORY = CAT_Prog|
                CAT_0|
                CAT_1|
                CAT_2|
                CAT_3|
                CAT_4|
                CAT_Stm|
                CAT_5|
                CAT_6|
                CAT_7|
                CAT_8|
                CAT_9|
                CAT_10|
                CAT_11|
                CAT_12|
                CAT_13|
                CAT_Exp|
                CAT_14|
                CAT_Exp1|
                CAT_15|
                CAT_Exp2|
                CAT_16|
                CAT_Exp3|
                CAT_Stm_List|
                CAT_17|
                CAT_Typ|
                CAT_Ident|
                CAT_Integer|
                CAT_Double|
                TOK_40|
                TOK_41|
                TOK_42|
                TOK_43|
                TOK_4343|
                TOK_59|
                TOK_60|
                TOK_61|
                TOK_double|
                TOK_int|
                TOK_while|
                TOK_OPEN_|
                TOK_CLOS_
  deriving (Eq,Ord,Show)
describe CAT_Prog = "Prog"
describe CAT_0 = "6-prefix of Fun (Typ Ident '(' ')' '{' [Stm])"
describe CAT_1 = "5-prefix of Fun (Typ Ident '(' ')' '{')"
describe CAT_2 = "4-prefix of Fun (Typ Ident '(' ')')"
describe CAT_3 = "3-prefix of Fun (Typ Ident '(')"
describe CAT_4 = "2-prefix of Fun (Typ Ident)"
describe CAT_Stm = "Stm"
describe CAT_5 = "2-prefix of SDecl (Typ Ident)"
describe CAT_6 = "3-prefix of SAss (Ident '=' Exp)"
describe CAT_7 = "2-prefix of SAss (Ident '=')"
describe CAT_8 = "2-prefix of SIncr (Ident '++')"
describe CAT_9 = "6-prefix of SWhile ('while' '(' Exp ')' '{' [Stm])"
describe CAT_10 = "5-prefix of SWhile ('while' '(' Exp ')' '{')"
describe CAT_11 = "4-prefix of SWhile ('while' '(' Exp ')')"
describe CAT_12 = "3-prefix of SWhile ('while' '(' Exp)"
describe CAT_13 = "2-prefix of SWhile ('while' '(')"
describe CAT_Exp = "Exp"
describe CAT_14 = "2-prefix of ELt (Exp1 '<')"
describe CAT_Exp1 = "Exp1"
describe CAT_15 = "2-prefix of EPlus (Exp1 '+')"
describe CAT_Exp2 = "Exp2"
describe CAT_16 = "2-prefix of ETimes (Exp2 '*')"
describe CAT_Exp3 = "Exp3"
describe CAT_Stm_List = "[Stm]"
describe CAT_17 = "2-prefix of id ('(' Exp)"
describe CAT_Typ = "Typ"
describe CAT_Ident = "Ident"
describe CAT_Integer = "Integer"
describe CAT_Double = "Double"
describe TOK_40 = "token ("
describe TOK_41 = "token )"
describe TOK_42 = "token *"
describe TOK_43 = "token +"
describe TOK_4343 = "token ++"
describe TOK_59 = "token ;"
describe TOK_60 = "token <"
describe TOK_61 = "token ="
describe TOK_double = "token double"
describe TOK_int = "token int"
describe TOK_while = "token while"
describe TOK_OPEN_ = "token {"
describe TOK_CLOS_ = "token }"
neighbors CAT_0 = [TOK_CLOS_]
neighbors CAT_1 = [CAT_10, 
                   CAT_11, 
                   CAT_12, 
                   CAT_13, 
                   CAT_5, 
                   CAT_6, 
                   CAT_7, 
                   CAT_8, 
                   CAT_9, 
                   CAT_Ident, 
                   CAT_Stm, 
                   CAT_Typ, 
                   CAT_Stm_List, 
                   TOK_double, 
                   TOK_int, 
                   TOK_while, 
                   TOK_CLOS_]
neighbors CAT_10 = [CAT_10, 
                    CAT_11, 
                    CAT_12, 
                    CAT_13, 
                    CAT_5, 
                    CAT_6, 
                    CAT_7, 
                    CAT_8, 
                    CAT_9, 
                    CAT_Ident, 
                    CAT_Stm, 
                    CAT_Typ, 
                    CAT_Stm_List, 
                    TOK_double, 
                    TOK_int, 
                    TOK_while, 
                    TOK_CLOS_]
neighbors CAT_11 = [TOK_OPEN_]
neighbors CAT_12 = [TOK_41]
neighbors CAT_13 = [CAT_14, 
                    CAT_15, 
                    CAT_16, 
                    CAT_17, 
                    CAT_Double, 
                    CAT_Exp, 
                    CAT_Exp1, 
                    CAT_Exp2, 
                    CAT_Exp3, 
                    CAT_Ident, 
                    CAT_Integer, 
                    TOK_40]
neighbors CAT_14 = [CAT_15, 
                    CAT_16, 
                    CAT_17, 
                    CAT_Double, 
                    CAT_Exp1, 
                    CAT_Exp2, 
                    CAT_Exp3, 
                    CAT_Ident, 
                    CAT_Integer, 
                    TOK_40]
neighbors CAT_15 = [CAT_16, 
                    CAT_17, 
                    CAT_Double, 
                    CAT_Exp2, 
                    CAT_Exp3, 
                    CAT_Ident, 
                    CAT_Integer, 
                    TOK_40]
neighbors CAT_16 = [CAT_17, 
                    CAT_Double, 
                    CAT_Exp3, 
                    CAT_Ident, 
                    CAT_Integer, 
                    TOK_40]
neighbors CAT_17 = [TOK_41]
neighbors CAT_2 = [TOK_OPEN_]
neighbors CAT_3 = [TOK_41]
neighbors CAT_4 = [TOK_40]
neighbors CAT_5 = [TOK_59]
neighbors CAT_6 = [TOK_59]
neighbors CAT_7 = [CAT_14, 
                   CAT_15, 
                   CAT_16, 
                   CAT_17, 
                   CAT_Double, 
                   CAT_Exp, 
                   CAT_Exp1, 
                   CAT_Exp2, 
                   CAT_Exp3, 
                   CAT_Ident, 
                   CAT_Integer, 
                   TOK_40]
neighbors CAT_8 = [TOK_59]
neighbors CAT_9 = [TOK_CLOS_]
neighbors CAT_Double = [TOK_41, TOK_42, TOK_43, TOK_59, TOK_60]
neighbors CAT_Exp = [TOK_41, TOK_59]
neighbors CAT_Exp1 = [TOK_41, TOK_43, TOK_59, TOK_60]
neighbors CAT_Exp2 = [TOK_41, TOK_42, TOK_43, TOK_59, TOK_60]
neighbors CAT_Exp3 = [TOK_41, TOK_42, TOK_43, TOK_59, TOK_60]
neighbors CAT_Ident = [TOK_40, 
                       TOK_41, 
                       TOK_42, 
                       TOK_43, 
                       TOK_4343, 
                       TOK_59, 
                       TOK_60, 
                       TOK_61]
neighbors CAT_Integer = [TOK_41, TOK_42, TOK_43, TOK_59, TOK_60]
neighbors CAT_Stm = [CAT_10, 
                     CAT_11, 
                     CAT_12, 
                     CAT_13, 
                     CAT_5, 
                     CAT_6, 
                     CAT_7, 
                     CAT_8, 
                     CAT_9, 
                     CAT_Ident, 
                     CAT_Stm, 
                     CAT_Typ, 
                     CAT_Stm_List, 
                     TOK_59, 
                     TOK_double, 
                     TOK_int, 
                     TOK_while, 
                     TOK_CLOS_]
neighbors CAT_Typ = [CAT_Ident]
neighbors CAT_Stm_List = [TOK_CLOS_]
neighbors TOK_40 = [CAT_14, 
                    CAT_15, 
                    CAT_16, 
                    CAT_17, 
                    CAT_Double, 
                    CAT_Exp, 
                    CAT_Exp1, 
                    CAT_Exp2, 
                    CAT_Exp3, 
                    CAT_Ident, 
                    CAT_Integer, 
                    TOK_40, 
                    TOK_41]
neighbors TOK_41 = [TOK_41, 
                    TOK_42, 
                    TOK_43, 
                    TOK_59, 
                    TOK_60, 
                    TOK_OPEN_]
neighbors TOK_42 = [CAT_17, 
                    CAT_Double, 
                    CAT_Exp3, 
                    CAT_Ident, 
                    CAT_Integer, 
                    TOK_40]
neighbors TOK_43 = [CAT_16, 
                    CAT_17, 
                    CAT_Double, 
                    CAT_Exp2, 
                    CAT_Exp3, 
                    CAT_Ident, 
                    CAT_Integer, 
                    TOK_40]
neighbors TOK_4343 = [TOK_59]
neighbors TOK_59 = [CAT_10, 
                    CAT_11, 
                    CAT_12, 
                    CAT_13, 
                    CAT_5, 
                    CAT_6, 
                    CAT_7, 
                    CAT_8, 
                    CAT_9, 
                    CAT_Ident, 
                    CAT_Stm, 
                    CAT_Typ, 
                    CAT_Stm_List, 
                    TOK_59, 
                    TOK_double, 
                    TOK_int, 
                    TOK_while, 
                    TOK_CLOS_]
neighbors TOK_60 = [CAT_15, 
                    CAT_16, 
                    CAT_17, 
                    CAT_Double, 
                    CAT_Exp1, 
                    CAT_Exp2, 
                    CAT_Exp3, 
                    CAT_Ident, 
                    CAT_Integer, 
                    TOK_40]
neighbors TOK_61 = [CAT_14, 
                    CAT_15, 
                    CAT_16, 
                    CAT_17, 
                    CAT_Double, 
                    CAT_Exp, 
                    CAT_Exp1, 
                    CAT_Exp2, 
                    CAT_Exp3, 
                    CAT_Ident, 
                    CAT_Integer, 
                    TOK_40]
neighbors TOK_double = [CAT_Ident]
neighbors TOK_int = [CAT_Ident]
neighbors TOK_while = [TOK_40]
neighbors TOK_OPEN_ = [CAT_10, 
                       CAT_11, 
                       CAT_12, 
                       CAT_13, 
                       CAT_5, 
                       CAT_6, 
                       CAT_7, 
                       CAT_8, 
                       CAT_9, 
                       CAT_Ident, 
                       CAT_Stm, 
                       CAT_Typ, 
                       CAT_Stm_List, 
                       TOK_double, 
                       TOK_int, 
                       TOK_while, 
                       TOK_CLOS_]
neighbors TOK_CLOS_ = [CAT_10, 
                       CAT_11, 
                       CAT_12, 
                       CAT_13, 
                       CAT_5, 
                       CAT_6, 
                       CAT_7, 
                       CAT_8, 
                       CAT_9, 
                       CAT_Ident, 
                       CAT_Stm, 
                       CAT_Typ, 
                       CAT_Stm_List, 
                       TOK_59, 
                       TOK_double, 
                       TOK_int, 
                       TOK_while, 
                       TOK_CLOS_]
neighbors _ = []
combine :: Bool -> CATEGORY -> CATEGORY -> Pair [(CATEGORY, Any -> Any -> Any)]
combine p CAT_0 TOK_CLOS_ = (((CAT_Prog, \x y -> unsafeCoerce# (unsafeCoerce# (x))):)$[])
                            :/:
                            ([])
combine p CAT_1 CAT_Stm_List = (((CAT_0, \x y -> unsafeCoerce# (unsafeCoerce# (x) (unsafeCoerce# (y)))):)$[])
                               :/:
                               ([])
combine p CAT_10 CAT_Stm_List = (((CAT_9, \x y -> unsafeCoerce# (unsafeCoerce# (x) (unsafeCoerce# (y)))):)$[])
                                :/:
                                ([])
combine p CAT_11 TOK_OPEN_ = (((CAT_10, \x y -> unsafeCoerce# (unsafeCoerce# (x))):)$
                              ((CAT_9, \x y -> unsafeCoerce# (unsafeCoerce# (x) ([]))):)$[])
                             :/:
                             ([])
combine p CAT_12 TOK_41 = (((CAT_11, \x y -> unsafeCoerce# (unsafeCoerce# (x))):)$[])
                          :/:
                          ([])
combine p CAT_13 CAT_Exp = (((CAT_12, \x y -> unsafeCoerce# (unsafeCoerce# (x) (unsafeCoerce# (y)))):)$[])
                           :/:
                           ([])
combine p CAT_14 CAT_Exp1 = (((CAT_Exp, \x y -> unsafeCoerce# (unsafeCoerce# (x) (unsafeCoerce# (y)))):)$[])
                            :/:
                            (((CAT_Exp, \x y -> unsafeCoerce# (unsafeCoerce# (x) (unsafeCoerce# (y)))):)$[])
combine p CAT_15 CAT_Exp2 = (((CAT_Exp1, \x y -> unsafeCoerce# (unsafeCoerce# (x) (unsafeCoerce# (y)))):)$
                             ((CAT_Exp, \x y -> unsafeCoerce# (unsafeCoerce# (x) (unsafeCoerce# (y)))):)$[])
                            :/:
                            (((CAT_Exp1, \x y -> unsafeCoerce# (unsafeCoerce# (x) (unsafeCoerce# (y)))):)$
                             ((CAT_Exp, \x y -> unsafeCoerce# (unsafeCoerce# (x) (unsafeCoerce# (y)))):)$[])
combine p CAT_16 CAT_Exp3 = (((CAT_Exp2, \x y -> unsafeCoerce# (unsafeCoerce# (x) (unsafeCoerce# (y)))):)$
                             ((CAT_Exp1, \x y -> unsafeCoerce# (unsafeCoerce# (x) (unsafeCoerce# (y)))):)$
                             ((CAT_Exp, \x y -> unsafeCoerce# (unsafeCoerce# (x) (unsafeCoerce# (y)))):)$[])
                            :/:
                            (((CAT_Exp2, \x y -> unsafeCoerce# (unsafeCoerce# (x) (unsafeCoerce# (y)))):)$
                             ((CAT_Exp1, \x y -> unsafeCoerce# (unsafeCoerce# (x) (unsafeCoerce# (y)))):)$
                             ((CAT_Exp, \x y -> unsafeCoerce# (unsafeCoerce# (x) (unsafeCoerce# (y)))):)$[])
combine p CAT_17 TOK_41 = (((CAT_Exp2, \x y -> unsafeCoerce# (unsafeCoerce# (x))):)$
                           ((CAT_Exp1, \x y -> unsafeCoerce# (unsafeCoerce# (x))):)$
                           ((CAT_Exp, \x y -> unsafeCoerce# (unsafeCoerce# (x))):)$[])
                          :/:
                          (((CAT_Exp3, \x y -> unsafeCoerce# (unsafeCoerce# (x))):)$
                           ((CAT_Exp2, \x y -> unsafeCoerce# (unsafeCoerce# (x))):)$
                           ((CAT_Exp1, \x y -> unsafeCoerce# (unsafeCoerce# (x))):)$
                           ((CAT_Exp, \x y -> unsafeCoerce# (unsafeCoerce# (x))):)$[])
combine p CAT_2 TOK_OPEN_ = (((CAT_1, \x y -> unsafeCoerce# (unsafeCoerce# (x))):)$
                             ((CAT_0, \x y -> unsafeCoerce# (unsafeCoerce# (x) ([]))):)$[])
                            :/:
                            ([])
combine p CAT_3 TOK_41 = (((CAT_2, \x y -> unsafeCoerce# (unsafeCoerce# (x))):)$[])
                         :/:
                         ([])
combine p CAT_4 TOK_40 = (((CAT_3, \x y -> unsafeCoerce# (unsafeCoerce# (x))):)$[])
                         :/:
                         ([])
combine p CAT_5 TOK_59 = (((CAT_Stm, \x y -> unsafeCoerce# (unsafeCoerce# (x))):)$[])
                         :/:
                         (((CAT_Stm_List, \x y -> unsafeCoerce# ((:) (unsafeCoerce# (x)) ([]))):)$[])
combine p CAT_6 TOK_59 = (((CAT_Stm, \x y -> unsafeCoerce# (unsafeCoerce# (x))):)$[])
                         :/:
                         (((CAT_Stm_List, \x y -> unsafeCoerce# ((:) (unsafeCoerce# (x)) ([]))):)$[])
combine p CAT_7 CAT_Exp = (((CAT_6, \x y -> unsafeCoerce# (unsafeCoerce# (x) (unsafeCoerce# (y)))):)$[])
                          :/:
                          ([])
combine p CAT_8 TOK_59 = (((CAT_Stm, \x y -> unsafeCoerce# (unsafeCoerce# (x))):)$[])
                         :/:
                         (((CAT_Stm_List, \x y -> unsafeCoerce# ((:) (unsafeCoerce# (x)) ([]))):)$[])
combine p CAT_9 TOK_CLOS_ = (((CAT_Stm, \x y -> unsafeCoerce# (unsafeCoerce# (x))):)$[])
                            :/:
                            (((CAT_Stm_List, \x y -> unsafeCoerce# ((:) (unsafeCoerce# (x)) ([]))):)$[])
combine p CAT_Exp1 TOK_43 = (((CAT_15, \x y -> unsafeCoerce# (EPlus (unsafeCoerce# (x)))):)$[])
                            :/:
                            ([])
combine p CAT_Exp1 TOK_60 = (((CAT_14, \x y -> unsafeCoerce# (ELt (unsafeCoerce# (x)))):)$[])
                            :/:
                            ([])
combine p CAT_Exp2 TOK_42 = (((CAT_16, \x y -> unsafeCoerce# (ETimes (unsafeCoerce# (x)))):)$[])
                            :/:
                            ([])
combine p CAT_Ident TOK_4343 = (((CAT_8, \x y -> unsafeCoerce# (SIncr (unsafeCoerce# (x)))):)$[])
                               :/:
                               ([])
combine p CAT_Ident TOK_61 = (((CAT_7, \x y -> unsafeCoerce# (SAss (unsafeCoerce# (x)))):)$[])
                             :/:
                             ([])
combine p CAT_Stm CAT_Stm_List = ([])
                                 :/:
                                 (((CAT_Stm_List, \x y -> unsafeCoerce# ((:) (unsafeCoerce# (x)) (unsafeCoerce# (y)))):)$[])
combine p CAT_Stm TOK_59 = (((CAT_Stm, \x y -> unsafeCoerce# (unsafeCoerce# (x))):)$[])
                           :/:
                           (((CAT_Stm_List, \x y -> unsafeCoerce# ((:) (unsafeCoerce# (x)) ([]))):)$[])
combine p CAT_Typ CAT_Ident = (((CAT_4, \x y -> unsafeCoerce# (Fun (unsafeCoerce# (x)) (unsafeCoerce# (y)))):)$
                               ((CAT_5, \x y -> unsafeCoerce# (SDecl (unsafeCoerce# (x)) (unsafeCoerce# (y)))):)$[])
                              :/:
                              ([])
combine p TOK_40 CAT_Exp = (((CAT_17, \x y -> unsafeCoerce# (unsafeCoerce# (y))):)$[])
                           :/:
                           ([])
combine p TOK_while TOK_40 = (((CAT_13, \x y -> unsafeCoerce# (SWhile)):)$[])
                             :/:
                             ([])
combine _ _ _ = pure []
tokenToCats :: Bool -> Token -> Pair [(CATEGORY,Any)]
tokenToCats p (PT (Pn _ l c) (TC x)) = ([]) :/: ([])
tokenToCats p (PT (Pn _ l c) (TL x)) = ([]) :/: ([])
tokenToCats p (PT (Pn _ l c) (TI x)) = (((CAT_Exp2, unsafeCoerce# (EInt (readInteger (x)))):)$
                                        ((CAT_Exp1, unsafeCoerce# (EInt (readInteger (x)))):)$
                                        ((CAT_Exp, unsafeCoerce# (EInt (readInteger (x)))):)$[])
                                       :/:
                                       (((CAT_Exp3, unsafeCoerce# (EInt (readInteger (x)))):)$
                                        ((CAT_Exp2, unsafeCoerce# (EInt (readInteger (x)))):)$
                                        ((CAT_Exp1, unsafeCoerce# (EInt (readInteger (x)))):)$
                                        ((CAT_Exp, unsafeCoerce# (EInt (readInteger (x)))):)$[])
tokenToCats p (PT (Pn _ l c) (TD x)) = (((CAT_Exp2, unsafeCoerce# (EDouble (readDouble (x)))):)$
                                        ((CAT_Exp1, unsafeCoerce# (EDouble (readDouble (x)))):)$
                                        ((CAT_Exp, unsafeCoerce# (EDouble (readDouble (x)))):)$[])
                                       :/:
                                       (((CAT_Exp3, unsafeCoerce# (EDouble (readDouble (x)))):)$
                                        ((CAT_Exp2, unsafeCoerce# (EDouble (readDouble (x)))):)$
                                        ((CAT_Exp1, unsafeCoerce# (EDouble (readDouble (x)))):)$
                                        ((CAT_Exp, unsafeCoerce# (EDouble (readDouble (x)))):)$[])
tokenToCats p (PT (Pn _ l c) (TV x)) = (((CAT_Ident, unsafeCoerce# (Ident (x))):)$
                                        ((CAT_Exp2, unsafeCoerce# (EVar (Ident (x)))):)$
                                        ((CAT_Exp1, unsafeCoerce# (EVar (Ident (x)))):)$
                                        ((CAT_Exp, unsafeCoerce# (EVar (Ident (x)))):)$[])
                                       :/:
                                       (((CAT_Ident, unsafeCoerce# (Ident (x))):)$
                                        ((CAT_Exp3, unsafeCoerce# (EVar (Ident (x)))):)$
                                        ((CAT_Exp2, unsafeCoerce# (EVar (Ident (x)))):)$
                                        ((CAT_Exp1, unsafeCoerce# (EVar (Ident (x)))):)$
                                        ((CAT_Exp, unsafeCoerce# (EVar (Ident (x)))):)$[])
 -- (
tokenToCats p (PT posn (TS _ 1)) = (((TOK_40, error"cannot access value of token: ("):)$[])
                                   :/:
                                   (((TOK_40, error"cannot access value of token: ("):)$[])
 -- )
tokenToCats p (PT posn (TS _ 2)) = ([])
                                   :/:
                                   (((TOK_41, error"cannot access value of token: )"):)$[])
 -- *
tokenToCats p (PT posn (TS _ 3)) = ([])
                                   :/:
                                   (((TOK_42, error"cannot access value of token: *"):)$[])
 -- +
tokenToCats p (PT posn (TS _ 4)) = ([])
                                   :/:
                                   (((TOK_43, error"cannot access value of token: +"):)$[])
 -- ++
tokenToCats p (PT posn (TS _ 5)) = ([])
                                   :/:
                                   (((TOK_4343, error"cannot access value of token: ++"):)$[])
 -- ;
tokenToCats p (PT posn (TS _ 6)) = ([])
                                   :/:
                                   (((TOK_59, error"cannot access value of token: ;"):)$[])
 -- <
tokenToCats p (PT posn (TS _ 7)) = ([])
                                   :/:
                                   (((TOK_60, error"cannot access value of token: <"):)$[])
 -- =
tokenToCats p (PT posn (TS _ 8)) = ([])
                                   :/:
                                   (((TOK_61, error"cannot access value of token: ="):)$[])
 -- double
tokenToCats p (PT posn (TS _ 9)) = (((CAT_Typ, unsafeCoerce# (TDouble)):)$[])
                                   :/:
                                   ([])
 -- int
tokenToCats p (PT posn (TS _ 10)) = (((CAT_Typ, unsafeCoerce# (TInt)):)$[])
                                    :/:
                                    ([])
 -- while
tokenToCats p (PT posn (TS _ 11)) = (((TOK_while, error"cannot access value of token: while"):)$[])
                                    :/:
                                    ([])
 -- {
tokenToCats p (PT posn (TS _ 12)) = ([])
                                    :/:
                                    (((TOK_OPEN_, error"cannot access value of token: {"):)$[])
 -- }
tokenToCats p (PT posn (TS _ 13)) = ([])
                                    :/:
                                    (((TOK_CLOS_, error"cannot access value of token: }"):)$[])
tokenToCats p t = error ("unknown token: " ++ show t)
{-Normalised grammar:
  ($). Prog ::= "0" }
($). 0 ::= "1" "[Stm]"
flip (($)) ([]). 0 ::= "1"
($). 1 ::= "2" {
($). 2 ::= "3" )
($). 3 ::= "4" (
Fun. 4 ::= "Typ" "Ident"
($). Stm ::= "5" ;
SDecl. 5 ::= "Typ" "Ident"
($). Stm ::= "6" ;
($). 6 ::= "7" "Exp"
SAss. 7 ::= "Ident" =
($). Stm ::= "8" ;
SIncr. 8 ::= "Ident" ++
($). Stm ::= "9" }
($). 9 ::= "10" "[Stm]"
flip (($)) ([]). 9 ::= "10"
($). 10 ::= "11" {
($). 11 ::= "12" )
($). 12 ::= "13" "Exp"
SWhile. 13 ::= while (
($). Exp ::= "14" "Exp1"
ELt. 14 ::= "Exp1" <
($). Exp1 ::= "15" "Exp2"
EPlus. 15 ::= "Exp1" +
($). Exp2 ::= "16" "Exp3"
ETimes. 16 ::= "Exp2" *
EVar. Exp3 ::= "Ident"
EInt. Exp3 ::= "Integer"
EDouble. Exp3 ::= "Double"
(:). [Stm] ::= "Stm" "[Stm]"
flip ((:)) ([]). [Stm] ::= "Stm"
id. Stm ::= "Stm" ;
id. Exp ::= "Exp1"
id. Exp1 ::= "Exp2"
id. Exp2 ::= "Exp3"
($). Exp3 ::= "17" )
id. 17 ::= ( "Exp"
TInt. Typ ::= int
TDouble. Typ ::= double

  Unit relation:
  flip (($)) ([]) : CAT_1 --> 0
  flip (($)) ([]) : CAT_10 --> 9
  EDouble : CAT_Double --> Exp3
  EDouble : CAT_Double --> Exp2
  EDouble : CAT_Double --> Exp1
  EDouble : CAT_Double --> Exp
  id : CAT_Exp1 --> Exp
  id : CAT_Exp2 --> Exp1
  id : CAT_Exp2 --> Exp
  id : CAT_Exp3 --> Exp2
  id : CAT_Exp3 --> Exp1
  id : CAT_Exp3 --> Exp
  EVar : CAT_Ident --> Exp3
  EVar : CAT_Ident --> Exp2
  EVar : CAT_Ident --> Exp1
  EVar : CAT_Ident --> Exp
  EInt : CAT_Integer --> Exp3
  EInt : CAT_Integer --> Exp2
  EInt : CAT_Integer --> Exp1
  EInt : CAT_Integer --> Exp
  flip ((:)) ([]) : CAT_Stm --> [Stm]
  TDouble : TOK_double --> Typ
  TInt : TOK_int --> Typ-}