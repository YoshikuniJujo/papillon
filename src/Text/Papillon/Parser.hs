{-# LANGUAGE FlexibleContexts, TemplateHaskell, UndecidableInstances, FlexibleContexts, PackageImports, TypeFamilies, RankNTypes #-}
module  Text.Papillon.Parser (
	Peg,
	Definition,
	Selection,
	ExpressionHs,
	NameLeaf(..),
	NameLeaf_(..),
	ReadFrom(..),
	parse,
	initialPos,
	showNameLeaf,
	nameFromRF,
	ParseError(..),
	Derivs(peg, pegFile, derivsChars),
	Pos(..),
	ListPos(..)
)  where
import "monads-tf" Control.Monad.State
import "monads-tf" Control.Monad.Error
import Control.Monad.Trans.Error (Error (..))

import Control.Applicative



import Data.Char
import Language.Haskell.TH
import Text.Papillon.SyntaxTree

data ParseError pos drv
    = ParseError {peCode :: String,
                  peMessage :: String,
                  peComment :: String,
                  peDerivs :: drv,
                  peReading :: ([String]),
                  pePosition :: pos}
instance Error (ParseError pos drv)
    where strMsg msg = ParseError "" msg "" undefined undefined undefined
data Derivs
    = Derivs {pegFile :: (Either (ParseError (Pos String) Derivs)
                                 ((PegFile, Derivs))),
              pragma :: (Either (ParseError (Pos String) Derivs)
                                ((Maybe String, Derivs))),
              pragmaStr :: (Either (ParseError (Pos String) Derivs)
                                   ((String, Derivs))),
              pragmaItems :: (Either (ParseError (Pos String) Derivs)
                                     (([String], Derivs))),
              delPragmas :: (Either (ParseError (Pos String) Derivs)
                                    (((), Derivs))),
              pragmaEnd :: (Either (ParseError (Pos String) Derivs)
                                   (((), Derivs))),
              moduleDec :: (Either (ParseError (Pos String) Derivs)
                                   ((Maybe String, Derivs))),
              moduleDecStr :: (Either (ParseError (Pos String) Derivs)
                                      ((String, Derivs))),
              whr :: (Either (ParseError (Pos String) Derivs) (((), Derivs))),
              preImpPap :: (Either (ParseError (Pos String) Derivs)
                                   ((String, Derivs))),
              prePeg :: (Either (ParseError (Pos String) Derivs)
                                ((String, Derivs))),
              afterPeg :: (Either (ParseError (Pos String) Derivs)
                                  ((String, Derivs))),
              importPapillon :: (Either (ParseError (Pos String) Derivs)
                                        (((), Derivs))),
              varToken :: (Either (ParseError (Pos String) Derivs)
                                  ((String, Derivs))),
              typToken :: (Either (ParseError (Pos String) Derivs)
                                  ((String, Derivs))),
              pap :: (Either (ParseError (Pos String) Derivs) (((), Derivs))),
              peg :: (Either (ParseError (Pos String) Derivs) ((TTPeg, Derivs))),
              sourceType :: (Either (ParseError (Pos String) Derivs)
                                    ((String, Derivs))),
              peg_ :: (Either (ParseError (Pos String) Derivs) ((Peg, Derivs))),
              definition :: (Either (ParseError (Pos String) Derivs)
                                    ((Definition, Derivs))),
              selection :: (Either (ParseError (Pos String) Derivs)
                                   ((Selection, Derivs))),
              expressionHs :: (Either (ParseError (Pos String) Derivs)
                                      ((ExpressionHs, Derivs))),
              expression :: (Either (ParseError (Pos String) Derivs)
                                    ((Expression, Derivs))),
              nameLeaf_ :: (Either (ParseError (Pos String) Derivs)
                                   ((NameLeaf_, Derivs))),
              nameLeaf :: (Either (ParseError (Pos String) Derivs)
                                  ((NameLeaf, Derivs))),
              nameLeafNoCom :: (Either (ParseError (Pos String) Derivs)
                                       ((NameLeaf, Derivs))),
              comForErr :: (Either (ParseError (Pos String) Derivs)
                                   ((String, Derivs))),
              leaf :: (Either (ParseError (Pos String) Derivs)
                              (((ReadFrom, Maybe ((ExpQ, String))), Derivs))),
              patOp :: (Either (ParseError (Pos String) Derivs)
                               ((PatQ, Derivs))),
              pat :: (Either (ParseError (Pos String) Derivs) ((PatQ, Derivs))),
              pat1 :: (Either (ParseError (Pos String) Derivs) ((PatQ, Derivs))),
              patList :: (Either (ParseError (Pos String) Derivs)
                                 (([PatQ], Derivs))),
              opConName :: (Either (ParseError (Pos String) Derivs)
                                   ((Name, Derivs))),
              charLit :: (Either (ParseError (Pos String) Derivs)
                                 ((Char, Derivs))),
              stringLit :: (Either (ParseError (Pos String) Derivs)
                                   ((String, Derivs))),
              escapeC :: (Either (ParseError (Pos String) Derivs)
                                 ((Char, Derivs))),
              pats :: (Either (ParseError (Pos String) Derivs)
                              ((PatQs, Derivs))),
              readFromLs :: (Either (ParseError (Pos String) Derivs)
                                    ((ReadFrom, Derivs))),
              readFrom :: (Either (ParseError (Pos String) Derivs)
                                  ((ReadFrom, Derivs))),
              test :: (Either (ParseError (Pos String) Derivs)
                              (((ExR, String), Derivs))),
              hsExpLam :: (Either (ParseError (Pos String) Derivs)
                                  ((ExR, Derivs))),
              hsExpTyp :: (Either (ParseError (Pos String) Derivs)
                                  ((ExR, Derivs))),
              hsExpOp :: (Either (ParseError (Pos String) Derivs)
                                 ((ExR, Derivs))),
              hsOp :: (Either (ParseError (Pos String) Derivs) ((ExR, Derivs))),
              opTail :: (Either (ParseError (Pos String) Derivs)
                                ((String, Derivs))),
              hsExp :: (Either (ParseError (Pos String) Derivs) ((Ex, Derivs))),
              hsExp1 :: (Either (ParseError (Pos String) Derivs)
                                ((ExR, Derivs))),
              hsExpTpl :: (Either (ParseError (Pos String) Derivs)
                                  ((ExRL, Derivs))),
              hsTypeArr :: (Either (ParseError (Pos String) Derivs)
                                   ((TypeQ, Derivs))),
              hsType :: (Either (ParseError (Pos String) Derivs)
                                ((Typ, Derivs))),
              hsType1 :: (Either (ParseError (Pos String) Derivs)
                                 ((TypeQ, Derivs))),
              hsTypeTpl :: (Either (ParseError (Pos String) Derivs)
                                   ((TypeQL, Derivs))),
              typ :: (Either (ParseError (Pos String) Derivs)
                             ((String, Derivs))),
              variable :: (Either (ParseError (Pos String) Derivs)
                                  ((String, Derivs))),
              tvtail :: (Either (ParseError (Pos String) Derivs)
                                ((String, Derivs))),
              integer :: (Either (ParseError (Pos String) Derivs)
                                 ((Integer, Derivs))),
              alpha :: (Either (ParseError (Pos String) Derivs)
                               ((Char, Derivs))),
              upper :: (Either (ParseError (Pos String) Derivs)
                               ((Char, Derivs))),
              lower :: (Either (ParseError (Pos String) Derivs)
                               ((Char, Derivs))),
              digit :: (Either (ParseError (Pos String) Derivs)
                               ((Char, Derivs))),
              spaces :: (Either (ParseError (Pos String) Derivs) (((), Derivs))),
              space :: (Either (ParseError (Pos String) Derivs) (((), Derivs))),
              notNLString :: (Either (ParseError (Pos String) Derivs)
                                     ((String, Derivs))),
              newLine :: (Either (ParseError (Pos String) Derivs)
                                 (((), Derivs))),
              comment :: (Either (ParseError (Pos String) Derivs)
                                 (((), Derivs))),
              comments :: (Either (ParseError (Pos String) Derivs)
                                  (((), Derivs))),
              notComStr :: (Either (ParseError (Pos String) Derivs)
                                   (((), Derivs))),
              comEnd :: (Either (ParseError (Pos String) Derivs) (((), Derivs))),
              derivsChars :: (Either (ParseError (Pos String) Derivs)
                                     ((Token String, Derivs))),
              derivsPosition :: (Pos String)}
parse :: String -> Derivs
parse = parse0_0 initialPos
          where parse0_0 pos s = d
                             where d = Derivs pegFile72_1 pragma73_2 pragmaStr74_3 pragmaItems75_4 delPragmas76_5 pragmaEnd77_6 moduleDec78_7 moduleDecStr79_8 whr80_9 preImpPap81_10 prePeg82_11 afterPeg83_12 importPapillon84_13 varToken85_14 typToken86_15 pap87_16 peg88_17 sourceType89_18 peg_90_19 definition91_20 selection92_21 expressionHs93_22 expression94_23 nameLeaf_95_24 nameLeaf96_25 nameLeafNoCom97_26 comForErr98_27 leaf99_28 patOp100_29 pat101_30 pat1102_31 patList103_32 opConName104_33 charLit105_34 stringLit106_35 escapeC107_36 pats108_37 readFromLs109_38 readFrom110_39 test111_40 hsExpLam112_41 hsExpTyp113_42 hsExpOp114_43 hsOp115_44 opTail116_45 hsExp117_46 hsExp1118_47 hsExpTpl119_48 hsTypeArr120_49 hsType121_50 hsType1122_51 hsTypeTpl123_52 typ124_53 variable125_54 tvtail126_55 integer127_56 alpha128_57 upper129_58 lower130_59 digit131_60 spaces132_61 space133_62 notNLString134_63 newLine135_64 comment136_65 comments137_66 notComStr138_67 comEnd139_68 chars140_69 pos
                                   pegFile72_1 = runStateT pegFile4_70 d
                                   pragma73_2 = runStateT pragma5_71 d
                                   pragmaStr74_3 = runStateT pragmaStr6_72 d
                                   pragmaItems75_4 = runStateT pragmaItems7_73 d
                                   delPragmas76_5 = runStateT delPragmas8_74 d
                                   pragmaEnd77_6 = runStateT pragmaEnd9_75 d
                                   moduleDec78_7 = runStateT moduleDec10_76 d
                                   moduleDecStr79_8 = runStateT moduleDecStr11_77 d
                                   whr80_9 = runStateT whr12_78 d
                                   preImpPap81_10 = runStateT preImpPap13_79 d
                                   prePeg82_11 = runStateT prePeg14_80 d
                                   afterPeg83_12 = runStateT afterPeg15_81 d
                                   importPapillon84_13 = runStateT importPapillon16_82 d
                                   varToken85_14 = runStateT varToken17_83 d
                                   typToken86_15 = runStateT typToken18_84 d
                                   pap87_16 = runStateT pap19_85 d
                                   peg88_17 = runStateT peg20_86 d
                                   sourceType89_18 = runStateT sourceType21_87 d
                                   peg_90_19 = runStateT peg_22_88 d
                                   definition91_20 = runStateT definition23_89 d
                                   selection92_21 = runStateT selection24_90 d
                                   expressionHs93_22 = runStateT expressionHs25_91 d
                                   expression94_23 = runStateT expression26_92 d
                                   nameLeaf_95_24 = runStateT nameLeaf_27_93 d
                                   nameLeaf96_25 = runStateT nameLeaf28_94 d
                                   nameLeafNoCom97_26 = runStateT nameLeafNoCom29_95 d
                                   comForErr98_27 = runStateT comForErr30_96 d
                                   leaf99_28 = runStateT leaf31_97 d
                                   patOp100_29 = runStateT patOp32_98 d
                                   pat101_30 = runStateT pat33_99 d
                                   pat1102_31 = runStateT pat134_100 d
                                   patList103_32 = runStateT patList35_101 d
                                   opConName104_33 = runStateT opConName36_102 d
                                   charLit105_34 = runStateT charLit37_103 d
                                   stringLit106_35 = runStateT stringLit38_104 d
                                   escapeC107_36 = runStateT escapeC39_105 d
                                   pats108_37 = runStateT pats40_106 d
                                   readFromLs109_38 = runStateT readFromLs41_107 d
                                   readFrom110_39 = runStateT readFrom42_108 d
                                   test111_40 = runStateT test43_109 d
                                   hsExpLam112_41 = runStateT hsExpLam44_110 d
                                   hsExpTyp113_42 = runStateT hsExpTyp45_111 d
                                   hsExpOp114_43 = runStateT hsExpOp46_112 d
                                   hsOp115_44 = runStateT hsOp47_113 d
                                   opTail116_45 = runStateT opTail48_114 d
                                   hsExp117_46 = runStateT hsExp49_115 d
                                   hsExp1118_47 = runStateT hsExp150_116 d
                                   hsExpTpl119_48 = runStateT hsExpTpl51_117 d
                                   hsTypeArr120_49 = runStateT hsTypeArr52_118 d
                                   hsType121_50 = runStateT hsType53_119 d
                                   hsType1122_51 = runStateT hsType154_120 d
                                   hsTypeTpl123_52 = runStateT hsTypeTpl55_121 d
                                   typ124_53 = runStateT typ56_122 d
                                   variable125_54 = runStateT variable57_123 d
                                   tvtail126_55 = runStateT tvtail58_124 d
                                   integer127_56 = runStateT integer59_125 d
                                   alpha128_57 = runStateT alpha60_126 d
                                   upper129_58 = runStateT upper61_127 d
                                   lower130_59 = runStateT lower62_128 d
                                   digit131_60 = runStateT digit63_129 d
                                   spaces132_61 = runStateT spaces64_130 d
                                   space133_62 = runStateT space65_131 d
                                   notNLString134_63 = runStateT notNLString66_132 d
                                   newLine135_64 = runStateT newLine67_133 d
                                   comment136_65 = runStateT comment68_134 d
                                   comments137_66 = runStateT comments69_135 d
                                   notComStr138_67 = runStateT notComStr70_136 d
                                   comEnd139_68 = runStateT comEnd71_137 d
                                   chars140_69 = runStateT (case getToken s of
                                                                Just (c,
                                                                      s') -> do put (parse0_0 (updatePos c pos) s')
                                                                                return c
                                                                _ -> gets derivsPosition >>= (throwError . ParseError "" "end of input" "" undefined [])) d
                pegFile4_70 = foldl1 mplus [do pr <- StateT pragma
                                               md <- StateT moduleDec
                                               pip <- StateT preImpPap
                                               _ <- StateT importPapillon
                                               return ()
                                               pp <- StateT prePeg
                                               _ <- StateT pap
                                               return ()
                                               p <- StateT peg
                                               _ <- StateT spaces
                                               return ()
                                               d158_138 <- get
                                               xx157_139 <- StateT derivsChars
                                               case xx157_139 of
                                                   '|' -> return ()
                                                   _ -> gets derivsPosition >>= (throwError . ParseError "'|'" "not match pattern: " "" d158_138 ["dvChars"])
                                               let '|' = xx157_139
                                               return ()
                                               d160_140 <- get
                                               xx159_141 <- StateT derivsChars
                                               case xx159_141 of
                                                   ']' -> return ()
                                                   _ -> gets derivsPosition >>= (throwError . ParseError "']'" "not match pattern: " "" d160_140 ["dvChars"])
                                               let ']' = xx159_141
                                               return ()
                                               d162_142 <- get
                                               xx161_143 <- StateT derivsChars
                                               case xx161_143 of
                                                   '\n' -> return ()
                                                   _ -> gets derivsPosition >>= (throwError . ParseError "'\\n'" "not match pattern: " "" d162_142 ["dvChars"])
                                               let '\n' = xx161_143
                                               return ()
                                               atp <- StateT afterPeg
                                               return (mkPegFile pr md pip pp p atp),
                                            do pr <- StateT pragma
                                               md <- StateT moduleDec
                                               pp <- StateT prePeg
                                               _ <- StateT pap
                                               return ()
                                               p <- StateT peg
                                               _ <- StateT spaces
                                               return ()
                                               d178_144 <- get
                                               xx177_145 <- StateT derivsChars
                                               case xx177_145 of
                                                   '|' -> return ()
                                                   _ -> gets derivsPosition >>= (throwError . ParseError "'|'" "not match pattern: " "" d178_144 ["dvChars"])
                                               let '|' = xx177_145
                                               return ()
                                               d180_146 <- get
                                               xx179_147 <- StateT derivsChars
                                               case xx179_147 of
                                                   ']' -> return ()
                                                   _ -> gets derivsPosition >>= (throwError . ParseError "']'" "not match pattern: " "" d180_146 ["dvChars"])
                                               let ']' = xx179_147
                                               return ()
                                               d182_148 <- get
                                               xx181_149 <- StateT derivsChars
                                               case xx181_149 of
                                                   '\n' -> return ()
                                                   _ -> gets derivsPosition >>= (throwError . ParseError "'\\n'" "not match pattern: " "" d182_148 ["dvChars"])
                                               let '\n' = xx181_149
                                               return ()
                                               atp <- StateT afterPeg
                                               return (mkPegFile pr md emp pp p atp)]
                pragma5_71 = foldl1 mplus [do _ <- StateT spaces
                                              return ()
                                              d188_150 <- get
                                              xx187_151 <- StateT derivsChars
                                              case xx187_151 of
                                                  '{' -> return ()
                                                  _ -> gets derivsPosition >>= (throwError . ParseError "'{'" "not match pattern: " "" d188_150 ["dvChars"])
                                              let '{' = xx187_151
                                              return ()
                                              d190_152 <- get
                                              xx189_153 <- StateT derivsChars
                                              case xx189_153 of
                                                  '-' -> return ()
                                                  _ -> gets derivsPosition >>= (throwError . ParseError "'-'" "not match pattern: " "" d190_152 ["dvChars"])
                                              let '-' = xx189_153
                                              return ()
                                              d192_154 <- get
                                              xx191_155 <- StateT derivsChars
                                              case xx191_155 of
                                                  '#' -> return ()
                                                  _ -> gets derivsPosition >>= (throwError . ParseError "'#'" "not match pattern: " "" d192_154 ["dvChars"])
                                              let '#' = xx191_155
                                              return ()
                                              _ <- StateT spaces
                                              return ()
                                              d196_156 <- get
                                              xx195_157 <- StateT derivsChars
                                              case xx195_157 of
                                                  'L' -> return ()
                                                  _ -> gets derivsPosition >>= (throwError . ParseError "'L'" "not match pattern: " "" d196_156 ["dvChars"])
                                              let 'L' = xx195_157
                                              return ()
                                              d198_158 <- get
                                              xx197_159 <- StateT derivsChars
                                              case xx197_159 of
                                                  'A' -> return ()
                                                  _ -> gets derivsPosition >>= (throwError . ParseError "'A'" "not match pattern: " "" d198_158 ["dvChars"])
                                              let 'A' = xx197_159
                                              return ()
                                              d200_160 <- get
                                              xx199_161 <- StateT derivsChars
                                              case xx199_161 of
                                                  'N' -> return ()
                                                  _ -> gets derivsPosition >>= (throwError . ParseError "'N'" "not match pattern: " "" d200_160 ["dvChars"])
                                              let 'N' = xx199_161
                                              return ()
                                              d202_162 <- get
                                              xx201_163 <- StateT derivsChars
                                              case xx201_163 of
                                                  'G' -> return ()
                                                  _ -> gets derivsPosition >>= (throwError . ParseError "'G'" "not match pattern: " "" d202_162 ["dvChars"])
                                              let 'G' = xx201_163
                                              return ()
                                              d204_164 <- get
                                              xx203_165 <- StateT derivsChars
                                              case xx203_165 of
                                                  'U' -> return ()
                                                  _ -> gets derivsPosition >>= (throwError . ParseError "'U'" "not match pattern: " "" d204_164 ["dvChars"])
                                              let 'U' = xx203_165
                                              return ()
                                              d206_166 <- get
                                              xx205_167 <- StateT derivsChars
                                              case xx205_167 of
                                                  'A' -> return ()
                                                  _ -> gets derivsPosition >>= (throwError . ParseError "'A'" "not match pattern: " "" d206_166 ["dvChars"])
                                              let 'A' = xx205_167
                                              return ()
                                              d208_168 <- get
                                              xx207_169 <- StateT derivsChars
                                              case xx207_169 of
                                                  'G' -> return ()
                                                  _ -> gets derivsPosition >>= (throwError . ParseError "'G'" "not match pattern: " "" d208_168 ["dvChars"])
                                              let 'G' = xx207_169
                                              return ()
                                              d210_170 <- get
                                              xx209_171 <- StateT derivsChars
                                              case xx209_171 of
                                                  'E' -> return ()
                                                  _ -> gets derivsPosition >>= (throwError . ParseError "'E'" "not match pattern: " "" d210_170 ["dvChars"])
                                              let 'E' = xx209_171
                                              return ()
                                              _ <- StateT spaces
                                              return ()
                                              s <- StateT pragmaItems
                                              _ <- StateT pragmaEnd
                                              return ()
                                              _ <- StateT spaces
                                              return ()
                                              return (just $ " LANGUAGE " ++ concatMap (++ ", ") s),
                                           do _ <- StateT spaces
                                              return ()
                                              return nothing]
                pragmaStr6_72 = foldl1 mplus [do _ <- StateT delPragmas
                                                 return ()
                                                 _ <- StateT spaces
                                                 return ()
                                                 d226_172 <- get
                                                 xx225_173 <- StateT derivsChars
                                                 case xx225_173 of
                                                     ',' -> return ()
                                                     _ -> gets derivsPosition >>= (throwError . ParseError "','" "not match pattern: " "" d226_172 ["dvChars"])
                                                 let ',' = xx225_173
                                                 return ()
                                                 _ <- StateT spaces
                                                 return ()
                                                 s <- StateT pragmaStr
                                                 return (' ' : s),
                                              do ddd231_174 <- get
                                                 do err <- ((do _ <- StateT pragmaEnd
                                                                return ()) >> return False) `catchError` const (return True)
                                                    unless err (gets derivsPosition >>= (throwError . ParseError ('!' : "_:pragmaEnd") "not match: " "" ddd231_174 ["pragmaEnd"]))
                                                 put ddd231_174
                                                 ddd234_175 <- get
                                                 do err <- ((do _ <- StateT delPragmas
                                                                return ()) >> return False) `catchError` const (return True)
                                                    unless err (gets derivsPosition >>= (throwError . ParseError ('!' : "_:delPragmas") "not match: " "" ddd234_175 ["delPragmas"]))
                                                 put ddd234_175
                                                 c <- StateT derivsChars
                                                 s <- StateT pragmaStr
                                                 return (c : s),
                                              return emp]
                pragmaItems7_73 = foldl1 mplus [do _ <- StateT delPragmas
                                                   return ()
                                                   _ <- StateT spaces
                                                   return ()
                                                   d246_176 <- get
                                                   xx245_177 <- StateT derivsChars
                                                   case xx245_177 of
                                                       ',' -> return ()
                                                       _ -> gets derivsPosition >>= (throwError . ParseError "','" "not match pattern: " "" d246_176 ["dvChars"])
                                                   let ',' = xx245_177
                                                   return ()
                                                   _ <- StateT spaces
                                                   return ()
                                                   i <- StateT pragmaItems
                                                   return i,
                                                do t <- StateT typToken
                                                   d254_178 <- get
                                                   xx253_179 <- StateT derivsChars
                                                   case xx253_179 of
                                                       ',' -> return ()
                                                       _ -> gets derivsPosition >>= (throwError . ParseError "','" "not match pattern: " "" d254_178 ["dvChars"])
                                                   let ',' = xx253_179
                                                   return ()
                                                   _ <- StateT spaces
                                                   return ()
                                                   i <- StateT pragmaItems
                                                   return (t : i),
                                                do _ <- StateT delPragmas
                                                   return ()
                                                   _ <- StateT spaces
                                                   return ()
                                                   return [],
                                                do t <- StateT typToken
                                                   return [t]]
                delPragmas8_74 = foldl1 mplus [do d266_180 <- get
                                                  xx265_181 <- StateT derivsChars
                                                  case xx265_181 of
                                                      'Q' -> return ()
                                                      _ -> gets derivsPosition >>= (throwError . ParseError "'Q'" "not match pattern: " "" d266_180 ["dvChars"])
                                                  let 'Q' = xx265_181
                                                  return ()
                                                  d268_182 <- get
                                                  xx267_183 <- StateT derivsChars
                                                  case xx267_183 of
                                                      'u' -> return ()
                                                      _ -> gets derivsPosition >>= (throwError . ParseError "'u'" "not match pattern: " "" d268_182 ["dvChars"])
                                                  let 'u' = xx267_183
                                                  return ()
                                                  d270_184 <- get
                                                  xx269_185 <- StateT derivsChars
                                                  case xx269_185 of
                                                      'a' -> return ()
                                                      _ -> gets derivsPosition >>= (throwError . ParseError "'a'" "not match pattern: " "" d270_184 ["dvChars"])
                                                  let 'a' = xx269_185
                                                  return ()
                                                  d272_186 <- get
                                                  xx271_187 <- StateT derivsChars
                                                  case xx271_187 of
                                                      's' -> return ()
                                                      _ -> gets derivsPosition >>= (throwError . ParseError "'s'" "not match pattern: " "" d272_186 ["dvChars"])
                                                  let 's' = xx271_187
                                                  return ()
                                                  d274_188 <- get
                                                  xx273_189 <- StateT derivsChars
                                                  case xx273_189 of
                                                      'i' -> return ()
                                                      _ -> gets derivsPosition >>= (throwError . ParseError "'i'" "not match pattern: " "" d274_188 ["dvChars"])
                                                  let 'i' = xx273_189
                                                  return ()
                                                  d276_190 <- get
                                                  xx275_191 <- StateT derivsChars
                                                  case xx275_191 of
                                                      'Q' -> return ()
                                                      _ -> gets derivsPosition >>= (throwError . ParseError "'Q'" "not match pattern: " "" d276_190 ["dvChars"])
                                                  let 'Q' = xx275_191
                                                  return ()
                                                  d278_192 <- get
                                                  xx277_193 <- StateT derivsChars
                                                  case xx277_193 of
                                                      'u' -> return ()
                                                      _ -> gets derivsPosition >>= (throwError . ParseError "'u'" "not match pattern: " "" d278_192 ["dvChars"])
                                                  let 'u' = xx277_193
                                                  return ()
                                                  d280_194 <- get
                                                  xx279_195 <- StateT derivsChars
                                                  case xx279_195 of
                                                      'o' -> return ()
                                                      _ -> gets derivsPosition >>= (throwError . ParseError "'o'" "not match pattern: " "" d280_194 ["dvChars"])
                                                  let 'o' = xx279_195
                                                  return ()
                                                  d282_196 <- get
                                                  xx281_197 <- StateT derivsChars
                                                  case xx281_197 of
                                                      't' -> return ()
                                                      _ -> gets derivsPosition >>= (throwError . ParseError "'t'" "not match pattern: " "" d282_196 ["dvChars"])
                                                  let 't' = xx281_197
                                                  return ()
                                                  d284_198 <- get
                                                  xx283_199 <- StateT derivsChars
                                                  case xx283_199 of
                                                      'e' -> return ()
                                                      _ -> gets derivsPosition >>= (throwError . ParseError "'e'" "not match pattern: " "" d284_198 ["dvChars"])
                                                  let 'e' = xx283_199
                                                  return ()
                                                  d286_200 <- get
                                                  xx285_201 <- StateT derivsChars
                                                  case xx285_201 of
                                                      's' -> return ()
                                                      _ -> gets derivsPosition >>= (throwError . ParseError "'s'" "not match pattern: " "" d286_200 ["dvChars"])
                                                  let 's' = xx285_201
                                                  return ()
                                                  return (),
                                               do d288_202 <- get
                                                  xx287_203 <- StateT derivsChars
                                                  case xx287_203 of
                                                      'T' -> return ()
                                                      _ -> gets derivsPosition >>= (throwError . ParseError "'T'" "not match pattern: " "" d288_202 ["dvChars"])
                                                  let 'T' = xx287_203
                                                  return ()
                                                  d290_204 <- get
                                                  xx289_205 <- StateT derivsChars
                                                  case xx289_205 of
                                                      'y' -> return ()
                                                      _ -> gets derivsPosition >>= (throwError . ParseError "'y'" "not match pattern: " "" d290_204 ["dvChars"])
                                                  let 'y' = xx289_205
                                                  return ()
                                                  d292_206 <- get
                                                  xx291_207 <- StateT derivsChars
                                                  case xx291_207 of
                                                      'p' -> return ()
                                                      _ -> gets derivsPosition >>= (throwError . ParseError "'p'" "not match pattern: " "" d292_206 ["dvChars"])
                                                  let 'p' = xx291_207
                                                  return ()
                                                  d294_208 <- get
                                                  xx293_209 <- StateT derivsChars
                                                  case xx293_209 of
                                                      'e' -> return ()
                                                      _ -> gets derivsPosition >>= (throwError . ParseError "'e'" "not match pattern: " "" d294_208 ["dvChars"])
                                                  let 'e' = xx293_209
                                                  return ()
                                                  d296_210 <- get
                                                  xx295_211 <- StateT derivsChars
                                                  case xx295_211 of
                                                      'F' -> return ()
                                                      _ -> gets derivsPosition >>= (throwError . ParseError "'F'" "not match pattern: " "" d296_210 ["dvChars"])
                                                  let 'F' = xx295_211
                                                  return ()
                                                  d298_212 <- get
                                                  xx297_213 <- StateT derivsChars
                                                  case xx297_213 of
                                                      'a' -> return ()
                                                      _ -> gets derivsPosition >>= (throwError . ParseError "'a'" "not match pattern: " "" d298_212 ["dvChars"])
                                                  let 'a' = xx297_213
                                                  return ()
                                                  d300_214 <- get
                                                  xx299_215 <- StateT derivsChars
                                                  case xx299_215 of
                                                      'm' -> return ()
                                                      _ -> gets derivsPosition >>= (throwError . ParseError "'m'" "not match pattern: " "" d300_214 ["dvChars"])
                                                  let 'm' = xx299_215
                                                  return ()
                                                  d302_216 <- get
                                                  xx301_217 <- StateT derivsChars
                                                  case xx301_217 of
                                                      'i' -> return ()
                                                      _ -> gets derivsPosition >>= (throwError . ParseError "'i'" "not match pattern: " "" d302_216 ["dvChars"])
                                                  let 'i' = xx301_217
                                                  return ()
                                                  d304_218 <- get
                                                  xx303_219 <- StateT derivsChars
                                                  case xx303_219 of
                                                      'l' -> return ()
                                                      _ -> gets derivsPosition >>= (throwError . ParseError "'l'" "not match pattern: " "" d304_218 ["dvChars"])
                                                  let 'l' = xx303_219
                                                  return ()
                                                  d306_220 <- get
                                                  xx305_221 <- StateT derivsChars
                                                  case xx305_221 of
                                                      'i' -> return ()
                                                      _ -> gets derivsPosition >>= (throwError . ParseError "'i'" "not match pattern: " "" d306_220 ["dvChars"])
                                                  let 'i' = xx305_221
                                                  return ()
                                                  d308_222 <- get
                                                  xx307_223 <- StateT derivsChars
                                                  case xx307_223 of
                                                      'e' -> return ()
                                                      _ -> gets derivsPosition >>= (throwError . ParseError "'e'" "not match pattern: " "" d308_222 ["dvChars"])
                                                  let 'e' = xx307_223
                                                  return ()
                                                  d310_224 <- get
                                                  xx309_225 <- StateT derivsChars
                                                  case xx309_225 of
                                                      's' -> return ()
                                                      _ -> gets derivsPosition >>= (throwError . ParseError "'s'" "not match pattern: " "" d310_224 ["dvChars"])
                                                  let 's' = xx309_225
                                                  return ()
                                                  return ()]
                pragmaEnd9_75 = foldl1 mplus [do _ <- StateT delPragmas
                                                 return ()
                                                 _ <- StateT spaces
                                                 return ()
                                                 d316_226 <- get
                                                 xx315_227 <- StateT derivsChars
                                                 case xx315_227 of
                                                     '#' -> return ()
                                                     _ -> gets derivsPosition >>= (throwError . ParseError "'#'" "not match pattern: " "" d316_226 ["dvChars"])
                                                 let '#' = xx315_227
                                                 return ()
                                                 d318_228 <- get
                                                 xx317_229 <- StateT derivsChars
                                                 case xx317_229 of
                                                     '-' -> return ()
                                                     _ -> gets derivsPosition >>= (throwError . ParseError "'-'" "not match pattern: " "" d318_228 ["dvChars"])
                                                 let '-' = xx317_229
                                                 return ()
                                                 d320_230 <- get
                                                 xx319_231 <- StateT derivsChars
                                                 case xx319_231 of
                                                     '}' -> return ()
                                                     _ -> gets derivsPosition >>= (throwError . ParseError "'}'" "not match pattern: " "" d320_230 ["dvChars"])
                                                 let '}' = xx319_231
                                                 return ()
                                                 return (),
                                              do d322_232 <- get
                                                 xx321_233 <- StateT derivsChars
                                                 case xx321_233 of
                                                     '#' -> return ()
                                                     _ -> gets derivsPosition >>= (throwError . ParseError "'#'" "not match pattern: " "" d322_232 ["dvChars"])
                                                 let '#' = xx321_233
                                                 return ()
                                                 d324_234 <- get
                                                 xx323_235 <- StateT derivsChars
                                                 case xx323_235 of
                                                     '-' -> return ()
                                                     _ -> gets derivsPosition >>= (throwError . ParseError "'-'" "not match pattern: " "" d324_234 ["dvChars"])
                                                 let '-' = xx323_235
                                                 return ()
                                                 d326_236 <- get
                                                 xx325_237 <- StateT derivsChars
                                                 case xx325_237 of
                                                     '}' -> return ()
                                                     _ -> gets derivsPosition >>= (throwError . ParseError "'}'" "not match pattern: " "" d326_236 ["dvChars"])
                                                 let '}' = xx325_237
                                                 return ()
                                                 return ()]
                moduleDec10_76 = foldl1 mplus [do d328_238 <- get
                                                  xx327_239 <- StateT derivsChars
                                                  case xx327_239 of
                                                      'm' -> return ()
                                                      _ -> gets derivsPosition >>= (throwError . ParseError "'m'" "not match pattern: " "" d328_238 ["dvChars"])
                                                  let 'm' = xx327_239
                                                  return ()
                                                  d330_240 <- get
                                                  xx329_241 <- StateT derivsChars
                                                  case xx329_241 of
                                                      'o' -> return ()
                                                      _ -> gets derivsPosition >>= (throwError . ParseError "'o'" "not match pattern: " "" d330_240 ["dvChars"])
                                                  let 'o' = xx329_241
                                                  return ()
                                                  d332_242 <- get
                                                  xx331_243 <- StateT derivsChars
                                                  case xx331_243 of
                                                      'd' -> return ()
                                                      _ -> gets derivsPosition >>= (throwError . ParseError "'d'" "not match pattern: " "" d332_242 ["dvChars"])
                                                  let 'd' = xx331_243
                                                  return ()
                                                  d334_244 <- get
                                                  xx333_245 <- StateT derivsChars
                                                  case xx333_245 of
                                                      'u' -> return ()
                                                      _ -> gets derivsPosition >>= (throwError . ParseError "'u'" "not match pattern: " "" d334_244 ["dvChars"])
                                                  let 'u' = xx333_245
                                                  return ()
                                                  d336_246 <- get
                                                  xx335_247 <- StateT derivsChars
                                                  case xx335_247 of
                                                      'l' -> return ()
                                                      _ -> gets derivsPosition >>= (throwError . ParseError "'l'" "not match pattern: " "" d336_246 ["dvChars"])
                                                  let 'l' = xx335_247
                                                  return ()
                                                  d338_248 <- get
                                                  xx337_249 <- StateT derivsChars
                                                  case xx337_249 of
                                                      'e' -> return ()
                                                      _ -> gets derivsPosition >>= (throwError . ParseError "'e'" "not match pattern: " "" d338_248 ["dvChars"])
                                                  let 'e' = xx337_249
                                                  return ()
                                                  s <- StateT moduleDecStr
                                                  _ <- StateT whr
                                                  return ()
                                                  return (just s),
                                               return nothing]
                moduleDecStr11_77 = foldl1 mplus [do ddd343_250 <- get
                                                     do err <- ((do _ <- StateT whr
                                                                    return ()) >> return False) `catchError` const (return True)
                                                        unless err (gets derivsPosition >>= (throwError . ParseError ('!' : "_:whr") "not match: " "" ddd343_250 ["whr"]))
                                                     put ddd343_250
                                                     c <- StateT derivsChars
                                                     s <- StateT moduleDecStr
                                                     return (cons c s),
                                                  return emp]
                whr12_78 = foldl1 mplus [do d351_251 <- get
                                            xx350_252 <- StateT derivsChars
                                            case xx350_252 of
                                                'w' -> return ()
                                                _ -> gets derivsPosition >>= (throwError . ParseError "'w'" "not match pattern: " "" d351_251 ["dvChars"])
                                            let 'w' = xx350_252
                                            return ()
                                            d353_253 <- get
                                            xx352_254 <- StateT derivsChars
                                            case xx352_254 of
                                                'h' -> return ()
                                                _ -> gets derivsPosition >>= (throwError . ParseError "'h'" "not match pattern: " "" d353_253 ["dvChars"])
                                            let 'h' = xx352_254
                                            return ()
                                            d355_255 <- get
                                            xx354_256 <- StateT derivsChars
                                            case xx354_256 of
                                                'e' -> return ()
                                                _ -> gets derivsPosition >>= (throwError . ParseError "'e'" "not match pattern: " "" d355_255 ["dvChars"])
                                            let 'e' = xx354_256
                                            return ()
                                            d357_257 <- get
                                            xx356_258 <- StateT derivsChars
                                            case xx356_258 of
                                                'r' -> return ()
                                                _ -> gets derivsPosition >>= (throwError . ParseError "'r'" "not match pattern: " "" d357_257 ["dvChars"])
                                            let 'r' = xx356_258
                                            return ()
                                            d359_259 <- get
                                            xx358_260 <- StateT derivsChars
                                            case xx358_260 of
                                                'e' -> return ()
                                                _ -> gets derivsPosition >>= (throwError . ParseError "'e'" "not match pattern: " "" d359_259 ["dvChars"])
                                            let 'e' = xx358_260
                                            return ()
                                            return ()]
                preImpPap13_79 = foldl1 mplus [do ddd360_261 <- get
                                                  do err <- ((do _ <- StateT importPapillon
                                                                 return ()) >> return False) `catchError` const (return True)
                                                     unless err (gets derivsPosition >>= (throwError . ParseError ('!' : "_:importPapillon") "not match: " "" ddd360_261 ["importPapillon"]))
                                                  put ddd360_261
                                                  ddd363_262 <- get
                                                  do err <- ((do _ <- StateT pap
                                                                 return ()) >> return False) `catchError` const (return True)
                                                     unless err (gets derivsPosition >>= (throwError . ParseError ('!' : "_:pap") "not match: " "" ddd363_262 ["pap"]))
                                                  put ddd363_262
                                                  c <- StateT derivsChars
                                                  pip <- StateT preImpPap
                                                  return (cons c pip),
                                               return emp]
                prePeg14_80 = foldl1 mplus [do ddd370_263 <- get
                                               do err <- ((do _ <- StateT pap
                                                              return ()) >> return False) `catchError` const (return True)
                                                  unless err (gets derivsPosition >>= (throwError . ParseError ('!' : "_:pap") "not match: " "" ddd370_263 ["pap"]))
                                               put ddd370_263
                                               c <- StateT derivsChars
                                               pp <- StateT prePeg
                                               return (cons c pp),
                                            return emp]
                afterPeg15_81 = foldl1 mplus [do c <- StateT derivsChars
                                                 atp <- StateT afterPeg
                                                 return (cons c atp),
                                              return emp]
                importPapillon16_82 = foldl1 mplus [do d382_264 <- get
                                                       xx381_265 <- StateT varToken
                                                       case xx381_265 of
                                                           "import" -> return ()
                                                           _ -> gets derivsPosition >>= (throwError . ParseError "\"import\"" "not match pattern: " "" d382_264 ["varToken"])
                                                       let "import" = xx381_265
                                                       return ()
                                                       d384_266 <- get
                                                       xx383_267 <- StateT typToken
                                                       case xx383_267 of
                                                           "Text" -> return ()
                                                           _ -> gets derivsPosition >>= (throwError . ParseError "\"Text\"" "not match pattern: " "" d384_266 ["typToken"])
                                                       let "Text" = xx383_267
                                                       return ()
                                                       d386_268 <- get
                                                       xx385_269 <- StateT derivsChars
                                                       case xx385_269 of
                                                           '.' -> return ()
                                                           _ -> gets derivsPosition >>= (throwError . ParseError "'.'" "not match pattern: " "" d386_268 ["dvChars"])
                                                       let '.' = xx385_269
                                                       return ()
                                                       _ <- StateT spaces
                                                       return ()
                                                       d390_270 <- get
                                                       xx389_271 <- StateT typToken
                                                       case xx389_271 of
                                                           "Papillon" -> return ()
                                                           _ -> gets derivsPosition >>= (throwError . ParseError "\"Papillon\"" "not match pattern: " "" d390_270 ["typToken"])
                                                       let "Papillon" = xx389_271
                                                       return ()
                                                       ddd391_272 <- get
                                                       do err <- ((do d393_273 <- get
                                                                      xx392_274 <- StateT derivsChars
                                                                      case xx392_274 of
                                                                          '.' -> return ()
                                                                          _ -> gets derivsPosition >>= (throwError . ParseError "'.'" "not match pattern: " "" d393_273 ["dvChars"])
                                                                      let '.' = xx392_274
                                                                      return ()) >> return False) `catchError` const (return True)
                                                          unless err (gets derivsPosition >>= (throwError . ParseError ('!' : "'.':") "not match: " "" ddd391_272 ["dvChars"]))
                                                       put ddd391_272
                                                       return ()]
                varToken17_83 = foldl1 mplus [do v <- StateT variable
                                                 _ <- StateT spaces
                                                 return ()
                                                 return v]
                typToken18_84 = foldl1 mplus [do t <- StateT typ
                                                 _ <- StateT spaces
                                                 return ()
                                                 return t]
                pap19_85 = foldl1 mplus [do d403_275 <- get
                                            xx402_276 <- StateT derivsChars
                                            case xx402_276 of
                                                '\n' -> return ()
                                                _ -> gets derivsPosition >>= (throwError . ParseError "'\\n'" "not match pattern: " "" d403_275 ["dvChars"])
                                            let '\n' = xx402_276
                                            return ()
                                            d405_277 <- get
                                            xx404_278 <- StateT derivsChars
                                            case xx404_278 of
                                                '[' -> return ()
                                                _ -> gets derivsPosition >>= (throwError . ParseError "'['" "not match pattern: " "" d405_277 ["dvChars"])
                                            let '[' = xx404_278
                                            return ()
                                            d407_279 <- get
                                            xx406_280 <- StateT derivsChars
                                            case xx406_280 of
                                                'p' -> return ()
                                                _ -> gets derivsPosition >>= (throwError . ParseError "'p'" "not match pattern: " "" d407_279 ["dvChars"])
                                            let 'p' = xx406_280
                                            return ()
                                            d409_281 <- get
                                            xx408_282 <- StateT derivsChars
                                            case xx408_282 of
                                                'a' -> return ()
                                                _ -> gets derivsPosition >>= (throwError . ParseError "'a'" "not match pattern: " "" d409_281 ["dvChars"])
                                            let 'a' = xx408_282
                                            return ()
                                            d411_283 <- get
                                            xx410_284 <- StateT derivsChars
                                            case xx410_284 of
                                                'p' -> return ()
                                                _ -> gets derivsPosition >>= (throwError . ParseError "'p'" "not match pattern: " "" d411_283 ["dvChars"])
                                            let 'p' = xx410_284
                                            return ()
                                            d413_285 <- get
                                            xx412_286 <- StateT derivsChars
                                            case xx412_286 of
                                                'i' -> return ()
                                                _ -> gets derivsPosition >>= (throwError . ParseError "'i'" "not match pattern: " "" d413_285 ["dvChars"])
                                            let 'i' = xx412_286
                                            return ()
                                            d415_287 <- get
                                            xx414_288 <- StateT derivsChars
                                            case xx414_288 of
                                                'l' -> return ()
                                                _ -> gets derivsPosition >>= (throwError . ParseError "'l'" "not match pattern: " "" d415_287 ["dvChars"])
                                            let 'l' = xx414_288
                                            return ()
                                            d417_289 <- get
                                            xx416_290 <- StateT derivsChars
                                            case xx416_290 of
                                                'l' -> return ()
                                                _ -> gets derivsPosition >>= (throwError . ParseError "'l'" "not match pattern: " "" d417_289 ["dvChars"])
                                            let 'l' = xx416_290
                                            return ()
                                            d419_291 <- get
                                            xx418_292 <- StateT derivsChars
                                            case xx418_292 of
                                                'o' -> return ()
                                                _ -> gets derivsPosition >>= (throwError . ParseError "'o'" "not match pattern: " "" d419_291 ["dvChars"])
                                            let 'o' = xx418_292
                                            return ()
                                            d421_293 <- get
                                            xx420_294 <- StateT derivsChars
                                            case xx420_294 of
                                                'n' -> return ()
                                                _ -> gets derivsPosition >>= (throwError . ParseError "'n'" "not match pattern: " "" d421_293 ["dvChars"])
                                            let 'n' = xx420_294
                                            return ()
                                            d423_295 <- get
                                            xx422_296 <- StateT derivsChars
                                            case xx422_296 of
                                                '|' -> return ()
                                                _ -> gets derivsPosition >>= (throwError . ParseError "'|'" "not match pattern: " "" d423_295 ["dvChars"])
                                            let '|' = xx422_296
                                            return ()
                                            d425_297 <- get
                                            xx424_298 <- StateT derivsChars
                                            case xx424_298 of
                                                '\n' -> return ()
                                                _ -> gets derivsPosition >>= (throwError . ParseError "'\\n'" "not match pattern: " "" d425_297 ["dvChars"])
                                            let '\n' = xx424_298
                                            return ()
                                            return ()]
                peg20_86 = foldl1 mplus [do _ <- StateT spaces
                                            return ()
                                            s <- StateT sourceType
                                            p <- StateT peg_
                                            return (mkTTPeg s p),
                                         do p <- StateT peg_
                                            return (mkTTPeg tString p)]
                sourceType21_87 = foldl1 mplus [do d435_299 <- get
                                                   xx434_300 <- StateT varToken
                                                   case xx434_300 of
                                                       "source" -> return ()
                                                       _ -> gets derivsPosition >>= (throwError . ParseError "\"source\"" "not match pattern: " "" d435_299 ["varToken"])
                                                   let "source" = xx434_300
                                                   return ()
                                                   d437_301 <- get
                                                   xx436_302 <- StateT derivsChars
                                                   case xx436_302 of
                                                       ':' -> return ()
                                                       _ -> gets derivsPosition >>= (throwError . ParseError "':'" "not match pattern: " "" d437_301 ["dvChars"])
                                                   let ':' = xx436_302
                                                   return ()
                                                   _ <- StateT spaces
                                                   return ()
                                                   v <- StateT typToken
                                                   return v]
                peg_22_88 = foldl1 mplus [do _ <- StateT spaces
                                             return ()
                                             d <- StateT definition
                                             p <- StateT peg_
                                             return (cons d p),
                                          return emp]
                definition23_89 = foldl1 mplus [do v <- StateT variable
                                                   _ <- StateT spaces
                                                   return ()
                                                   d453_303 <- get
                                                   xx452_304 <- StateT derivsChars
                                                   case xx452_304 of
                                                       ':' -> return ()
                                                       _ -> gets derivsPosition >>= (throwError . ParseError "':'" "not match pattern: " "" d453_303 ["dvChars"])
                                                   let ':' = xx452_304
                                                   return ()
                                                   d455_305 <- get
                                                   xx454_306 <- StateT derivsChars
                                                   case xx454_306 of
                                                       ':' -> return ()
                                                       _ -> gets derivsPosition >>= (throwError . ParseError "':'" "not match pattern: " "" d455_305 ["dvChars"])
                                                   let ':' = xx454_306
                                                   return ()
                                                   _ <- StateT spaces
                                                   return ()
                                                   t <- StateT hsTypeArr
                                                   _ <- StateT spaces
                                                   return ()
                                                   d463_307 <- get
                                                   xx462_308 <- StateT derivsChars
                                                   case xx462_308 of
                                                       '=' -> return ()
                                                       _ -> gets derivsPosition >>= (throwError . ParseError "'='" "not match pattern: " "" d463_307 ["dvChars"])
                                                   let '=' = xx462_308
                                                   return ()
                                                   _ <- StateT spaces
                                                   return ()
                                                   sel <- StateT selection
                                                   _ <- StateT spaces
                                                   return ()
                                                   d471_309 <- get
                                                   xx470_310 <- StateT derivsChars
                                                   case xx470_310 of
                                                       ';' -> return ()
                                                       _ -> gets derivsPosition >>= (throwError . ParseError "';'" "not match pattern: " "" d471_309 ["dvChars"])
                                                   let ';' = xx470_310
                                                   return ()
                                                   return (mkDef v t sel)]
                selection24_90 = foldl1 mplus [do ex <- StateT expressionHs
                                                  _ <- StateT spaces
                                                  return ()
                                                  d477_311 <- get
                                                  xx476_312 <- StateT derivsChars
                                                  case xx476_312 of
                                                      '/' -> return ()
                                                      _ -> gets derivsPosition >>= (throwError . ParseError "'/'" "not match pattern: " "" d477_311 ["dvChars"])
                                                  let '/' = xx476_312
                                                  return ()
                                                  _ <- StateT spaces
                                                  return ()
                                                  sel <- StateT selection
                                                  return (cons ex sel),
                                               do ex <- StateT expressionHs
                                                  return (cons ex emp)]
                expressionHs25_91 = foldl1 mplus [do e <- StateT expression
                                                     _ <- StateT spaces
                                                     return ()
                                                     d489_313 <- get
                                                     xx488_314 <- StateT derivsChars
                                                     case xx488_314 of
                                                         '{' -> return ()
                                                         _ -> gets derivsPosition >>= (throwError . ParseError "'{'" "not match pattern: " "" d489_313 ["dvChars"])
                                                     let '{' = xx488_314
                                                     return ()
                                                     _ <- StateT spaces
                                                     return ()
                                                     h <- StateT hsExpLam
                                                     _ <- StateT spaces
                                                     return ()
                                                     d497_315 <- get
                                                     xx496_316 <- StateT derivsChars
                                                     case xx496_316 of
                                                         '}' -> return ()
                                                         _ -> gets derivsPosition >>= (throwError . ParseError "'}'" "not match pattern: " "" d497_315 ["dvChars"])
                                                     let '}' = xx496_316
                                                     return ()
                                                     return (mkExpressionHs e h)]
                expression26_92 = foldl1 mplus [do l <- StateT nameLeaf_
                                                   _ <- StateT spaces
                                                   return ()
                                                   e <- StateT expression
                                                   return (cons l e),
                                                return emp]
                nameLeaf_27_93 = foldl1 mplus [do d505_317 <- get
                                                  xx504_318 <- StateT derivsChars
                                                  case xx504_318 of
                                                      '!' -> return ()
                                                      _ -> gets derivsPosition >>= (throwError . ParseError "'!'" "not match pattern: " "" d505_317 ["dvChars"])
                                                  let '!' = xx504_318
                                                  return ()
                                                  nl <- StateT nameLeafNoCom
                                                  _ <- StateT spaces
                                                  return ()
                                                  com <- optional3_319 (StateT comForErr)
                                                  return (NotAfter nl $ maybe "" id com),
                                               do d513_320 <- get
                                                  xx512_321 <- StateT derivsChars
                                                  let c = xx512_321
                                                  unless (isAmp c) (gets derivsPosition >>= (throwError . ParseError "isAmp c" "not match: " "" d513_320 ["dvChars"]))
                                                  nl <- StateT nameLeaf
                                                  return (After nl),
                                               do nl <- StateT nameLeaf
                                                  return (Here nl)]
                nameLeaf28_94 = foldl1 mplus [do n <- StateT pat1
                                                 _ <- StateT spaces
                                                 return ()
                                                 com <- optional3_319 (StateT comForErr)
                                                 d525_322 <- get
                                                 xx524_323 <- StateT derivsChars
                                                 case xx524_323 of
                                                     ':' -> return ()
                                                     _ -> gets derivsPosition >>= (throwError . ParseError "':'" "not match pattern: " "" d525_322 ["dvChars"])
                                                 let ':' = xx524_323
                                                 return ()
                                                 (rf, p) <- StateT leaf
                                                 return (NameLeaf (n, maybe "" id com) rf p),
                                              do n <- StateT pat1
                                                 _ <- StateT spaces
                                                 return ()
                                                 com <- optional3_319 (StateT comForErr)
                                                 return (NameLeaf (n,
                                                                   maybe "" id com) FromToken Nothing)]
                nameLeafNoCom29_95 = foldl1 mplus [do n <- StateT pat1
                                                      _ <- StateT spaces
                                                      return ()
                                                      com <- optional3_319 (StateT comForErr)
                                                      d541_324 <- get
                                                      xx540_325 <- StateT derivsChars
                                                      case xx540_325 of
                                                          ':' -> return ()
                                                          _ -> gets derivsPosition >>= (throwError . ParseError "':'" "not match pattern: " "" d541_324 ["dvChars"])
                                                      let ':' = xx540_325
                                                      return ()
                                                      (rf, p) <- StateT leaf
                                                      return (NameLeaf (n, maybe "" id com) rf p),
                                                   do n <- StateT pat1
                                                      _ <- StateT spaces
                                                      return ()
                                                      return (NameLeaf (n, "") FromToken Nothing)]
                comForErr30_96 = foldl1 mplus [do d549_326 <- get
                                                  xx548_327 <- StateT derivsChars
                                                  case xx548_327 of
                                                      '{' -> return ()
                                                      _ -> gets derivsPosition >>= (throwError . ParseError "'{'" "not match pattern: " "" d549_326 ["dvChars"])
                                                  let '{' = xx548_327
                                                  return ()
                                                  d551_328 <- get
                                                  xx550_329 <- StateT derivsChars
                                                  case xx550_329 of
                                                      '-' -> return ()
                                                      _ -> gets derivsPosition >>= (throwError . ParseError "'-'" "not match pattern: " "" d551_328 ["dvChars"])
                                                  let '-' = xx550_329
                                                  return ()
                                                  d553_330 <- get
                                                  xx552_331 <- StateT derivsChars
                                                  case xx552_331 of
                                                      '#' -> return ()
                                                      _ -> gets derivsPosition >>= (throwError . ParseError "'#'" "not match pattern: " "" d553_330 ["dvChars"])
                                                  let '#' = xx552_331
                                                  return ()
                                                  _ <- StateT spaces
                                                  return ()
                                                  d557_332 <- get
                                                  xx556_333 <- StateT derivsChars
                                                  case xx556_333 of
                                                      '"' -> return ()
                                                      _ -> gets derivsPosition >>= (throwError . ParseError "'\"'" "not match pattern: " "" d557_332 ["dvChars"])
                                                  let '"' = xx556_333
                                                  return ()
                                                  s <- StateT stringLit
                                                  d561_334 <- get
                                                  xx560_335 <- StateT derivsChars
                                                  case xx560_335 of
                                                      '"' -> return ()
                                                      _ -> gets derivsPosition >>= (throwError . ParseError "'\"'" "not match pattern: " "" d561_334 ["dvChars"])
                                                  let '"' = xx560_335
                                                  return ()
                                                  _ <- StateT spaces
                                                  return ()
                                                  d565_336 <- get
                                                  xx564_337 <- StateT derivsChars
                                                  case xx564_337 of
                                                      '#' -> return ()
                                                      _ -> gets derivsPosition >>= (throwError . ParseError "'#'" "not match pattern: " "" d565_336 ["dvChars"])
                                                  let '#' = xx564_337
                                                  return ()
                                                  d567_338 <- get
                                                  xx566_339 <- StateT derivsChars
                                                  case xx566_339 of
                                                      '-' -> return ()
                                                      _ -> gets derivsPosition >>= (throwError . ParseError "'-'" "not match pattern: " "" d567_338 ["dvChars"])
                                                  let '-' = xx566_339
                                                  return ()
                                                  d569_340 <- get
                                                  xx568_341 <- StateT derivsChars
                                                  case xx568_341 of
                                                      '}' -> return ()
                                                      _ -> gets derivsPosition >>= (throwError . ParseError "'}'" "not match pattern: " "" d569_340 ["dvChars"])
                                                  let '}' = xx568_341
                                                  return ()
                                                  _ <- StateT spaces
                                                  return ()
                                                  return s]
                leaf31_97 = foldl1 mplus [do rf <- StateT readFromLs
                                             t <- StateT test
                                             return (rf, Just t),
                                          do rf <- StateT readFromLs
                                             return (rf, Nothing),
                                          do t <- StateT test
                                             return (FromToken, Just t)]
                patOp32_98 = foldl1 mplus [do p <- StateT pat
                                              o <- StateT opConName
                                              po <- StateT patOp
                                              return (uInfixP p o po),
                                           do p <- StateT pat
                                              _ <- StateT spaces
                                              return ()
                                              d591_342 <- get
                                              xx590_343 <- StateT derivsChars
                                              let q = xx590_343
                                              unless (isBQ q) (gets derivsPosition >>= (throwError . ParseError "isBQ q" "not match: " "" d591_342 ["dvChars"]))
                                              t <- StateT typ
                                              d595_344 <- get
                                              xx594_345 <- StateT derivsChars
                                              let q_ = xx594_345
                                              unless (isBQ q_) (gets derivsPosition >>= (throwError . ParseError "isBQ q_" "not match: " "" d595_344 ["dvChars"]))
                                              _ <- StateT spaces
                                              return ()
                                              po <- StateT patOp
                                              return (uInfixP p (mkName t) po),
                                           do p <- StateT pat
                                              return p]
                pat33_99 = foldl1 mplus [do t <- StateT typ
                                            _ <- StateT spaces
                                            return ()
                                            ps <- StateT pats
                                            return (conToPatQ t ps),
                                         do d609_346 <- get
                                            xx608_347 <- StateT derivsChars
                                            case xx608_347 of
                                                '(' -> return ()
                                                _ -> gets derivsPosition >>= (throwError . ParseError "'('" "not match pattern: " "" d609_346 ["dvChars"])
                                            let '(' = xx608_347
                                            return ()
                                            o <- StateT opConName
                                            d613_348 <- get
                                            xx612_349 <- StateT derivsChars
                                            case xx612_349 of
                                                ')' -> return ()
                                                _ -> gets derivsPosition >>= (throwError . ParseError "')'" "not match pattern: " "" d613_348 ["dvChars"])
                                            let ')' = xx612_349
                                            return ()
                                            _ <- StateT spaces
                                            return ()
                                            ps <- StateT pats
                                            return (conP o ps),
                                         do p <- StateT pat1
                                            return p]
                pat134_100 = foldl1 mplus [do t <- StateT typ
                                              return (conToPatQ t emp),
                                           do d623_350 <- get
                                              xx622_351 <- StateT variable
                                              case xx622_351 of
                                                  "_" -> return ()
                                                  _ -> gets derivsPosition >>= (throwError . ParseError "\"_\"" "not match pattern: " "" d623_350 ["variable"])
                                              let "_" = xx622_351
                                              return ()
                                              return wildP,
                                           do n <- StateT variable
                                              return (strToPatQ n),
                                           do i <- StateT integer
                                              return (litP (integerL i)),
                                           do d629_352 <- get
                                              xx628_353 <- StateT derivsChars
                                              case xx628_353 of
                                                  '-' -> return ()
                                                  _ -> gets derivsPosition >>= (throwError . ParseError "'-'" "not match pattern: " "" d629_352 ["dvChars"])
                                              let '-' = xx628_353
                                              return ()
                                              _ <- StateT spaces
                                              return ()
                                              i <- StateT integer
                                              return (litP (integerL $ negate i)),
                                           do d635_354 <- get
                                              xx634_355 <- StateT derivsChars
                                              case xx634_355 of
                                                  '\'' -> return ()
                                                  _ -> gets derivsPosition >>= (throwError . ParseError "'\\''" "not match pattern: " "" d635_354 ["dvChars"])
                                              let '\'' = xx634_355
                                              return ()
                                              c <- StateT charLit
                                              d639_356 <- get
                                              xx638_357 <- StateT derivsChars
                                              case xx638_357 of
                                                  '\'' -> return ()
                                                  _ -> gets derivsPosition >>= (throwError . ParseError "'\\''" "not match pattern: " "" d639_356 ["dvChars"])
                                              let '\'' = xx638_357
                                              return ()
                                              return (charP c),
                                           do d641_358 <- get
                                              xx640_359 <- StateT derivsChars
                                              case xx640_359 of
                                                  '"' -> return ()
                                                  _ -> gets derivsPosition >>= (throwError . ParseError "'\"'" "not match pattern: " "" d641_358 ["dvChars"])
                                              let '"' = xx640_359
                                              return ()
                                              s <- StateT stringLit
                                              d645_360 <- get
                                              xx644_361 <- StateT derivsChars
                                              case xx644_361 of
                                                  '"' -> return ()
                                                  _ -> gets derivsPosition >>= (throwError . ParseError "'\"'" "not match pattern: " "" d645_360 ["dvChars"])
                                              let '"' = xx644_361
                                              return ()
                                              return (stringP s),
                                           do d647_362 <- get
                                              xx646_363 <- StateT derivsChars
                                              case xx646_363 of
                                                  '(' -> return ()
                                                  _ -> gets derivsPosition >>= (throwError . ParseError "'('" "not match pattern: " "" d647_362 ["dvChars"])
                                              let '(' = xx646_363
                                              return ()
                                              p <- StateT patList
                                              d651_364 <- get
                                              xx650_365 <- StateT derivsChars
                                              case xx650_365 of
                                                  ')' -> return ()
                                                  _ -> gets derivsPosition >>= (throwError . ParseError "')'" "not match pattern: " "" d651_364 ["dvChars"])
                                              let ')' = xx650_365
                                              return ()
                                              return (tupP p),
                                           do d653_366 <- get
                                              xx652_367 <- StateT derivsChars
                                              case xx652_367 of
                                                  '[' -> return ()
                                                  _ -> gets derivsPosition >>= (throwError . ParseError "'['" "not match pattern: " "" d653_366 ["dvChars"])
                                              let '[' = xx652_367
                                              return ()
                                              p <- StateT patList
                                              d657_368 <- get
                                              xx656_369 <- StateT derivsChars
                                              case xx656_369 of
                                                  ']' -> return ()
                                                  _ -> gets derivsPosition >>= (throwError . ParseError "']'" "not match pattern: " "" d657_368 ["dvChars"])
                                              let ']' = xx656_369
                                              return ()
                                              return (listP p)]
                patList35_101 = foldl1 mplus [do p <- StateT patOp
                                                 _ <- StateT spaces
                                                 return ()
                                                 d663_370 <- get
                                                 xx662_371 <- StateT derivsChars
                                                 case xx662_371 of
                                                     ',' -> return ()
                                                     _ -> gets derivsPosition >>= (throwError . ParseError "','" "not match pattern: " "" d663_370 ["dvChars"])
                                                 let ',' = xx662_371
                                                 return ()
                                                 _ <- StateT spaces
                                                 return ()
                                                 ps <- StateT patList
                                                 return (p : ps),
                                              do p <- StateT patOp
                                                 return [p],
                                              return []]
                opConName36_102 = foldl1 mplus [do d671_372 <- get
                                                   xx670_373 <- StateT derivsChars
                                                   case xx670_373 of
                                                       ':' -> return ()
                                                       _ -> gets derivsPosition >>= (throwError . ParseError "':'" "not match pattern: " "" d671_372 ["dvChars"])
                                                   let ':' = xx670_373
                                                   return ()
                                                   ot <- StateT opTail
                                                   return (mkName $ colon : ot)]
                charLit37_103 = foldl1 mplus [do d675_374 <- get
                                                 xx674_375 <- StateT derivsChars
                                                 let c = xx674_375
                                                 unless (isAlphaNumOt c) (gets derivsPosition >>= (throwError . ParseError "isAlphaNumOt c" "not match: " "" d675_374 ["dvChars"]))
                                                 return c,
                                              do d677_376 <- get
                                                 xx676_377 <- StateT derivsChars
                                                 case xx676_377 of
                                                     '\\' -> return ()
                                                     _ -> gets derivsPosition >>= (throwError . ParseError "'\\\\'" "not match pattern: " "" d677_376 ["dvChars"])
                                                 let '\\' = xx676_377
                                                 return ()
                                                 c <- StateT escapeC
                                                 return c]
                stringLit38_104 = foldl1 mplus [do d681_378 <- get
                                                   xx680_379 <- StateT derivsChars
                                                   let c = xx680_379
                                                   unless (isStrLitC c) (gets derivsPosition >>= (throwError . ParseError "isStrLitC c" "not match: " "" d681_378 ["dvChars"]))
                                                   s <- StateT stringLit
                                                   return (cons c s),
                                                do d685_380 <- get
                                                   xx684_381 <- StateT derivsChars
                                                   case xx684_381 of
                                                       '\\' -> return ()
                                                       _ -> gets derivsPosition >>= (throwError . ParseError "'\\\\'" "not match pattern: " "" d685_380 ["dvChars"])
                                                   let '\\' = xx684_381
                                                   return ()
                                                   c <- StateT escapeC
                                                   s <- StateT stringLit
                                                   return (c : s),
                                                return emp]
                escapeC39_105 = foldl1 mplus [do d691_382 <- get
                                                 xx690_383 <- StateT derivsChars
                                                 case xx690_383 of
                                                     '"' -> return ()
                                                     _ -> gets derivsPosition >>= (throwError . ParseError "'\"'" "not match pattern: " "" d691_382 ["dvChars"])
                                                 let '"' = xx690_383
                                                 return ()
                                                 return '"',
                                              do d693_384 <- get
                                                 xx692_385 <- StateT derivsChars
                                                 case xx692_385 of
                                                     '\'' -> return ()
                                                     _ -> gets derivsPosition >>= (throwError . ParseError "'\\''" "not match pattern: " "" d693_384 ["dvChars"])
                                                 let '\'' = xx692_385
                                                 return ()
                                                 return '\'',
                                              do d695_386 <- get
                                                 xx694_387 <- StateT derivsChars
                                                 case xx694_387 of
                                                     '\\' -> return ()
                                                     _ -> gets derivsPosition >>= (throwError . ParseError "'\\\\'" "not match pattern: " "" d695_386 ["dvChars"])
                                                 let '\\' = xx694_387
                                                 return ()
                                                 return '\\',
                                              do d697_388 <- get
                                                 xx696_389 <- StateT derivsChars
                                                 case xx696_389 of
                                                     'n' -> return ()
                                                     _ -> gets derivsPosition >>= (throwError . ParseError "'n'" "not match pattern: " "" d697_388 ["dvChars"])
                                                 let 'n' = xx696_389
                                                 return ()
                                                 return '\n',
                                              do d699_390 <- get
                                                 xx698_391 <- StateT derivsChars
                                                 case xx698_391 of
                                                     't' -> return ()
                                                     _ -> gets derivsPosition >>= (throwError . ParseError "'t'" "not match pattern: " "" d699_390 ["dvChars"])
                                                 let 't' = xx698_391
                                                 return ()
                                                 return tab]
                pats40_106 = foldl1 mplus [do p <- StateT pat
                                              _ <- StateT spaces
                                              return ()
                                              ps <- StateT pats
                                              return (cons p ps),
                                           return emp]
                readFromLs41_107 = foldl1 mplus [do rf <- StateT readFrom
                                                    d709_392 <- get
                                                    xx708_393 <- StateT derivsChars
                                                    case xx708_393 of
                                                        '*' -> return ()
                                                        _ -> gets derivsPosition >>= (throwError . ParseError "'*'" "not match pattern: " "" d709_392 ["dvChars"])
                                                    let '*' = xx708_393
                                                    return ()
                                                    return (FromList rf),
                                                 do rf <- StateT readFrom
                                                    d713_394 <- get
                                                    xx712_395 <- StateT derivsChars
                                                    case xx712_395 of
                                                        '+' -> return ()
                                                        _ -> gets derivsPosition >>= (throwError . ParseError "'+'" "not match pattern: " "" d713_394 ["dvChars"])
                                                    let '+' = xx712_395
                                                    return ()
                                                    return (FromList1 rf),
                                                 do rf <- StateT readFrom
                                                    d717_396 <- get
                                                    xx716_397 <- StateT derivsChars
                                                    case xx716_397 of
                                                        '?' -> return ()
                                                        _ -> gets derivsPosition >>= (throwError . ParseError "'?'" "not match pattern: " "" d717_396 ["dvChars"])
                                                    let '?' = xx716_397
                                                    return ()
                                                    return (FromOptional rf),
                                                 do rf <- StateT readFrom
                                                    return rf]
                readFrom42_108 = foldl1 mplus [do v <- StateT variable
                                                  return (FromVariable v),
                                               do d723_398 <- get
                                                  xx722_399 <- StateT derivsChars
                                                  case xx722_399 of
                                                      '(' -> return ()
                                                      _ -> gets derivsPosition >>= (throwError . ParseError "'('" "not match pattern: " "" d723_398 ["dvChars"])
                                                  let '(' = xx722_399
                                                  return ()
                                                  s <- StateT selection
                                                  d727_400 <- get
                                                  xx726_401 <- StateT derivsChars
                                                  case xx726_401 of
                                                      ')' -> return ()
                                                      _ -> gets derivsPosition >>= (throwError . ParseError "')'" "not match pattern: " "" d727_400 ["dvChars"])
                                                  let ')' = xx726_401
                                                  return ()
                                                  return (FromSelection s)]
                test43_109 = foldl1 mplus [do d729_402 <- get
                                              xx728_403 <- StateT derivsChars
                                              case xx728_403 of
                                                  '[' -> return ()
                                                  _ -> gets derivsPosition >>= (throwError . ParseError "'['" "not match pattern: " "" d729_402 ["dvChars"])
                                              let '[' = xx728_403
                                              return ()
                                              h <- StateT hsExpLam
                                              _ <- StateT spaces
                                              return ()
                                              com <- optional3_319 (StateT comForErr)
                                              d737_404 <- get
                                              xx736_405 <- StateT derivsChars
                                              case xx736_405 of
                                                  ']' -> return ()
                                                  _ -> gets derivsPosition >>= (throwError . ParseError "']'" "not match pattern: " "" d737_404 ["dvChars"])
                                              let ']' = xx736_405
                                              return ()
                                              return (h, maybe "" id com)]
                hsExpLam44_110 = foldl1 mplus [do d739_406 <- get
                                                  xx738_407 <- StateT derivsChars
                                                  case xx738_407 of
                                                      '\\' -> return ()
                                                      _ -> gets derivsPosition >>= (throwError . ParseError "'\\\\'" "not match pattern: " "" d739_406 ["dvChars"])
                                                  let '\\' = xx738_407
                                                  return ()
                                                  _ <- StateT spaces
                                                  return ()
                                                  ps <- StateT pats
                                                  _ <- StateT spaces
                                                  return ()
                                                  d747_408 <- get
                                                  xx746_409 <- StateT derivsChars
                                                  case xx746_409 of
                                                      '-' -> return ()
                                                      _ -> gets derivsPosition >>= (throwError . ParseError "'-'" "not match pattern: " "" d747_408 ["dvChars"])
                                                  let '-' = xx746_409
                                                  return ()
                                                  d749_410 <- get
                                                  xx748_411 <- StateT derivsChars
                                                  let c = xx748_411
                                                  unless (isGt c) (gets derivsPosition >>= (throwError . ParseError "isGt c" "not match: " "" d749_410 ["dvChars"]))
                                                  _ <- StateT spaces
                                                  return ()
                                                  e <- StateT hsExpTyp
                                                  return (lamE ps e),
                                               do e <- StateT hsExpTyp
                                                  return e]
                hsExpTyp45_111 = foldl1 mplus [do eo <- StateT hsExpOp
                                                  d759_412 <- get
                                                  xx758_413 <- StateT derivsChars
                                                  case xx758_413 of
                                                      ':' -> return ()
                                                      _ -> gets derivsPosition >>= (throwError . ParseError "':'" "not match pattern: " "" d759_412 ["dvChars"])
                                                  let ':' = xx758_413
                                                  return ()
                                                  d761_414 <- get
                                                  xx760_415 <- StateT derivsChars
                                                  case xx760_415 of
                                                      ':' -> return ()
                                                      _ -> gets derivsPosition >>= (throwError . ParseError "':'" "not match pattern: " "" d761_414 ["dvChars"])
                                                  let ':' = xx760_415
                                                  return ()
                                                  _ <- StateT spaces
                                                  return ()
                                                  t <- StateT hsTypeArr
                                                  return (sigE eo t),
                                               do eo <- StateT hsExpOp
                                                  return eo]
                hsExpOp46_112 = foldl1 mplus [do l <- StateT hsExp
                                                 _ <- StateT spaces
                                                 return ()
                                                 o <- StateT hsOp
                                                 _ <- StateT spaces
                                                 return ()
                                                 r <- StateT hsExpOp
                                                 return (uInfixE (getEx l) o r),
                                              do e <- StateT hsExp
                                                 return (getEx e)]
                hsOp47_113 = foldl1 mplus [do d781_416 <- get
                                              xx780_417 <- StateT derivsChars
                                              let c = xx780_417
                                              unless (isOpHeadChar c) (gets derivsPosition >>= (throwError . ParseError "isOpHeadChar c" "not match: " "" d781_416 ["dvChars"]))
                                              o <- StateT opTail
                                              return (varE (mkName (cons c o))),
                                           do d785_418 <- get
                                              xx784_419 <- StateT derivsChars
                                              case xx784_419 of
                                                  ':' -> return ()
                                                  _ -> gets derivsPosition >>= (throwError . ParseError "':'" "not match pattern: " "" d785_418 ["dvChars"])
                                              let ':' = xx784_419
                                              return ()
                                              ddd786_420 <- get
                                              do err <- ((do d788_421 <- get
                                                             xx787_422 <- StateT derivsChars
                                                             case xx787_422 of
                                                                 ':' -> return ()
                                                                 _ -> gets derivsPosition >>= (throwError . ParseError "':'" "not match pattern: " "" d788_421 ["dvChars"])
                                                             let ':' = xx787_422
                                                             return ()) >> return False) `catchError` const (return True)
                                                 unless err (gets derivsPosition >>= (throwError . ParseError ('!' : "':':") "not match: " "" ddd786_420 ["dvChars"]))
                                              put ddd786_420
                                              o <- StateT opTail
                                              return (conE (mkName (':' : o))),
                                           do d792_423 <- get
                                              xx791_424 <- StateT derivsChars
                                              let c = xx791_424
                                              unless (isBQ c) (gets derivsPosition >>= (throwError . ParseError "isBQ c" "not match: " "" d792_423 ["dvChars"]))
                                              v <- StateT variable
                                              d796_425 <- get
                                              xx795_426 <- StateT derivsChars
                                              let c_ = xx795_426
                                              unless (isBQ c_) (gets derivsPosition >>= (throwError . ParseError "isBQ c_" "not match: " "" d796_425 ["dvChars"]))
                                              return (varE (mkName v)),
                                           do d798_427 <- get
                                              xx797_428 <- StateT derivsChars
                                              let c = xx797_428
                                              unless (isBQ c) (gets derivsPosition >>= (throwError . ParseError "isBQ c" "not match: " "" d798_427 ["dvChars"]))
                                              t <- StateT typ
                                              d802_429 <- get
                                              xx801_430 <- StateT derivsChars
                                              let c_ = xx801_430
                                              unless (isBQ c_) (gets derivsPosition >>= (throwError . ParseError "isBQ c_" "not match: " "" d802_429 ["dvChars"]))
                                              return (conE (mkName t))]
                opTail48_114 = foldl1 mplus [do d804_431 <- get
                                                xx803_432 <- StateT derivsChars
                                                let c = xx803_432
                                                unless (isOpTailChar c) (gets derivsPosition >>= (throwError . ParseError "isOpTailChar c" "not match: " "" d804_431 ["dvChars"]))
                                                s <- StateT opTail
                                                return (cons c s),
                                             return emp]
                hsExp49_115 = foldl1 mplus [do e <- StateT hsExp1
                                               _ <- StateT spaces
                                               return ()
                                               h <- StateT hsExp
                                               return (applyExR e h),
                                            do e <- StateT hsExp1
                                               return (toEx e)]
                hsExp150_116 = foldl1 mplus [do d816_433 <- get
                                                xx815_434 <- StateT derivsChars
                                                case xx815_434 of
                                                    '(' -> return ()
                                                    _ -> gets derivsPosition >>= (throwError . ParseError "'('" "not match pattern: " "" d816_433 ["dvChars"])
                                                let '(' = xx815_434
                                                return ()
                                                l <- optional3_319 (foldl1 mplus [do e <- StateT hsExpTyp
                                                                                     return e])
                                                _ <- StateT spaces
                                                return ()
                                                o <- StateT hsOp
                                                _ <- StateT spaces
                                                return ()
                                                r <- optional3_319 (foldl1 mplus [do e <- StateT hsExpTyp
                                                                                     return e])
                                                d832_435 <- get
                                                xx831_436 <- StateT derivsChars
                                                case xx831_436 of
                                                    ')' -> return ()
                                                    _ -> gets derivsPosition >>= (throwError . ParseError "')'" "not match pattern: " "" d832_435 ["dvChars"])
                                                let ')' = xx831_436
                                                return ()
                                                return (infixE l o r),
                                             do d834_437 <- get
                                                xx833_438 <- StateT derivsChars
                                                case xx833_438 of
                                                    '(' -> return ()
                                                    _ -> gets derivsPosition >>= (throwError . ParseError "'('" "not match pattern: " "" d834_437 ["dvChars"])
                                                let '(' = xx833_438
                                                return ()
                                                et <- StateT hsExpTpl
                                                d838_439 <- get
                                                xx837_440 <- StateT derivsChars
                                                case xx837_440 of
                                                    ')' -> return ()
                                                    _ -> gets derivsPosition >>= (throwError . ParseError "')'" "not match pattern: " "" d838_439 ["dvChars"])
                                                let ')' = xx837_440
                                                return ()
                                                return (tupE et),
                                             do d840_441 <- get
                                                xx839_442 <- StateT derivsChars
                                                case xx839_442 of
                                                    '[' -> return ()
                                                    _ -> gets derivsPosition >>= (throwError . ParseError "'['" "not match pattern: " "" d840_441 ["dvChars"])
                                                let '[' = xx839_442
                                                return ()
                                                et <- StateT hsExpTpl
                                                d844_443 <- get
                                                xx843_444 <- StateT derivsChars
                                                case xx843_444 of
                                                    ']' -> return ()
                                                    _ -> gets derivsPosition >>= (throwError . ParseError "']'" "not match pattern: " "" d844_443 ["dvChars"])
                                                let ']' = xx843_444
                                                return ()
                                                return (listE et),
                                             do v <- StateT variable
                                                return (varE (mkName v)),
                                             do t <- StateT typ
                                                return (conE (mkName t)),
                                             do i <- StateT integer
                                                _ <- StateT spaces
                                                return ()
                                                return (litE (integerL i)),
                                             do d854_445 <- get
                                                xx853_446 <- StateT derivsChars
                                                case xx853_446 of
                                                    '\'' -> return ()
                                                    _ -> gets derivsPosition >>= (throwError . ParseError "'\\''" "not match pattern: " "" d854_445 ["dvChars"])
                                                let '\'' = xx853_446
                                                return ()
                                                c <- StateT charLit
                                                d858_447 <- get
                                                xx857_448 <- StateT derivsChars
                                                case xx857_448 of
                                                    '\'' -> return ()
                                                    _ -> gets derivsPosition >>= (throwError . ParseError "'\\''" "not match pattern: " "" d858_447 ["dvChars"])
                                                let '\'' = xx857_448
                                                return ()
                                                return (litE (charL c)),
                                             do d860_449 <- get
                                                xx859_450 <- StateT derivsChars
                                                case xx859_450 of
                                                    '"' -> return ()
                                                    _ -> gets derivsPosition >>= (throwError . ParseError "'\"'" "not match pattern: " "" d860_449 ["dvChars"])
                                                let '"' = xx859_450
                                                return ()
                                                s <- StateT stringLit
                                                d864_451 <- get
                                                xx863_452 <- StateT derivsChars
                                                case xx863_452 of
                                                    '"' -> return ()
                                                    _ -> gets derivsPosition >>= (throwError . ParseError "'\"'" "not match pattern: " "" d864_451 ["dvChars"])
                                                let '"' = xx863_452
                                                return ()
                                                return (litE (stringL s)),
                                             do d866_453 <- get
                                                xx865_454 <- StateT derivsChars
                                                case xx865_454 of
                                                    '-' -> return ()
                                                    _ -> gets derivsPosition >>= (throwError . ParseError "'-'" "not match pattern: " "" d866_453 ["dvChars"])
                                                let '-' = xx865_454
                                                return ()
                                                _ <- StateT spaces
                                                return ()
                                                e <- StateT hsExp1
                                                return (appE (varE $ mkName "negate") e)]
                hsExpTpl51_117 = foldl1 mplus [do e <- StateT hsExpLam
                                                  _ <- StateT spaces
                                                  return ()
                                                  d876_455 <- get
                                                  xx875_456 <- StateT derivsChars
                                                  let c = xx875_456
                                                  unless (isComma c) (gets derivsPosition >>= (throwError . ParseError "isComma c" "not match: " "" d876_455 ["dvChars"]))
                                                  _ <- StateT spaces
                                                  return ()
                                                  et <- StateT hsExpTpl
                                                  return (cons e et),
                                               do e <- StateT hsExpLam
                                                  return (cons e emp),
                                               return emp]
                hsTypeArr52_118 = foldl1 mplus [do l <- StateT hsType
                                                   d886_457 <- get
                                                   xx885_458 <- StateT derivsChars
                                                   case xx885_458 of
                                                       '-' -> return ()
                                                       _ -> gets derivsPosition >>= (throwError . ParseError "'-'" "not match pattern: " "" d886_457 ["dvChars"])
                                                   let '-' = xx885_458
                                                   return ()
                                                   d888_459 <- get
                                                   xx887_460 <- StateT derivsChars
                                                   let c = xx887_460
                                                   unless (isGt c) (gets derivsPosition >>= (throwError . ParseError "isGt c" "not match: " "" d888_459 ["dvChars"]))
                                                   _ <- StateT spaces
                                                   return ()
                                                   r <- StateT hsTypeArr
                                                   return (appT (appT arrowT (getTyp l)) r),
                                                do t <- StateT hsType
                                                   return (getTyp t)]
                hsType53_119 = foldl1 mplus [do t <- StateT hsType1
                                                ts <- StateT hsType
                                                return (applyTyp (toTyp t) ts),
                                             do t <- StateT hsType1
                                                return (toTyp t)]
                hsType154_120 = foldl1 mplus [do d902_461 <- get
                                                 xx901_462 <- StateT derivsChars
                                                 case xx901_462 of
                                                     '[' -> return ()
                                                     _ -> gets derivsPosition >>= (throwError . ParseError "'['" "not match pattern: " "" d902_461 ["dvChars"])
                                                 let '[' = xx901_462
                                                 return ()
                                                 d904_463 <- get
                                                 xx903_464 <- StateT derivsChars
                                                 case xx903_464 of
                                                     ']' -> return ()
                                                     _ -> gets derivsPosition >>= (throwError . ParseError "']'" "not match pattern: " "" d904_463 ["dvChars"])
                                                 let ']' = xx903_464
                                                 return ()
                                                 _ <- StateT spaces
                                                 return ()
                                                 return listT,
                                              do d908_465 <- get
                                                 xx907_466 <- StateT derivsChars
                                                 case xx907_466 of
                                                     '[' -> return ()
                                                     _ -> gets derivsPosition >>= (throwError . ParseError "'['" "not match pattern: " "" d908_465 ["dvChars"])
                                                 let '[' = xx907_466
                                                 return ()
                                                 t <- StateT hsTypeArr
                                                 d912_467 <- get
                                                 xx911_468 <- StateT derivsChars
                                                 case xx911_468 of
                                                     ']' -> return ()
                                                     _ -> gets derivsPosition >>= (throwError . ParseError "']'" "not match pattern: " "" d912_467 ["dvChars"])
                                                 let ']' = xx911_468
                                                 return ()
                                                 _ <- StateT spaces
                                                 return ()
                                                 return (appT listT t),
                                              do d916_469 <- get
                                                 xx915_470 <- StateT derivsChars
                                                 case xx915_470 of
                                                     '(' -> return ()
                                                     _ -> gets derivsPosition >>= (throwError . ParseError "'('" "not match pattern: " "" d916_469 ["dvChars"])
                                                 let '(' = xx915_470
                                                 return ()
                                                 _ <- StateT spaces
                                                 return ()
                                                 tt <- StateT hsTypeTpl
                                                 d922_471 <- get
                                                 xx921_472 <- StateT derivsChars
                                                 case xx921_472 of
                                                     ')' -> return ()
                                                     _ -> gets derivsPosition >>= (throwError . ParseError "')'" "not match pattern: " "" d922_471 ["dvChars"])
                                                 let ')' = xx921_472
                                                 return ()
                                                 return (tupT tt),
                                              do t <- StateT typToken
                                                 return (conT (mkName t)),
                                              do d926_473 <- get
                                                 xx925_474 <- StateT derivsChars
                                                 case xx925_474 of
                                                     '(' -> return ()
                                                     _ -> gets derivsPosition >>= (throwError . ParseError "'('" "not match pattern: " "" d926_473 ["dvChars"])
                                                 let '(' = xx925_474
                                                 return ()
                                                 d928_475 <- get
                                                 xx927_476 <- StateT derivsChars
                                                 case xx927_476 of
                                                     '-' -> return ()
                                                     _ -> gets derivsPosition >>= (throwError . ParseError "'-'" "not match pattern: " "" d928_475 ["dvChars"])
                                                 let '-' = xx927_476
                                                 return ()
                                                 d930_477 <- get
                                                 xx929_478 <- StateT derivsChars
                                                 let c = xx929_478
                                                 unless (isGt c) (gets derivsPosition >>= (throwError . ParseError "isGt c" "not match: " "" d930_477 ["dvChars"]))
                                                 d932_479 <- get
                                                 xx931_480 <- StateT derivsChars
                                                 case xx931_480 of
                                                     ')' -> return ()
                                                     _ -> gets derivsPosition >>= (throwError . ParseError "')'" "not match pattern: " "" d932_479 ["dvChars"])
                                                 let ')' = xx931_480
                                                 return ()
                                                 _ <- StateT spaces
                                                 return ()
                                                 return arrowT]
                hsTypeTpl55_121 = foldl1 mplus [do t <- StateT hsTypeArr
                                                   d938_481 <- get
                                                   xx937_482 <- StateT derivsChars
                                                   let c = xx937_482
                                                   unless (isComma c) (gets derivsPosition >>= (throwError . ParseError "isComma c" "not match: " "" d938_481 ["dvChars"]))
                                                   _ <- StateT spaces
                                                   return ()
                                                   tt <- StateT hsTypeTpl
                                                   return (cons t tt),
                                                do t <- StateT hsTypeArr
                                                   return (cons t emp),
                                                return emp]
                typ56_122 = foldl1 mplus [do u <- StateT upper
                                             t <- StateT tvtail
                                             return (cons u t)]
                variable57_123 = foldl1 mplus [do l <- StateT lower
                                                  t <- StateT tvtail
                                                  return (cons l t)]
                tvtail58_124 = foldl1 mplus [do a <- StateT alpha
                                                t <- StateT tvtail
                                                return (cons a t),
                                             return emp]
                integer59_125 = foldl1 mplus [do dh <- StateT digit
                                                 ds <- list1_483 (foldl1 mplus [do d <- StateT digit
                                                                                   return d])
                                                 return (read (cons dh ds))]
                alpha60_126 = foldl1 mplus [do u <- StateT upper
                                               return u,
                                            do l <- StateT lower
                                               return l,
                                            do d <- StateT digit
                                               return d,
                                            do d970_484 <- get
                                               xx969_485 <- StateT derivsChars
                                               case xx969_485 of
                                                   '\'' -> return ()
                                                   _ -> gets derivsPosition >>= (throwError . ParseError "'\\''" "not match pattern: " "" d970_484 ["dvChars"])
                                               let '\'' = xx969_485
                                               return ()
                                               return '\'']
                upper61_127 = foldl1 mplus [do d972_486 <- get
                                               xx971_487 <- StateT derivsChars
                                               let u = xx971_487
                                               unless (isUpper u) (gets derivsPosition >>= (throwError . ParseError "isUpper u" "not match: " "" d972_486 ["dvChars"]))
                                               return u]
                lower62_128 = foldl1 mplus [do d974_488 <- get
                                               xx973_489 <- StateT derivsChars
                                               let l = xx973_489
                                               unless (isLowerU l) (gets derivsPosition >>= (throwError . ParseError "isLowerU l" "not match: " "" d974_488 ["dvChars"]))
                                               return l]
                digit63_129 = foldl1 mplus [do d976_490 <- get
                                               xx975_491 <- StateT derivsChars
                                               let d = xx975_491
                                               unless (isDigit d) (gets derivsPosition >>= (throwError . ParseError "isDigit d" "not match: " "" d976_490 ["dvChars"]))
                                               return d]
                spaces64_130 = foldl1 mplus [do _ <- StateT space
                                                return ()
                                                _ <- StateT spaces
                                                return ()
                                                return (),
                                             return ()]
                space65_131 = foldl1 mplus [do d982_492 <- get
                                               xx981_493 <- StateT derivsChars
                                               let s = xx981_493
                                               unless (isSpace s) (gets derivsPosition >>= (throwError . ParseError "isSpace s" "not match: " "" d982_492 ["dvChars"]))
                                               return (),
                                            do d984_494 <- get
                                               xx983_495 <- StateT derivsChars
                                               case xx983_495 of
                                                   '-' -> return ()
                                                   _ -> gets derivsPosition >>= (throwError . ParseError "'-'" "not match pattern: " "" d984_494 ["dvChars"])
                                               let '-' = xx983_495
                                               return ()
                                               d986_496 <- get
                                               xx985_497 <- StateT derivsChars
                                               case xx985_497 of
                                                   '-' -> return ()
                                                   _ -> gets derivsPosition >>= (throwError . ParseError "'-'" "not match pattern: " "" d986_496 ["dvChars"])
                                               let '-' = xx985_497
                                               return ()
                                               _ <- StateT notNLString
                                               return ()
                                               _ <- StateT newLine
                                               return ()
                                               return (),
                                            do _ <- StateT comment
                                               return ()
                                               return ()]
                notNLString66_132 = foldl1 mplus [do ddd993_498 <- get
                                                     do err <- ((do _ <- StateT newLine
                                                                    return ()) >> return False) `catchError` const (return True)
                                                        unless err (gets derivsPosition >>= (throwError . ParseError ('!' : "_:newLine") "not match: " "" ddd993_498 ["newLine"]))
                                                     put ddd993_498
                                                     c <- StateT derivsChars
                                                     s <- StateT notNLString
                                                     return (cons c s),
                                                  return emp]
                newLine67_133 = foldl1 mplus [do d1001_499 <- get
                                                 xx1000_500 <- StateT derivsChars
                                                 case xx1000_500 of
                                                     '\n' -> return ()
                                                     _ -> gets derivsPosition >>= (throwError . ParseError "'\\n'" "not match pattern: " "" d1001_499 ["dvChars"])
                                                 let '\n' = xx1000_500
                                                 return ()
                                                 return ()]
                comment68_134 = foldl1 mplus [do d1003_501 <- get
                                                 xx1002_502 <- StateT derivsChars
                                                 case xx1002_502 of
                                                     '{' -> return ()
                                                     _ -> gets derivsPosition >>= (throwError . ParseError "'{'" "not match pattern: " "" d1003_501 ["dvChars"])
                                                 let '{' = xx1002_502
                                                 return ()
                                                 d1005_503 <- get
                                                 xx1004_504 <- StateT derivsChars
                                                 case xx1004_504 of
                                                     '-' -> return ()
                                                     _ -> gets derivsPosition >>= (throwError . ParseError "'-'" "not match pattern: " "" d1005_503 ["dvChars"])
                                                 let '-' = xx1004_504
                                                 return ()
                                                 ddd1006_505 <- get
                                                 do err <- ((do d1008_506 <- get
                                                                xx1007_507 <- StateT derivsChars
                                                                case xx1007_507 of
                                                                    '#' -> return ()
                                                                    _ -> gets derivsPosition >>= (throwError . ParseError "'#'" "not match pattern: " "" d1008_506 ["dvChars"])
                                                                let '#' = xx1007_507
                                                                return ()) >> return False) `catchError` const (return True)
                                                    unless err (gets derivsPosition >>= (throwError . ParseError ('!' : "'#':") "not match: " "" ddd1006_505 ["dvChars"]))
                                                 put ddd1006_505
                                                 _ <- StateT comments
                                                 return ()
                                                 _ <- StateT comEnd
                                                 return ()
                                                 return ()]
                comments69_135 = foldl1 mplus [do _ <- StateT notComStr
                                                  return ()
                                                  _ <- StateT comment
                                                  return ()
                                                  _ <- StateT comments
                                                  return ()
                                                  return (),
                                               do _ <- StateT notComStr
                                                  return ()
                                                  return ()]
                notComStr70_136 = foldl1 mplus [do ddd1021_508 <- get
                                                   do err <- ((do _ <- StateT comment
                                                                  return ()) >> return False) `catchError` const (return True)
                                                      unless err (gets derivsPosition >>= (throwError . ParseError ('!' : "_:comment") "not match: " "" ddd1021_508 ["comment"]))
                                                   put ddd1021_508
                                                   ddd1024_509 <- get
                                                   do err <- ((do _ <- StateT comEnd
                                                                  return ()) >> return False) `catchError` const (return True)
                                                      unless err (gets derivsPosition >>= (throwError . ParseError ('!' : "_:comEnd") "not match: " "" ddd1024_509 ["comEnd"]))
                                                   put ddd1024_509
                                                   _ <- StateT derivsChars
                                                   return ()
                                                   _ <- StateT notComStr
                                                   return ()
                                                   return (),
                                                return ()]
                comEnd71_137 = foldl1 mplus [do d1032_510 <- get
                                                xx1031_511 <- StateT derivsChars
                                                case xx1031_511 of
                                                    '-' -> return ()
                                                    _ -> gets derivsPosition >>= (throwError . ParseError "'-'" "not match pattern: " "" d1032_510 ["dvChars"])
                                                let '-' = xx1031_511
                                                return ()
                                                d1034_512 <- get
                                                xx1033_513 <- StateT derivsChars
                                                case xx1033_513 of
                                                    '}' -> return ()
                                                    _ -> gets derivsPosition >>= (throwError . ParseError "'}'" "not match pattern: " "" d1034_512 ["dvChars"])
                                                let '}' = xx1033_513
                                                return ()
                                                return ()]
                list1_483 :: forall m a . (MonadPlus m, Applicative m) =>
                                          m a -> m ([a])
                list12_514 :: forall m a . (MonadPlus m, Applicative m) =>
                                           m a -> m ([a])
                list1_483 p = list12_514 p `mplus` return []
                list12_514 p = ((:) <$> p) <*> list1_483 p
                optional3_319 :: forall m a . (MonadPlus m, Applicative m) =>
                                              m a -> m (Maybe a)
                optional3_319 p = (Just <$> p) `mplus` return Nothing
class Source sl
    where type Token sl
          data Pos sl
          getToken :: sl -> Maybe ((Token sl, sl))
          initialPos :: Pos sl
          updatePos :: Token sl -> Pos sl -> Pos sl
class SourceList c
    where data ListPos c
          listToken :: [c] -> Maybe ((c, [c]))
          listInitialPos :: ListPos c
          listUpdatePos :: c -> ListPos c -> ListPos c
instance SourceList c => Source ([c])
    where type Token ([c]) = c
          newtype Pos ([c]) = ListPos (ListPos c)
          getToken = listToken
          initialPos = ListPos listInitialPos
          updatePos c (ListPos p) = ListPos (listUpdatePos c p)
instance SourceList Char
    where newtype ListPos Char = CharPos ((Int, Int)) deriving (Show)
          listToken (c : s) = Just (c, s)
          listToken _ = Nothing
          listInitialPos = CharPos (1, 1)
          listUpdatePos '\n' (CharPos (y, _)) = CharPos (y + 1, 0)
          listUpdatePos _ (CharPos (y, x)) = CharPos (y, x + 1)

