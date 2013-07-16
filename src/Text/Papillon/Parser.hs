{-# LANGUAGE FlexibleContexts, TemplateHaskell, UndecidableInstances, FlexibleContexts, PackageImports, TypeFamilies, RankNTypes #-}
module Text.Papillon.Parser (
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

import Text.Papillon.Papillon

import Control.Applicative



import Data.Char
import Language.Haskell.TH
import Text.Papillon.SyntaxTree
-- import Data.List

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
                                   ((Maybe (([String], String)), Derivs))),
              moduleName :: (Either (ParseError (Pos String) Derivs)
                                    (([String], Derivs))),
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
                             where d = Derivs pegFile73_1 pragma74_2 pragmaStr75_3 pragmaItems76_4 delPragmas77_5 pragmaEnd78_6 moduleDec79_7 moduleName80_8 moduleDecStr81_9 whr82_10 preImpPap83_11 prePeg84_12 afterPeg85_13 importPapillon86_14 varToken87_15 typToken88_16 pap89_17 peg90_18 sourceType91_19 peg_92_20 definition93_21 selection94_22 expressionHs95_23 expression96_24 nameLeaf_97_25 nameLeaf98_26 nameLeafNoCom99_27 comForErr100_28 leaf101_29 patOp102_30 pat103_31 pat1104_32 patList105_33 opConName106_34 charLit107_35 stringLit108_36 escapeC109_37 pats110_38 readFromLs111_39 readFrom112_40 test113_41 hsExpLam114_42 hsExpTyp115_43 hsExpOp116_44 hsOp117_45 opTail118_46 hsExp119_47 hsExp1120_48 hsExpTpl121_49 hsTypeArr122_50 hsType123_51 hsType1124_52 hsTypeTpl125_53 typ126_54 variable127_55 tvtail128_56 integer129_57 alpha130_58 upper131_59 lower132_60 digit133_61 spaces134_62 space135_63 notNLString136_64 newLine137_65 comment138_66 comments139_67 notComStr140_68 comEnd141_69 chars142_70 pos
                                   pegFile73_1 = runStateT pegFile4_71 d
                                   pragma74_2 = runStateT pragma5_72 d
                                   pragmaStr75_3 = runStateT pragmaStr6_73 d
                                   pragmaItems76_4 = runStateT pragmaItems7_74 d
                                   delPragmas77_5 = runStateT delPragmas8_75 d
                                   pragmaEnd78_6 = runStateT pragmaEnd9_76 d
                                   moduleDec79_7 = runStateT moduleDec10_77 d
                                   moduleName80_8 = runStateT moduleName11_78 d
                                   moduleDecStr81_9 = runStateT moduleDecStr12_79 d
                                   whr82_10 = runStateT whr13_80 d
                                   preImpPap83_11 = runStateT preImpPap14_81 d
                                   prePeg84_12 = runStateT prePeg15_82 d
                                   afterPeg85_13 = runStateT afterPeg16_83 d
                                   importPapillon86_14 = runStateT importPapillon17_84 d
                                   varToken87_15 = runStateT varToken18_85 d
                                   typToken88_16 = runStateT typToken19_86 d
                                   pap89_17 = runStateT pap20_87 d
                                   peg90_18 = runStateT peg21_88 d
                                   sourceType91_19 = runStateT sourceType22_89 d
                                   peg_92_20 = runStateT peg_23_90 d
                                   definition93_21 = runStateT definition24_91 d
                                   selection94_22 = runStateT selection25_92 d
                                   expressionHs95_23 = runStateT expressionHs26_93 d
                                   expression96_24 = runStateT expression27_94 d
                                   nameLeaf_97_25 = runStateT nameLeaf_28_95 d
                                   nameLeaf98_26 = runStateT nameLeaf29_96 d
                                   nameLeafNoCom99_27 = runStateT nameLeafNoCom30_97 d
                                   comForErr100_28 = runStateT comForErr31_98 d
                                   leaf101_29 = runStateT leaf32_99 d
                                   patOp102_30 = runStateT patOp33_100 d
                                   pat103_31 = runStateT pat34_101 d
                                   pat1104_32 = runStateT pat135_102 d
                                   patList105_33 = runStateT patList36_103 d
                                   opConName106_34 = runStateT opConName37_104 d
                                   charLit107_35 = runStateT charLit38_105 d
                                   stringLit108_36 = runStateT stringLit39_106 d
                                   escapeC109_37 = runStateT escapeC40_107 d
                                   pats110_38 = runStateT pats41_108 d
                                   readFromLs111_39 = runStateT readFromLs42_109 d
                                   readFrom112_40 = runStateT readFrom43_110 d
                                   test113_41 = runStateT test44_111 d
                                   hsExpLam114_42 = runStateT hsExpLam45_112 d
                                   hsExpTyp115_43 = runStateT hsExpTyp46_113 d
                                   hsExpOp116_44 = runStateT hsExpOp47_114 d
                                   hsOp117_45 = runStateT hsOp48_115 d
                                   opTail118_46 = runStateT opTail49_116 d
                                   hsExp119_47 = runStateT hsExp50_117 d
                                   hsExp1120_48 = runStateT hsExp151_118 d
                                   hsExpTpl121_49 = runStateT hsExpTpl52_119 d
                                   hsTypeArr122_50 = runStateT hsTypeArr53_120 d
                                   hsType123_51 = runStateT hsType54_121 d
                                   hsType1124_52 = runStateT hsType155_122 d
                                   hsTypeTpl125_53 = runStateT hsTypeTpl56_123 d
                                   typ126_54 = runStateT typ57_124 d
                                   variable127_55 = runStateT variable58_125 d
                                   tvtail128_56 = runStateT tvtail59_126 d
                                   integer129_57 = runStateT integer60_127 d
                                   alpha130_58 = runStateT alpha61_128 d
                                   upper131_59 = runStateT upper62_129 d
                                   lower132_60 = runStateT lower63_130 d
                                   digit133_61 = runStateT digit64_131 d
                                   spaces134_62 = runStateT spaces65_132 d
                                   space135_63 = runStateT space66_133 d
                                   notNLString136_64 = runStateT notNLString67_134 d
                                   newLine137_65 = runStateT newLine68_135 d
                                   comment138_66 = runStateT comment69_136 d
                                   comments139_67 = runStateT comments70_137 d
                                   notComStr140_68 = runStateT notComStr71_138 d
                                   comEnd141_69 = runStateT comEnd72_139 d
                                   chars142_70 = runStateT (case getToken s of
                                                                Just (c,
                                                                      s') -> do put (parse0_0 (updatePos c pos) s')
                                                                                return c
                                                                _ -> gets derivsPosition >>= (throwError . ParseError "" "end of input" "" undefined [])) d
                pegFile4_71 = foldl1 mplus [do pr <- StateT pragma
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
                                               d160_140 <- get
                                               xx159_141 <- StateT derivsChars
                                               case xx159_141 of
                                                   '|' -> return ()
                                                   _ -> gets derivsPosition >>= (throwError . ParseError "'|'" "not match pattern: " "" d160_140 ["dvChars"])
                                               let '|' = xx159_141
                                               return ()
                                               d162_142 <- get
                                               xx161_143 <- StateT derivsChars
                                               case xx161_143 of
                                                   ']' -> return ()
                                                   _ -> gets derivsPosition >>= (throwError . ParseError "']'" "not match pattern: " "" d162_142 ["dvChars"])
                                               let ']' = xx161_143
                                               return ()
                                               d164_144 <- get
                                               xx163_145 <- StateT derivsChars
                                               case xx163_145 of
                                                   '\n' -> return ()
                                                   _ -> gets derivsPosition >>= (throwError . ParseError "'\\n'" "not match pattern: " "" d164_144 ["dvChars"])
                                               let '\n' = xx163_145
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
                                               d180_146 <- get
                                               xx179_147 <- StateT derivsChars
                                               case xx179_147 of
                                                   '|' -> return ()
                                                   _ -> gets derivsPosition >>= (throwError . ParseError "'|'" "not match pattern: " "" d180_146 ["dvChars"])
                                               let '|' = xx179_147
                                               return ()
                                               d182_148 <- get
                                               xx181_149 <- StateT derivsChars
                                               case xx181_149 of
                                                   ']' -> return ()
                                                   _ -> gets derivsPosition >>= (throwError . ParseError "']'" "not match pattern: " "" d182_148 ["dvChars"])
                                               let ']' = xx181_149
                                               return ()
                                               d184_150 <- get
                                               xx183_151 <- StateT derivsChars
                                               case xx183_151 of
                                                   '\n' -> return ()
                                                   _ -> gets derivsPosition >>= (throwError . ParseError "'\\n'" "not match pattern: " "" d184_150 ["dvChars"])
                                               let '\n' = xx183_151
                                               return ()
                                               atp <- StateT afterPeg
                                               return (mkPegFile pr md emp pp p atp)]
                pragma5_72 = foldl1 mplus [do _ <- StateT spaces
                                              return ()
                                              d190_152 <- get
                                              xx189_153 <- StateT derivsChars
                                              case xx189_153 of
                                                  '{' -> return ()
                                                  _ -> gets derivsPosition >>= (throwError . ParseError "'{'" "not match pattern: " "" d190_152 ["dvChars"])
                                              let '{' = xx189_153
                                              return ()
                                              d192_154 <- get
                                              xx191_155 <- StateT derivsChars
                                              case xx191_155 of
                                                  '-' -> return ()
                                                  _ -> gets derivsPosition >>= (throwError . ParseError "'-'" "not match pattern: " "" d192_154 ["dvChars"])
                                              let '-' = xx191_155
                                              return ()
                                              d194_156 <- get
                                              xx193_157 <- StateT derivsChars
                                              case xx193_157 of
                                                  '#' -> return ()
                                                  _ -> gets derivsPosition >>= (throwError . ParseError "'#'" "not match pattern: " "" d194_156 ["dvChars"])
                                              let '#' = xx193_157
                                              return ()
                                              _ <- StateT spaces
                                              return ()
                                              d198_158 <- get
                                              xx197_159 <- StateT derivsChars
                                              case xx197_159 of
                                                  'L' -> return ()
                                                  _ -> gets derivsPosition >>= (throwError . ParseError "'L'" "not match pattern: " "" d198_158 ["dvChars"])
                                              let 'L' = xx197_159
                                              return ()
                                              d200_160 <- get
                                              xx199_161 <- StateT derivsChars
                                              case xx199_161 of
                                                  'A' -> return ()
                                                  _ -> gets derivsPosition >>= (throwError . ParseError "'A'" "not match pattern: " "" d200_160 ["dvChars"])
                                              let 'A' = xx199_161
                                              return ()
                                              d202_162 <- get
                                              xx201_163 <- StateT derivsChars
                                              case xx201_163 of
                                                  'N' -> return ()
                                                  _ -> gets derivsPosition >>= (throwError . ParseError "'N'" "not match pattern: " "" d202_162 ["dvChars"])
                                              let 'N' = xx201_163
                                              return ()
                                              d204_164 <- get
                                              xx203_165 <- StateT derivsChars
                                              case xx203_165 of
                                                  'G' -> return ()
                                                  _ -> gets derivsPosition >>= (throwError . ParseError "'G'" "not match pattern: " "" d204_164 ["dvChars"])
                                              let 'G' = xx203_165
                                              return ()
                                              d206_166 <- get
                                              xx205_167 <- StateT derivsChars
                                              case xx205_167 of
                                                  'U' -> return ()
                                                  _ -> gets derivsPosition >>= (throwError . ParseError "'U'" "not match pattern: " "" d206_166 ["dvChars"])
                                              let 'U' = xx205_167
                                              return ()
                                              d208_168 <- get
                                              xx207_169 <- StateT derivsChars
                                              case xx207_169 of
                                                  'A' -> return ()
                                                  _ -> gets derivsPosition >>= (throwError . ParseError "'A'" "not match pattern: " "" d208_168 ["dvChars"])
                                              let 'A' = xx207_169
                                              return ()
                                              d210_170 <- get
                                              xx209_171 <- StateT derivsChars
                                              case xx209_171 of
                                                  'G' -> return ()
                                                  _ -> gets derivsPosition >>= (throwError . ParseError "'G'" "not match pattern: " "" d210_170 ["dvChars"])
                                              let 'G' = xx209_171
                                              return ()
                                              d212_172 <- get
                                              xx211_173 <- StateT derivsChars
                                              case xx211_173 of
                                                  'E' -> return ()
                                                  _ -> gets derivsPosition >>= (throwError . ParseError "'E'" "not match pattern: " "" d212_172 ["dvChars"])
                                              let 'E' = xx211_173
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
                pragmaStr6_73 = foldl1 mplus [do _ <- StateT delPragmas
                                                 return ()
                                                 _ <- StateT spaces
                                                 return ()
                                                 d228_174 <- get
                                                 xx227_175 <- StateT derivsChars
                                                 case xx227_175 of
                                                     ',' -> return ()
                                                     _ -> gets derivsPosition >>= (throwError . ParseError "','" "not match pattern: " "" d228_174 ["dvChars"])
                                                 let ',' = xx227_175
                                                 return ()
                                                 _ <- StateT spaces
                                                 return ()
                                                 s <- StateT pragmaStr
                                                 return (' ' : s),
                                              do ddd233_176 <- get
                                                 do err <- ((do _ <- StateT pragmaEnd
                                                                return ()) >> return False) `catchError` const (return True)
                                                    unless err (gets derivsPosition >>= (throwError . ParseError ('!' : "_:pragmaEnd") "not match: " "" ddd233_176 ["pragmaEnd"]))
                                                 put ddd233_176
                                                 ddd236_177 <- get
                                                 do err <- ((do _ <- StateT delPragmas
                                                                return ()) >> return False) `catchError` const (return True)
                                                    unless err (gets derivsPosition >>= (throwError . ParseError ('!' : "_:delPragmas") "not match: " "" ddd236_177 ["delPragmas"]))
                                                 put ddd236_177
                                                 c <- StateT derivsChars
                                                 s <- StateT pragmaStr
                                                 return (c : s),
                                              return emp]
                pragmaItems7_74 = foldl1 mplus [do _ <- StateT delPragmas
                                                   return ()
                                                   _ <- StateT spaces
                                                   return ()
                                                   d248_178 <- get
                                                   xx247_179 <- StateT derivsChars
                                                   case xx247_179 of
                                                       ',' -> return ()
                                                       _ -> gets derivsPosition >>= (throwError . ParseError "','" "not match pattern: " "" d248_178 ["dvChars"])
                                                   let ',' = xx247_179
                                                   return ()
                                                   _ <- StateT spaces
                                                   return ()
                                                   i <- StateT pragmaItems
                                                   return i,
                                                do t <- StateT typToken
                                                   d256_180 <- get
                                                   xx255_181 <- StateT derivsChars
                                                   case xx255_181 of
                                                       ',' -> return ()
                                                       _ -> gets derivsPosition >>= (throwError . ParseError "','" "not match pattern: " "" d256_180 ["dvChars"])
                                                   let ',' = xx255_181
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
                delPragmas8_75 = foldl1 mplus [do d268_182 <- get
                                                  xx267_183 <- StateT derivsChars
                                                  case xx267_183 of
                                                      'Q' -> return ()
                                                      _ -> gets derivsPosition >>= (throwError . ParseError "'Q'" "not match pattern: " "" d268_182 ["dvChars"])
                                                  let 'Q' = xx267_183
                                                  return ()
                                                  d270_184 <- get
                                                  xx269_185 <- StateT derivsChars
                                                  case xx269_185 of
                                                      'u' -> return ()
                                                      _ -> gets derivsPosition >>= (throwError . ParseError "'u'" "not match pattern: " "" d270_184 ["dvChars"])
                                                  let 'u' = xx269_185
                                                  return ()
                                                  d272_186 <- get
                                                  xx271_187 <- StateT derivsChars
                                                  case xx271_187 of
                                                      'a' -> return ()
                                                      _ -> gets derivsPosition >>= (throwError . ParseError "'a'" "not match pattern: " "" d272_186 ["dvChars"])
                                                  let 'a' = xx271_187
                                                  return ()
                                                  d274_188 <- get
                                                  xx273_189 <- StateT derivsChars
                                                  case xx273_189 of
                                                      's' -> return ()
                                                      _ -> gets derivsPosition >>= (throwError . ParseError "'s'" "not match pattern: " "" d274_188 ["dvChars"])
                                                  let 's' = xx273_189
                                                  return ()
                                                  d276_190 <- get
                                                  xx275_191 <- StateT derivsChars
                                                  case xx275_191 of
                                                      'i' -> return ()
                                                      _ -> gets derivsPosition >>= (throwError . ParseError "'i'" "not match pattern: " "" d276_190 ["dvChars"])
                                                  let 'i' = xx275_191
                                                  return ()
                                                  d278_192 <- get
                                                  xx277_193 <- StateT derivsChars
                                                  case xx277_193 of
                                                      'Q' -> return ()
                                                      _ -> gets derivsPosition >>= (throwError . ParseError "'Q'" "not match pattern: " "" d278_192 ["dvChars"])
                                                  let 'Q' = xx277_193
                                                  return ()
                                                  d280_194 <- get
                                                  xx279_195 <- StateT derivsChars
                                                  case xx279_195 of
                                                      'u' -> return ()
                                                      _ -> gets derivsPosition >>= (throwError . ParseError "'u'" "not match pattern: " "" d280_194 ["dvChars"])
                                                  let 'u' = xx279_195
                                                  return ()
                                                  d282_196 <- get
                                                  xx281_197 <- StateT derivsChars
                                                  case xx281_197 of
                                                      'o' -> return ()
                                                      _ -> gets derivsPosition >>= (throwError . ParseError "'o'" "not match pattern: " "" d282_196 ["dvChars"])
                                                  let 'o' = xx281_197
                                                  return ()
                                                  d284_198 <- get
                                                  xx283_199 <- StateT derivsChars
                                                  case xx283_199 of
                                                      't' -> return ()
                                                      _ -> gets derivsPosition >>= (throwError . ParseError "'t'" "not match pattern: " "" d284_198 ["dvChars"])
                                                  let 't' = xx283_199
                                                  return ()
                                                  d286_200 <- get
                                                  xx285_201 <- StateT derivsChars
                                                  case xx285_201 of
                                                      'e' -> return ()
                                                      _ -> gets derivsPosition >>= (throwError . ParseError "'e'" "not match pattern: " "" d286_200 ["dvChars"])
                                                  let 'e' = xx285_201
                                                  return ()
                                                  d288_202 <- get
                                                  xx287_203 <- StateT derivsChars
                                                  case xx287_203 of
                                                      's' -> return ()
                                                      _ -> gets derivsPosition >>= (throwError . ParseError "'s'" "not match pattern: " "" d288_202 ["dvChars"])
                                                  let 's' = xx287_203
                                                  return ()
                                                  return (),
                                               do d290_204 <- get
                                                  xx289_205 <- StateT derivsChars
                                                  case xx289_205 of
                                                      'T' -> return ()
                                                      _ -> gets derivsPosition >>= (throwError . ParseError "'T'" "not match pattern: " "" d290_204 ["dvChars"])
                                                  let 'T' = xx289_205
                                                  return ()
                                                  d292_206 <- get
                                                  xx291_207 <- StateT derivsChars
                                                  case xx291_207 of
                                                      'y' -> return ()
                                                      _ -> gets derivsPosition >>= (throwError . ParseError "'y'" "not match pattern: " "" d292_206 ["dvChars"])
                                                  let 'y' = xx291_207
                                                  return ()
                                                  d294_208 <- get
                                                  xx293_209 <- StateT derivsChars
                                                  case xx293_209 of
                                                      'p' -> return ()
                                                      _ -> gets derivsPosition >>= (throwError . ParseError "'p'" "not match pattern: " "" d294_208 ["dvChars"])
                                                  let 'p' = xx293_209
                                                  return ()
                                                  d296_210 <- get
                                                  xx295_211 <- StateT derivsChars
                                                  case xx295_211 of
                                                      'e' -> return ()
                                                      _ -> gets derivsPosition >>= (throwError . ParseError "'e'" "not match pattern: " "" d296_210 ["dvChars"])
                                                  let 'e' = xx295_211
                                                  return ()
                                                  d298_212 <- get
                                                  xx297_213 <- StateT derivsChars
                                                  case xx297_213 of
                                                      'F' -> return ()
                                                      _ -> gets derivsPosition >>= (throwError . ParseError "'F'" "not match pattern: " "" d298_212 ["dvChars"])
                                                  let 'F' = xx297_213
                                                  return ()
                                                  d300_214 <- get
                                                  xx299_215 <- StateT derivsChars
                                                  case xx299_215 of
                                                      'a' -> return ()
                                                      _ -> gets derivsPosition >>= (throwError . ParseError "'a'" "not match pattern: " "" d300_214 ["dvChars"])
                                                  let 'a' = xx299_215
                                                  return ()
                                                  d302_216 <- get
                                                  xx301_217 <- StateT derivsChars
                                                  case xx301_217 of
                                                      'm' -> return ()
                                                      _ -> gets derivsPosition >>= (throwError . ParseError "'m'" "not match pattern: " "" d302_216 ["dvChars"])
                                                  let 'm' = xx301_217
                                                  return ()
                                                  d304_218 <- get
                                                  xx303_219 <- StateT derivsChars
                                                  case xx303_219 of
                                                      'i' -> return ()
                                                      _ -> gets derivsPosition >>= (throwError . ParseError "'i'" "not match pattern: " "" d304_218 ["dvChars"])
                                                  let 'i' = xx303_219
                                                  return ()
                                                  d306_220 <- get
                                                  xx305_221 <- StateT derivsChars
                                                  case xx305_221 of
                                                      'l' -> return ()
                                                      _ -> gets derivsPosition >>= (throwError . ParseError "'l'" "not match pattern: " "" d306_220 ["dvChars"])
                                                  let 'l' = xx305_221
                                                  return ()
                                                  d308_222 <- get
                                                  xx307_223 <- StateT derivsChars
                                                  case xx307_223 of
                                                      'i' -> return ()
                                                      _ -> gets derivsPosition >>= (throwError . ParseError "'i'" "not match pattern: " "" d308_222 ["dvChars"])
                                                  let 'i' = xx307_223
                                                  return ()
                                                  d310_224 <- get
                                                  xx309_225 <- StateT derivsChars
                                                  case xx309_225 of
                                                      'e' -> return ()
                                                      _ -> gets derivsPosition >>= (throwError . ParseError "'e'" "not match pattern: " "" d310_224 ["dvChars"])
                                                  let 'e' = xx309_225
                                                  return ()
                                                  d312_226 <- get
                                                  xx311_227 <- StateT derivsChars
                                                  case xx311_227 of
                                                      's' -> return ()
                                                      _ -> gets derivsPosition >>= (throwError . ParseError "'s'" "not match pattern: " "" d312_226 ["dvChars"])
                                                  let 's' = xx311_227
                                                  return ()
                                                  return ()]
                pragmaEnd9_76 = foldl1 mplus [do _ <- StateT delPragmas
                                                 return ()
                                                 _ <- StateT spaces
                                                 return ()
                                                 d318_228 <- get
                                                 xx317_229 <- StateT derivsChars
                                                 case xx317_229 of
                                                     '#' -> return ()
                                                     _ -> gets derivsPosition >>= (throwError . ParseError "'#'" "not match pattern: " "" d318_228 ["dvChars"])
                                                 let '#' = xx317_229
                                                 return ()
                                                 d320_230 <- get
                                                 xx319_231 <- StateT derivsChars
                                                 case xx319_231 of
                                                     '-' -> return ()
                                                     _ -> gets derivsPosition >>= (throwError . ParseError "'-'" "not match pattern: " "" d320_230 ["dvChars"])
                                                 let '-' = xx319_231
                                                 return ()
                                                 d322_232 <- get
                                                 xx321_233 <- StateT derivsChars
                                                 case xx321_233 of
                                                     '}' -> return ()
                                                     _ -> gets derivsPosition >>= (throwError . ParseError "'}'" "not match pattern: " "" d322_232 ["dvChars"])
                                                 let '}' = xx321_233
                                                 return ()
                                                 return (),
                                              do d324_234 <- get
                                                 xx323_235 <- StateT derivsChars
                                                 case xx323_235 of
                                                     '#' -> return ()
                                                     _ -> gets derivsPosition >>= (throwError . ParseError "'#'" "not match pattern: " "" d324_234 ["dvChars"])
                                                 let '#' = xx323_235
                                                 return ()
                                                 d326_236 <- get
                                                 xx325_237 <- StateT derivsChars
                                                 case xx325_237 of
                                                     '-' -> return ()
                                                     _ -> gets derivsPosition >>= (throwError . ParseError "'-'" "not match pattern: " "" d326_236 ["dvChars"])
                                                 let '-' = xx325_237
                                                 return ()
                                                 d328_238 <- get
                                                 xx327_239 <- StateT derivsChars
                                                 case xx327_239 of
                                                     '}' -> return ()
                                                     _ -> gets derivsPosition >>= (throwError . ParseError "'}'" "not match pattern: " "" d328_238 ["dvChars"])
                                                 let '}' = xx327_239
                                                 return ()
                                                 return ()]
                moduleDec10_77 = foldl1 mplus [do d330_240 <- get
                                                  xx329_241 <- StateT derivsChars
                                                  case xx329_241 of
                                                      'm' -> return ()
                                                      _ -> gets derivsPosition >>= (throwError . ParseError "'m'" "not match pattern: " "" d330_240 ["dvChars"])
                                                  let 'm' = xx329_241
                                                  return ()
                                                  d332_242 <- get
                                                  xx331_243 <- StateT derivsChars
                                                  case xx331_243 of
                                                      'o' -> return ()
                                                      _ -> gets derivsPosition >>= (throwError . ParseError "'o'" "not match pattern: " "" d332_242 ["dvChars"])
                                                  let 'o' = xx331_243
                                                  return ()
                                                  d334_244 <- get
                                                  xx333_245 <- StateT derivsChars
                                                  case xx333_245 of
                                                      'd' -> return ()
                                                      _ -> gets derivsPosition >>= (throwError . ParseError "'d'" "not match pattern: " "" d334_244 ["dvChars"])
                                                  let 'd' = xx333_245
                                                  return ()
                                                  d336_246 <- get
                                                  xx335_247 <- StateT derivsChars
                                                  case xx335_247 of
                                                      'u' -> return ()
                                                      _ -> gets derivsPosition >>= (throwError . ParseError "'u'" "not match pattern: " "" d336_246 ["dvChars"])
                                                  let 'u' = xx335_247
                                                  return ()
                                                  d338_248 <- get
                                                  xx337_249 <- StateT derivsChars
                                                  case xx337_249 of
                                                      'l' -> return ()
                                                      _ -> gets derivsPosition >>= (throwError . ParseError "'l'" "not match pattern: " "" d338_248 ["dvChars"])
                                                  let 'l' = xx337_249
                                                  return ()
                                                  d340_250 <- get
                                                  xx339_251 <- StateT derivsChars
                                                  case xx339_251 of
                                                      'e' -> return ()
                                                      _ -> gets derivsPosition >>= (throwError . ParseError "'e'" "not match pattern: " "" d340_250 ["dvChars"])
                                                  let 'e' = xx339_251
                                                  return ()
                                                  _ <- StateT spaces
                                                  return ()
                                                  n <- StateT moduleName
                                                  s <- StateT moduleDecStr
                                                  _ <- StateT whr
                                                  return ()
                                                  return (just (n, s)),
                                               return nothing]
                moduleName11_78 = foldl1 mplus [do t <- StateT typ
                                                   d352_252 <- get
                                                   xx351_253 <- StateT derivsChars
                                                   case xx351_253 of
                                                       '.' -> return ()
                                                       _ -> gets derivsPosition >>= (throwError . ParseError "'.'" "not match pattern: " "" d352_252 ["dvChars"])
                                                   let '.' = xx351_253
                                                   return ()
                                                   n <- StateT moduleName
                                                   return (t : n),
                                                do t <- StateT typ
                                                   return [t]]
                moduleDecStr12_79 = foldl1 mplus [do ddd357_254 <- get
                                                     do err <- ((do _ <- StateT whr
                                                                    return ()) >> return False) `catchError` const (return True)
                                                        unless err (gets derivsPosition >>= (throwError . ParseError ('!' : "_:whr") "not match: " "" ddd357_254 ["whr"]))
                                                     put ddd357_254
                                                     c <- StateT derivsChars
                                                     s <- StateT moduleDecStr
                                                     return (cons c s),
                                                  return emp]
                whr13_80 = foldl1 mplus [do d365_255 <- get
                                            xx364_256 <- StateT derivsChars
                                            case xx364_256 of
                                                'w' -> return ()
                                                _ -> gets derivsPosition >>= (throwError . ParseError "'w'" "not match pattern: " "" d365_255 ["dvChars"])
                                            let 'w' = xx364_256
                                            return ()
                                            d367_257 <- get
                                            xx366_258 <- StateT derivsChars
                                            case xx366_258 of
                                                'h' -> return ()
                                                _ -> gets derivsPosition >>= (throwError . ParseError "'h'" "not match pattern: " "" d367_257 ["dvChars"])
                                            let 'h' = xx366_258
                                            return ()
                                            d369_259 <- get
                                            xx368_260 <- StateT derivsChars
                                            case xx368_260 of
                                                'e' -> return ()
                                                _ -> gets derivsPosition >>= (throwError . ParseError "'e'" "not match pattern: " "" d369_259 ["dvChars"])
                                            let 'e' = xx368_260
                                            return ()
                                            d371_261 <- get
                                            xx370_262 <- StateT derivsChars
                                            case xx370_262 of
                                                'r' -> return ()
                                                _ -> gets derivsPosition >>= (throwError . ParseError "'r'" "not match pattern: " "" d371_261 ["dvChars"])
                                            let 'r' = xx370_262
                                            return ()
                                            d373_263 <- get
                                            xx372_264 <- StateT derivsChars
                                            case xx372_264 of
                                                'e' -> return ()
                                                _ -> gets derivsPosition >>= (throwError . ParseError "'e'" "not match pattern: " "" d373_263 ["dvChars"])
                                            let 'e' = xx372_264
                                            return ()
                                            return ()]
                preImpPap14_81 = foldl1 mplus [do ddd374_265 <- get
                                                  do err <- ((do _ <- StateT importPapillon
                                                                 return ()) >> return False) `catchError` const (return True)
                                                     unless err (gets derivsPosition >>= (throwError . ParseError ('!' : "_:importPapillon") "not match: " "" ddd374_265 ["importPapillon"]))
                                                  put ddd374_265
                                                  ddd377_266 <- get
                                                  do err <- ((do _ <- StateT pap
                                                                 return ()) >> return False) `catchError` const (return True)
                                                     unless err (gets derivsPosition >>= (throwError . ParseError ('!' : "_:pap") "not match: " "" ddd377_266 ["pap"]))
                                                  put ddd377_266
                                                  c <- StateT derivsChars
                                                  pip <- StateT preImpPap
                                                  return (cons c pip),
                                               return emp]
                prePeg15_82 = foldl1 mplus [do ddd384_267 <- get
                                               do err <- ((do _ <- StateT pap
                                                              return ()) >> return False) `catchError` const (return True)
                                                  unless err (gets derivsPosition >>= (throwError . ParseError ('!' : "_:pap") "not match: " "" ddd384_267 ["pap"]))
                                               put ddd384_267
                                               c <- StateT derivsChars
                                               pp <- StateT prePeg
                                               return (cons c pp),
                                            return emp]
                afterPeg16_83 = foldl1 mplus [do c <- StateT derivsChars
                                                 atp <- StateT afterPeg
                                                 return (cons c atp),
                                              return emp]
                importPapillon17_84 = foldl1 mplus [do d396_268 <- get
                                                       xx395_269 <- StateT varToken
                                                       case xx395_269 of
                                                           "import" -> return ()
                                                           _ -> gets derivsPosition >>= (throwError . ParseError "\"import\"" "not match pattern: " "" d396_268 ["varToken"])
                                                       let "import" = xx395_269
                                                       return ()
                                                       d398_270 <- get
                                                       xx397_271 <- StateT typToken
                                                       case xx397_271 of
                                                           "Text" -> return ()
                                                           _ -> gets derivsPosition >>= (throwError . ParseError "\"Text\"" "not match pattern: " "" d398_270 ["typToken"])
                                                       let "Text" = xx397_271
                                                       return ()
                                                       d400_272 <- get
                                                       xx399_273 <- StateT derivsChars
                                                       case xx399_273 of
                                                           '.' -> return ()
                                                           _ -> gets derivsPosition >>= (throwError . ParseError "'.'" "not match pattern: " "" d400_272 ["dvChars"])
                                                       let '.' = xx399_273
                                                       return ()
                                                       _ <- StateT spaces
                                                       return ()
                                                       d404_274 <- get
                                                       xx403_275 <- StateT typToken
                                                       case xx403_275 of
                                                           "Papillon" -> return ()
                                                           _ -> gets derivsPosition >>= (throwError . ParseError "\"Papillon\"" "not match pattern: " "" d404_274 ["typToken"])
                                                       let "Papillon" = xx403_275
                                                       return ()
                                                       ddd405_276 <- get
                                                       do err <- ((do d407_277 <- get
                                                                      xx406_278 <- StateT derivsChars
                                                                      case xx406_278 of
                                                                          '.' -> return ()
                                                                          _ -> gets derivsPosition >>= (throwError . ParseError "'.'" "not match pattern: " "" d407_277 ["dvChars"])
                                                                      let '.' = xx406_278
                                                                      return ()) >> return False) `catchError` const (return True)
                                                          unless err (gets derivsPosition >>= (throwError . ParseError ('!' : "'.':") "not match: " "" ddd405_276 ["dvChars"]))
                                                       put ddd405_276
                                                       return ()]
                varToken18_85 = foldl1 mplus [do v <- StateT variable
                                                 _ <- StateT spaces
                                                 return ()
                                                 return v]
                typToken19_86 = foldl1 mplus [do t <- StateT typ
                                                 _ <- StateT spaces
                                                 return ()
                                                 return t]
                pap20_87 = foldl1 mplus [do d417_279 <- get
                                            xx416_280 <- StateT derivsChars
                                            case xx416_280 of
                                                '\n' -> return ()
                                                _ -> gets derivsPosition >>= (throwError . ParseError "'\\n'" "not match pattern: " "" d417_279 ["dvChars"])
                                            let '\n' = xx416_280
                                            return ()
                                            d419_281 <- get
                                            xx418_282 <- StateT derivsChars
                                            case xx418_282 of
                                                '[' -> return ()
                                                _ -> gets derivsPosition >>= (throwError . ParseError "'['" "not match pattern: " "" d419_281 ["dvChars"])
                                            let '[' = xx418_282
                                            return ()
                                            d421_283 <- get
                                            xx420_284 <- StateT derivsChars
                                            case xx420_284 of
                                                'p' -> return ()
                                                _ -> gets derivsPosition >>= (throwError . ParseError "'p'" "not match pattern: " "" d421_283 ["dvChars"])
                                            let 'p' = xx420_284
                                            return ()
                                            d423_285 <- get
                                            xx422_286 <- StateT derivsChars
                                            case xx422_286 of
                                                'a' -> return ()
                                                _ -> gets derivsPosition >>= (throwError . ParseError "'a'" "not match pattern: " "" d423_285 ["dvChars"])
                                            let 'a' = xx422_286
                                            return ()
                                            d425_287 <- get
                                            xx424_288 <- StateT derivsChars
                                            case xx424_288 of
                                                'p' -> return ()
                                                _ -> gets derivsPosition >>= (throwError . ParseError "'p'" "not match pattern: " "" d425_287 ["dvChars"])
                                            let 'p' = xx424_288
                                            return ()
                                            d427_289 <- get
                                            xx426_290 <- StateT derivsChars
                                            case xx426_290 of
                                                'i' -> return ()
                                                _ -> gets derivsPosition >>= (throwError . ParseError "'i'" "not match pattern: " "" d427_289 ["dvChars"])
                                            let 'i' = xx426_290
                                            return ()
                                            d429_291 <- get
                                            xx428_292 <- StateT derivsChars
                                            case xx428_292 of
                                                'l' -> return ()
                                                _ -> gets derivsPosition >>= (throwError . ParseError "'l'" "not match pattern: " "" d429_291 ["dvChars"])
                                            let 'l' = xx428_292
                                            return ()
                                            d431_293 <- get
                                            xx430_294 <- StateT derivsChars
                                            case xx430_294 of
                                                'l' -> return ()
                                                _ -> gets derivsPosition >>= (throwError . ParseError "'l'" "not match pattern: " "" d431_293 ["dvChars"])
                                            let 'l' = xx430_294
                                            return ()
                                            d433_295 <- get
                                            xx432_296 <- StateT derivsChars
                                            case xx432_296 of
                                                'o' -> return ()
                                                _ -> gets derivsPosition >>= (throwError . ParseError "'o'" "not match pattern: " "" d433_295 ["dvChars"])
                                            let 'o' = xx432_296
                                            return ()
                                            d435_297 <- get
                                            xx434_298 <- StateT derivsChars
                                            case xx434_298 of
                                                'n' -> return ()
                                                _ -> gets derivsPosition >>= (throwError . ParseError "'n'" "not match pattern: " "" d435_297 ["dvChars"])
                                            let 'n' = xx434_298
                                            return ()
                                            d437_299 <- get
                                            xx436_300 <- StateT derivsChars
                                            case xx436_300 of
                                                '|' -> return ()
                                                _ -> gets derivsPosition >>= (throwError . ParseError "'|'" "not match pattern: " "" d437_299 ["dvChars"])
                                            let '|' = xx436_300
                                            return ()
                                            d439_301 <- get
                                            xx438_302 <- StateT derivsChars
                                            case xx438_302 of
                                                '\n' -> return ()
                                                _ -> gets derivsPosition >>= (throwError . ParseError "'\\n'" "not match pattern: " "" d439_301 ["dvChars"])
                                            let '\n' = xx438_302
                                            return ()
                                            return ()]
                peg21_88 = foldl1 mplus [do _ <- StateT spaces
                                            return ()
                                            s <- StateT sourceType
                                            p <- StateT peg_
                                            return (mkTTPeg s p),
                                         do p <- StateT peg_
                                            return (mkTTPeg tString p)]
                sourceType22_89 = foldl1 mplus [do d449_303 <- get
                                                   xx448_304 <- StateT varToken
                                                   case xx448_304 of
                                                       "source" -> return ()
                                                       _ -> gets derivsPosition >>= (throwError . ParseError "\"source\"" "not match pattern: " "" d449_303 ["varToken"])
                                                   let "source" = xx448_304
                                                   return ()
                                                   d451_305 <- get
                                                   xx450_306 <- StateT derivsChars
                                                   case xx450_306 of
                                                       ':' -> return ()
                                                       _ -> gets derivsPosition >>= (throwError . ParseError "':'" "not match pattern: " "" d451_305 ["dvChars"])
                                                   let ':' = xx450_306
                                                   return ()
                                                   _ <- StateT spaces
                                                   return ()
                                                   v <- StateT typToken
                                                   return v]
                peg_23_90 = foldl1 mplus [do _ <- StateT spaces
                                             return ()
                                             d <- StateT definition
                                             p <- StateT peg_
                                             return (cons d p),
                                          return emp]
                definition24_91 = foldl1 mplus [do v <- StateT variable
                                                   _ <- StateT spaces
                                                   return ()
                                                   d467_307 <- get
                                                   xx466_308 <- StateT derivsChars
                                                   case xx466_308 of
                                                       ':' -> return ()
                                                       _ -> gets derivsPosition >>= (throwError . ParseError "':'" "not match pattern: " "" d467_307 ["dvChars"])
                                                   let ':' = xx466_308
                                                   return ()
                                                   d469_309 <- get
                                                   xx468_310 <- StateT derivsChars
                                                   case xx468_310 of
                                                       ':' -> return ()
                                                       _ -> gets derivsPosition >>= (throwError . ParseError "':'" "not match pattern: " "" d469_309 ["dvChars"])
                                                   let ':' = xx468_310
                                                   return ()
                                                   _ <- StateT spaces
                                                   return ()
                                                   t <- StateT hsTypeArr
                                                   _ <- StateT spaces
                                                   return ()
                                                   d477_311 <- get
                                                   xx476_312 <- StateT derivsChars
                                                   case xx476_312 of
                                                       '=' -> return ()
                                                       _ -> gets derivsPosition >>= (throwError . ParseError "'='" "not match pattern: " "" d477_311 ["dvChars"])
                                                   let '=' = xx476_312
                                                   return ()
                                                   _ <- StateT spaces
                                                   return ()
                                                   sel <- StateT selection
                                                   _ <- StateT spaces
                                                   return ()
                                                   d485_313 <- get
                                                   xx484_314 <- StateT derivsChars
                                                   case xx484_314 of
                                                       ';' -> return ()
                                                       _ -> gets derivsPosition >>= (throwError . ParseError "';'" "not match pattern: " "" d485_313 ["dvChars"])
                                                   let ';' = xx484_314
                                                   return ()
                                                   return (mkDef v t sel)]
                selection25_92 = foldl1 mplus [do ex <- StateT expressionHs
                                                  _ <- StateT spaces
                                                  return ()
                                                  d491_315 <- get
                                                  xx490_316 <- StateT derivsChars
                                                  case xx490_316 of
                                                      '/' -> return ()
                                                      _ -> gets derivsPosition >>= (throwError . ParseError "'/'" "not match pattern: " "" d491_315 ["dvChars"])
                                                  let '/' = xx490_316
                                                  return ()
                                                  _ <- StateT spaces
                                                  return ()
                                                  sel <- StateT selection
                                                  return (cons ex sel),
                                               do ex <- StateT expressionHs
                                                  return (cons ex emp)]
                expressionHs26_93 = foldl1 mplus [do e <- StateT expression
                                                     _ <- StateT spaces
                                                     return ()
                                                     d503_317 <- get
                                                     xx502_318 <- StateT derivsChars
                                                     case xx502_318 of
                                                         '{' -> return ()
                                                         _ -> gets derivsPosition >>= (throwError . ParseError "'{'" "not match pattern: " "" d503_317 ["dvChars"])
                                                     let '{' = xx502_318
                                                     return ()
                                                     _ <- StateT spaces
                                                     return ()
                                                     h <- StateT hsExpLam
                                                     _ <- StateT spaces
                                                     return ()
                                                     d511_319 <- get
                                                     xx510_320 <- StateT derivsChars
                                                     case xx510_320 of
                                                         '}' -> return ()
                                                         _ -> gets derivsPosition >>= (throwError . ParseError "'}'" "not match pattern: " "" d511_319 ["dvChars"])
                                                     let '}' = xx510_320
                                                     return ()
                                                     return (mkExpressionHs e h)]
                expression27_94 = foldl1 mplus [do l <- StateT nameLeaf_
                                                   _ <- StateT spaces
                                                   return ()
                                                   e <- StateT expression
                                                   return (cons l e),
                                                return emp]
                nameLeaf_28_95 = foldl1 mplus [do d519_321 <- get
                                                  xx518_322 <- StateT derivsChars
                                                  case xx518_322 of
                                                      '!' -> return ()
                                                      _ -> gets derivsPosition >>= (throwError . ParseError "'!'" "not match pattern: " "" d519_321 ["dvChars"])
                                                  let '!' = xx518_322
                                                  return ()
                                                  nl <- StateT nameLeafNoCom
                                                  _ <- StateT spaces
                                                  return ()
                                                  com <- optional3_323 (StateT comForErr)
                                                  return (NotAfter nl $ maybe "" id com),
                                               do d527_324 <- get
                                                  xx526_325 <- StateT derivsChars
                                                  let c = xx526_325
                                                  unless (isAmp c) (gets derivsPosition >>= (throwError . ParseError "isAmp c" "not match: " "" d527_324 ["dvChars"]))
                                                  nl <- StateT nameLeaf
                                                  return (After nl),
                                               do nl <- StateT nameLeaf
                                                  return (Here nl)]
                nameLeaf29_96 = foldl1 mplus [do n <- StateT pat1
                                                 _ <- StateT spaces
                                                 return ()
                                                 com <- optional3_323 (StateT comForErr)
                                                 d539_326 <- get
                                                 xx538_327 <- StateT derivsChars
                                                 case xx538_327 of
                                                     ':' -> return ()
                                                     _ -> gets derivsPosition >>= (throwError . ParseError "':'" "not match pattern: " "" d539_326 ["dvChars"])
                                                 let ':' = xx538_327
                                                 return ()
                                                 (rf, p) <- StateT leaf
                                                 return (NameLeaf (n, maybe "" id com) rf p),
                                              do n <- StateT pat1
                                                 _ <- StateT spaces
                                                 return ()
                                                 com <- optional3_323 (StateT comForErr)
                                                 return (NameLeaf (n,
                                                                   maybe "" id com) FromToken Nothing)]
                nameLeafNoCom30_97 = foldl1 mplus [do n <- StateT pat1
                                                      _ <- StateT spaces
                                                      return ()
                                                      com <- optional3_323 (StateT comForErr)
                                                      d555_328 <- get
                                                      xx554_329 <- StateT derivsChars
                                                      case xx554_329 of
                                                          ':' -> return ()
                                                          _ -> gets derivsPosition >>= (throwError . ParseError "':'" "not match pattern: " "" d555_328 ["dvChars"])
                                                      let ':' = xx554_329
                                                      return ()
                                                      (rf, p) <- StateT leaf
                                                      return (NameLeaf (n, maybe "" id com) rf p),
                                                   do n <- StateT pat1
                                                      _ <- StateT spaces
                                                      return ()
                                                      return (NameLeaf (n, "") FromToken Nothing)]
                comForErr31_98 = foldl1 mplus [do d563_330 <- get
                                                  xx562_331 <- StateT derivsChars
                                                  case xx562_331 of
                                                      '{' -> return ()
                                                      _ -> gets derivsPosition >>= (throwError . ParseError "'{'" "not match pattern: " "" d563_330 ["dvChars"])
                                                  let '{' = xx562_331
                                                  return ()
                                                  d565_332 <- get
                                                  xx564_333 <- StateT derivsChars
                                                  case xx564_333 of
                                                      '-' -> return ()
                                                      _ -> gets derivsPosition >>= (throwError . ParseError "'-'" "not match pattern: " "" d565_332 ["dvChars"])
                                                  let '-' = xx564_333
                                                  return ()
                                                  d567_334 <- get
                                                  xx566_335 <- StateT derivsChars
                                                  case xx566_335 of
                                                      '#' -> return ()
                                                      _ -> gets derivsPosition >>= (throwError . ParseError "'#'" "not match pattern: " "" d567_334 ["dvChars"])
                                                  let '#' = xx566_335
                                                  return ()
                                                  _ <- StateT spaces
                                                  return ()
                                                  d571_336 <- get
                                                  xx570_337 <- StateT derivsChars
                                                  case xx570_337 of
                                                      '"' -> return ()
                                                      _ -> gets derivsPosition >>= (throwError . ParseError "'\"'" "not match pattern: " "" d571_336 ["dvChars"])
                                                  let '"' = xx570_337
                                                  return ()
                                                  s <- StateT stringLit
                                                  d575_338 <- get
                                                  xx574_339 <- StateT derivsChars
                                                  case xx574_339 of
                                                      '"' -> return ()
                                                      _ -> gets derivsPosition >>= (throwError . ParseError "'\"'" "not match pattern: " "" d575_338 ["dvChars"])
                                                  let '"' = xx574_339
                                                  return ()
                                                  _ <- StateT spaces
                                                  return ()
                                                  d579_340 <- get
                                                  xx578_341 <- StateT derivsChars
                                                  case xx578_341 of
                                                      '#' -> return ()
                                                      _ -> gets derivsPosition >>= (throwError . ParseError "'#'" "not match pattern: " "" d579_340 ["dvChars"])
                                                  let '#' = xx578_341
                                                  return ()
                                                  d581_342 <- get
                                                  xx580_343 <- StateT derivsChars
                                                  case xx580_343 of
                                                      '-' -> return ()
                                                      _ -> gets derivsPosition >>= (throwError . ParseError "'-'" "not match pattern: " "" d581_342 ["dvChars"])
                                                  let '-' = xx580_343
                                                  return ()
                                                  d583_344 <- get
                                                  xx582_345 <- StateT derivsChars
                                                  case xx582_345 of
                                                      '}' -> return ()
                                                      _ -> gets derivsPosition >>= (throwError . ParseError "'}'" "not match pattern: " "" d583_344 ["dvChars"])
                                                  let '}' = xx582_345
                                                  return ()
                                                  _ <- StateT spaces
                                                  return ()
                                                  return s]
                leaf32_99 = foldl1 mplus [do rf <- StateT readFromLs
                                             t <- StateT test
                                             return (rf, Just t),
                                          do rf <- StateT readFromLs
                                             return (rf, Nothing),
                                          do t <- StateT test
                                             return (FromToken, Just t)]
                patOp33_100 = foldl1 mplus [do p <- StateT pat
                                               o <- StateT opConName
                                               po <- StateT patOp
                                               return (uInfixP p o po),
                                            do p <- StateT pat
                                               _ <- StateT spaces
                                               return ()
                                               d605_346 <- get
                                               xx604_347 <- StateT derivsChars
                                               let q = xx604_347
                                               unless (isBQ q) (gets derivsPosition >>= (throwError . ParseError "isBQ q" "not match: " "" d605_346 ["dvChars"]))
                                               t <- StateT typ
                                               d609_348 <- get
                                               xx608_349 <- StateT derivsChars
                                               let q_ = xx608_349
                                               unless (isBQ q_) (gets derivsPosition >>= (throwError . ParseError "isBQ q_" "not match: " "" d609_348 ["dvChars"]))
                                               _ <- StateT spaces
                                               return ()
                                               po <- StateT patOp
                                               return (uInfixP p (mkName t) po),
                                            do p <- StateT pat
                                               return p]
                pat34_101 = foldl1 mplus [do t <- StateT typ
                                             _ <- StateT spaces
                                             return ()
                                             ps <- StateT pats
                                             return (conToPatQ t ps),
                                          do d623_350 <- get
                                             xx622_351 <- StateT derivsChars
                                             case xx622_351 of
                                                 '(' -> return ()
                                                 _ -> gets derivsPosition >>= (throwError . ParseError "'('" "not match pattern: " "" d623_350 ["dvChars"])
                                             let '(' = xx622_351
                                             return ()
                                             o <- StateT opConName
                                             d627_352 <- get
                                             xx626_353 <- StateT derivsChars
                                             case xx626_353 of
                                                 ')' -> return ()
                                                 _ -> gets derivsPosition >>= (throwError . ParseError "')'" "not match pattern: " "" d627_352 ["dvChars"])
                                             let ')' = xx626_353
                                             return ()
                                             _ <- StateT spaces
                                             return ()
                                             ps <- StateT pats
                                             return (conP o ps),
                                          do p <- StateT pat1
                                             return p]
                pat135_102 = foldl1 mplus [do t <- StateT typ
                                              return (conToPatQ t emp),
                                           do d637_354 <- get
                                              xx636_355 <- StateT variable
                                              case xx636_355 of
                                                  "_" -> return ()
                                                  _ -> gets derivsPosition >>= (throwError . ParseError "\"_\"" "not match pattern: " "" d637_354 ["variable"])
                                              let "_" = xx636_355
                                              return ()
                                              return wildP,
                                           do n <- StateT variable
                                              return (strToPatQ n),
                                           do i <- StateT integer
                                              return (litP (integerL i)),
                                           do d643_356 <- get
                                              xx642_357 <- StateT derivsChars
                                              case xx642_357 of
                                                  '-' -> return ()
                                                  _ -> gets derivsPosition >>= (throwError . ParseError "'-'" "not match pattern: " "" d643_356 ["dvChars"])
                                              let '-' = xx642_357
                                              return ()
                                              _ <- StateT spaces
                                              return ()
                                              i <- StateT integer
                                              return (litP (integerL $ negate i)),
                                           do d649_358 <- get
                                              xx648_359 <- StateT derivsChars
                                              case xx648_359 of
                                                  '\'' -> return ()
                                                  _ -> gets derivsPosition >>= (throwError . ParseError "'\\''" "not match pattern: " "" d649_358 ["dvChars"])
                                              let '\'' = xx648_359
                                              return ()
                                              c <- StateT charLit
                                              d653_360 <- get
                                              xx652_361 <- StateT derivsChars
                                              case xx652_361 of
                                                  '\'' -> return ()
                                                  _ -> gets derivsPosition >>= (throwError . ParseError "'\\''" "not match pattern: " "" d653_360 ["dvChars"])
                                              let '\'' = xx652_361
                                              return ()
                                              return (charP c),
                                           do d655_362 <- get
                                              xx654_363 <- StateT derivsChars
                                              case xx654_363 of
                                                  '"' -> return ()
                                                  _ -> gets derivsPosition >>= (throwError . ParseError "'\"'" "not match pattern: " "" d655_362 ["dvChars"])
                                              let '"' = xx654_363
                                              return ()
                                              s <- StateT stringLit
                                              d659_364 <- get
                                              xx658_365 <- StateT derivsChars
                                              case xx658_365 of
                                                  '"' -> return ()
                                                  _ -> gets derivsPosition >>= (throwError . ParseError "'\"'" "not match pattern: " "" d659_364 ["dvChars"])
                                              let '"' = xx658_365
                                              return ()
                                              return (stringP s),
                                           do d661_366 <- get
                                              xx660_367 <- StateT derivsChars
                                              case xx660_367 of
                                                  '(' -> return ()
                                                  _ -> gets derivsPosition >>= (throwError . ParseError "'('" "not match pattern: " "" d661_366 ["dvChars"])
                                              let '(' = xx660_367
                                              return ()
                                              p <- StateT patList
                                              d665_368 <- get
                                              xx664_369 <- StateT derivsChars
                                              case xx664_369 of
                                                  ')' -> return ()
                                                  _ -> gets derivsPosition >>= (throwError . ParseError "')'" "not match pattern: " "" d665_368 ["dvChars"])
                                              let ')' = xx664_369
                                              return ()
                                              return (tupP p),
                                           do d667_370 <- get
                                              xx666_371 <- StateT derivsChars
                                              case xx666_371 of
                                                  '[' -> return ()
                                                  _ -> gets derivsPosition >>= (throwError . ParseError "'['" "not match pattern: " "" d667_370 ["dvChars"])
                                              let '[' = xx666_371
                                              return ()
                                              p <- StateT patList
                                              d671_372 <- get
                                              xx670_373 <- StateT derivsChars
                                              case xx670_373 of
                                                  ']' -> return ()
                                                  _ -> gets derivsPosition >>= (throwError . ParseError "']'" "not match pattern: " "" d671_372 ["dvChars"])
                                              let ']' = xx670_373
                                              return ()
                                              return (listP p)]
                patList36_103 = foldl1 mplus [do p <- StateT patOp
                                                 _ <- StateT spaces
                                                 return ()
                                                 d677_374 <- get
                                                 xx676_375 <- StateT derivsChars
                                                 case xx676_375 of
                                                     ',' -> return ()
                                                     _ -> gets derivsPosition >>= (throwError . ParseError "','" "not match pattern: " "" d677_374 ["dvChars"])
                                                 let ',' = xx676_375
                                                 return ()
                                                 _ <- StateT spaces
                                                 return ()
                                                 ps <- StateT patList
                                                 return (p : ps),
                                              do p <- StateT patOp
                                                 return [p],
                                              return []]
                opConName37_104 = foldl1 mplus [do d685_376 <- get
                                                   xx684_377 <- StateT derivsChars
                                                   case xx684_377 of
                                                       ':' -> return ()
                                                       _ -> gets derivsPosition >>= (throwError . ParseError "':'" "not match pattern: " "" d685_376 ["dvChars"])
                                                   let ':' = xx684_377
                                                   return ()
                                                   ot <- StateT opTail
                                                   return (mkName $ colon : ot)]
                charLit38_105 = foldl1 mplus [do d689_378 <- get
                                                 xx688_379 <- StateT derivsChars
                                                 let c = xx688_379
                                                 unless (isAlphaNumOt c) (gets derivsPosition >>= (throwError . ParseError "isAlphaNumOt c" "not match: " "" d689_378 ["dvChars"]))
                                                 return c,
                                              do d691_380 <- get
                                                 xx690_381 <- StateT derivsChars
                                                 case xx690_381 of
                                                     '\\' -> return ()
                                                     _ -> gets derivsPosition >>= (throwError . ParseError "'\\\\'" "not match pattern: " "" d691_380 ["dvChars"])
                                                 let '\\' = xx690_381
                                                 return ()
                                                 c <- StateT escapeC
                                                 return c]
                stringLit39_106 = foldl1 mplus [do d695_382 <- get
                                                   xx694_383 <- StateT derivsChars
                                                   let c = xx694_383
                                                   unless (isStrLitC c) (gets derivsPosition >>= (throwError . ParseError "isStrLitC c" "not match: " "" d695_382 ["dvChars"]))
                                                   s <- StateT stringLit
                                                   return (cons c s),
                                                do d699_384 <- get
                                                   xx698_385 <- StateT derivsChars
                                                   case xx698_385 of
                                                       '\\' -> return ()
                                                       _ -> gets derivsPosition >>= (throwError . ParseError "'\\\\'" "not match pattern: " "" d699_384 ["dvChars"])
                                                   let '\\' = xx698_385
                                                   return ()
                                                   c <- StateT escapeC
                                                   s <- StateT stringLit
                                                   return (c : s),
                                                return emp]
                escapeC40_107 = foldl1 mplus [do d705_386 <- get
                                                 xx704_387 <- StateT derivsChars
                                                 case xx704_387 of
                                                     '"' -> return ()
                                                     _ -> gets derivsPosition >>= (throwError . ParseError "'\"'" "not match pattern: " "" d705_386 ["dvChars"])
                                                 let '"' = xx704_387
                                                 return ()
                                                 return '"',
                                              do d707_388 <- get
                                                 xx706_389 <- StateT derivsChars
                                                 case xx706_389 of
                                                     '\'' -> return ()
                                                     _ -> gets derivsPosition >>= (throwError . ParseError "'\\''" "not match pattern: " "" d707_388 ["dvChars"])
                                                 let '\'' = xx706_389
                                                 return ()
                                                 return '\'',
                                              do d709_390 <- get
                                                 xx708_391 <- StateT derivsChars
                                                 case xx708_391 of
                                                     '\\' -> return ()
                                                     _ -> gets derivsPosition >>= (throwError . ParseError "'\\\\'" "not match pattern: " "" d709_390 ["dvChars"])
                                                 let '\\' = xx708_391
                                                 return ()
                                                 return '\\',
                                              do d711_392 <- get
                                                 xx710_393 <- StateT derivsChars
                                                 case xx710_393 of
                                                     'n' -> return ()
                                                     _ -> gets derivsPosition >>= (throwError . ParseError "'n'" "not match pattern: " "" d711_392 ["dvChars"])
                                                 let 'n' = xx710_393
                                                 return ()
                                                 return '\n',
                                              do d713_394 <- get
                                                 xx712_395 <- StateT derivsChars
                                                 case xx712_395 of
                                                     't' -> return ()
                                                     _ -> gets derivsPosition >>= (throwError . ParseError "'t'" "not match pattern: " "" d713_394 ["dvChars"])
                                                 let 't' = xx712_395
                                                 return ()
                                                 return tab]
                pats41_108 = foldl1 mplus [do p <- StateT pat
                                              _ <- StateT spaces
                                              return ()
                                              ps <- StateT pats
                                              return (cons p ps),
                                           return emp]
                readFromLs42_109 = foldl1 mplus [do rf <- StateT readFrom
                                                    d723_396 <- get
                                                    xx722_397 <- StateT derivsChars
                                                    case xx722_397 of
                                                        '*' -> return ()
                                                        _ -> gets derivsPosition >>= (throwError . ParseError "'*'" "not match pattern: " "" d723_396 ["dvChars"])
                                                    let '*' = xx722_397
                                                    return ()
                                                    return (FromList rf),
                                                 do rf <- StateT readFrom
                                                    d727_398 <- get
                                                    xx726_399 <- StateT derivsChars
                                                    case xx726_399 of
                                                        '+' -> return ()
                                                        _ -> gets derivsPosition >>= (throwError . ParseError "'+'" "not match pattern: " "" d727_398 ["dvChars"])
                                                    let '+' = xx726_399
                                                    return ()
                                                    return (FromList1 rf),
                                                 do rf <- StateT readFrom
                                                    d731_400 <- get
                                                    xx730_401 <- StateT derivsChars
                                                    case xx730_401 of
                                                        '?' -> return ()
                                                        _ -> gets derivsPosition >>= (throwError . ParseError "'?'" "not match pattern: " "" d731_400 ["dvChars"])
                                                    let '?' = xx730_401
                                                    return ()
                                                    return (FromOptional rf),
                                                 do rf <- StateT readFrom
                                                    return rf]
                readFrom43_110 = foldl1 mplus [do v <- StateT variable
                                                  return (FromVariable v),
                                               do d737_402 <- get
                                                  xx736_403 <- StateT derivsChars
                                                  case xx736_403 of
                                                      '(' -> return ()
                                                      _ -> gets derivsPosition >>= (throwError . ParseError "'('" "not match pattern: " "" d737_402 ["dvChars"])
                                                  let '(' = xx736_403
                                                  return ()
                                                  s <- StateT selection
                                                  d741_404 <- get
                                                  xx740_405 <- StateT derivsChars
                                                  case xx740_405 of
                                                      ')' -> return ()
                                                      _ -> gets derivsPosition >>= (throwError . ParseError "')'" "not match pattern: " "" d741_404 ["dvChars"])
                                                  let ')' = xx740_405
                                                  return ()
                                                  return (FromSelection s)]
                test44_111 = foldl1 mplus [do d743_406 <- get
                                              xx742_407 <- StateT derivsChars
                                              case xx742_407 of
                                                  '[' -> return ()
                                                  _ -> gets derivsPosition >>= (throwError . ParseError "'['" "not match pattern: " "" d743_406 ["dvChars"])
                                              let '[' = xx742_407
                                              return ()
                                              h <- StateT hsExpLam
                                              _ <- StateT spaces
                                              return ()
                                              com <- optional3_323 (StateT comForErr)
                                              d751_408 <- get
                                              xx750_409 <- StateT derivsChars
                                              case xx750_409 of
                                                  ']' -> return ()
                                                  _ -> gets derivsPosition >>= (throwError . ParseError "']'" "not match pattern: " "" d751_408 ["dvChars"])
                                              let ']' = xx750_409
                                              return ()
                                              return (h, maybe "" id com)]
                hsExpLam45_112 = foldl1 mplus [do d753_410 <- get
                                                  xx752_411 <- StateT derivsChars
                                                  case xx752_411 of
                                                      '\\' -> return ()
                                                      _ -> gets derivsPosition >>= (throwError . ParseError "'\\\\'" "not match pattern: " "" d753_410 ["dvChars"])
                                                  let '\\' = xx752_411
                                                  return ()
                                                  _ <- StateT spaces
                                                  return ()
                                                  ps <- StateT pats
                                                  _ <- StateT spaces
                                                  return ()
                                                  d761_412 <- get
                                                  xx760_413 <- StateT derivsChars
                                                  case xx760_413 of
                                                      '-' -> return ()
                                                      _ -> gets derivsPosition >>= (throwError . ParseError "'-'" "not match pattern: " "" d761_412 ["dvChars"])
                                                  let '-' = xx760_413
                                                  return ()
                                                  d763_414 <- get
                                                  xx762_415 <- StateT derivsChars
                                                  let c = xx762_415
                                                  unless (isGt c) (gets derivsPosition >>= (throwError . ParseError "isGt c" "not match: " "" d763_414 ["dvChars"]))
                                                  _ <- StateT spaces
                                                  return ()
                                                  e <- StateT hsExpTyp
                                                  return (lamE ps e),
                                               do e <- StateT hsExpTyp
                                                  return e]
                hsExpTyp46_113 = foldl1 mplus [do eo <- StateT hsExpOp
                                                  d773_416 <- get
                                                  xx772_417 <- StateT derivsChars
                                                  case xx772_417 of
                                                      ':' -> return ()
                                                      _ -> gets derivsPosition >>= (throwError . ParseError "':'" "not match pattern: " "" d773_416 ["dvChars"])
                                                  let ':' = xx772_417
                                                  return ()
                                                  d775_418 <- get
                                                  xx774_419 <- StateT derivsChars
                                                  case xx774_419 of
                                                      ':' -> return ()
                                                      _ -> gets derivsPosition >>= (throwError . ParseError "':'" "not match pattern: " "" d775_418 ["dvChars"])
                                                  let ':' = xx774_419
                                                  return ()
                                                  _ <- StateT spaces
                                                  return ()
                                                  t <- StateT hsTypeArr
                                                  return (sigE eo t),
                                               do eo <- StateT hsExpOp
                                                  return eo]
                hsExpOp47_114 = foldl1 mplus [do l <- StateT hsExp
                                                 _ <- StateT spaces
                                                 return ()
                                                 o <- StateT hsOp
                                                 _ <- StateT spaces
                                                 return ()
                                                 r <- StateT hsExpOp
                                                 return (uInfixE (getEx l) o r),
                                              do e <- StateT hsExp
                                                 return (getEx e)]
                hsOp48_115 = foldl1 mplus [do d795_420 <- get
                                              xx794_421 <- StateT derivsChars
                                              let c = xx794_421
                                              unless (isOpHeadChar c) (gets derivsPosition >>= (throwError . ParseError "isOpHeadChar c" "not match: " "" d795_420 ["dvChars"]))
                                              o <- StateT opTail
                                              return (varE (mkName (cons c o))),
                                           do d799_422 <- get
                                              xx798_423 <- StateT derivsChars
                                              case xx798_423 of
                                                  ':' -> return ()
                                                  _ -> gets derivsPosition >>= (throwError . ParseError "':'" "not match pattern: " "" d799_422 ["dvChars"])
                                              let ':' = xx798_423
                                              return ()
                                              ddd800_424 <- get
                                              do err <- ((do d802_425 <- get
                                                             xx801_426 <- StateT derivsChars
                                                             case xx801_426 of
                                                                 ':' -> return ()
                                                                 _ -> gets derivsPosition >>= (throwError . ParseError "':'" "not match pattern: " "" d802_425 ["dvChars"])
                                                             let ':' = xx801_426
                                                             return ()) >> return False) `catchError` const (return True)
                                                 unless err (gets derivsPosition >>= (throwError . ParseError ('!' : "':':") "not match: " "" ddd800_424 ["dvChars"]))
                                              put ddd800_424
                                              o <- StateT opTail
                                              return (conE (mkName (':' : o))),
                                           do d806_427 <- get
                                              xx805_428 <- StateT derivsChars
                                              let c = xx805_428
                                              unless (isBQ c) (gets derivsPosition >>= (throwError . ParseError "isBQ c" "not match: " "" d806_427 ["dvChars"]))
                                              v <- StateT variable
                                              d810_429 <- get
                                              xx809_430 <- StateT derivsChars
                                              let c_ = xx809_430
                                              unless (isBQ c_) (gets derivsPosition >>= (throwError . ParseError "isBQ c_" "not match: " "" d810_429 ["dvChars"]))
                                              return (varE (mkName v)),
                                           do d812_431 <- get
                                              xx811_432 <- StateT derivsChars
                                              let c = xx811_432
                                              unless (isBQ c) (gets derivsPosition >>= (throwError . ParseError "isBQ c" "not match: " "" d812_431 ["dvChars"]))
                                              t <- StateT typ
                                              d816_433 <- get
                                              xx815_434 <- StateT derivsChars
                                              let c_ = xx815_434
                                              unless (isBQ c_) (gets derivsPosition >>= (throwError . ParseError "isBQ c_" "not match: " "" d816_433 ["dvChars"]))
                                              return (conE (mkName t))]
                opTail49_116 = foldl1 mplus [do d818_435 <- get
                                                xx817_436 <- StateT derivsChars
                                                let c = xx817_436
                                                unless (isOpTailChar c) (gets derivsPosition >>= (throwError . ParseError "isOpTailChar c" "not match: " "" d818_435 ["dvChars"]))
                                                s <- StateT opTail
                                                return (cons c s),
                                             return emp]
                hsExp50_117 = foldl1 mplus [do e <- StateT hsExp1
                                               _ <- StateT spaces
                                               return ()
                                               h <- StateT hsExp
                                               return (applyExR e h),
                                            do e <- StateT hsExp1
                                               return (toEx e)]
                hsExp151_118 = foldl1 mplus [do d830_437 <- get
                                                xx829_438 <- StateT derivsChars
                                                case xx829_438 of
                                                    '(' -> return ()
                                                    _ -> gets derivsPosition >>= (throwError . ParseError "'('" "not match pattern: " "" d830_437 ["dvChars"])
                                                let '(' = xx829_438
                                                return ()
                                                l <- optional3_323 (foldl1 mplus [do e <- StateT hsExpTyp
                                                                                     return e])
                                                _ <- StateT spaces
                                                return ()
                                                o <- StateT hsOp
                                                _ <- StateT spaces
                                                return ()
                                                r <- optional3_323 (foldl1 mplus [do e <- StateT hsExpTyp
                                                                                     return e])
                                                d846_439 <- get
                                                xx845_440 <- StateT derivsChars
                                                case xx845_440 of
                                                    ')' -> return ()
                                                    _ -> gets derivsPosition >>= (throwError . ParseError "')'" "not match pattern: " "" d846_439 ["dvChars"])
                                                let ')' = xx845_440
                                                return ()
                                                return (infixE l o r),
                                             do d848_441 <- get
                                                xx847_442 <- StateT derivsChars
                                                case xx847_442 of
                                                    '(' -> return ()
                                                    _ -> gets derivsPosition >>= (throwError . ParseError "'('" "not match pattern: " "" d848_441 ["dvChars"])
                                                let '(' = xx847_442
                                                return ()
                                                et <- StateT hsExpTpl
                                                d852_443 <- get
                                                xx851_444 <- StateT derivsChars
                                                case xx851_444 of
                                                    ')' -> return ()
                                                    _ -> gets derivsPosition >>= (throwError . ParseError "')'" "not match pattern: " "" d852_443 ["dvChars"])
                                                let ')' = xx851_444
                                                return ()
                                                return (tupE et),
                                             do d854_445 <- get
                                                xx853_446 <- StateT derivsChars
                                                case xx853_446 of
                                                    '[' -> return ()
                                                    _ -> gets derivsPosition >>= (throwError . ParseError "'['" "not match pattern: " "" d854_445 ["dvChars"])
                                                let '[' = xx853_446
                                                return ()
                                                et <- StateT hsExpTpl
                                                d858_447 <- get
                                                xx857_448 <- StateT derivsChars
                                                case xx857_448 of
                                                    ']' -> return ()
                                                    _ -> gets derivsPosition >>= (throwError . ParseError "']'" "not match pattern: " "" d858_447 ["dvChars"])
                                                let ']' = xx857_448
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
                                             do d868_449 <- get
                                                xx867_450 <- StateT derivsChars
                                                case xx867_450 of
                                                    '\'' -> return ()
                                                    _ -> gets derivsPosition >>= (throwError . ParseError "'\\''" "not match pattern: " "" d868_449 ["dvChars"])
                                                let '\'' = xx867_450
                                                return ()
                                                c <- StateT charLit
                                                d872_451 <- get
                                                xx871_452 <- StateT derivsChars
                                                case xx871_452 of
                                                    '\'' -> return ()
                                                    _ -> gets derivsPosition >>= (throwError . ParseError "'\\''" "not match pattern: " "" d872_451 ["dvChars"])
                                                let '\'' = xx871_452
                                                return ()
                                                return (litE (charL c)),
                                             do d874_453 <- get
                                                xx873_454 <- StateT derivsChars
                                                case xx873_454 of
                                                    '"' -> return ()
                                                    _ -> gets derivsPosition >>= (throwError . ParseError "'\"'" "not match pattern: " "" d874_453 ["dvChars"])
                                                let '"' = xx873_454
                                                return ()
                                                s <- StateT stringLit
                                                d878_455 <- get
                                                xx877_456 <- StateT derivsChars
                                                case xx877_456 of
                                                    '"' -> return ()
                                                    _ -> gets derivsPosition >>= (throwError . ParseError "'\"'" "not match pattern: " "" d878_455 ["dvChars"])
                                                let '"' = xx877_456
                                                return ()
                                                return (litE (stringL s)),
                                             do d880_457 <- get
                                                xx879_458 <- StateT derivsChars
                                                case xx879_458 of
                                                    '-' -> return ()
                                                    _ -> gets derivsPosition >>= (throwError . ParseError "'-'" "not match pattern: " "" d880_457 ["dvChars"])
                                                let '-' = xx879_458
                                                return ()
                                                _ <- StateT spaces
                                                return ()
                                                e <- StateT hsExp1
                                                return (appE (varE $ mkName "negate") e)]
                hsExpTpl52_119 = foldl1 mplus [do e <- StateT hsExpLam
                                                  _ <- StateT spaces
                                                  return ()
                                                  d890_459 <- get
                                                  xx889_460 <- StateT derivsChars
                                                  let c = xx889_460
                                                  unless (isComma c) (gets derivsPosition >>= (throwError . ParseError "isComma c" "not match: " "" d890_459 ["dvChars"]))
                                                  _ <- StateT spaces
                                                  return ()
                                                  et <- StateT hsExpTpl
                                                  return (cons e et),
                                               do e <- StateT hsExpLam
                                                  return (cons e emp),
                                               return emp]
                hsTypeArr53_120 = foldl1 mplus [do l <- StateT hsType
                                                   d900_461 <- get
                                                   xx899_462 <- StateT derivsChars
                                                   case xx899_462 of
                                                       '-' -> return ()
                                                       _ -> gets derivsPosition >>= (throwError . ParseError "'-'" "not match pattern: " "" d900_461 ["dvChars"])
                                                   let '-' = xx899_462
                                                   return ()
                                                   d902_463 <- get
                                                   xx901_464 <- StateT derivsChars
                                                   let c = xx901_464
                                                   unless (isGt c) (gets derivsPosition >>= (throwError . ParseError "isGt c" "not match: " "" d902_463 ["dvChars"]))
                                                   _ <- StateT spaces
                                                   return ()
                                                   r <- StateT hsTypeArr
                                                   return (appT (appT arrowT (getTyp l)) r),
                                                do t <- StateT hsType
                                                   return (getTyp t)]
                hsType54_121 = foldl1 mplus [do t <- StateT hsType1
                                                ts <- StateT hsType
                                                return (applyTyp (toTyp t) ts),
                                             do t <- StateT hsType1
                                                return (toTyp t)]
                hsType155_122 = foldl1 mplus [do d916_465 <- get
                                                 xx915_466 <- StateT derivsChars
                                                 case xx915_466 of
                                                     '[' -> return ()
                                                     _ -> gets derivsPosition >>= (throwError . ParseError "'['" "not match pattern: " "" d916_465 ["dvChars"])
                                                 let '[' = xx915_466
                                                 return ()
                                                 d918_467 <- get
                                                 xx917_468 <- StateT derivsChars
                                                 case xx917_468 of
                                                     ']' -> return ()
                                                     _ -> gets derivsPosition >>= (throwError . ParseError "']'" "not match pattern: " "" d918_467 ["dvChars"])
                                                 let ']' = xx917_468
                                                 return ()
                                                 _ <- StateT spaces
                                                 return ()
                                                 return listT,
                                              do d922_469 <- get
                                                 xx921_470 <- StateT derivsChars
                                                 case xx921_470 of
                                                     '[' -> return ()
                                                     _ -> gets derivsPosition >>= (throwError . ParseError "'['" "not match pattern: " "" d922_469 ["dvChars"])
                                                 let '[' = xx921_470
                                                 return ()
                                                 t <- StateT hsTypeArr
                                                 d926_471 <- get
                                                 xx925_472 <- StateT derivsChars
                                                 case xx925_472 of
                                                     ']' -> return ()
                                                     _ -> gets derivsPosition >>= (throwError . ParseError "']'" "not match pattern: " "" d926_471 ["dvChars"])
                                                 let ']' = xx925_472
                                                 return ()
                                                 _ <- StateT spaces
                                                 return ()
                                                 return (appT listT t),
                                              do d930_473 <- get
                                                 xx929_474 <- StateT derivsChars
                                                 case xx929_474 of
                                                     '(' -> return ()
                                                     _ -> gets derivsPosition >>= (throwError . ParseError "'('" "not match pattern: " "" d930_473 ["dvChars"])
                                                 let '(' = xx929_474
                                                 return ()
                                                 _ <- StateT spaces
                                                 return ()
                                                 tt <- StateT hsTypeTpl
                                                 d936_475 <- get
                                                 xx935_476 <- StateT derivsChars
                                                 case xx935_476 of
                                                     ')' -> return ()
                                                     _ -> gets derivsPosition >>= (throwError . ParseError "')'" "not match pattern: " "" d936_475 ["dvChars"])
                                                 let ')' = xx935_476
                                                 return ()
                                                 return (tupT tt),
                                              do t <- StateT typToken
                                                 return (conT (mkName t)),
                                              do d940_477 <- get
                                                 xx939_478 <- StateT derivsChars
                                                 case xx939_478 of
                                                     '(' -> return ()
                                                     _ -> gets derivsPosition >>= (throwError . ParseError "'('" "not match pattern: " "" d940_477 ["dvChars"])
                                                 let '(' = xx939_478
                                                 return ()
                                                 d942_479 <- get
                                                 xx941_480 <- StateT derivsChars
                                                 case xx941_480 of
                                                     '-' -> return ()
                                                     _ -> gets derivsPosition >>= (throwError . ParseError "'-'" "not match pattern: " "" d942_479 ["dvChars"])
                                                 let '-' = xx941_480
                                                 return ()
                                                 d944_481 <- get
                                                 xx943_482 <- StateT derivsChars
                                                 let c = xx943_482
                                                 unless (isGt c) (gets derivsPosition >>= (throwError . ParseError "isGt c" "not match: " "" d944_481 ["dvChars"]))
                                                 d946_483 <- get
                                                 xx945_484 <- StateT derivsChars
                                                 case xx945_484 of
                                                     ')' -> return ()
                                                     _ -> gets derivsPosition >>= (throwError . ParseError "')'" "not match pattern: " "" d946_483 ["dvChars"])
                                                 let ')' = xx945_484
                                                 return ()
                                                 _ <- StateT spaces
                                                 return ()
                                                 return arrowT]
                hsTypeTpl56_123 = foldl1 mplus [do t <- StateT hsTypeArr
                                                   d952_485 <- get
                                                   xx951_486 <- StateT derivsChars
                                                   let c = xx951_486
                                                   unless (isComma c) (gets derivsPosition >>= (throwError . ParseError "isComma c" "not match: " "" d952_485 ["dvChars"]))
                                                   _ <- StateT spaces
                                                   return ()
                                                   tt <- StateT hsTypeTpl
                                                   return (cons t tt),
                                                do t <- StateT hsTypeArr
                                                   return (cons t emp),
                                                return emp]
                typ57_124 = foldl1 mplus [do u <- StateT upper
                                             t <- StateT tvtail
                                             return (cons u t)]
                variable58_125 = foldl1 mplus [do l <- StateT lower
                                                  t <- StateT tvtail
                                                  return (cons l t)]
                tvtail59_126 = foldl1 mplus [do a <- StateT alpha
                                                t <- StateT tvtail
                                                return (cons a t),
                                             return emp]
                integer60_127 = foldl1 mplus [do dh <- StateT digit
                                                 ds <- list1_487 (foldl1 mplus [do d <- StateT digit
                                                                                   return d])
                                                 return (read (cons dh ds))]
                alpha61_128 = foldl1 mplus [do u <- StateT upper
                                               return u,
                                            do l <- StateT lower
                                               return l,
                                            do d <- StateT digit
                                               return d,
                                            do d984_488 <- get
                                               xx983_489 <- StateT derivsChars
                                               case xx983_489 of
                                                   '\'' -> return ()
                                                   _ -> gets derivsPosition >>= (throwError . ParseError "'\\''" "not match pattern: " "" d984_488 ["dvChars"])
                                               let '\'' = xx983_489
                                               return ()
                                               return '\'']
                upper62_129 = foldl1 mplus [do d986_490 <- get
                                               xx985_491 <- StateT derivsChars
                                               let u = xx985_491
                                               unless (isUpper u) (gets derivsPosition >>= (throwError . ParseError "isUpper u" "not match: " "" d986_490 ["dvChars"]))
                                               return u]
                lower63_130 = foldl1 mplus [do d988_492 <- get
                                               xx987_493 <- StateT derivsChars
                                               let l = xx987_493
                                               unless (isLowerU l) (gets derivsPosition >>= (throwError . ParseError "isLowerU l" "not match: " "" d988_492 ["dvChars"]))
                                               return l]
                digit64_131 = foldl1 mplus [do d990_494 <- get
                                               xx989_495 <- StateT derivsChars
                                               let d = xx989_495
                                               unless (isDigit d) (gets derivsPosition >>= (throwError . ParseError "isDigit d" "not match: " "" d990_494 ["dvChars"]))
                                               return d]
                spaces65_132 = foldl1 mplus [do _ <- StateT space
                                                return ()
                                                _ <- StateT spaces
                                                return ()
                                                return (),
                                             return ()]
                space66_133 = foldl1 mplus [do d996_496 <- get
                                               xx995_497 <- StateT derivsChars
                                               let s = xx995_497
                                               unless (isSpace s) (gets derivsPosition >>= (throwError . ParseError "isSpace s" "not match: " "" d996_496 ["dvChars"]))
                                               return (),
                                            do d998_498 <- get
                                               xx997_499 <- StateT derivsChars
                                               case xx997_499 of
                                                   '-' -> return ()
                                                   _ -> gets derivsPosition >>= (throwError . ParseError "'-'" "not match pattern: " "" d998_498 ["dvChars"])
                                               let '-' = xx997_499
                                               return ()
                                               d1000_500 <- get
                                               xx999_501 <- StateT derivsChars
                                               case xx999_501 of
                                                   '-' -> return ()
                                                   _ -> gets derivsPosition >>= (throwError . ParseError "'-'" "not match pattern: " "" d1000_500 ["dvChars"])
                                               let '-' = xx999_501
                                               return ()
                                               _ <- StateT notNLString
                                               return ()
                                               _ <- StateT newLine
                                               return ()
                                               return (),
                                            do _ <- StateT comment
                                               return ()
                                               return ()]
                notNLString67_134 = foldl1 mplus [do ddd1007_502 <- get
                                                     do err <- ((do _ <- StateT newLine
                                                                    return ()) >> return False) `catchError` const (return True)
                                                        unless err (gets derivsPosition >>= (throwError . ParseError ('!' : "_:newLine") "not match: " "" ddd1007_502 ["newLine"]))
                                                     put ddd1007_502
                                                     c <- StateT derivsChars
                                                     s <- StateT notNLString
                                                     return (cons c s),
                                                  return emp]
                newLine68_135 = foldl1 mplus [do d1015_503 <- get
                                                 xx1014_504 <- StateT derivsChars
                                                 case xx1014_504 of
                                                     '\n' -> return ()
                                                     _ -> gets derivsPosition >>= (throwError . ParseError "'\\n'" "not match pattern: " "" d1015_503 ["dvChars"])
                                                 let '\n' = xx1014_504
                                                 return ()
                                                 return ()]
                comment69_136 = foldl1 mplus [do d1017_505 <- get
                                                 xx1016_506 <- StateT derivsChars
                                                 case xx1016_506 of
                                                     '{' -> return ()
                                                     _ -> gets derivsPosition >>= (throwError . ParseError "'{'" "not match pattern: " "" d1017_505 ["dvChars"])
                                                 let '{' = xx1016_506
                                                 return ()
                                                 d1019_507 <- get
                                                 xx1018_508 <- StateT derivsChars
                                                 case xx1018_508 of
                                                     '-' -> return ()
                                                     _ -> gets derivsPosition >>= (throwError . ParseError "'-'" "not match pattern: " "" d1019_507 ["dvChars"])
                                                 let '-' = xx1018_508
                                                 return ()
                                                 ddd1020_509 <- get
                                                 do err <- ((do d1022_510 <- get
                                                                xx1021_511 <- StateT derivsChars
                                                                case xx1021_511 of
                                                                    '#' -> return ()
                                                                    _ -> gets derivsPosition >>= (throwError . ParseError "'#'" "not match pattern: " "" d1022_510 ["dvChars"])
                                                                let '#' = xx1021_511
                                                                return ()) >> return False) `catchError` const (return True)
                                                    unless err (gets derivsPosition >>= (throwError . ParseError ('!' : "'#':") "not match: " "" ddd1020_509 ["dvChars"]))
                                                 put ddd1020_509
                                                 _ <- StateT comments
                                                 return ()
                                                 _ <- StateT comEnd
                                                 return ()
                                                 return ()]
                comments70_137 = foldl1 mplus [do _ <- StateT notComStr
                                                  return ()
                                                  _ <- StateT comment
                                                  return ()
                                                  _ <- StateT comments
                                                  return ()
                                                  return (),
                                               do _ <- StateT notComStr
                                                  return ()
                                                  return ()]
                notComStr71_138 = foldl1 mplus [do ddd1035_512 <- get
                                                   do err <- ((do _ <- StateT comment
                                                                  return ()) >> return False) `catchError` const (return True)
                                                      unless err (gets derivsPosition >>= (throwError . ParseError ('!' : "_:comment") "not match: " "" ddd1035_512 ["comment"]))
                                                   put ddd1035_512
                                                   ddd1038_513 <- get
                                                   do err <- ((do _ <- StateT comEnd
                                                                  return ()) >> return False) `catchError` const (return True)
                                                      unless err (gets derivsPosition >>= (throwError . ParseError ('!' : "_:comEnd") "not match: " "" ddd1038_513 ["comEnd"]))
                                                   put ddd1038_513
                                                   _ <- StateT derivsChars
                                                   return ()
                                                   _ <- StateT notComStr
                                                   return ()
                                                   return (),
                                                return ()]
                comEnd72_139 = foldl1 mplus [do d1046_514 <- get
                                                xx1045_515 <- StateT derivsChars
                                                case xx1045_515 of
                                                    '-' -> return ()
                                                    _ -> gets derivsPosition >>= (throwError . ParseError "'-'" "not match pattern: " "" d1046_514 ["dvChars"])
                                                let '-' = xx1045_515
                                                return ()
                                                d1048_516 <- get
                                                xx1047_517 <- StateT derivsChars
                                                case xx1047_517 of
                                                    '}' -> return ()
                                                    _ -> gets derivsPosition >>= (throwError . ParseError "'}'" "not match pattern: " "" d1048_516 ["dvChars"])
                                                let '}' = xx1047_517
                                                return ()
                                                return ()]
                list1_487 :: forall m a . (MonadPlus m, Applicative m) =>
                                          m a -> m ([a])
                list12_518 :: forall m a . (MonadPlus m, Applicative m) =>
                                           m a -> m ([a])
                list1_487 p = list12_518 p `mplus` return []
                list12_518 p = ((:) <$> p) <*> list1_487 p
                optional3_323 :: forall m a . (MonadPlus m, Applicative m) =>
                                              m a -> m (Maybe a)
                optional3_323 p = (Just <$> p) `mplus` return Nothing
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

