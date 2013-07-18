{-# LANGUAGE FlexibleContexts, TemplateHaskell, UndecidableInstances, PackageImports, TypeFamilies, RankNTypes #-}
module Text.Papillon.Parser (
	Peg,
	Definition,
	Selection,
	ExpressionHs,
	NameLeaf(..),
	NameLeaf_(..),
	ReadFrom(..),
	parse,
	showNameLeaf,
	nameFromRF,
	ParseError(..),
	Derivs(peg, pegFile, derivsChars),
	Pos(..),
	ListPos(..),
	pePositionS,
	Source(..),
	SourceList(..),

	PPragma(..),
	ModuleName
)  where
import "monads-tf" Control.Monad.State
import "monads-tf" Control.Monad.Error

import Text.Papillon.Papillon

import Control.Applicative



import Data.Char
import Language.Haskell.TH
import Text.Papillon.SyntaxTree

data Derivs
    = Derivs {pegFile :: (Either (ParseError (Pos String) Derivs)
                                 ((PegFile, Derivs))),
              pragmas :: (Either (ParseError (Pos String) Derivs)
                                 (([PPragma], Derivs))),
              pragma :: (Either (ParseError (Pos String) Derivs)
                                ((PPragma, Derivs))),
              pragmaStr2 :: (Either (ParseError (Pos String) Derivs)
                                    ((String, Derivs))),
              pragmaItems :: (Either (ParseError (Pos String) Derivs)
                                     (([String], Derivs))),
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
                             where d = Derivs pegFile73_1 pragmas74_2 pragma75_3 pragmaStr276_4 pragmaItems77_5 pragmaEnd78_6 moduleDec79_7 moduleName80_8 moduleDecStr81_9 whr82_10 preImpPap83_11 prePeg84_12 afterPeg85_13 importPapillon86_14 varToken87_15 typToken88_16 pap89_17 peg90_18 sourceType91_19 peg_92_20 definition93_21 selection94_22 expressionHs95_23 expression96_24 nameLeaf_97_25 nameLeaf98_26 nameLeafNoCom99_27 comForErr100_28 leaf101_29 patOp102_30 pat103_31 pat1104_32 patList105_33 opConName106_34 charLit107_35 stringLit108_36 escapeC109_37 pats110_38 readFromLs111_39 readFrom112_40 test113_41 hsExpLam114_42 hsExpTyp115_43 hsExpOp116_44 hsOp117_45 opTail118_46 hsExp119_47 hsExp1120_48 hsExpTpl121_49 hsTypeArr122_50 hsType123_51 hsType1124_52 hsTypeTpl125_53 typ126_54 variable127_55 tvtail128_56 integer129_57 alpha130_58 upper131_59 lower132_60 digit133_61 spaces134_62 space135_63 notNLString136_64 newLine137_65 comment138_66 comments139_67 notComStr140_68 comEnd141_69 chars142_70 pos
                                   pegFile73_1 = runStateT pegFile4_71 d
                                   pragmas74_2 = runStateT pragmas5_72 d
                                   pragma75_3 = runStateT pragma6_73 d
                                   pragmaStr276_4 = runStateT pragmaStr27_74 d
                                   pragmaItems77_5 = runStateT pragmaItems8_75 d
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
                pegFile4_71 = foldl1 mplus [do pr <- StateT pragmas
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
                                                   _ -> gets derivsPosition >>= (throwError . ParseError "'|'" "not match pattern: " "" d160_140 ["derivsChars"])
                                               let '|' = xx159_141
                                               return ()
                                               d162_142 <- get
                                               xx161_143 <- StateT derivsChars
                                               case xx161_143 of
                                                   ']' -> return ()
                                                   _ -> gets derivsPosition >>= (throwError . ParseError "']'" "not match pattern: " "" d162_142 ["derivsChars"])
                                               let ']' = xx161_143
                                               return ()
                                               d164_144 <- get
                                               xx163_145 <- StateT derivsChars
                                               case xx163_145 of
                                                   '\n' -> return ()
                                                   _ -> gets derivsPosition >>= (throwError . ParseError "'\\n'" "not match pattern: " "" d164_144 ["derivsChars"])
                                               let '\n' = xx163_145
                                               return ()
                                               atp <- StateT afterPeg
                                               return (mkPegFile pr md pip pp p atp),
                                            do pr <- StateT pragmas
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
                                                   _ -> gets derivsPosition >>= (throwError . ParseError "'|'" "not match pattern: " "" d180_146 ["derivsChars"])
                                               let '|' = xx179_147
                                               return ()
                                               d182_148 <- get
                                               xx181_149 <- StateT derivsChars
                                               case xx181_149 of
                                                   ']' -> return ()
                                                   _ -> gets derivsPosition >>= (throwError . ParseError "']'" "not match pattern: " "" d182_148 ["derivsChars"])
                                               let ']' = xx181_149
                                               return ()
                                               d184_150 <- get
                                               xx183_151 <- StateT derivsChars
                                               case xx183_151 of
                                                   '\n' -> return ()
                                                   _ -> gets derivsPosition >>= (throwError . ParseError "'\\n'" "not match pattern: " "" d184_150 ["derivsChars"])
                                               let '\n' = xx183_151
                                               return ()
                                               atp <- StateT afterPeg
                                               return (mkPegFile pr md emp pp p atp)]
                pragmas5_72 = foldl1 mplus [do _ <- StateT spaces
                                               return ()
                                               pr <- StateT pragma
                                               prs <- StateT pragmas
                                               return (pr : prs),
                                            do _ <- StateT spaces
                                               return ()
                                               return []]
                pragma6_73 = foldl1 mplus [do d196_152 <- get
                                              xx195_153 <- StateT derivsChars
                                              case xx195_153 of
                                                  '{' -> return ()
                                                  _ -> gets derivsPosition >>= (throwError . ParseError "'{'" "not match pattern: " "" d196_152 ["derivsChars"])
                                              let '{' = xx195_153
                                              return ()
                                              d198_154 <- get
                                              xx197_155 <- StateT derivsChars
                                              case xx197_155 of
                                                  '-' -> return ()
                                                  _ -> gets derivsPosition >>= (throwError . ParseError "'-'" "not match pattern: " "" d198_154 ["derivsChars"])
                                              let '-' = xx197_155
                                              return ()
                                              d200_156 <- get
                                              xx199_157 <- StateT derivsChars
                                              case xx199_157 of
                                                  '#' -> return ()
                                                  _ -> gets derivsPosition >>= (throwError . ParseError "'#'" "not match pattern: " "" d200_156 ["derivsChars"])
                                              let '#' = xx199_157
                                              return ()
                                              _ <- StateT spaces
                                              return ()
                                              d204_158 <- get
                                              xx203_159 <- StateT derivsChars
                                              case xx203_159 of
                                                  'L' -> return ()
                                                  _ -> gets derivsPosition >>= (throwError . ParseError "'L'" "not match pattern: " "" d204_158 ["derivsChars"])
                                              let 'L' = xx203_159
                                              return ()
                                              d206_160 <- get
                                              xx205_161 <- StateT derivsChars
                                              case xx205_161 of
                                                  'A' -> return ()
                                                  _ -> gets derivsPosition >>= (throwError . ParseError "'A'" "not match pattern: " "" d206_160 ["derivsChars"])
                                              let 'A' = xx205_161
                                              return ()
                                              d208_162 <- get
                                              xx207_163 <- StateT derivsChars
                                              case xx207_163 of
                                                  'N' -> return ()
                                                  _ -> gets derivsPosition >>= (throwError . ParseError "'N'" "not match pattern: " "" d208_162 ["derivsChars"])
                                              let 'N' = xx207_163
                                              return ()
                                              d210_164 <- get
                                              xx209_165 <- StateT derivsChars
                                              case xx209_165 of
                                                  'G' -> return ()
                                                  _ -> gets derivsPosition >>= (throwError . ParseError "'G'" "not match pattern: " "" d210_164 ["derivsChars"])
                                              let 'G' = xx209_165
                                              return ()
                                              d212_166 <- get
                                              xx211_167 <- StateT derivsChars
                                              case xx211_167 of
                                                  'U' -> return ()
                                                  _ -> gets derivsPosition >>= (throwError . ParseError "'U'" "not match pattern: " "" d212_166 ["derivsChars"])
                                              let 'U' = xx211_167
                                              return ()
                                              d214_168 <- get
                                              xx213_169 <- StateT derivsChars
                                              case xx213_169 of
                                                  'A' -> return ()
                                                  _ -> gets derivsPosition >>= (throwError . ParseError "'A'" "not match pattern: " "" d214_168 ["derivsChars"])
                                              let 'A' = xx213_169
                                              return ()
                                              d216_170 <- get
                                              xx215_171 <- StateT derivsChars
                                              case xx215_171 of
                                                  'G' -> return ()
                                                  _ -> gets derivsPosition >>= (throwError . ParseError "'G'" "not match pattern: " "" d216_170 ["derivsChars"])
                                              let 'G' = xx215_171
                                              return ()
                                              d218_172 <- get
                                              xx217_173 <- StateT derivsChars
                                              case xx217_173 of
                                                  'E' -> return ()
                                                  _ -> gets derivsPosition >>= (throwError . ParseError "'E'" "not match pattern: " "" d218_172 ["derivsChars"])
                                              let 'E' = xx217_173
                                              return ()
                                              _ <- StateT spaces
                                              return ()
                                              s <- StateT pragmaItems
                                              _ <- StateT pragmaEnd
                                              return ()
                                              _ <- StateT spaces
                                              return ()
                                              return (LanguagePragma s),
                                           do d228_174 <- get
                                              xx227_175 <- StateT derivsChars
                                              case xx227_175 of
                                                  '{' -> return ()
                                                  _ -> gets derivsPosition >>= (throwError . ParseError "'{'" "not match pattern: " "" d228_174 ["derivsChars"])
                                              let '{' = xx227_175
                                              return ()
                                              d230_176 <- get
                                              xx229_177 <- StateT derivsChars
                                              case xx229_177 of
                                                  '-' -> return ()
                                                  _ -> gets derivsPosition >>= (throwError . ParseError "'-'" "not match pattern: " "" d230_176 ["derivsChars"])
                                              let '-' = xx229_177
                                              return ()
                                              d232_178 <- get
                                              xx231_179 <- StateT derivsChars
                                              case xx231_179 of
                                                  '#' -> return ()
                                                  _ -> gets derivsPosition >>= (throwError . ParseError "'#'" "not match pattern: " "" d232_178 ["derivsChars"])
                                              let '#' = xx231_179
                                              return ()
                                              _ <- StateT spaces
                                              return ()
                                              s <- StateT pragmaStr2
                                              _ <- StateT pragmaEnd
                                              return ()
                                              return (OtherPragma s)]
                pragmaStr27_74 = foldl1 mplus [do ddd239_180 <- get
                                                  do err <- ((do _ <- StateT pragmaEnd
                                                                 return ()) >> return False) `catchError` const (return True)
                                                     unless err (gets derivsPosition >>= (throwError . ParseError ('!' : "_:pragmaEnd") "not match: " "" ddd239_180 ["pragmaEnd"]))
                                                  put ddd239_180
                                                  c <- StateT derivsChars
                                                  s <- StateT pragmaStr2
                                                  return (c : s),
                                               return ""]
                pragmaItems8_75 = foldl1 mplus [do t <- StateT typToken
                                                   d249_181 <- get
                                                   xx248_182 <- StateT derivsChars
                                                   case xx248_182 of
                                                       ',' -> return ()
                                                       _ -> gets derivsPosition >>= (throwError . ParseError "','" "not match pattern: " "" d249_181 ["derivsChars"])
                                                   let ',' = xx248_182
                                                   return ()
                                                   _ <- StateT spaces
                                                   return ()
                                                   i <- StateT pragmaItems
                                                   return (t : i),
                                                do t <- StateT typToken
                                                   return [t]]
                pragmaEnd9_76 = foldl1 mplus [do _ <- StateT spaces
                                                 return ()
                                                 d259_183 <- get
                                                 xx258_184 <- StateT derivsChars
                                                 case xx258_184 of
                                                     '#' -> return ()
                                                     _ -> gets derivsPosition >>= (throwError . ParseError "'#'" "not match pattern: " "" d259_183 ["derivsChars"])
                                                 let '#' = xx258_184
                                                 return ()
                                                 d261_185 <- get
                                                 xx260_186 <- StateT derivsChars
                                                 case xx260_186 of
                                                     '-' -> return ()
                                                     _ -> gets derivsPosition >>= (throwError . ParseError "'-'" "not match pattern: " "" d261_185 ["derivsChars"])
                                                 let '-' = xx260_186
                                                 return ()
                                                 d263_187 <- get
                                                 xx262_188 <- StateT derivsChars
                                                 case xx262_188 of
                                                     '}' -> return ()
                                                     _ -> gets derivsPosition >>= (throwError . ParseError "'}'" "not match pattern: " "" d263_187 ["derivsChars"])
                                                 let '}' = xx262_188
                                                 return ()
                                                 return ()]
                moduleDec10_77 = foldl1 mplus [do d265_189 <- get
                                                  xx264_190 <- StateT derivsChars
                                                  case xx264_190 of
                                                      'm' -> return ()
                                                      _ -> gets derivsPosition >>= (throwError . ParseError "'m'" "not match pattern: " "" d265_189 ["derivsChars"])
                                                  let 'm' = xx264_190
                                                  return ()
                                                  d267_191 <- get
                                                  xx266_192 <- StateT derivsChars
                                                  case xx266_192 of
                                                      'o' -> return ()
                                                      _ -> gets derivsPosition >>= (throwError . ParseError "'o'" "not match pattern: " "" d267_191 ["derivsChars"])
                                                  let 'o' = xx266_192
                                                  return ()
                                                  d269_193 <- get
                                                  xx268_194 <- StateT derivsChars
                                                  case xx268_194 of
                                                      'd' -> return ()
                                                      _ -> gets derivsPosition >>= (throwError . ParseError "'d'" "not match pattern: " "" d269_193 ["derivsChars"])
                                                  let 'd' = xx268_194
                                                  return ()
                                                  d271_195 <- get
                                                  xx270_196 <- StateT derivsChars
                                                  case xx270_196 of
                                                      'u' -> return ()
                                                      _ -> gets derivsPosition >>= (throwError . ParseError "'u'" "not match pattern: " "" d271_195 ["derivsChars"])
                                                  let 'u' = xx270_196
                                                  return ()
                                                  d273_197 <- get
                                                  xx272_198 <- StateT derivsChars
                                                  case xx272_198 of
                                                      'l' -> return ()
                                                      _ -> gets derivsPosition >>= (throwError . ParseError "'l'" "not match pattern: " "" d273_197 ["derivsChars"])
                                                  let 'l' = xx272_198
                                                  return ()
                                                  d275_199 <- get
                                                  xx274_200 <- StateT derivsChars
                                                  case xx274_200 of
                                                      'e' -> return ()
                                                      _ -> gets derivsPosition >>= (throwError . ParseError "'e'" "not match pattern: " "" d275_199 ["derivsChars"])
                                                  let 'e' = xx274_200
                                                  return ()
                                                  _ <- StateT spaces
                                                  return ()
                                                  n <- StateT moduleName
                                                  s <- StateT moduleDecStr
                                                  _ <- StateT whr
                                                  return ()
                                                  return (Just (n, s)),
                                               return Nothing]
                moduleName11_78 = foldl1 mplus [do t <- StateT typ
                                                   d287_201 <- get
                                                   xx286_202 <- StateT derivsChars
                                                   case xx286_202 of
                                                       '.' -> return ()
                                                       _ -> gets derivsPosition >>= (throwError . ParseError "'.'" "not match pattern: " "" d287_201 ["derivsChars"])
                                                   let '.' = xx286_202
                                                   return ()
                                                   n <- StateT moduleName
                                                   return (t : n),
                                                do t <- StateT typ
                                                   return [t]]
                moduleDecStr12_79 = foldl1 mplus [do ddd292_203 <- get
                                                     do err <- ((do _ <- StateT whr
                                                                    return ()) >> return False) `catchError` const (return True)
                                                        unless err (gets derivsPosition >>= (throwError . ParseError ('!' : "_:whr") "not match: " "" ddd292_203 ["whr"]))
                                                     put ddd292_203
                                                     c <- StateT derivsChars
                                                     s <- StateT moduleDecStr
                                                     return (c : s),
                                                  return ""]
                whr13_80 = foldl1 mplus [do d300_204 <- get
                                            xx299_205 <- StateT derivsChars
                                            case xx299_205 of
                                                'w' -> return ()
                                                _ -> gets derivsPosition >>= (throwError . ParseError "'w'" "not match pattern: " "" d300_204 ["derivsChars"])
                                            let 'w' = xx299_205
                                            return ()
                                            d302_206 <- get
                                            xx301_207 <- StateT derivsChars
                                            case xx301_207 of
                                                'h' -> return ()
                                                _ -> gets derivsPosition >>= (throwError . ParseError "'h'" "not match pattern: " "" d302_206 ["derivsChars"])
                                            let 'h' = xx301_207
                                            return ()
                                            d304_208 <- get
                                            xx303_209 <- StateT derivsChars
                                            case xx303_209 of
                                                'e' -> return ()
                                                _ -> gets derivsPosition >>= (throwError . ParseError "'e'" "not match pattern: " "" d304_208 ["derivsChars"])
                                            let 'e' = xx303_209
                                            return ()
                                            d306_210 <- get
                                            xx305_211 <- StateT derivsChars
                                            case xx305_211 of
                                                'r' -> return ()
                                                _ -> gets derivsPosition >>= (throwError . ParseError "'r'" "not match pattern: " "" d306_210 ["derivsChars"])
                                            let 'r' = xx305_211
                                            return ()
                                            d308_212 <- get
                                            xx307_213 <- StateT derivsChars
                                            case xx307_213 of
                                                'e' -> return ()
                                                _ -> gets derivsPosition >>= (throwError . ParseError "'e'" "not match pattern: " "" d308_212 ["derivsChars"])
                                            let 'e' = xx307_213
                                            return ()
                                            return ()]
                preImpPap14_81 = foldl1 mplus [do ddd309_214 <- get
                                                  do err <- ((do _ <- StateT importPapillon
                                                                 return ()) >> return False) `catchError` const (return True)
                                                     unless err (gets derivsPosition >>= (throwError . ParseError ('!' : "_:importPapillon") "not match: " "" ddd309_214 ["importPapillon"]))
                                                  put ddd309_214
                                                  ddd312_215 <- get
                                                  do err <- ((do _ <- StateT pap
                                                                 return ()) >> return False) `catchError` const (return True)
                                                     unless err (gets derivsPosition >>= (throwError . ParseError ('!' : "_:pap") "not match: " "" ddd312_215 ["pap"]))
                                                  put ddd312_215
                                                  c <- StateT derivsChars
                                                  pip <- StateT preImpPap
                                                  return (cons c pip),
                                               return emp]
                prePeg15_82 = foldl1 mplus [do ddd319_216 <- get
                                               do err <- ((do _ <- StateT pap
                                                              return ()) >> return False) `catchError` const (return True)
                                                  unless err (gets derivsPosition >>= (throwError . ParseError ('!' : "_:pap") "not match: " "" ddd319_216 ["pap"]))
                                               put ddd319_216
                                               c <- StateT derivsChars
                                               pp <- StateT prePeg
                                               return (cons c pp),
                                            return emp]
                afterPeg16_83 = foldl1 mplus [do c <- StateT derivsChars
                                                 atp <- StateT afterPeg
                                                 return (cons c atp),
                                              return emp]
                importPapillon17_84 = foldl1 mplus [do d331_217 <- get
                                                       xx330_218 <- StateT varToken
                                                       case xx330_218 of
                                                           "import" -> return ()
                                                           _ -> gets derivsPosition >>= (throwError . ParseError "\"import\"" "not match pattern: " "" d331_217 ["varToken"])
                                                       let "import" = xx330_218
                                                       return ()
                                                       d333_219 <- get
                                                       xx332_220 <- StateT typToken
                                                       case xx332_220 of
                                                           "Text" -> return ()
                                                           _ -> gets derivsPosition >>= (throwError . ParseError "\"Text\"" "not match pattern: " "" d333_219 ["typToken"])
                                                       let "Text" = xx332_220
                                                       return ()
                                                       d335_221 <- get
                                                       xx334_222 <- StateT derivsChars
                                                       case xx334_222 of
                                                           '.' -> return ()
                                                           _ -> gets derivsPosition >>= (throwError . ParseError "'.'" "not match pattern: " "" d335_221 ["derivsChars"])
                                                       let '.' = xx334_222
                                                       return ()
                                                       _ <- StateT spaces
                                                       return ()
                                                       d339_223 <- get
                                                       xx338_224 <- StateT typToken
                                                       case xx338_224 of
                                                           "Papillon" -> return ()
                                                           _ -> gets derivsPosition >>= (throwError . ParseError "\"Papillon\"" "not match pattern: " "" d339_223 ["typToken"])
                                                       let "Papillon" = xx338_224
                                                       return ()
                                                       ddd340_225 <- get
                                                       do err <- ((do d342_226 <- get
                                                                      xx341_227 <- StateT derivsChars
                                                                      case xx341_227 of
                                                                          '.' -> return ()
                                                                          _ -> gets derivsPosition >>= (throwError . ParseError "'.'" "not match pattern: " "" d342_226 ["derivsChars"])
                                                                      let '.' = xx341_227
                                                                      return ()) >> return False) `catchError` const (return True)
                                                          unless err (gets derivsPosition >>= (throwError . ParseError ('!' : "'.':") "not match: " "" ddd340_225 ["derivsChars"]))
                                                       put ddd340_225
                                                       return ()]
                varToken18_85 = foldl1 mplus [do v <- StateT variable
                                                 _ <- StateT spaces
                                                 return ()
                                                 return v]
                typToken19_86 = foldl1 mplus [do t <- StateT typ
                                                 _ <- StateT spaces
                                                 return ()
                                                 return t]
                pap20_87 = foldl1 mplus [do d352_228 <- get
                                            xx351_229 <- StateT derivsChars
                                            case xx351_229 of
                                                '\n' -> return ()
                                                _ -> gets derivsPosition >>= (throwError . ParseError "'\\n'" "not match pattern: " "" d352_228 ["derivsChars"])
                                            let '\n' = xx351_229
                                            return ()
                                            d354_230 <- get
                                            xx353_231 <- StateT derivsChars
                                            case xx353_231 of
                                                '[' -> return ()
                                                _ -> gets derivsPosition >>= (throwError . ParseError "'['" "not match pattern: " "" d354_230 ["derivsChars"])
                                            let '[' = xx353_231
                                            return ()
                                            d356_232 <- get
                                            xx355_233 <- StateT derivsChars
                                            case xx355_233 of
                                                'p' -> return ()
                                                _ -> gets derivsPosition >>= (throwError . ParseError "'p'" "not match pattern: " "" d356_232 ["derivsChars"])
                                            let 'p' = xx355_233
                                            return ()
                                            d358_234 <- get
                                            xx357_235 <- StateT derivsChars
                                            case xx357_235 of
                                                'a' -> return ()
                                                _ -> gets derivsPosition >>= (throwError . ParseError "'a'" "not match pattern: " "" d358_234 ["derivsChars"])
                                            let 'a' = xx357_235
                                            return ()
                                            d360_236 <- get
                                            xx359_237 <- StateT derivsChars
                                            case xx359_237 of
                                                'p' -> return ()
                                                _ -> gets derivsPosition >>= (throwError . ParseError "'p'" "not match pattern: " "" d360_236 ["derivsChars"])
                                            let 'p' = xx359_237
                                            return ()
                                            d362_238 <- get
                                            xx361_239 <- StateT derivsChars
                                            case xx361_239 of
                                                'i' -> return ()
                                                _ -> gets derivsPosition >>= (throwError . ParseError "'i'" "not match pattern: " "" d362_238 ["derivsChars"])
                                            let 'i' = xx361_239
                                            return ()
                                            d364_240 <- get
                                            xx363_241 <- StateT derivsChars
                                            case xx363_241 of
                                                'l' -> return ()
                                                _ -> gets derivsPosition >>= (throwError . ParseError "'l'" "not match pattern: " "" d364_240 ["derivsChars"])
                                            let 'l' = xx363_241
                                            return ()
                                            d366_242 <- get
                                            xx365_243 <- StateT derivsChars
                                            case xx365_243 of
                                                'l' -> return ()
                                                _ -> gets derivsPosition >>= (throwError . ParseError "'l'" "not match pattern: " "" d366_242 ["derivsChars"])
                                            let 'l' = xx365_243
                                            return ()
                                            d368_244 <- get
                                            xx367_245 <- StateT derivsChars
                                            case xx367_245 of
                                                'o' -> return ()
                                                _ -> gets derivsPosition >>= (throwError . ParseError "'o'" "not match pattern: " "" d368_244 ["derivsChars"])
                                            let 'o' = xx367_245
                                            return ()
                                            d370_246 <- get
                                            xx369_247 <- StateT derivsChars
                                            case xx369_247 of
                                                'n' -> return ()
                                                _ -> gets derivsPosition >>= (throwError . ParseError "'n'" "not match pattern: " "" d370_246 ["derivsChars"])
                                            let 'n' = xx369_247
                                            return ()
                                            d372_248 <- get
                                            xx371_249 <- StateT derivsChars
                                            case xx371_249 of
                                                '|' -> return ()
                                                _ -> gets derivsPosition >>= (throwError . ParseError "'|'" "not match pattern: " "" d372_248 ["derivsChars"])
                                            let '|' = xx371_249
                                            return ()
                                            d374_250 <- get
                                            xx373_251 <- StateT derivsChars
                                            case xx373_251 of
                                                '\n' -> return ()
                                                _ -> gets derivsPosition >>= (throwError . ParseError "'\\n'" "not match pattern: " "" d374_250 ["derivsChars"])
                                            let '\n' = xx373_251
                                            return ()
                                            return ()]
                peg21_88 = foldl1 mplus [do _ <- StateT spaces
                                            return ()
                                            s <- StateT sourceType
                                            p <- StateT peg_
                                            return (mkTTPeg s p),
                                         do p <- StateT peg_
                                            return (mkTTPeg tString p)]
                sourceType22_89 = foldl1 mplus [do d384_252 <- get
                                                   xx383_253 <- StateT varToken
                                                   case xx383_253 of
                                                       "source" -> return ()
                                                       _ -> gets derivsPosition >>= (throwError . ParseError "\"source\"" "not match pattern: " "" d384_252 ["varToken"])
                                                   let "source" = xx383_253
                                                   return ()
                                                   d386_254 <- get
                                                   xx385_255 <- StateT derivsChars
                                                   case xx385_255 of
                                                       ':' -> return ()
                                                       _ -> gets derivsPosition >>= (throwError . ParseError "':'" "not match pattern: " "" d386_254 ["derivsChars"])
                                                   let ':' = xx385_255
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
                                                   d402_256 <- get
                                                   xx401_257 <- StateT derivsChars
                                                   case xx401_257 of
                                                       ':' -> return ()
                                                       _ -> gets derivsPosition >>= (throwError . ParseError "':'" "not match pattern: " "" d402_256 ["derivsChars"])
                                                   let ':' = xx401_257
                                                   return ()
                                                   d404_258 <- get
                                                   xx403_259 <- StateT derivsChars
                                                   case xx403_259 of
                                                       ':' -> return ()
                                                       _ -> gets derivsPosition >>= (throwError . ParseError "':'" "not match pattern: " "" d404_258 ["derivsChars"])
                                                   let ':' = xx403_259
                                                   return ()
                                                   _ <- StateT spaces
                                                   return ()
                                                   t <- StateT hsTypeArr
                                                   _ <- StateT spaces
                                                   return ()
                                                   d412_260 <- get
                                                   xx411_261 <- StateT derivsChars
                                                   case xx411_261 of
                                                       '=' -> return ()
                                                       _ -> gets derivsPosition >>= (throwError . ParseError "'='" "not match pattern: " "" d412_260 ["derivsChars"])
                                                   let '=' = xx411_261
                                                   return ()
                                                   _ <- StateT spaces
                                                   return ()
                                                   sel <- StateT selection
                                                   _ <- StateT spaces
                                                   return ()
                                                   d420_262 <- get
                                                   xx419_263 <- StateT derivsChars
                                                   case xx419_263 of
                                                       ';' -> return ()
                                                       _ -> gets derivsPosition >>= (throwError . ParseError "';'" "not match pattern: " "" d420_262 ["derivsChars"])
                                                   let ';' = xx419_263
                                                   return ()
                                                   return (mkDef v t sel)]
                selection25_92 = foldl1 mplus [do ex <- StateT expressionHs
                                                  _ <- StateT spaces
                                                  return ()
                                                  d426_264 <- get
                                                  xx425_265 <- StateT derivsChars
                                                  case xx425_265 of
                                                      '/' -> return ()
                                                      _ -> gets derivsPosition >>= (throwError . ParseError "'/'" "not match pattern: " "" d426_264 ["derivsChars"])
                                                  let '/' = xx425_265
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
                                                     d438_266 <- get
                                                     xx437_267 <- StateT derivsChars
                                                     case xx437_267 of
                                                         '{' -> return ()
                                                         _ -> gets derivsPosition >>= (throwError . ParseError "'{'" "not match pattern: " "" d438_266 ["derivsChars"])
                                                     let '{' = xx437_267
                                                     return ()
                                                     _ <- StateT spaces
                                                     return ()
                                                     h <- StateT hsExpLam
                                                     _ <- StateT spaces
                                                     return ()
                                                     d446_268 <- get
                                                     xx445_269 <- StateT derivsChars
                                                     case xx445_269 of
                                                         '}' -> return ()
                                                         _ -> gets derivsPosition >>= (throwError . ParseError "'}'" "not match pattern: " "" d446_268 ["derivsChars"])
                                                     let '}' = xx445_269
                                                     return ()
                                                     return (mkExpressionHs e h)]
                expression27_94 = foldl1 mplus [do l <- StateT nameLeaf_
                                                   _ <- StateT spaces
                                                   return ()
                                                   e <- StateT expression
                                                   return (cons l e),
                                                return emp]
                nameLeaf_28_95 = foldl1 mplus [do d454_270 <- get
                                                  xx453_271 <- StateT derivsChars
                                                  case xx453_271 of
                                                      '!' -> return ()
                                                      _ -> gets derivsPosition >>= (throwError . ParseError "'!'" "not match pattern: " "" d454_270 ["derivsChars"])
                                                  let '!' = xx453_271
                                                  return ()
                                                  nl <- StateT nameLeafNoCom
                                                  _ <- StateT spaces
                                                  return ()
                                                  com <- optional3_272 (StateT comForErr)
                                                  return (NotAfter nl $ maybe "" id com),
                                               do d462_273 <- get
                                                  xx461_274 <- StateT derivsChars
                                                  let c = xx461_274
                                                  unless (isAmp c) (gets derivsPosition >>= (throwError . ParseError "isAmp c" "not match: " "" d462_273 ["derivsChars"]))
                                                  nl <- StateT nameLeaf
                                                  return (After nl),
                                               do nl <- StateT nameLeaf
                                                  return (Here nl)]
                nameLeaf29_96 = foldl1 mplus [do n <- StateT pat1
                                                 _ <- StateT spaces
                                                 return ()
                                                 com <- optional3_272 (StateT comForErr)
                                                 d474_275 <- get
                                                 xx473_276 <- StateT derivsChars
                                                 case xx473_276 of
                                                     ':' -> return ()
                                                     _ -> gets derivsPosition >>= (throwError . ParseError "':'" "not match pattern: " "" d474_275 ["derivsChars"])
                                                 let ':' = xx473_276
                                                 return ()
                                                 (rf, p) <- StateT leaf
                                                 return (NameLeaf (n, maybe "" id com) rf p),
                                              do n <- StateT pat1
                                                 _ <- StateT spaces
                                                 return ()
                                                 com <- optional3_272 (StateT comForErr)
                                                 return (NameLeaf (n,
                                                                   maybe "" id com) FromToken Nothing)]
                nameLeafNoCom30_97 = foldl1 mplus [do n <- StateT pat1
                                                      _ <- StateT spaces
                                                      return ()
                                                      com <- optional3_272 (StateT comForErr)
                                                      d490_277 <- get
                                                      xx489_278 <- StateT derivsChars
                                                      case xx489_278 of
                                                          ':' -> return ()
                                                          _ -> gets derivsPosition >>= (throwError . ParseError "':'" "not match pattern: " "" d490_277 ["derivsChars"])
                                                      let ':' = xx489_278
                                                      return ()
                                                      (rf, p) <- StateT leaf
                                                      return (NameLeaf (n, maybe "" id com) rf p),
                                                   do n <- StateT pat1
                                                      _ <- StateT spaces
                                                      return ()
                                                      return (NameLeaf (n, "") FromToken Nothing)]
                comForErr31_98 = foldl1 mplus [do d498_279 <- get
                                                  xx497_280 <- StateT derivsChars
                                                  case xx497_280 of
                                                      '{' -> return ()
                                                      _ -> gets derivsPosition >>= (throwError . ParseError "'{'" "not match pattern: " "" d498_279 ["derivsChars"])
                                                  let '{' = xx497_280
                                                  return ()
                                                  d500_281 <- get
                                                  xx499_282 <- StateT derivsChars
                                                  case xx499_282 of
                                                      '-' -> return ()
                                                      _ -> gets derivsPosition >>= (throwError . ParseError "'-'" "not match pattern: " "" d500_281 ["derivsChars"])
                                                  let '-' = xx499_282
                                                  return ()
                                                  d502_283 <- get
                                                  xx501_284 <- StateT derivsChars
                                                  case xx501_284 of
                                                      '#' -> return ()
                                                      _ -> gets derivsPosition >>= (throwError . ParseError "'#'" "not match pattern: " "" d502_283 ["derivsChars"])
                                                  let '#' = xx501_284
                                                  return ()
                                                  _ <- StateT spaces
                                                  return ()
                                                  d506_285 <- get
                                                  xx505_286 <- StateT derivsChars
                                                  case xx505_286 of
                                                      '"' -> return ()
                                                      _ -> gets derivsPosition >>= (throwError . ParseError "'\"'" "not match pattern: " "" d506_285 ["derivsChars"])
                                                  let '"' = xx505_286
                                                  return ()
                                                  s <- StateT stringLit
                                                  d510_287 <- get
                                                  xx509_288 <- StateT derivsChars
                                                  case xx509_288 of
                                                      '"' -> return ()
                                                      _ -> gets derivsPosition >>= (throwError . ParseError "'\"'" "not match pattern: " "" d510_287 ["derivsChars"])
                                                  let '"' = xx509_288
                                                  return ()
                                                  _ <- StateT spaces
                                                  return ()
                                                  d514_289 <- get
                                                  xx513_290 <- StateT derivsChars
                                                  case xx513_290 of
                                                      '#' -> return ()
                                                      _ -> gets derivsPosition >>= (throwError . ParseError "'#'" "not match pattern: " "" d514_289 ["derivsChars"])
                                                  let '#' = xx513_290
                                                  return ()
                                                  d516_291 <- get
                                                  xx515_292 <- StateT derivsChars
                                                  case xx515_292 of
                                                      '-' -> return ()
                                                      _ -> gets derivsPosition >>= (throwError . ParseError "'-'" "not match pattern: " "" d516_291 ["derivsChars"])
                                                  let '-' = xx515_292
                                                  return ()
                                                  d518_293 <- get
                                                  xx517_294 <- StateT derivsChars
                                                  case xx517_294 of
                                                      '}' -> return ()
                                                      _ -> gets derivsPosition >>= (throwError . ParseError "'}'" "not match pattern: " "" d518_293 ["derivsChars"])
                                                  let '}' = xx517_294
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
                                               d540_295 <- get
                                               xx539_296 <- StateT derivsChars
                                               let q = xx539_296
                                               unless (isBQ q) (gets derivsPosition >>= (throwError . ParseError "isBQ q" "not match: " "" d540_295 ["derivsChars"]))
                                               t <- StateT typ
                                               d544_297 <- get
                                               xx543_298 <- StateT derivsChars
                                               let q_ = xx543_298
                                               unless (isBQ q_) (gets derivsPosition >>= (throwError . ParseError "isBQ q_" "not match: " "" d544_297 ["derivsChars"]))
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
                                          do d558_299 <- get
                                             xx557_300 <- StateT derivsChars
                                             case xx557_300 of
                                                 '(' -> return ()
                                                 _ -> gets derivsPosition >>= (throwError . ParseError "'('" "not match pattern: " "" d558_299 ["derivsChars"])
                                             let '(' = xx557_300
                                             return ()
                                             o <- StateT opConName
                                             d562_301 <- get
                                             xx561_302 <- StateT derivsChars
                                             case xx561_302 of
                                                 ')' -> return ()
                                                 _ -> gets derivsPosition >>= (throwError . ParseError "')'" "not match pattern: " "" d562_301 ["derivsChars"])
                                             let ')' = xx561_302
                                             return ()
                                             _ <- StateT spaces
                                             return ()
                                             ps <- StateT pats
                                             return (conP o ps),
                                          do p <- StateT pat1
                                             return p]
                pat135_102 = foldl1 mplus [do t <- StateT typ
                                              return (conToPatQ t emp),
                                           do d572_303 <- get
                                              xx571_304 <- StateT variable
                                              case xx571_304 of
                                                  "_" -> return ()
                                                  _ -> gets derivsPosition >>= (throwError . ParseError "\"_\"" "not match pattern: " "" d572_303 ["variable"])
                                              let "_" = xx571_304
                                              return ()
                                              return wildP,
                                           do n <- StateT variable
                                              return (strToPatQ n),
                                           do i <- StateT integer
                                              return (litP (integerL i)),
                                           do d578_305 <- get
                                              xx577_306 <- StateT derivsChars
                                              case xx577_306 of
                                                  '-' -> return ()
                                                  _ -> gets derivsPosition >>= (throwError . ParseError "'-'" "not match pattern: " "" d578_305 ["derivsChars"])
                                              let '-' = xx577_306
                                              return ()
                                              _ <- StateT spaces
                                              return ()
                                              i <- StateT integer
                                              return (litP (integerL $ negate i)),
                                           do d584_307 <- get
                                              xx583_308 <- StateT derivsChars
                                              case xx583_308 of
                                                  '\'' -> return ()
                                                  _ -> gets derivsPosition >>= (throwError . ParseError "'\\''" "not match pattern: " "" d584_307 ["derivsChars"])
                                              let '\'' = xx583_308
                                              return ()
                                              c <- StateT charLit
                                              d588_309 <- get
                                              xx587_310 <- StateT derivsChars
                                              case xx587_310 of
                                                  '\'' -> return ()
                                                  _ -> gets derivsPosition >>= (throwError . ParseError "'\\''" "not match pattern: " "" d588_309 ["derivsChars"])
                                              let '\'' = xx587_310
                                              return ()
                                              return (charP c),
                                           do d590_311 <- get
                                              xx589_312 <- StateT derivsChars
                                              case xx589_312 of
                                                  '"' -> return ()
                                                  _ -> gets derivsPosition >>= (throwError . ParseError "'\"'" "not match pattern: " "" d590_311 ["derivsChars"])
                                              let '"' = xx589_312
                                              return ()
                                              s <- StateT stringLit
                                              d594_313 <- get
                                              xx593_314 <- StateT derivsChars
                                              case xx593_314 of
                                                  '"' -> return ()
                                                  _ -> gets derivsPosition >>= (throwError . ParseError "'\"'" "not match pattern: " "" d594_313 ["derivsChars"])
                                              let '"' = xx593_314
                                              return ()
                                              return (stringP s),
                                           do d596_315 <- get
                                              xx595_316 <- StateT derivsChars
                                              case xx595_316 of
                                                  '(' -> return ()
                                                  _ -> gets derivsPosition >>= (throwError . ParseError "'('" "not match pattern: " "" d596_315 ["derivsChars"])
                                              let '(' = xx595_316
                                              return ()
                                              p <- StateT patList
                                              d600_317 <- get
                                              xx599_318 <- StateT derivsChars
                                              case xx599_318 of
                                                  ')' -> return ()
                                                  _ -> gets derivsPosition >>= (throwError . ParseError "')'" "not match pattern: " "" d600_317 ["derivsChars"])
                                              let ')' = xx599_318
                                              return ()
                                              return (tupP p),
                                           do d602_319 <- get
                                              xx601_320 <- StateT derivsChars
                                              case xx601_320 of
                                                  '[' -> return ()
                                                  _ -> gets derivsPosition >>= (throwError . ParseError "'['" "not match pattern: " "" d602_319 ["derivsChars"])
                                              let '[' = xx601_320
                                              return ()
                                              p <- StateT patList
                                              d606_321 <- get
                                              xx605_322 <- StateT derivsChars
                                              case xx605_322 of
                                                  ']' -> return ()
                                                  _ -> gets derivsPosition >>= (throwError . ParseError "']'" "not match pattern: " "" d606_321 ["derivsChars"])
                                              let ']' = xx605_322
                                              return ()
                                              return (listP p)]
                patList36_103 = foldl1 mplus [do p <- StateT patOp
                                                 _ <- StateT spaces
                                                 return ()
                                                 d612_323 <- get
                                                 xx611_324 <- StateT derivsChars
                                                 case xx611_324 of
                                                     ',' -> return ()
                                                     _ -> gets derivsPosition >>= (throwError . ParseError "','" "not match pattern: " "" d612_323 ["derivsChars"])
                                                 let ',' = xx611_324
                                                 return ()
                                                 _ <- StateT spaces
                                                 return ()
                                                 ps <- StateT patList
                                                 return (p : ps),
                                              do p <- StateT patOp
                                                 return [p],
                                              return []]
                opConName37_104 = foldl1 mplus [do d620_325 <- get
                                                   xx619_326 <- StateT derivsChars
                                                   case xx619_326 of
                                                       ':' -> return ()
                                                       _ -> gets derivsPosition >>= (throwError . ParseError "':'" "not match pattern: " "" d620_325 ["derivsChars"])
                                                   let ':' = xx619_326
                                                   return ()
                                                   ot <- StateT opTail
                                                   return (mkName $ colon : ot)]
                charLit38_105 = foldl1 mplus [do d624_327 <- get
                                                 xx623_328 <- StateT derivsChars
                                                 let c = xx623_328
                                                 unless (isAlphaNumOt c) (gets derivsPosition >>= (throwError . ParseError "isAlphaNumOt c" "not match: " "" d624_327 ["derivsChars"]))
                                                 return c,
                                              do d626_329 <- get
                                                 xx625_330 <- StateT derivsChars
                                                 case xx625_330 of
                                                     '\\' -> return ()
                                                     _ -> gets derivsPosition >>= (throwError . ParseError "'\\\\'" "not match pattern: " "" d626_329 ["derivsChars"])
                                                 let '\\' = xx625_330
                                                 return ()
                                                 c <- StateT escapeC
                                                 return c]
                stringLit39_106 = foldl1 mplus [do d630_331 <- get
                                                   xx629_332 <- StateT derivsChars
                                                   let c = xx629_332
                                                   unless (isStrLitC c) (gets derivsPosition >>= (throwError . ParseError "isStrLitC c" "not match: " "" d630_331 ["derivsChars"]))
                                                   s <- StateT stringLit
                                                   return (cons c s),
                                                do d634_333 <- get
                                                   xx633_334 <- StateT derivsChars
                                                   case xx633_334 of
                                                       '\\' -> return ()
                                                       _ -> gets derivsPosition >>= (throwError . ParseError "'\\\\'" "not match pattern: " "" d634_333 ["derivsChars"])
                                                   let '\\' = xx633_334
                                                   return ()
                                                   c <- StateT escapeC
                                                   s <- StateT stringLit
                                                   return (c : s),
                                                return emp]
                escapeC40_107 = foldl1 mplus [do d640_335 <- get
                                                 xx639_336 <- StateT derivsChars
                                                 case xx639_336 of
                                                     '"' -> return ()
                                                     _ -> gets derivsPosition >>= (throwError . ParseError "'\"'" "not match pattern: " "" d640_335 ["derivsChars"])
                                                 let '"' = xx639_336
                                                 return ()
                                                 return '"',
                                              do d642_337 <- get
                                                 xx641_338 <- StateT derivsChars
                                                 case xx641_338 of
                                                     '\'' -> return ()
                                                     _ -> gets derivsPosition >>= (throwError . ParseError "'\\''" "not match pattern: " "" d642_337 ["derivsChars"])
                                                 let '\'' = xx641_338
                                                 return ()
                                                 return '\'',
                                              do d644_339 <- get
                                                 xx643_340 <- StateT derivsChars
                                                 case xx643_340 of
                                                     '\\' -> return ()
                                                     _ -> gets derivsPosition >>= (throwError . ParseError "'\\\\'" "not match pattern: " "" d644_339 ["derivsChars"])
                                                 let '\\' = xx643_340
                                                 return ()
                                                 return '\\',
                                              do d646_341 <- get
                                                 xx645_342 <- StateT derivsChars
                                                 case xx645_342 of
                                                     'n' -> return ()
                                                     _ -> gets derivsPosition >>= (throwError . ParseError "'n'" "not match pattern: " "" d646_341 ["derivsChars"])
                                                 let 'n' = xx645_342
                                                 return ()
                                                 return '\n',
                                              do d648_343 <- get
                                                 xx647_344 <- StateT derivsChars
                                                 case xx647_344 of
                                                     't' -> return ()
                                                     _ -> gets derivsPosition >>= (throwError . ParseError "'t'" "not match pattern: " "" d648_343 ["derivsChars"])
                                                 let 't' = xx647_344
                                                 return ()
                                                 return tab]
                pats41_108 = foldl1 mplus [do p <- StateT pat
                                              _ <- StateT spaces
                                              return ()
                                              ps <- StateT pats
                                              return (cons p ps),
                                           return emp]
                readFromLs42_109 = foldl1 mplus [do rf <- StateT readFrom
                                                    d658_345 <- get
                                                    xx657_346 <- StateT derivsChars
                                                    case xx657_346 of
                                                        '*' -> return ()
                                                        _ -> gets derivsPosition >>= (throwError . ParseError "'*'" "not match pattern: " "" d658_345 ["derivsChars"])
                                                    let '*' = xx657_346
                                                    return ()
                                                    return (FromList rf),
                                                 do rf <- StateT readFrom
                                                    d662_347 <- get
                                                    xx661_348 <- StateT derivsChars
                                                    case xx661_348 of
                                                        '+' -> return ()
                                                        _ -> gets derivsPosition >>= (throwError . ParseError "'+'" "not match pattern: " "" d662_347 ["derivsChars"])
                                                    let '+' = xx661_348
                                                    return ()
                                                    return (FromList1 rf),
                                                 do rf <- StateT readFrom
                                                    d666_349 <- get
                                                    xx665_350 <- StateT derivsChars
                                                    case xx665_350 of
                                                        '?' -> return ()
                                                        _ -> gets derivsPosition >>= (throwError . ParseError "'?'" "not match pattern: " "" d666_349 ["derivsChars"])
                                                    let '?' = xx665_350
                                                    return ()
                                                    return (FromOptional rf),
                                                 do rf <- StateT readFrom
                                                    return rf]
                readFrom43_110 = foldl1 mplus [do v <- StateT variable
                                                  return (FromVariable v),
                                               do d672_351 <- get
                                                  xx671_352 <- StateT derivsChars
                                                  case xx671_352 of
                                                      '(' -> return ()
                                                      _ -> gets derivsPosition >>= (throwError . ParseError "'('" "not match pattern: " "" d672_351 ["derivsChars"])
                                                  let '(' = xx671_352
                                                  return ()
                                                  s <- StateT selection
                                                  d676_353 <- get
                                                  xx675_354 <- StateT derivsChars
                                                  case xx675_354 of
                                                      ')' -> return ()
                                                      _ -> gets derivsPosition >>= (throwError . ParseError "')'" "not match pattern: " "" d676_353 ["derivsChars"])
                                                  let ')' = xx675_354
                                                  return ()
                                                  return (FromSelection s)]
                test44_111 = foldl1 mplus [do d678_355 <- get
                                              xx677_356 <- StateT derivsChars
                                              case xx677_356 of
                                                  '[' -> return ()
                                                  _ -> gets derivsPosition >>= (throwError . ParseError "'['" "not match pattern: " "" d678_355 ["derivsChars"])
                                              let '[' = xx677_356
                                              return ()
                                              h <- StateT hsExpLam
                                              _ <- StateT spaces
                                              return ()
                                              com <- optional3_272 (StateT comForErr)
                                              d686_357 <- get
                                              xx685_358 <- StateT derivsChars
                                              case xx685_358 of
                                                  ']' -> return ()
                                                  _ -> gets derivsPosition >>= (throwError . ParseError "']'" "not match pattern: " "" d686_357 ["derivsChars"])
                                              let ']' = xx685_358
                                              return ()
                                              return (h, maybe "" id com)]
                hsExpLam45_112 = foldl1 mplus [do d688_359 <- get
                                                  xx687_360 <- StateT derivsChars
                                                  case xx687_360 of
                                                      '\\' -> return ()
                                                      _ -> gets derivsPosition >>= (throwError . ParseError "'\\\\'" "not match pattern: " "" d688_359 ["derivsChars"])
                                                  let '\\' = xx687_360
                                                  return ()
                                                  _ <- StateT spaces
                                                  return ()
                                                  ps <- StateT pats
                                                  _ <- StateT spaces
                                                  return ()
                                                  d696_361 <- get
                                                  xx695_362 <- StateT derivsChars
                                                  case xx695_362 of
                                                      '-' -> return ()
                                                      _ -> gets derivsPosition >>= (throwError . ParseError "'-'" "not match pattern: " "" d696_361 ["derivsChars"])
                                                  let '-' = xx695_362
                                                  return ()
                                                  d698_363 <- get
                                                  xx697_364 <- StateT derivsChars
                                                  let c = xx697_364
                                                  unless (isGt c) (gets derivsPosition >>= (throwError . ParseError "isGt c" "not match: " "" d698_363 ["derivsChars"]))
                                                  _ <- StateT spaces
                                                  return ()
                                                  e <- StateT hsExpTyp
                                                  return (lamE ps e),
                                               do e <- StateT hsExpTyp
                                                  return e]
                hsExpTyp46_113 = foldl1 mplus [do eo <- StateT hsExpOp
                                                  d708_365 <- get
                                                  xx707_366 <- StateT derivsChars
                                                  case xx707_366 of
                                                      ':' -> return ()
                                                      _ -> gets derivsPosition >>= (throwError . ParseError "':'" "not match pattern: " "" d708_365 ["derivsChars"])
                                                  let ':' = xx707_366
                                                  return ()
                                                  d710_367 <- get
                                                  xx709_368 <- StateT derivsChars
                                                  case xx709_368 of
                                                      ':' -> return ()
                                                      _ -> gets derivsPosition >>= (throwError . ParseError "':'" "not match pattern: " "" d710_367 ["derivsChars"])
                                                  let ':' = xx709_368
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
                hsOp48_115 = foldl1 mplus [do d730_369 <- get
                                              xx729_370 <- StateT derivsChars
                                              let c = xx729_370
                                              unless (isOpHeadChar c) (gets derivsPosition >>= (throwError . ParseError "isOpHeadChar c" "not match: " "" d730_369 ["derivsChars"]))
                                              o <- StateT opTail
                                              return (varE (mkName (cons c o))),
                                           do d734_371 <- get
                                              xx733_372 <- StateT derivsChars
                                              case xx733_372 of
                                                  ':' -> return ()
                                                  _ -> gets derivsPosition >>= (throwError . ParseError "':'" "not match pattern: " "" d734_371 ["derivsChars"])
                                              let ':' = xx733_372
                                              return ()
                                              ddd735_373 <- get
                                              do err <- ((do d737_374 <- get
                                                             xx736_375 <- StateT derivsChars
                                                             case xx736_375 of
                                                                 ':' -> return ()
                                                                 _ -> gets derivsPosition >>= (throwError . ParseError "':'" "not match pattern: " "" d737_374 ["derivsChars"])
                                                             let ':' = xx736_375
                                                             return ()) >> return False) `catchError` const (return True)
                                                 unless err (gets derivsPosition >>= (throwError . ParseError ('!' : "':':") "not match: " "" ddd735_373 ["derivsChars"]))
                                              put ddd735_373
                                              o <- StateT opTail
                                              return (conE (mkName (':' : o))),
                                           do d741_376 <- get
                                              xx740_377 <- StateT derivsChars
                                              let c = xx740_377
                                              unless (isBQ c) (gets derivsPosition >>= (throwError . ParseError "isBQ c" "not match: " "" d741_376 ["derivsChars"]))
                                              v <- StateT variable
                                              d745_378 <- get
                                              xx744_379 <- StateT derivsChars
                                              let c_ = xx744_379
                                              unless (isBQ c_) (gets derivsPosition >>= (throwError . ParseError "isBQ c_" "not match: " "" d745_378 ["derivsChars"]))
                                              return (varE (mkName v)),
                                           do d747_380 <- get
                                              xx746_381 <- StateT derivsChars
                                              let c = xx746_381
                                              unless (isBQ c) (gets derivsPosition >>= (throwError . ParseError "isBQ c" "not match: " "" d747_380 ["derivsChars"]))
                                              t <- StateT typ
                                              d751_382 <- get
                                              xx750_383 <- StateT derivsChars
                                              let c_ = xx750_383
                                              unless (isBQ c_) (gets derivsPosition >>= (throwError . ParseError "isBQ c_" "not match: " "" d751_382 ["derivsChars"]))
                                              return (conE (mkName t))]
                opTail49_116 = foldl1 mplus [do d753_384 <- get
                                                xx752_385 <- StateT derivsChars
                                                let c = xx752_385
                                                unless (isOpTailChar c) (gets derivsPosition >>= (throwError . ParseError "isOpTailChar c" "not match: " "" d753_384 ["derivsChars"]))
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
                hsExp151_118 = foldl1 mplus [do d765_386 <- get
                                                xx764_387 <- StateT derivsChars
                                                case xx764_387 of
                                                    '(' -> return ()
                                                    _ -> gets derivsPosition >>= (throwError . ParseError "'('" "not match pattern: " "" d765_386 ["derivsChars"])
                                                let '(' = xx764_387
                                                return ()
                                                l <- optional3_272 (foldl1 mplus [do e <- StateT hsExpTyp
                                                                                     return e])
                                                _ <- StateT spaces
                                                return ()
                                                o <- StateT hsOp
                                                _ <- StateT spaces
                                                return ()
                                                r <- optional3_272 (foldl1 mplus [do e <- StateT hsExpTyp
                                                                                     return e])
                                                d781_388 <- get
                                                xx780_389 <- StateT derivsChars
                                                case xx780_389 of
                                                    ')' -> return ()
                                                    _ -> gets derivsPosition >>= (throwError . ParseError "')'" "not match pattern: " "" d781_388 ["derivsChars"])
                                                let ')' = xx780_389
                                                return ()
                                                return (infixE l o r),
                                             do d783_390 <- get
                                                xx782_391 <- StateT derivsChars
                                                case xx782_391 of
                                                    '(' -> return ()
                                                    _ -> gets derivsPosition >>= (throwError . ParseError "'('" "not match pattern: " "" d783_390 ["derivsChars"])
                                                let '(' = xx782_391
                                                return ()
                                                et <- StateT hsExpTpl
                                                d787_392 <- get
                                                xx786_393 <- StateT derivsChars
                                                case xx786_393 of
                                                    ')' -> return ()
                                                    _ -> gets derivsPosition >>= (throwError . ParseError "')'" "not match pattern: " "" d787_392 ["derivsChars"])
                                                let ')' = xx786_393
                                                return ()
                                                return (tupE et),
                                             do d789_394 <- get
                                                xx788_395 <- StateT derivsChars
                                                case xx788_395 of
                                                    '[' -> return ()
                                                    _ -> gets derivsPosition >>= (throwError . ParseError "'['" "not match pattern: " "" d789_394 ["derivsChars"])
                                                let '[' = xx788_395
                                                return ()
                                                et <- StateT hsExpTpl
                                                d793_396 <- get
                                                xx792_397 <- StateT derivsChars
                                                case xx792_397 of
                                                    ']' -> return ()
                                                    _ -> gets derivsPosition >>= (throwError . ParseError "']'" "not match pattern: " "" d793_396 ["derivsChars"])
                                                let ']' = xx792_397
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
                                             do d803_398 <- get
                                                xx802_399 <- StateT derivsChars
                                                case xx802_399 of
                                                    '\'' -> return ()
                                                    _ -> gets derivsPosition >>= (throwError . ParseError "'\\''" "not match pattern: " "" d803_398 ["derivsChars"])
                                                let '\'' = xx802_399
                                                return ()
                                                c <- StateT charLit
                                                d807_400 <- get
                                                xx806_401 <- StateT derivsChars
                                                case xx806_401 of
                                                    '\'' -> return ()
                                                    _ -> gets derivsPosition >>= (throwError . ParseError "'\\''" "not match pattern: " "" d807_400 ["derivsChars"])
                                                let '\'' = xx806_401
                                                return ()
                                                return (litE (charL c)),
                                             do d809_402 <- get
                                                xx808_403 <- StateT derivsChars
                                                case xx808_403 of
                                                    '"' -> return ()
                                                    _ -> gets derivsPosition >>= (throwError . ParseError "'\"'" "not match pattern: " "" d809_402 ["derivsChars"])
                                                let '"' = xx808_403
                                                return ()
                                                s <- StateT stringLit
                                                d813_404 <- get
                                                xx812_405 <- StateT derivsChars
                                                case xx812_405 of
                                                    '"' -> return ()
                                                    _ -> gets derivsPosition >>= (throwError . ParseError "'\"'" "not match pattern: " "" d813_404 ["derivsChars"])
                                                let '"' = xx812_405
                                                return ()
                                                return (litE (stringL s)),
                                             do d815_406 <- get
                                                xx814_407 <- StateT derivsChars
                                                case xx814_407 of
                                                    '-' -> return ()
                                                    _ -> gets derivsPosition >>= (throwError . ParseError "'-'" "not match pattern: " "" d815_406 ["derivsChars"])
                                                let '-' = xx814_407
                                                return ()
                                                _ <- StateT spaces
                                                return ()
                                                e <- StateT hsExp1
                                                return (appE (varE $ mkName "negate") e)]
                hsExpTpl52_119 = foldl1 mplus [do e <- StateT hsExpLam
                                                  _ <- StateT spaces
                                                  return ()
                                                  d825_408 <- get
                                                  xx824_409 <- StateT derivsChars
                                                  let c = xx824_409
                                                  unless (isComma c) (gets derivsPosition >>= (throwError . ParseError "isComma c" "not match: " "" d825_408 ["derivsChars"]))
                                                  _ <- StateT spaces
                                                  return ()
                                                  et <- StateT hsExpTpl
                                                  return (cons e et),
                                               do e <- StateT hsExpLam
                                                  return (cons e emp),
                                               return emp]
                hsTypeArr53_120 = foldl1 mplus [do l <- StateT hsType
                                                   d835_410 <- get
                                                   xx834_411 <- StateT derivsChars
                                                   case xx834_411 of
                                                       '-' -> return ()
                                                       _ -> gets derivsPosition >>= (throwError . ParseError "'-'" "not match pattern: " "" d835_410 ["derivsChars"])
                                                   let '-' = xx834_411
                                                   return ()
                                                   d837_412 <- get
                                                   xx836_413 <- StateT derivsChars
                                                   let c = xx836_413
                                                   unless (isGt c) (gets derivsPosition >>= (throwError . ParseError "isGt c" "not match: " "" d837_412 ["derivsChars"]))
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
                hsType155_122 = foldl1 mplus [do d851_414 <- get
                                                 xx850_415 <- StateT derivsChars
                                                 case xx850_415 of
                                                     '[' -> return ()
                                                     _ -> gets derivsPosition >>= (throwError . ParseError "'['" "not match pattern: " "" d851_414 ["derivsChars"])
                                                 let '[' = xx850_415
                                                 return ()
                                                 d853_416 <- get
                                                 xx852_417 <- StateT derivsChars
                                                 case xx852_417 of
                                                     ']' -> return ()
                                                     _ -> gets derivsPosition >>= (throwError . ParseError "']'" "not match pattern: " "" d853_416 ["derivsChars"])
                                                 let ']' = xx852_417
                                                 return ()
                                                 _ <- StateT spaces
                                                 return ()
                                                 return listT,
                                              do d857_418 <- get
                                                 xx856_419 <- StateT derivsChars
                                                 case xx856_419 of
                                                     '[' -> return ()
                                                     _ -> gets derivsPosition >>= (throwError . ParseError "'['" "not match pattern: " "" d857_418 ["derivsChars"])
                                                 let '[' = xx856_419
                                                 return ()
                                                 t <- StateT hsTypeArr
                                                 d861_420 <- get
                                                 xx860_421 <- StateT derivsChars
                                                 case xx860_421 of
                                                     ']' -> return ()
                                                     _ -> gets derivsPosition >>= (throwError . ParseError "']'" "not match pattern: " "" d861_420 ["derivsChars"])
                                                 let ']' = xx860_421
                                                 return ()
                                                 _ <- StateT spaces
                                                 return ()
                                                 return (appT listT t),
                                              do d865_422 <- get
                                                 xx864_423 <- StateT derivsChars
                                                 case xx864_423 of
                                                     '(' -> return ()
                                                     _ -> gets derivsPosition >>= (throwError . ParseError "'('" "not match pattern: " "" d865_422 ["derivsChars"])
                                                 let '(' = xx864_423
                                                 return ()
                                                 _ <- StateT spaces
                                                 return ()
                                                 tt <- StateT hsTypeTpl
                                                 d871_424 <- get
                                                 xx870_425 <- StateT derivsChars
                                                 case xx870_425 of
                                                     ')' -> return ()
                                                     _ -> gets derivsPosition >>= (throwError . ParseError "')'" "not match pattern: " "" d871_424 ["derivsChars"])
                                                 let ')' = xx870_425
                                                 return ()
                                                 return (tupT tt),
                                              do t <- StateT typToken
                                                 return (conT (mkName t)),
                                              do d875_426 <- get
                                                 xx874_427 <- StateT derivsChars
                                                 case xx874_427 of
                                                     '(' -> return ()
                                                     _ -> gets derivsPosition >>= (throwError . ParseError "'('" "not match pattern: " "" d875_426 ["derivsChars"])
                                                 let '(' = xx874_427
                                                 return ()
                                                 d877_428 <- get
                                                 xx876_429 <- StateT derivsChars
                                                 case xx876_429 of
                                                     '-' -> return ()
                                                     _ -> gets derivsPosition >>= (throwError . ParseError "'-'" "not match pattern: " "" d877_428 ["derivsChars"])
                                                 let '-' = xx876_429
                                                 return ()
                                                 d879_430 <- get
                                                 xx878_431 <- StateT derivsChars
                                                 let c = xx878_431
                                                 unless (isGt c) (gets derivsPosition >>= (throwError . ParseError "isGt c" "not match: " "" d879_430 ["derivsChars"]))
                                                 d881_432 <- get
                                                 xx880_433 <- StateT derivsChars
                                                 case xx880_433 of
                                                     ')' -> return ()
                                                     _ -> gets derivsPosition >>= (throwError . ParseError "')'" "not match pattern: " "" d881_432 ["derivsChars"])
                                                 let ')' = xx880_433
                                                 return ()
                                                 _ <- StateT spaces
                                                 return ()
                                                 return arrowT]
                hsTypeTpl56_123 = foldl1 mplus [do t <- StateT hsTypeArr
                                                   d887_434 <- get
                                                   xx886_435 <- StateT derivsChars
                                                   let c = xx886_435
                                                   unless (isComma c) (gets derivsPosition >>= (throwError . ParseError "isComma c" "not match: " "" d887_434 ["derivsChars"]))
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
                                                 ds <- list1_436 (foldl1 mplus [do d <- StateT digit
                                                                                   return d])
                                                 return (read (cons dh ds))]
                alpha61_128 = foldl1 mplus [do u <- StateT upper
                                               return u,
                                            do l <- StateT lower
                                               return l,
                                            do d <- StateT digit
                                               return d,
                                            do d919_437 <- get
                                               xx918_438 <- StateT derivsChars
                                               case xx918_438 of
                                                   '\'' -> return ()
                                                   _ -> gets derivsPosition >>= (throwError . ParseError "'\\''" "not match pattern: " "" d919_437 ["derivsChars"])
                                               let '\'' = xx918_438
                                               return ()
                                               return '\'']
                upper62_129 = foldl1 mplus [do d921_439 <- get
                                               xx920_440 <- StateT derivsChars
                                               let u = xx920_440
                                               unless (isUpper u) (gets derivsPosition >>= (throwError . ParseError "isUpper u" "not match: " "" d921_439 ["derivsChars"]))
                                               return u]
                lower63_130 = foldl1 mplus [do d923_441 <- get
                                               xx922_442 <- StateT derivsChars
                                               let l = xx922_442
                                               unless (isLowerU l) (gets derivsPosition >>= (throwError . ParseError "isLowerU l" "not match: " "" d923_441 ["derivsChars"]))
                                               return l]
                digit64_131 = foldl1 mplus [do d925_443 <- get
                                               xx924_444 <- StateT derivsChars
                                               let d = xx924_444
                                               unless (isDigit d) (gets derivsPosition >>= (throwError . ParseError "isDigit d" "not match: " "" d925_443 ["derivsChars"]))
                                               return d]
                spaces65_132 = foldl1 mplus [do _ <- StateT space
                                                return ()
                                                _ <- StateT spaces
                                                return ()
                                                return (),
                                             return ()]
                space66_133 = foldl1 mplus [do d931_445 <- get
                                               xx930_446 <- StateT derivsChars
                                               let s = xx930_446
                                               unless (isSpace s) (gets derivsPosition >>= (throwError . ParseError "isSpace s" "not match: " "" d931_445 ["derivsChars"]))
                                               return (),
                                            do d933_447 <- get
                                               xx932_448 <- StateT derivsChars
                                               case xx932_448 of
                                                   '-' -> return ()
                                                   _ -> gets derivsPosition >>= (throwError . ParseError "'-'" "not match pattern: " "" d933_447 ["derivsChars"])
                                               let '-' = xx932_448
                                               return ()
                                               d935_449 <- get
                                               xx934_450 <- StateT derivsChars
                                               case xx934_450 of
                                                   '-' -> return ()
                                                   _ -> gets derivsPosition >>= (throwError . ParseError "'-'" "not match pattern: " "" d935_449 ["derivsChars"])
                                               let '-' = xx934_450
                                               return ()
                                               _ <- StateT notNLString
                                               return ()
                                               _ <- StateT newLine
                                               return ()
                                               return (),
                                            do _ <- StateT comment
                                               return ()
                                               return ()]
                notNLString67_134 = foldl1 mplus [do ddd942_451 <- get
                                                     do err <- ((do _ <- StateT newLine
                                                                    return ()) >> return False) `catchError` const (return True)
                                                        unless err (gets derivsPosition >>= (throwError . ParseError ('!' : "_:newLine") "not match: " "" ddd942_451 ["newLine"]))
                                                     put ddd942_451
                                                     c <- StateT derivsChars
                                                     s <- StateT notNLString
                                                     return (cons c s),
                                                  return emp]
                newLine68_135 = foldl1 mplus [do d950_452 <- get
                                                 xx949_453 <- StateT derivsChars
                                                 case xx949_453 of
                                                     '\n' -> return ()
                                                     _ -> gets derivsPosition >>= (throwError . ParseError "'\\n'" "not match pattern: " "" d950_452 ["derivsChars"])
                                                 let '\n' = xx949_453
                                                 return ()
                                                 return ()]
                comment69_136 = foldl1 mplus [do d952_454 <- get
                                                 xx951_455 <- StateT derivsChars
                                                 case xx951_455 of
                                                     '{' -> return ()
                                                     _ -> gets derivsPosition >>= (throwError . ParseError "'{'" "not match pattern: " "" d952_454 ["derivsChars"])
                                                 let '{' = xx951_455
                                                 return ()
                                                 d954_456 <- get
                                                 xx953_457 <- StateT derivsChars
                                                 case xx953_457 of
                                                     '-' -> return ()
                                                     _ -> gets derivsPosition >>= (throwError . ParseError "'-'" "not match pattern: " "" d954_456 ["derivsChars"])
                                                 let '-' = xx953_457
                                                 return ()
                                                 ddd955_458 <- get
                                                 do err <- ((do d957_459 <- get
                                                                xx956_460 <- StateT derivsChars
                                                                case xx956_460 of
                                                                    '#' -> return ()
                                                                    _ -> gets derivsPosition >>= (throwError . ParseError "'#'" "not match pattern: " "" d957_459 ["derivsChars"])
                                                                let '#' = xx956_460
                                                                return ()) >> return False) `catchError` const (return True)
                                                    unless err (gets derivsPosition >>= (throwError . ParseError ('!' : "'#':") "not match: " "" ddd955_458 ["derivsChars"]))
                                                 put ddd955_458
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
                notComStr71_138 = foldl1 mplus [do ddd970_461 <- get
                                                   do err <- ((do _ <- StateT comment
                                                                  return ()) >> return False) `catchError` const (return True)
                                                      unless err (gets derivsPosition >>= (throwError . ParseError ('!' : "_:comment") "not match: " "" ddd970_461 ["comment"]))
                                                   put ddd970_461
                                                   ddd973_462 <- get
                                                   do err <- ((do _ <- StateT comEnd
                                                                  return ()) >> return False) `catchError` const (return True)
                                                      unless err (gets derivsPosition >>= (throwError . ParseError ('!' : "_:comEnd") "not match: " "" ddd973_462 ["comEnd"]))
                                                   put ddd973_462
                                                   _ <- StateT derivsChars
                                                   return ()
                                                   _ <- StateT notComStr
                                                   return ()
                                                   return (),
                                                return ()]
                comEnd72_139 = foldl1 mplus [do d981_463 <- get
                                                xx980_464 <- StateT derivsChars
                                                case xx980_464 of
                                                    '-' -> return ()
                                                    _ -> gets derivsPosition >>= (throwError . ParseError "'-'" "not match pattern: " "" d981_463 ["derivsChars"])
                                                let '-' = xx980_464
                                                return ()
                                                d983_465 <- get
                                                xx982_466 <- StateT derivsChars
                                                case xx982_466 of
                                                    '}' -> return ()
                                                    _ -> gets derivsPosition >>= (throwError . ParseError "'}'" "not match pattern: " "" d983_465 ["derivsChars"])
                                                let '}' = xx982_466
                                                return ()
                                                return ()]
                list1_436 :: forall m a . (MonadPlus m, Applicative m) =>
                                          m a -> m ([a])
                list12_467 :: forall m a . (MonadPlus m, Applicative m) =>
                                           m a -> m ([a])
                list1_436 p = list12_467 p `mplus` return []
                list12_467 p = ((:) <$> p) <*> list1_436 p
                optional3_272 :: forall m a . (MonadPlus m, Applicative m) =>
                                              m a -> m (Maybe a)
                optional3_272 p = (Just <$> p) `mplus` return Nothing

