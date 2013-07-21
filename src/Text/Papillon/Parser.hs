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
	mkParseError,
	Derivs(peg, pegFile, char),
	Pos(..),
	ListPos(..),
	pePositionS,
	Source(..),
	SourceList(..),

	PPragma(..),
	ModuleName,
	ExportList,
	Code
) where

import Text.Papillon.Papillon
import Control.Applicative
import "monads-tf" Control.Monad.State
import "monads-tf" Control.Monad.Error



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
              pragmaStr :: (Either (ParseError (Pos String) Derivs)
                                   ((String, Derivs))),
              pragmaItems :: (Either (ParseError (Pos String) Derivs)
                                     (([String], Derivs))),
              pragmaEnd :: (Either (ParseError (Pos String) Derivs)
                                   (((), Derivs))),
              moduleDec :: (Either (ParseError (Pos String) Derivs)
                                   ((Maybe (([String], Maybe String)), Derivs))),
              moduleName :: (Either (ParseError (Pos String) Derivs)
                                    ((ModuleName, Derivs))),
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
              char :: (Either (ParseError (Pos String) Derivs)
                              ((Token String, Derivs))),
              position :: (Pos String)}
parse :: String -> Derivs
parse = parse0_0 initialPos
          where parse0_0 pos s = d
                             where d = Derivs pegFile73_1 pragmas74_2 pragma75_3 pragmaStr76_4 pragmaItems77_5 pragmaEnd78_6 moduleDec79_7 moduleName80_8 moduleDecStr81_9 whr82_10 preImpPap83_11 prePeg84_12 afterPeg85_13 importPapillon86_14 varToken87_15 typToken88_16 pap89_17 peg90_18 sourceType91_19 peg_92_20 definition93_21 selection94_22 expressionHs95_23 expression96_24 nameLeaf_97_25 nameLeaf98_26 nameLeafNoCom99_27 comForErr100_28 leaf101_29 patOp102_30 pat103_31 pat1104_32 patList105_33 opConName106_34 charLit107_35 stringLit108_36 escapeC109_37 pats110_38 readFromLs111_39 readFrom112_40 test113_41 hsExpLam114_42 hsExpTyp115_43 hsExpOp116_44 hsOp117_45 opTail118_46 hsExp119_47 hsExp1120_48 hsExpTpl121_49 hsTypeArr122_50 hsType123_51 hsType1124_52 hsTypeTpl125_53 typ126_54 variable127_55 tvtail128_56 integer129_57 alpha130_58 upper131_59 lower132_60 digit133_61 spaces134_62 space135_63 notNLString136_64 newLine137_65 comment138_66 comments139_67 notComStr140_68 comEnd141_69 chars142_70 pos
                                   pegFile73_1 = runStateT pegFile4_71 d
                                   pragmas74_2 = runStateT pragmas5_72 d
                                   pragma75_3 = runStateT pragma6_73 d
                                   pragmaStr76_4 = runStateT pragmaStr7_74 d
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
                                                                _ -> gets position >>= (throwError . mkParseError "" "end of input" "" undefined [])) d
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
                                               xx159_141 <- StateT char
                                               case xx159_141 of
                                                   '|' -> return ()
                                                   _ -> gets position >>= (throwError . mkParseError "'|'" "not match pattern: " "" d160_140 ["char"])
                                               let '|' = xx159_141
                                               return ()
                                               d162_142 <- get
                                               xx161_143 <- StateT char
                                               case xx161_143 of
                                                   ']' -> return ()
                                                   _ -> gets position >>= (throwError . mkParseError "']'" "not match pattern: " "" d162_142 ["char"])
                                               let ']' = xx161_143
                                               return ()
                                               d164_144 <- get
                                               xx163_145 <- StateT char
                                               case xx163_145 of
                                                   '\n' -> return ()
                                                   _ -> gets position >>= (throwError . mkParseError "'\\n'" "not match pattern: " "" d164_144 ["char"])
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
                                               xx179_147 <- StateT char
                                               case xx179_147 of
                                                   '|' -> return ()
                                                   _ -> gets position >>= (throwError . mkParseError "'|'" "not match pattern: " "" d180_146 ["char"])
                                               let '|' = xx179_147
                                               return ()
                                               d182_148 <- get
                                               xx181_149 <- StateT char
                                               case xx181_149 of
                                                   ']' -> return ()
                                                   _ -> gets position >>= (throwError . mkParseError "']'" "not match pattern: " "" d182_148 ["char"])
                                               let ']' = xx181_149
                                               return ()
                                               d184_150 <- get
                                               xx183_151 <- StateT char
                                               case xx183_151 of
                                                   '\n' -> return ()
                                                   _ -> gets position >>= (throwError . mkParseError "'\\n'" "not match pattern: " "" d184_150 ["char"])
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
                                              xx195_153 <- StateT char
                                              case xx195_153 of
                                                  '{' -> return ()
                                                  _ -> gets position >>= (throwError . mkParseError "'{'" "not match pattern: " "" d196_152 ["char"])
                                              let '{' = xx195_153
                                              return ()
                                              d198_154 <- get
                                              xx197_155 <- StateT char
                                              case xx197_155 of
                                                  '-' -> return ()
                                                  _ -> gets position >>= (throwError . mkParseError "'-'" "not match pattern: " "" d198_154 ["char"])
                                              let '-' = xx197_155
                                              return ()
                                              d200_156 <- get
                                              xx199_157 <- StateT char
                                              case xx199_157 of
                                                  '#' -> return ()
                                                  _ -> gets position >>= (throwError . mkParseError "'#'" "not match pattern: " "" d200_156 ["char"])
                                              let '#' = xx199_157
                                              return ()
                                              _ <- StateT spaces
                                              return ()
                                              d204_158 <- get
                                              xx203_159 <- StateT char
                                              case xx203_159 of
                                                  'L' -> return ()
                                                  _ -> gets position >>= (throwError . mkParseError "'L'" "not match pattern: " "" d204_158 ["char"])
                                              let 'L' = xx203_159
                                              return ()
                                              d206_160 <- get
                                              xx205_161 <- StateT char
                                              case xx205_161 of
                                                  'A' -> return ()
                                                  _ -> gets position >>= (throwError . mkParseError "'A'" "not match pattern: " "" d206_160 ["char"])
                                              let 'A' = xx205_161
                                              return ()
                                              d208_162 <- get
                                              xx207_163 <- StateT char
                                              case xx207_163 of
                                                  'N' -> return ()
                                                  _ -> gets position >>= (throwError . mkParseError "'N'" "not match pattern: " "" d208_162 ["char"])
                                              let 'N' = xx207_163
                                              return ()
                                              d210_164 <- get
                                              xx209_165 <- StateT char
                                              case xx209_165 of
                                                  'G' -> return ()
                                                  _ -> gets position >>= (throwError . mkParseError "'G'" "not match pattern: " "" d210_164 ["char"])
                                              let 'G' = xx209_165
                                              return ()
                                              d212_166 <- get
                                              xx211_167 <- StateT char
                                              case xx211_167 of
                                                  'U' -> return ()
                                                  _ -> gets position >>= (throwError . mkParseError "'U'" "not match pattern: " "" d212_166 ["char"])
                                              let 'U' = xx211_167
                                              return ()
                                              d214_168 <- get
                                              xx213_169 <- StateT char
                                              case xx213_169 of
                                                  'A' -> return ()
                                                  _ -> gets position >>= (throwError . mkParseError "'A'" "not match pattern: " "" d214_168 ["char"])
                                              let 'A' = xx213_169
                                              return ()
                                              d216_170 <- get
                                              xx215_171 <- StateT char
                                              case xx215_171 of
                                                  'G' -> return ()
                                                  _ -> gets position >>= (throwError . mkParseError "'G'" "not match pattern: " "" d216_170 ["char"])
                                              let 'G' = xx215_171
                                              return ()
                                              d218_172 <- get
                                              xx217_173 <- StateT char
                                              case xx217_173 of
                                                  'E' -> return ()
                                                  _ -> gets position >>= (throwError . mkParseError "'E'" "not match pattern: " "" d218_172 ["char"])
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
                                              xx227_175 <- StateT char
                                              case xx227_175 of
                                                  '{' -> return ()
                                                  _ -> gets position >>= (throwError . mkParseError "'{'" "not match pattern: " "" d228_174 ["char"])
                                              let '{' = xx227_175
                                              return ()
                                              d230_176 <- get
                                              xx229_177 <- StateT char
                                              case xx229_177 of
                                                  '-' -> return ()
                                                  _ -> gets position >>= (throwError . mkParseError "'-'" "not match pattern: " "" d230_176 ["char"])
                                              let '-' = xx229_177
                                              return ()
                                              d232_178 <- get
                                              xx231_179 <- StateT char
                                              case xx231_179 of
                                                  '#' -> return ()
                                                  _ -> gets position >>= (throwError . mkParseError "'#'" "not match pattern: " "" d232_178 ["char"])
                                              let '#' = xx231_179
                                              return ()
                                              _ <- StateT spaces
                                              return ()
                                              s <- StateT pragmaStr
                                              _ <- StateT pragmaEnd
                                              return ()
                                              return (OtherPragma s)]
                pragmaStr7_74 = foldl1 mplus [do ddd239_180 <- get
                                                 do err <- ((do _ <- StateT pragmaEnd
                                                                return ()) >> return False) `catchError` const (return True)
                                                    unless err (gets position >>= (throwError . mkParseError ('!' : "_:pragmaEnd") "not match: " "" ddd239_180 ["pragmaEnd"]))
                                                 put ddd239_180
                                                 c <- StateT char
                                                 s <- StateT pragmaStr
                                                 return (c : s),
                                              return ""]
                pragmaItems8_75 = foldl1 mplus [do t <- StateT typToken
                                                   d249_181 <- get
                                                   xx248_182 <- StateT char
                                                   case xx248_182 of
                                                       ',' -> return ()
                                                       _ -> gets position >>= (throwError . mkParseError "','" "not match pattern: " "" d249_181 ["char"])
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
                                                 xx258_184 <- StateT char
                                                 case xx258_184 of
                                                     '#' -> return ()
                                                     _ -> gets position >>= (throwError . mkParseError "'#'" "not match pattern: " "" d259_183 ["char"])
                                                 let '#' = xx258_184
                                                 return ()
                                                 d261_185 <- get
                                                 xx260_186 <- StateT char
                                                 case xx260_186 of
                                                     '-' -> return ()
                                                     _ -> gets position >>= (throwError . mkParseError "'-'" "not match pattern: " "" d261_185 ["char"])
                                                 let '-' = xx260_186
                                                 return ()
                                                 d263_187 <- get
                                                 xx262_188 <- StateT char
                                                 case xx262_188 of
                                                     '}' -> return ()
                                                     _ -> gets position >>= (throwError . mkParseError "'}'" "not match pattern: " "" d263_187 ["char"])
                                                 let '}' = xx262_188
                                                 return ()
                                                 return ()]
                moduleDec10_77 = foldl1 mplus [do d265_189 <- get
                                                  xx264_190 <- StateT char
                                                  case xx264_190 of
                                                      'm' -> return ()
                                                      _ -> gets position >>= (throwError . mkParseError "'m'" "not match pattern: " "" d265_189 ["char"])
                                                  let 'm' = xx264_190
                                                  return ()
                                                  d267_191 <- get
                                                  xx266_192 <- StateT char
                                                  case xx266_192 of
                                                      'o' -> return ()
                                                      _ -> gets position >>= (throwError . mkParseError "'o'" "not match pattern: " "" d267_191 ["char"])
                                                  let 'o' = xx266_192
                                                  return ()
                                                  d269_193 <- get
                                                  xx268_194 <- StateT char
                                                  case xx268_194 of
                                                      'd' -> return ()
                                                      _ -> gets position >>= (throwError . mkParseError "'d'" "not match pattern: " "" d269_193 ["char"])
                                                  let 'd' = xx268_194
                                                  return ()
                                                  d271_195 <- get
                                                  xx270_196 <- StateT char
                                                  case xx270_196 of
                                                      'u' -> return ()
                                                      _ -> gets position >>= (throwError . mkParseError "'u'" "not match pattern: " "" d271_195 ["char"])
                                                  let 'u' = xx270_196
                                                  return ()
                                                  d273_197 <- get
                                                  xx272_198 <- StateT char
                                                  case xx272_198 of
                                                      'l' -> return ()
                                                      _ -> gets position >>= (throwError . mkParseError "'l'" "not match pattern: " "" d273_197 ["char"])
                                                  let 'l' = xx272_198
                                                  return ()
                                                  d275_199 <- get
                                                  xx274_200 <- StateT char
                                                  case xx274_200 of
                                                      'e' -> return ()
                                                      _ -> gets position >>= (throwError . mkParseError "'e'" "not match pattern: " "" d275_199 ["char"])
                                                  let 'e' = xx274_200
                                                  return ()
                                                  _ <- StateT spaces
                                                  return ()
                                                  n <- StateT moduleName
                                                  _ <- StateT spaces
                                                  return ()
                                                  d283_201 <- get
                                                  xx282_202 <- StateT char
                                                  case xx282_202 of
                                                      '(' -> return ()
                                                      _ -> gets position >>= (throwError . mkParseError "'('" "not match pattern: " "" d283_201 ["char"])
                                                  let '(' = xx282_202
                                                  return ()
                                                  _ <- StateT spaces
                                                  return ()
                                                  s <- StateT moduleDecStr
                                                  _ <- StateT whr
                                                  return ()
                                                  return (Just (n, Just s)),
                                               do d291_203 <- get
                                                  xx290_204 <- StateT char
                                                  case xx290_204 of
                                                      'm' -> return ()
                                                      _ -> gets position >>= (throwError . mkParseError "'m'" "not match pattern: " "" d291_203 ["char"])
                                                  let 'm' = xx290_204
                                                  return ()
                                                  d293_205 <- get
                                                  xx292_206 <- StateT char
                                                  case xx292_206 of
                                                      'o' -> return ()
                                                      _ -> gets position >>= (throwError . mkParseError "'o'" "not match pattern: " "" d293_205 ["char"])
                                                  let 'o' = xx292_206
                                                  return ()
                                                  d295_207 <- get
                                                  xx294_208 <- StateT char
                                                  case xx294_208 of
                                                      'd' -> return ()
                                                      _ -> gets position >>= (throwError . mkParseError "'d'" "not match pattern: " "" d295_207 ["char"])
                                                  let 'd' = xx294_208
                                                  return ()
                                                  d297_209 <- get
                                                  xx296_210 <- StateT char
                                                  case xx296_210 of
                                                      'u' -> return ()
                                                      _ -> gets position >>= (throwError . mkParseError "'u'" "not match pattern: " "" d297_209 ["char"])
                                                  let 'u' = xx296_210
                                                  return ()
                                                  d299_211 <- get
                                                  xx298_212 <- StateT char
                                                  case xx298_212 of
                                                      'l' -> return ()
                                                      _ -> gets position >>= (throwError . mkParseError "'l'" "not match pattern: " "" d299_211 ["char"])
                                                  let 'l' = xx298_212
                                                  return ()
                                                  d301_213 <- get
                                                  xx300_214 <- StateT char
                                                  case xx300_214 of
                                                      'e' -> return ()
                                                      _ -> gets position >>= (throwError . mkParseError "'e'" "not match pattern: " "" d301_213 ["char"])
                                                  let 'e' = xx300_214
                                                  return ()
                                                  _ <- StateT spaces
                                                  return ()
                                                  n <- StateT moduleName
                                                  _ <- StateT spaces
                                                  return ()
                                                  d309_215 <- get
                                                  xx308_216 <- StateT char
                                                  case xx308_216 of
                                                      'w' -> return ()
                                                      _ -> gets position >>= (throwError . mkParseError "'w'" "not match pattern: " "" d309_215 ["char"])
                                                  let 'w' = xx308_216
                                                  return ()
                                                  d311_217 <- get
                                                  xx310_218 <- StateT char
                                                  case xx310_218 of
                                                      'h' -> return ()
                                                      _ -> gets position >>= (throwError . mkParseError "'h'" "not match pattern: " "" d311_217 ["char"])
                                                  let 'h' = xx310_218
                                                  return ()
                                                  d313_219 <- get
                                                  xx312_220 <- StateT char
                                                  case xx312_220 of
                                                      'e' -> return ()
                                                      _ -> gets position >>= (throwError . mkParseError "'e'" "not match pattern: " "" d313_219 ["char"])
                                                  let 'e' = xx312_220
                                                  return ()
                                                  d315_221 <- get
                                                  xx314_222 <- StateT char
                                                  case xx314_222 of
                                                      'r' -> return ()
                                                      _ -> gets position >>= (throwError . mkParseError "'r'" "not match pattern: " "" d315_221 ["char"])
                                                  let 'r' = xx314_222
                                                  return ()
                                                  d317_223 <- get
                                                  xx316_224 <- StateT char
                                                  case xx316_224 of
                                                      'e' -> return ()
                                                      _ -> gets position >>= (throwError . mkParseError "'e'" "not match pattern: " "" d317_223 ["char"])
                                                  let 'e' = xx316_224
                                                  return ()
                                                  _ <- StateT spaces
                                                  return ()
                                                  return (Just (n, Nothing)),
                                               return Nothing]
                moduleName11_78 = foldl1 mplus [do t <- StateT typ
                                                   d323_225 <- get
                                                   xx322_226 <- StateT char
                                                   case xx322_226 of
                                                       '.' -> return ()
                                                       _ -> gets position >>= (throwError . mkParseError "'.'" "not match pattern: " "" d323_225 ["char"])
                                                   let '.' = xx322_226
                                                   return ()
                                                   n <- StateT moduleName
                                                   return (t : n),
                                                do t <- StateT typ
                                                   return [t]]
                moduleDecStr12_79 = foldl1 mplus [do ddd328_227 <- get
                                                     do err <- ((do _ <- StateT whr
                                                                    return ()) >> return False) `catchError` const (return True)
                                                        unless err (gets position >>= (throwError . mkParseError ('!' : "_:whr") "not match: " "" ddd328_227 ["whr"]))
                                                     put ddd328_227
                                                     c <- StateT char
                                                     s <- StateT moduleDecStr
                                                     return (c : s),
                                                  return ""]
                whr13_80 = foldl1 mplus [do _ <- StateT spaces
                                            return ()
                                            d338_228 <- get
                                            xx337_229 <- StateT char
                                            case xx337_229 of
                                                ')' -> return ()
                                                _ -> gets position >>= (throwError . mkParseError "')'" "not match pattern: " "" d338_228 ["char"])
                                            let ')' = xx337_229
                                            return ()
                                            _ <- StateT spaces
                                            return ()
                                            d342_230 <- get
                                            xx341_231 <- StateT char
                                            case xx341_231 of
                                                'w' -> return ()
                                                _ -> gets position >>= (throwError . mkParseError "'w'" "not match pattern: " "" d342_230 ["char"])
                                            let 'w' = xx341_231
                                            return ()
                                            d344_232 <- get
                                            xx343_233 <- StateT char
                                            case xx343_233 of
                                                'h' -> return ()
                                                _ -> gets position >>= (throwError . mkParseError "'h'" "not match pattern: " "" d344_232 ["char"])
                                            let 'h' = xx343_233
                                            return ()
                                            d346_234 <- get
                                            xx345_235 <- StateT char
                                            case xx345_235 of
                                                'e' -> return ()
                                                _ -> gets position >>= (throwError . mkParseError "'e'" "not match pattern: " "" d346_234 ["char"])
                                            let 'e' = xx345_235
                                            return ()
                                            d348_236 <- get
                                            xx347_237 <- StateT char
                                            case xx347_237 of
                                                'r' -> return ()
                                                _ -> gets position >>= (throwError . mkParseError "'r'" "not match pattern: " "" d348_236 ["char"])
                                            let 'r' = xx347_237
                                            return ()
                                            d350_238 <- get
                                            xx349_239 <- StateT char
                                            case xx349_239 of
                                                'e' -> return ()
                                                _ -> gets position >>= (throwError . mkParseError "'e'" "not match pattern: " "" d350_238 ["char"])
                                            let 'e' = xx349_239
                                            return ()
                                            return ()]
                preImpPap14_81 = foldl1 mplus [do ddd351_240 <- get
                                                  do err <- ((do _ <- StateT importPapillon
                                                                 return ()) >> return False) `catchError` const (return True)
                                                     unless err (gets position >>= (throwError . mkParseError ('!' : "_:importPapillon") "not match: " "" ddd351_240 ["importPapillon"]))
                                                  put ddd351_240
                                                  ddd354_241 <- get
                                                  do err <- ((do _ <- StateT pap
                                                                 return ()) >> return False) `catchError` const (return True)
                                                     unless err (gets position >>= (throwError . mkParseError ('!' : "_:pap") "not match: " "" ddd354_241 ["pap"]))
                                                  put ddd354_241
                                                  c <- StateT char
                                                  pip <- StateT preImpPap
                                                  return (cons c pip),
                                               return emp]
                prePeg15_82 = foldl1 mplus [do ddd361_242 <- get
                                               do err <- ((do _ <- StateT pap
                                                              return ()) >> return False) `catchError` const (return True)
                                                  unless err (gets position >>= (throwError . mkParseError ('!' : "_:pap") "not match: " "" ddd361_242 ["pap"]))
                                               put ddd361_242
                                               c <- StateT char
                                               pp <- StateT prePeg
                                               return (cons c pp),
                                            return emp]
                afterPeg16_83 = foldl1 mplus [do c <- StateT char
                                                 atp <- StateT afterPeg
                                                 return (cons c atp),
                                              return emp]
                importPapillon17_84 = foldl1 mplus [do d373_243 <- get
                                                       xx372_244 <- StateT varToken
                                                       case xx372_244 of
                                                           "import" -> return ()
                                                           _ -> gets position >>= (throwError . mkParseError "\"import\"" "not match pattern: " "" d373_243 ["varToken"])
                                                       let "import" = xx372_244
                                                       return ()
                                                       d375_245 <- get
                                                       xx374_246 <- StateT typToken
                                                       case xx374_246 of
                                                           "Text" -> return ()
                                                           _ -> gets position >>= (throwError . mkParseError "\"Text\"" "not match pattern: " "" d375_245 ["typToken"])
                                                       let "Text" = xx374_246
                                                       return ()
                                                       d377_247 <- get
                                                       xx376_248 <- StateT char
                                                       case xx376_248 of
                                                           '.' -> return ()
                                                           _ -> gets position >>= (throwError . mkParseError "'.'" "not match pattern: " "" d377_247 ["char"])
                                                       let '.' = xx376_248
                                                       return ()
                                                       _ <- StateT spaces
                                                       return ()
                                                       d381_249 <- get
                                                       xx380_250 <- StateT typToken
                                                       case xx380_250 of
                                                           "Papillon" -> return ()
                                                           _ -> gets position >>= (throwError . mkParseError "\"Papillon\"" "not match pattern: " "" d381_249 ["typToken"])
                                                       let "Papillon" = xx380_250
                                                       return ()
                                                       ddd382_251 <- get
                                                       do err <- ((do d384_252 <- get
                                                                      xx383_253 <- StateT char
                                                                      case xx383_253 of
                                                                          '.' -> return ()
                                                                          _ -> gets position >>= (throwError . mkParseError "'.'" "not match pattern: " "" d384_252 ["char"])
                                                                      let '.' = xx383_253
                                                                      return ()) >> return False) `catchError` const (return True)
                                                          unless err (gets position >>= (throwError . mkParseError ('!' : "'.':") "not match: " "" ddd382_251 ["char"]))
                                                       put ddd382_251
                                                       return ()]
                varToken18_85 = foldl1 mplus [do v <- StateT variable
                                                 _ <- StateT spaces
                                                 return ()
                                                 return v]
                typToken19_86 = foldl1 mplus [do t <- StateT typ
                                                 _ <- StateT spaces
                                                 return ()
                                                 return t]
                pap20_87 = foldl1 mplus [do d394_254 <- get
                                            xx393_255 <- StateT char
                                            case xx393_255 of
                                                '\n' -> return ()
                                                _ -> gets position >>= (throwError . mkParseError "'\\n'" "not match pattern: " "" d394_254 ["char"])
                                            let '\n' = xx393_255
                                            return ()
                                            d396_256 <- get
                                            xx395_257 <- StateT char
                                            case xx395_257 of
                                                '[' -> return ()
                                                _ -> gets position >>= (throwError . mkParseError "'['" "not match pattern: " "" d396_256 ["char"])
                                            let '[' = xx395_257
                                            return ()
                                            d398_258 <- get
                                            xx397_259 <- StateT char
                                            case xx397_259 of
                                                'p' -> return ()
                                                _ -> gets position >>= (throwError . mkParseError "'p'" "not match pattern: " "" d398_258 ["char"])
                                            let 'p' = xx397_259
                                            return ()
                                            d400_260 <- get
                                            xx399_261 <- StateT char
                                            case xx399_261 of
                                                'a' -> return ()
                                                _ -> gets position >>= (throwError . mkParseError "'a'" "not match pattern: " "" d400_260 ["char"])
                                            let 'a' = xx399_261
                                            return ()
                                            d402_262 <- get
                                            xx401_263 <- StateT char
                                            case xx401_263 of
                                                'p' -> return ()
                                                _ -> gets position >>= (throwError . mkParseError "'p'" "not match pattern: " "" d402_262 ["char"])
                                            let 'p' = xx401_263
                                            return ()
                                            d404_264 <- get
                                            xx403_265 <- StateT char
                                            case xx403_265 of
                                                'i' -> return ()
                                                _ -> gets position >>= (throwError . mkParseError "'i'" "not match pattern: " "" d404_264 ["char"])
                                            let 'i' = xx403_265
                                            return ()
                                            d406_266 <- get
                                            xx405_267 <- StateT char
                                            case xx405_267 of
                                                'l' -> return ()
                                                _ -> gets position >>= (throwError . mkParseError "'l'" "not match pattern: " "" d406_266 ["char"])
                                            let 'l' = xx405_267
                                            return ()
                                            d408_268 <- get
                                            xx407_269 <- StateT char
                                            case xx407_269 of
                                                'l' -> return ()
                                                _ -> gets position >>= (throwError . mkParseError "'l'" "not match pattern: " "" d408_268 ["char"])
                                            let 'l' = xx407_269
                                            return ()
                                            d410_270 <- get
                                            xx409_271 <- StateT char
                                            case xx409_271 of
                                                'o' -> return ()
                                                _ -> gets position >>= (throwError . mkParseError "'o'" "not match pattern: " "" d410_270 ["char"])
                                            let 'o' = xx409_271
                                            return ()
                                            d412_272 <- get
                                            xx411_273 <- StateT char
                                            case xx411_273 of
                                                'n' -> return ()
                                                _ -> gets position >>= (throwError . mkParseError "'n'" "not match pattern: " "" d412_272 ["char"])
                                            let 'n' = xx411_273
                                            return ()
                                            d414_274 <- get
                                            xx413_275 <- StateT char
                                            case xx413_275 of
                                                '|' -> return ()
                                                _ -> gets position >>= (throwError . mkParseError "'|'" "not match pattern: " "" d414_274 ["char"])
                                            let '|' = xx413_275
                                            return ()
                                            d416_276 <- get
                                            xx415_277 <- StateT char
                                            case xx415_277 of
                                                '\n' -> return ()
                                                _ -> gets position >>= (throwError . mkParseError "'\\n'" "not match pattern: " "" d416_276 ["char"])
                                            let '\n' = xx415_277
                                            return ()
                                            return ()]
                peg21_88 = foldl1 mplus [do _ <- StateT spaces
                                            return ()
                                            s <- StateT sourceType
                                            p <- StateT peg_
                                            return (mkTTPeg s p),
                                         do p <- StateT peg_
                                            return (mkTTPeg tString p)]
                sourceType22_89 = foldl1 mplus [do d426_278 <- get
                                                   xx425_279 <- StateT varToken
                                                   case xx425_279 of
                                                       "source" -> return ()
                                                       _ -> gets position >>= (throwError . mkParseError "\"source\"" "not match pattern: " "" d426_278 ["varToken"])
                                                   let "source" = xx425_279
                                                   return ()
                                                   d428_280 <- get
                                                   xx427_281 <- StateT char
                                                   case xx427_281 of
                                                       ':' -> return ()
                                                       _ -> gets position >>= (throwError . mkParseError "':'" "not match pattern: " "" d428_280 ["char"])
                                                   let ':' = xx427_281
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
                                                   d444_282 <- get
                                                   xx443_283 <- StateT char
                                                   case xx443_283 of
                                                       ':' -> return ()
                                                       _ -> gets position >>= (throwError . mkParseError "':'" "not match pattern: " "" d444_282 ["char"])
                                                   let ':' = xx443_283
                                                   return ()
                                                   d446_284 <- get
                                                   xx445_285 <- StateT char
                                                   case xx445_285 of
                                                       ':' -> return ()
                                                       _ -> gets position >>= (throwError . mkParseError "':'" "not match pattern: " "" d446_284 ["char"])
                                                   let ':' = xx445_285
                                                   return ()
                                                   _ <- StateT spaces
                                                   return ()
                                                   t <- StateT hsTypeArr
                                                   _ <- StateT spaces
                                                   return ()
                                                   d454_286 <- get
                                                   xx453_287 <- StateT char
                                                   case xx453_287 of
                                                       '=' -> return ()
                                                       _ -> gets position >>= (throwError . mkParseError "'='" "not match pattern: " "" d454_286 ["char"])
                                                   let '=' = xx453_287
                                                   return ()
                                                   _ <- StateT spaces
                                                   return ()
                                                   sel <- StateT selection
                                                   _ <- StateT spaces
                                                   return ()
                                                   d462_288 <- get
                                                   xx461_289 <- StateT char
                                                   case xx461_289 of
                                                       ';' -> return ()
                                                       _ -> gets position >>= (throwError . mkParseError "';'" "not match pattern: " "" d462_288 ["char"])
                                                   let ';' = xx461_289
                                                   return ()
                                                   return (mkDef v t sel)]
                selection25_92 = foldl1 mplus [do ex <- StateT expressionHs
                                                  _ <- StateT spaces
                                                  return ()
                                                  d468_290 <- get
                                                  xx467_291 <- StateT char
                                                  case xx467_291 of
                                                      '/' -> return ()
                                                      _ -> gets position >>= (throwError . mkParseError "'/'" "not match pattern: " "" d468_290 ["char"])
                                                  let '/' = xx467_291
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
                                                     d480_292 <- get
                                                     xx479_293 <- StateT char
                                                     case xx479_293 of
                                                         '{' -> return ()
                                                         _ -> gets position >>= (throwError . mkParseError "'{'" "not match pattern: " "" d480_292 ["char"])
                                                     let '{' = xx479_293
                                                     return ()
                                                     _ <- StateT spaces
                                                     return ()
                                                     h <- StateT hsExpLam
                                                     _ <- StateT spaces
                                                     return ()
                                                     d488_294 <- get
                                                     xx487_295 <- StateT char
                                                     case xx487_295 of
                                                         '}' -> return ()
                                                         _ -> gets position >>= (throwError . mkParseError "'}'" "not match pattern: " "" d488_294 ["char"])
                                                     let '}' = xx487_295
                                                     return ()
                                                     return (mkExpressionHs e h)]
                expression27_94 = foldl1 mplus [do l <- StateT nameLeaf_
                                                   _ <- StateT spaces
                                                   return ()
                                                   e <- StateT expression
                                                   return (cons l e),
                                                return emp]
                nameLeaf_28_95 = foldl1 mplus [do d496_296 <- get
                                                  xx495_297 <- StateT char
                                                  case xx495_297 of
                                                      '!' -> return ()
                                                      _ -> gets position >>= (throwError . mkParseError "'!'" "not match pattern: " "" d496_296 ["char"])
                                                  let '!' = xx495_297
                                                  return ()
                                                  nl <- StateT nameLeafNoCom
                                                  _ <- StateT spaces
                                                  return ()
                                                  com <- optional3_298 (StateT comForErr)
                                                  return (NotAfter nl $ maybe "" id com),
                                               do d504_299 <- get
                                                  xx503_300 <- StateT char
                                                  let c = xx503_300
                                                  unless (isAmp c) (gets position >>= (throwError . mkParseError "isAmp c" "not match: " "" d504_299 ["char"]))
                                                  nl <- StateT nameLeaf
                                                  return (After nl),
                                               do nl <- StateT nameLeaf
                                                  return (Here nl)]
                nameLeaf29_96 = foldl1 mplus [do n <- StateT pat1
                                                 _ <- StateT spaces
                                                 return ()
                                                 com <- optional3_298 (StateT comForErr)
                                                 d516_301 <- get
                                                 xx515_302 <- StateT char
                                                 case xx515_302 of
                                                     ':' -> return ()
                                                     _ -> gets position >>= (throwError . mkParseError "':'" "not match pattern: " "" d516_301 ["char"])
                                                 let ':' = xx515_302
                                                 return ()
                                                 (rf, p) <- StateT leaf
                                                 return (NameLeaf (n, maybe "" id com) rf p),
                                              do n <- StateT pat1
                                                 _ <- StateT spaces
                                                 return ()
                                                 com <- optional3_298 (StateT comForErr)
                                                 return (NameLeaf (n,
                                                                   maybe "" id com) FromToken Nothing)]
                nameLeafNoCom30_97 = foldl1 mplus [do n <- StateT pat1
                                                      _ <- StateT spaces
                                                      return ()
                                                      com <- optional3_298 (StateT comForErr)
                                                      d532_303 <- get
                                                      xx531_304 <- StateT char
                                                      case xx531_304 of
                                                          ':' -> return ()
                                                          _ -> gets position >>= (throwError . mkParseError "':'" "not match pattern: " "" d532_303 ["char"])
                                                      let ':' = xx531_304
                                                      return ()
                                                      (rf, p) <- StateT leaf
                                                      return (NameLeaf (n, maybe "" id com) rf p),
                                                   do n <- StateT pat1
                                                      _ <- StateT spaces
                                                      return ()
                                                      return (NameLeaf (n, "") FromToken Nothing)]
                comForErr31_98 = foldl1 mplus [do d540_305 <- get
                                                  xx539_306 <- StateT char
                                                  case xx539_306 of
                                                      '{' -> return ()
                                                      _ -> gets position >>= (throwError . mkParseError "'{'" "not match pattern: " "" d540_305 ["char"])
                                                  let '{' = xx539_306
                                                  return ()
                                                  d542_307 <- get
                                                  xx541_308 <- StateT char
                                                  case xx541_308 of
                                                      '-' -> return ()
                                                      _ -> gets position >>= (throwError . mkParseError "'-'" "not match pattern: " "" d542_307 ["char"])
                                                  let '-' = xx541_308
                                                  return ()
                                                  d544_309 <- get
                                                  xx543_310 <- StateT char
                                                  case xx543_310 of
                                                      '#' -> return ()
                                                      _ -> gets position >>= (throwError . mkParseError "'#'" "not match pattern: " "" d544_309 ["char"])
                                                  let '#' = xx543_310
                                                  return ()
                                                  _ <- StateT spaces
                                                  return ()
                                                  d548_311 <- get
                                                  xx547_312 <- StateT char
                                                  case xx547_312 of
                                                      '"' -> return ()
                                                      _ -> gets position >>= (throwError . mkParseError "'\"'" "not match pattern: " "" d548_311 ["char"])
                                                  let '"' = xx547_312
                                                  return ()
                                                  s <- StateT stringLit
                                                  d552_313 <- get
                                                  xx551_314 <- StateT char
                                                  case xx551_314 of
                                                      '"' -> return ()
                                                      _ -> gets position >>= (throwError . mkParseError "'\"'" "not match pattern: " "" d552_313 ["char"])
                                                  let '"' = xx551_314
                                                  return ()
                                                  _ <- StateT spaces
                                                  return ()
                                                  d556_315 <- get
                                                  xx555_316 <- StateT char
                                                  case xx555_316 of
                                                      '#' -> return ()
                                                      _ -> gets position >>= (throwError . mkParseError "'#'" "not match pattern: " "" d556_315 ["char"])
                                                  let '#' = xx555_316
                                                  return ()
                                                  d558_317 <- get
                                                  xx557_318 <- StateT char
                                                  case xx557_318 of
                                                      '-' -> return ()
                                                      _ -> gets position >>= (throwError . mkParseError "'-'" "not match pattern: " "" d558_317 ["char"])
                                                  let '-' = xx557_318
                                                  return ()
                                                  d560_319 <- get
                                                  xx559_320 <- StateT char
                                                  case xx559_320 of
                                                      '}' -> return ()
                                                      _ -> gets position >>= (throwError . mkParseError "'}'" "not match pattern: " "" d560_319 ["char"])
                                                  let '}' = xx559_320
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
                                               d582_321 <- get
                                               xx581_322 <- StateT char
                                               let q = xx581_322
                                               unless (isBQ q) (gets position >>= (throwError . mkParseError "isBQ q" "not match: " "" d582_321 ["char"]))
                                               t <- StateT typ
                                               d586_323 <- get
                                               xx585_324 <- StateT char
                                               let q_ = xx585_324
                                               unless (isBQ q_) (gets position >>= (throwError . mkParseError "isBQ q_" "not match: " "" d586_323 ["char"]))
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
                                          do d600_325 <- get
                                             xx599_326 <- StateT char
                                             case xx599_326 of
                                                 '(' -> return ()
                                                 _ -> gets position >>= (throwError . mkParseError "'('" "not match pattern: " "" d600_325 ["char"])
                                             let '(' = xx599_326
                                             return ()
                                             o <- StateT opConName
                                             d604_327 <- get
                                             xx603_328 <- StateT char
                                             case xx603_328 of
                                                 ')' -> return ()
                                                 _ -> gets position >>= (throwError . mkParseError "')'" "not match pattern: " "" d604_327 ["char"])
                                             let ')' = xx603_328
                                             return ()
                                             _ <- StateT spaces
                                             return ()
                                             ps <- StateT pats
                                             return (conP o ps),
                                          do p <- StateT pat1
                                             return p]
                pat135_102 = foldl1 mplus [do t <- StateT typ
                                              return (conToPatQ t emp),
                                           do d614_329 <- get
                                              xx613_330 <- StateT variable
                                              case xx613_330 of
                                                  "_" -> return ()
                                                  _ -> gets position >>= (throwError . mkParseError "\"_\"" "not match pattern: " "" d614_329 ["variable"])
                                              let "_" = xx613_330
                                              return ()
                                              return wildP,
                                           do n <- StateT variable
                                              return (strToPatQ n),
                                           do i <- StateT integer
                                              return (litP (integerL i)),
                                           do d620_331 <- get
                                              xx619_332 <- StateT char
                                              case xx619_332 of
                                                  '-' -> return ()
                                                  _ -> gets position >>= (throwError . mkParseError "'-'" "not match pattern: " "" d620_331 ["char"])
                                              let '-' = xx619_332
                                              return ()
                                              _ <- StateT spaces
                                              return ()
                                              i <- StateT integer
                                              return (litP (integerL $ negate i)),
                                           do d626_333 <- get
                                              xx625_334 <- StateT char
                                              case xx625_334 of
                                                  '\'' -> return ()
                                                  _ -> gets position >>= (throwError . mkParseError "'\\''" "not match pattern: " "" d626_333 ["char"])
                                              let '\'' = xx625_334
                                              return ()
                                              c <- StateT charLit
                                              d630_335 <- get
                                              xx629_336 <- StateT char
                                              case xx629_336 of
                                                  '\'' -> return ()
                                                  _ -> gets position >>= (throwError . mkParseError "'\\''" "not match pattern: " "" d630_335 ["char"])
                                              let '\'' = xx629_336
                                              return ()
                                              return (charP c),
                                           do d632_337 <- get
                                              xx631_338 <- StateT char
                                              case xx631_338 of
                                                  '"' -> return ()
                                                  _ -> gets position >>= (throwError . mkParseError "'\"'" "not match pattern: " "" d632_337 ["char"])
                                              let '"' = xx631_338
                                              return ()
                                              s <- StateT stringLit
                                              d636_339 <- get
                                              xx635_340 <- StateT char
                                              case xx635_340 of
                                                  '"' -> return ()
                                                  _ -> gets position >>= (throwError . mkParseError "'\"'" "not match pattern: " "" d636_339 ["char"])
                                              let '"' = xx635_340
                                              return ()
                                              return (stringP s),
                                           do d638_341 <- get
                                              xx637_342 <- StateT char
                                              case xx637_342 of
                                                  '(' -> return ()
                                                  _ -> gets position >>= (throwError . mkParseError "'('" "not match pattern: " "" d638_341 ["char"])
                                              let '(' = xx637_342
                                              return ()
                                              p <- StateT patList
                                              d642_343 <- get
                                              xx641_344 <- StateT char
                                              case xx641_344 of
                                                  ')' -> return ()
                                                  _ -> gets position >>= (throwError . mkParseError "')'" "not match pattern: " "" d642_343 ["char"])
                                              let ')' = xx641_344
                                              return ()
                                              return (tupP p),
                                           do d644_345 <- get
                                              xx643_346 <- StateT char
                                              case xx643_346 of
                                                  '[' -> return ()
                                                  _ -> gets position >>= (throwError . mkParseError "'['" "not match pattern: " "" d644_345 ["char"])
                                              let '[' = xx643_346
                                              return ()
                                              p <- StateT patList
                                              d648_347 <- get
                                              xx647_348 <- StateT char
                                              case xx647_348 of
                                                  ']' -> return ()
                                                  _ -> gets position >>= (throwError . mkParseError "']'" "not match pattern: " "" d648_347 ["char"])
                                              let ']' = xx647_348
                                              return ()
                                              return (listP p)]
                patList36_103 = foldl1 mplus [do p <- StateT patOp
                                                 _ <- StateT spaces
                                                 return ()
                                                 d654_349 <- get
                                                 xx653_350 <- StateT char
                                                 case xx653_350 of
                                                     ',' -> return ()
                                                     _ -> gets position >>= (throwError . mkParseError "','" "not match pattern: " "" d654_349 ["char"])
                                                 let ',' = xx653_350
                                                 return ()
                                                 _ <- StateT spaces
                                                 return ()
                                                 ps <- StateT patList
                                                 return (p : ps),
                                              do p <- StateT patOp
                                                 return [p],
                                              return []]
                opConName37_104 = foldl1 mplus [do d662_351 <- get
                                                   xx661_352 <- StateT char
                                                   case xx661_352 of
                                                       ':' -> return ()
                                                       _ -> gets position >>= (throwError . mkParseError "':'" "not match pattern: " "" d662_351 ["char"])
                                                   let ':' = xx661_352
                                                   return ()
                                                   ot <- StateT opTail
                                                   return (mkName $ colon : ot)]
                charLit38_105 = foldl1 mplus [do d666_353 <- get
                                                 xx665_354 <- StateT char
                                                 let c = xx665_354
                                                 unless (isAlphaNumOt c) (gets position >>= (throwError . mkParseError "isAlphaNumOt c" "not match: " "" d666_353 ["char"]))
                                                 return c,
                                              do d668_355 <- get
                                                 xx667_356 <- StateT char
                                                 case xx667_356 of
                                                     '\\' -> return ()
                                                     _ -> gets position >>= (throwError . mkParseError "'\\\\'" "not match pattern: " "" d668_355 ["char"])
                                                 let '\\' = xx667_356
                                                 return ()
                                                 c <- StateT escapeC
                                                 return c]
                stringLit39_106 = foldl1 mplus [do d672_357 <- get
                                                   xx671_358 <- StateT char
                                                   let c = xx671_358
                                                   unless (isStrLitC c) (gets position >>= (throwError . mkParseError "isStrLitC c" "not match: " "" d672_357 ["char"]))
                                                   s <- StateT stringLit
                                                   return (cons c s),
                                                do d676_359 <- get
                                                   xx675_360 <- StateT char
                                                   case xx675_360 of
                                                       '\\' -> return ()
                                                       _ -> gets position >>= (throwError . mkParseError "'\\\\'" "not match pattern: " "" d676_359 ["char"])
                                                   let '\\' = xx675_360
                                                   return ()
                                                   c <- StateT escapeC
                                                   s <- StateT stringLit
                                                   return (c : s),
                                                return emp]
                escapeC40_107 = foldl1 mplus [do d682_361 <- get
                                                 xx681_362 <- StateT char
                                                 case xx681_362 of
                                                     '"' -> return ()
                                                     _ -> gets position >>= (throwError . mkParseError "'\"'" "not match pattern: " "" d682_361 ["char"])
                                                 let '"' = xx681_362
                                                 return ()
                                                 return '"',
                                              do d684_363 <- get
                                                 xx683_364 <- StateT char
                                                 case xx683_364 of
                                                     '\'' -> return ()
                                                     _ -> gets position >>= (throwError . mkParseError "'\\''" "not match pattern: " "" d684_363 ["char"])
                                                 let '\'' = xx683_364
                                                 return ()
                                                 return '\'',
                                              do d686_365 <- get
                                                 xx685_366 <- StateT char
                                                 case xx685_366 of
                                                     '\\' -> return ()
                                                     _ -> gets position >>= (throwError . mkParseError "'\\\\'" "not match pattern: " "" d686_365 ["char"])
                                                 let '\\' = xx685_366
                                                 return ()
                                                 return '\\',
                                              do d688_367 <- get
                                                 xx687_368 <- StateT char
                                                 case xx687_368 of
                                                     'n' -> return ()
                                                     _ -> gets position >>= (throwError . mkParseError "'n'" "not match pattern: " "" d688_367 ["char"])
                                                 let 'n' = xx687_368
                                                 return ()
                                                 return '\n',
                                              do d690_369 <- get
                                                 xx689_370 <- StateT char
                                                 case xx689_370 of
                                                     't' -> return ()
                                                     _ -> gets position >>= (throwError . mkParseError "'t'" "not match pattern: " "" d690_369 ["char"])
                                                 let 't' = xx689_370
                                                 return ()
                                                 return tab]
                pats41_108 = foldl1 mplus [do p <- StateT pat
                                              _ <- StateT spaces
                                              return ()
                                              ps <- StateT pats
                                              return (cons p ps),
                                           return emp]
                readFromLs42_109 = foldl1 mplus [do rf <- StateT readFrom
                                                    d700_371 <- get
                                                    xx699_372 <- StateT char
                                                    case xx699_372 of
                                                        '*' -> return ()
                                                        _ -> gets position >>= (throwError . mkParseError "'*'" "not match pattern: " "" d700_371 ["char"])
                                                    let '*' = xx699_372
                                                    return ()
                                                    return (FromList rf),
                                                 do rf <- StateT readFrom
                                                    d704_373 <- get
                                                    xx703_374 <- StateT char
                                                    case xx703_374 of
                                                        '+' -> return ()
                                                        _ -> gets position >>= (throwError . mkParseError "'+'" "not match pattern: " "" d704_373 ["char"])
                                                    let '+' = xx703_374
                                                    return ()
                                                    return (FromList1 rf),
                                                 do rf <- StateT readFrom
                                                    d708_375 <- get
                                                    xx707_376 <- StateT char
                                                    case xx707_376 of
                                                        '?' -> return ()
                                                        _ -> gets position >>= (throwError . mkParseError "'?'" "not match pattern: " "" d708_375 ["char"])
                                                    let '?' = xx707_376
                                                    return ()
                                                    return (FromOptional rf),
                                                 do rf <- StateT readFrom
                                                    return rf]
                readFrom43_110 = foldl1 mplus [do v <- StateT variable
                                                  return (FromVariable v),
                                               do d714_377 <- get
                                                  xx713_378 <- StateT char
                                                  case xx713_378 of
                                                      '(' -> return ()
                                                      _ -> gets position >>= (throwError . mkParseError "'('" "not match pattern: " "" d714_377 ["char"])
                                                  let '(' = xx713_378
                                                  return ()
                                                  s <- StateT selection
                                                  d718_379 <- get
                                                  xx717_380 <- StateT char
                                                  case xx717_380 of
                                                      ')' -> return ()
                                                      _ -> gets position >>= (throwError . mkParseError "')'" "not match pattern: " "" d718_379 ["char"])
                                                  let ')' = xx717_380
                                                  return ()
                                                  return (FromSelection s)]
                test44_111 = foldl1 mplus [do d720_381 <- get
                                              xx719_382 <- StateT char
                                              case xx719_382 of
                                                  '[' -> return ()
                                                  _ -> gets position >>= (throwError . mkParseError "'['" "not match pattern: " "" d720_381 ["char"])
                                              let '[' = xx719_382
                                              return ()
                                              h <- StateT hsExpLam
                                              _ <- StateT spaces
                                              return ()
                                              com <- optional3_298 (StateT comForErr)
                                              d728_383 <- get
                                              xx727_384 <- StateT char
                                              case xx727_384 of
                                                  ']' -> return ()
                                                  _ -> gets position >>= (throwError . mkParseError "']'" "not match pattern: " "" d728_383 ["char"])
                                              let ']' = xx727_384
                                              return ()
                                              return (h, maybe "" id com)]
                hsExpLam45_112 = foldl1 mplus [do d730_385 <- get
                                                  xx729_386 <- StateT char
                                                  case xx729_386 of
                                                      '\\' -> return ()
                                                      _ -> gets position >>= (throwError . mkParseError "'\\\\'" "not match pattern: " "" d730_385 ["char"])
                                                  let '\\' = xx729_386
                                                  return ()
                                                  _ <- StateT spaces
                                                  return ()
                                                  ps <- StateT pats
                                                  _ <- StateT spaces
                                                  return ()
                                                  d738_387 <- get
                                                  xx737_388 <- StateT char
                                                  case xx737_388 of
                                                      '-' -> return ()
                                                      _ -> gets position >>= (throwError . mkParseError "'-'" "not match pattern: " "" d738_387 ["char"])
                                                  let '-' = xx737_388
                                                  return ()
                                                  d740_389 <- get
                                                  xx739_390 <- StateT char
                                                  let c = xx739_390
                                                  unless (isGt c) (gets position >>= (throwError . mkParseError "isGt c" "not match: " "" d740_389 ["char"]))
                                                  _ <- StateT spaces
                                                  return ()
                                                  e <- StateT hsExpTyp
                                                  return (lamE ps e),
                                               do e <- StateT hsExpTyp
                                                  return e]
                hsExpTyp46_113 = foldl1 mplus [do eo <- StateT hsExpOp
                                                  d750_391 <- get
                                                  xx749_392 <- StateT char
                                                  case xx749_392 of
                                                      ':' -> return ()
                                                      _ -> gets position >>= (throwError . mkParseError "':'" "not match pattern: " "" d750_391 ["char"])
                                                  let ':' = xx749_392
                                                  return ()
                                                  d752_393 <- get
                                                  xx751_394 <- StateT char
                                                  case xx751_394 of
                                                      ':' -> return ()
                                                      _ -> gets position >>= (throwError . mkParseError "':'" "not match pattern: " "" d752_393 ["char"])
                                                  let ':' = xx751_394
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
                hsOp48_115 = foldl1 mplus [do d772_395 <- get
                                              xx771_396 <- StateT char
                                              let c = xx771_396
                                              unless (isOpHeadChar c) (gets position >>= (throwError . mkParseError "isOpHeadChar c" "not match: " "" d772_395 ["char"]))
                                              o <- StateT opTail
                                              return (varE (mkName (cons c o))),
                                           do d776_397 <- get
                                              xx775_398 <- StateT char
                                              case xx775_398 of
                                                  ':' -> return ()
                                                  _ -> gets position >>= (throwError . mkParseError "':'" "not match pattern: " "" d776_397 ["char"])
                                              let ':' = xx775_398
                                              return ()
                                              ddd777_399 <- get
                                              do err <- ((do d779_400 <- get
                                                             xx778_401 <- StateT char
                                                             case xx778_401 of
                                                                 ':' -> return ()
                                                                 _ -> gets position >>= (throwError . mkParseError "':'" "not match pattern: " "" d779_400 ["char"])
                                                             let ':' = xx778_401
                                                             return ()) >> return False) `catchError` const (return True)
                                                 unless err (gets position >>= (throwError . mkParseError ('!' : "':':") "not match: " "" ddd777_399 ["char"]))
                                              put ddd777_399
                                              o <- StateT opTail
                                              return (conE (mkName (':' : o))),
                                           do d783_402 <- get
                                              xx782_403 <- StateT char
                                              let c = xx782_403
                                              unless (isBQ c) (gets position >>= (throwError . mkParseError "isBQ c" "not match: " "" d783_402 ["char"]))
                                              v <- StateT variable
                                              d787_404 <- get
                                              xx786_405 <- StateT char
                                              let c_ = xx786_405
                                              unless (isBQ c_) (gets position >>= (throwError . mkParseError "isBQ c_" "not match: " "" d787_404 ["char"]))
                                              return (varE (mkName v)),
                                           do d789_406 <- get
                                              xx788_407 <- StateT char
                                              let c = xx788_407
                                              unless (isBQ c) (gets position >>= (throwError . mkParseError "isBQ c" "not match: " "" d789_406 ["char"]))
                                              t <- StateT typ
                                              d793_408 <- get
                                              xx792_409 <- StateT char
                                              let c_ = xx792_409
                                              unless (isBQ c_) (gets position >>= (throwError . mkParseError "isBQ c_" "not match: " "" d793_408 ["char"]))
                                              return (conE (mkName t))]
                opTail49_116 = foldl1 mplus [do d795_410 <- get
                                                xx794_411 <- StateT char
                                                let c = xx794_411
                                                unless (isOpTailChar c) (gets position >>= (throwError . mkParseError "isOpTailChar c" "not match: " "" d795_410 ["char"]))
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
                hsExp151_118 = foldl1 mplus [do d807_412 <- get
                                                xx806_413 <- StateT char
                                                case xx806_413 of
                                                    '(' -> return ()
                                                    _ -> gets position >>= (throwError . mkParseError "'('" "not match pattern: " "" d807_412 ["char"])
                                                let '(' = xx806_413
                                                return ()
                                                l <- optional3_298 (foldl1 mplus [do e <- StateT hsExpTyp
                                                                                     return e])
                                                _ <- StateT spaces
                                                return ()
                                                o <- StateT hsOp
                                                _ <- StateT spaces
                                                return ()
                                                r <- optional3_298 (foldl1 mplus [do e <- StateT hsExpTyp
                                                                                     return e])
                                                d823_414 <- get
                                                xx822_415 <- StateT char
                                                case xx822_415 of
                                                    ')' -> return ()
                                                    _ -> gets position >>= (throwError . mkParseError "')'" "not match pattern: " "" d823_414 ["char"])
                                                let ')' = xx822_415
                                                return ()
                                                return (infixE l o r),
                                             do d825_416 <- get
                                                xx824_417 <- StateT char
                                                case xx824_417 of
                                                    '(' -> return ()
                                                    _ -> gets position >>= (throwError . mkParseError "'('" "not match pattern: " "" d825_416 ["char"])
                                                let '(' = xx824_417
                                                return ()
                                                et <- StateT hsExpTpl
                                                d829_418 <- get
                                                xx828_419 <- StateT char
                                                case xx828_419 of
                                                    ')' -> return ()
                                                    _ -> gets position >>= (throwError . mkParseError "')'" "not match pattern: " "" d829_418 ["char"])
                                                let ')' = xx828_419
                                                return ()
                                                return (tupE et),
                                             do d831_420 <- get
                                                xx830_421 <- StateT char
                                                case xx830_421 of
                                                    '[' -> return ()
                                                    _ -> gets position >>= (throwError . mkParseError "'['" "not match pattern: " "" d831_420 ["char"])
                                                let '[' = xx830_421
                                                return ()
                                                et <- StateT hsExpTpl
                                                d835_422 <- get
                                                xx834_423 <- StateT char
                                                case xx834_423 of
                                                    ']' -> return ()
                                                    _ -> gets position >>= (throwError . mkParseError "']'" "not match pattern: " "" d835_422 ["char"])
                                                let ']' = xx834_423
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
                                             do d845_424 <- get
                                                xx844_425 <- StateT char
                                                case xx844_425 of
                                                    '\'' -> return ()
                                                    _ -> gets position >>= (throwError . mkParseError "'\\''" "not match pattern: " "" d845_424 ["char"])
                                                let '\'' = xx844_425
                                                return ()
                                                c <- StateT charLit
                                                d849_426 <- get
                                                xx848_427 <- StateT char
                                                case xx848_427 of
                                                    '\'' -> return ()
                                                    _ -> gets position >>= (throwError . mkParseError "'\\''" "not match pattern: " "" d849_426 ["char"])
                                                let '\'' = xx848_427
                                                return ()
                                                return (litE (charL c)),
                                             do d851_428 <- get
                                                xx850_429 <- StateT char
                                                case xx850_429 of
                                                    '"' -> return ()
                                                    _ -> gets position >>= (throwError . mkParseError "'\"'" "not match pattern: " "" d851_428 ["char"])
                                                let '"' = xx850_429
                                                return ()
                                                s <- StateT stringLit
                                                d855_430 <- get
                                                xx854_431 <- StateT char
                                                case xx854_431 of
                                                    '"' -> return ()
                                                    _ -> gets position >>= (throwError . mkParseError "'\"'" "not match pattern: " "" d855_430 ["char"])
                                                let '"' = xx854_431
                                                return ()
                                                return (litE (stringL s)),
                                             do d857_432 <- get
                                                xx856_433 <- StateT char
                                                case xx856_433 of
                                                    '-' -> return ()
                                                    _ -> gets position >>= (throwError . mkParseError "'-'" "not match pattern: " "" d857_432 ["char"])
                                                let '-' = xx856_433
                                                return ()
                                                _ <- StateT spaces
                                                return ()
                                                e <- StateT hsExp1
                                                return (appE (varE $ mkName "negate") e)]
                hsExpTpl52_119 = foldl1 mplus [do e <- StateT hsExpLam
                                                  _ <- StateT spaces
                                                  return ()
                                                  d867_434 <- get
                                                  xx866_435 <- StateT char
                                                  let c = xx866_435
                                                  unless (isComma c) (gets position >>= (throwError . mkParseError "isComma c" "not match: " "" d867_434 ["char"]))
                                                  _ <- StateT spaces
                                                  return ()
                                                  et <- StateT hsExpTpl
                                                  return (cons e et),
                                               do e <- StateT hsExpLam
                                                  return (cons e emp),
                                               return emp]
                hsTypeArr53_120 = foldl1 mplus [do l <- StateT hsType
                                                   d877_436 <- get
                                                   xx876_437 <- StateT char
                                                   case xx876_437 of
                                                       '-' -> return ()
                                                       _ -> gets position >>= (throwError . mkParseError "'-'" "not match pattern: " "" d877_436 ["char"])
                                                   let '-' = xx876_437
                                                   return ()
                                                   d879_438 <- get
                                                   xx878_439 <- StateT char
                                                   let c = xx878_439
                                                   unless (isGt c) (gets position >>= (throwError . mkParseError "isGt c" "not match: " "" d879_438 ["char"]))
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
                hsType155_122 = foldl1 mplus [do d893_440 <- get
                                                 xx892_441 <- StateT char
                                                 case xx892_441 of
                                                     '[' -> return ()
                                                     _ -> gets position >>= (throwError . mkParseError "'['" "not match pattern: " "" d893_440 ["char"])
                                                 let '[' = xx892_441
                                                 return ()
                                                 d895_442 <- get
                                                 xx894_443 <- StateT char
                                                 case xx894_443 of
                                                     ']' -> return ()
                                                     _ -> gets position >>= (throwError . mkParseError "']'" "not match pattern: " "" d895_442 ["char"])
                                                 let ']' = xx894_443
                                                 return ()
                                                 _ <- StateT spaces
                                                 return ()
                                                 return listT,
                                              do d899_444 <- get
                                                 xx898_445 <- StateT char
                                                 case xx898_445 of
                                                     '[' -> return ()
                                                     _ -> gets position >>= (throwError . mkParseError "'['" "not match pattern: " "" d899_444 ["char"])
                                                 let '[' = xx898_445
                                                 return ()
                                                 t <- StateT hsTypeArr
                                                 d903_446 <- get
                                                 xx902_447 <- StateT char
                                                 case xx902_447 of
                                                     ']' -> return ()
                                                     _ -> gets position >>= (throwError . mkParseError "']'" "not match pattern: " "" d903_446 ["char"])
                                                 let ']' = xx902_447
                                                 return ()
                                                 _ <- StateT spaces
                                                 return ()
                                                 return (appT listT t),
                                              do d907_448 <- get
                                                 xx906_449 <- StateT char
                                                 case xx906_449 of
                                                     '(' -> return ()
                                                     _ -> gets position >>= (throwError . mkParseError "'('" "not match pattern: " "" d907_448 ["char"])
                                                 let '(' = xx906_449
                                                 return ()
                                                 _ <- StateT spaces
                                                 return ()
                                                 tt <- StateT hsTypeTpl
                                                 d913_450 <- get
                                                 xx912_451 <- StateT char
                                                 case xx912_451 of
                                                     ')' -> return ()
                                                     _ -> gets position >>= (throwError . mkParseError "')'" "not match pattern: " "" d913_450 ["char"])
                                                 let ')' = xx912_451
                                                 return ()
                                                 return (tupT tt),
                                              do t <- StateT typToken
                                                 return (conT (mkName t)),
                                              do d917_452 <- get
                                                 xx916_453 <- StateT char
                                                 case xx916_453 of
                                                     '(' -> return ()
                                                     _ -> gets position >>= (throwError . mkParseError "'('" "not match pattern: " "" d917_452 ["char"])
                                                 let '(' = xx916_453
                                                 return ()
                                                 d919_454 <- get
                                                 xx918_455 <- StateT char
                                                 case xx918_455 of
                                                     '-' -> return ()
                                                     _ -> gets position >>= (throwError . mkParseError "'-'" "not match pattern: " "" d919_454 ["char"])
                                                 let '-' = xx918_455
                                                 return ()
                                                 d921_456 <- get
                                                 xx920_457 <- StateT char
                                                 let c = xx920_457
                                                 unless (isGt c) (gets position >>= (throwError . mkParseError "isGt c" "not match: " "" d921_456 ["char"]))
                                                 d923_458 <- get
                                                 xx922_459 <- StateT char
                                                 case xx922_459 of
                                                     ')' -> return ()
                                                     _ -> gets position >>= (throwError . mkParseError "')'" "not match pattern: " "" d923_458 ["char"])
                                                 let ')' = xx922_459
                                                 return ()
                                                 _ <- StateT spaces
                                                 return ()
                                                 return arrowT]
                hsTypeTpl56_123 = foldl1 mplus [do t <- StateT hsTypeArr
                                                   d929_460 <- get
                                                   xx928_461 <- StateT char
                                                   let c = xx928_461
                                                   unless (isComma c) (gets position >>= (throwError . mkParseError "isComma c" "not match: " "" d929_460 ["char"]))
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
                                                 ds <- list1_462 (foldl1 mplus [do d <- StateT digit
                                                                                   return d])
                                                 return (read (cons dh ds))]
                alpha61_128 = foldl1 mplus [do u <- StateT upper
                                               return u,
                                            do l <- StateT lower
                                               return l,
                                            do d <- StateT digit
                                               return d,
                                            do d961_463 <- get
                                               xx960_464 <- StateT char
                                               case xx960_464 of
                                                   '\'' -> return ()
                                                   _ -> gets position >>= (throwError . mkParseError "'\\''" "not match pattern: " "" d961_463 ["char"])
                                               let '\'' = xx960_464
                                               return ()
                                               return '\'']
                upper62_129 = foldl1 mplus [do d963_465 <- get
                                               xx962_466 <- StateT char
                                               let u = xx962_466
                                               unless (isUpper u) (gets position >>= (throwError . mkParseError "isUpper u" "not match: " "" d963_465 ["char"]))
                                               return u]
                lower63_130 = foldl1 mplus [do d965_467 <- get
                                               xx964_468 <- StateT char
                                               let l = xx964_468
                                               unless (isLowerU l) (gets position >>= (throwError . mkParseError "isLowerU l" "not match: " "" d965_467 ["char"]))
                                               return l]
                digit64_131 = foldl1 mplus [do d967_469 <- get
                                               xx966_470 <- StateT char
                                               let d = xx966_470
                                               unless (isDigit d) (gets position >>= (throwError . mkParseError "isDigit d" "not match: " "" d967_469 ["char"]))
                                               return d]
                spaces65_132 = foldl1 mplus [do _ <- StateT space
                                                return ()
                                                _ <- StateT spaces
                                                return ()
                                                return (),
                                             return ()]
                space66_133 = foldl1 mplus [do d973_471 <- get
                                               xx972_472 <- StateT char
                                               let s = xx972_472
                                               unless (isSpace s) (gets position >>= (throwError . mkParseError "isSpace s" "not match: " "" d973_471 ["char"]))
                                               return (),
                                            do d975_473 <- get
                                               xx974_474 <- StateT char
                                               case xx974_474 of
                                                   '-' -> return ()
                                                   _ -> gets position >>= (throwError . mkParseError "'-'" "not match pattern: " "" d975_473 ["char"])
                                               let '-' = xx974_474
                                               return ()
                                               d977_475 <- get
                                               xx976_476 <- StateT char
                                               case xx976_476 of
                                                   '-' -> return ()
                                                   _ -> gets position >>= (throwError . mkParseError "'-'" "not match pattern: " "" d977_475 ["char"])
                                               let '-' = xx976_476
                                               return ()
                                               _ <- StateT notNLString
                                               return ()
                                               _ <- StateT newLine
                                               return ()
                                               return (),
                                            do _ <- StateT comment
                                               return ()
                                               return ()]
                notNLString67_134 = foldl1 mplus [do ddd984_477 <- get
                                                     do err <- ((do _ <- StateT newLine
                                                                    return ()) >> return False) `catchError` const (return True)
                                                        unless err (gets position >>= (throwError . mkParseError ('!' : "_:newLine") "not match: " "" ddd984_477 ["newLine"]))
                                                     put ddd984_477
                                                     c <- StateT char
                                                     s <- StateT notNLString
                                                     return (cons c s),
                                                  return emp]
                newLine68_135 = foldl1 mplus [do d992_478 <- get
                                                 xx991_479 <- StateT char
                                                 case xx991_479 of
                                                     '\n' -> return ()
                                                     _ -> gets position >>= (throwError . mkParseError "'\\n'" "not match pattern: " "" d992_478 ["char"])
                                                 let '\n' = xx991_479
                                                 return ()
                                                 return ()]
                comment69_136 = foldl1 mplus [do d994_480 <- get
                                                 xx993_481 <- StateT char
                                                 case xx993_481 of
                                                     '{' -> return ()
                                                     _ -> gets position >>= (throwError . mkParseError "'{'" "not match pattern: " "" d994_480 ["char"])
                                                 let '{' = xx993_481
                                                 return ()
                                                 d996_482 <- get
                                                 xx995_483 <- StateT char
                                                 case xx995_483 of
                                                     '-' -> return ()
                                                     _ -> gets position >>= (throwError . mkParseError "'-'" "not match pattern: " "" d996_482 ["char"])
                                                 let '-' = xx995_483
                                                 return ()
                                                 ddd997_484 <- get
                                                 do err <- ((do d999_485 <- get
                                                                xx998_486 <- StateT char
                                                                case xx998_486 of
                                                                    '#' -> return ()
                                                                    _ -> gets position >>= (throwError . mkParseError "'#'" "not match pattern: " "" d999_485 ["char"])
                                                                let '#' = xx998_486
                                                                return ()) >> return False) `catchError` const (return True)
                                                    unless err (gets position >>= (throwError . mkParseError ('!' : "'#':") "not match: " "" ddd997_484 ["char"]))
                                                 put ddd997_484
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
                notComStr71_138 = foldl1 mplus [do ddd1012_487 <- get
                                                   do err <- ((do _ <- StateT comment
                                                                  return ()) >> return False) `catchError` const (return True)
                                                      unless err (gets position >>= (throwError . mkParseError ('!' : "_:comment") "not match: " "" ddd1012_487 ["comment"]))
                                                   put ddd1012_487
                                                   ddd1015_488 <- get
                                                   do err <- ((do _ <- StateT comEnd
                                                                  return ()) >> return False) `catchError` const (return True)
                                                      unless err (gets position >>= (throwError . mkParseError ('!' : "_:comEnd") "not match: " "" ddd1015_488 ["comEnd"]))
                                                   put ddd1015_488
                                                   _ <- StateT char
                                                   return ()
                                                   _ <- StateT notComStr
                                                   return ()
                                                   return (),
                                                return ()]
                comEnd72_139 = foldl1 mplus [do d1023_489 <- get
                                                xx1022_490 <- StateT char
                                                case xx1022_490 of
                                                    '-' -> return ()
                                                    _ -> gets position >>= (throwError . mkParseError "'-'" "not match pattern: " "" d1023_489 ["char"])
                                                let '-' = xx1022_490
                                                return ()
                                                d1025_491 <- get
                                                xx1024_492 <- StateT char
                                                case xx1024_492 of
                                                    '}' -> return ()
                                                    _ -> gets position >>= (throwError . mkParseError "'}'" "not match pattern: " "" d1025_491 ["char"])
                                                let '}' = xx1024_492
                                                return ()
                                                return ()]
                list1_462 :: forall m a . (MonadPlus m, Applicative m) =>
                                          m a -> m ([a])
                list12_493 :: forall m a . (MonadPlus m, Applicative m) =>
                                           m a -> m ([a])
                list1_462 p = list12_493 p `mplus` return []
                list12_493 p = ((:) <$> p) <*> list1_462 p
                optional3_298 :: forall m a . (MonadPlus m, Applicative m) =>
                                              m a -> m (Maybe a)
                optional3_298 p = (Just <$> p) `mplus` return Nothing

