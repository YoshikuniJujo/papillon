{-# LANGUAGE FlexibleContexts, TemplateHaskell, UndecidableInstances, PackageImports, TypeFamilies, RankNTypes #-}
module Text.Papillon.Parser (
	Peg,
	Definition(..),
	Selection(..),
	getSelectionType,
	ExpressionHs(..),
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
              normalSelection :: (Either (ParseError (Pos String) Derivs)
                                         ((Selection, Derivs))),
              plainSelection :: (Either (ParseError (Pos String) Derivs)
                                        ((Selection, Derivs))),
              expressionHs :: (Either (ParseError (Pos String) Derivs)
                                      ((ExpressionHs, Derivs))),
              plainExpressionHs :: (Either (ParseError (Pos String) Derivs)
                                           ((ExpressionHs, Derivs))),
              plainReadFromLs :: (Either (ParseError (Pos String) Derivs)
                                         ((ReadFrom, Derivs))),
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
              selectCharsLs :: (Either (ParseError (Pos String) Derivs)
                                       ((ReadFrom, Derivs))),
              selectChars :: (Either (ParseError (Pos String) Derivs)
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
                             where d = Derivs pegFile79_1 pragmas80_2 pragma81_3 pragmaStr82_4 pragmaItems83_5 pragmaEnd84_6 moduleDec85_7 moduleName86_8 moduleDecStr87_9 whr88_10 preImpPap89_11 prePeg90_12 afterPeg91_13 importPapillon92_14 varToken93_15 typToken94_16 pap95_17 peg96_18 sourceType97_19 peg_98_20 definition99_21 selection100_22 normalSelection101_23 plainSelection102_24 expressionHs103_25 plainExpressionHs104_26 plainReadFromLs105_27 expression106_28 nameLeaf_107_29 nameLeaf108_30 nameLeafNoCom109_31 comForErr110_32 leaf111_33 patOp112_34 pat113_35 pat1114_36 patList115_37 opConName116_38 charLit117_39 stringLit118_40 escapeC119_41 pats120_42 readFromLs121_43 readFrom122_44 selectCharsLs123_45 selectChars124_46 test125_47 hsExpLam126_48 hsExpTyp127_49 hsExpOp128_50 hsOp129_51 opTail130_52 hsExp131_53 hsExp1132_54 hsExpTpl133_55 hsTypeArr134_56 hsType135_57 hsType1136_58 hsTypeTpl137_59 typ138_60 variable139_61 tvtail140_62 integer141_63 alpha142_64 upper143_65 lower144_66 digit145_67 spaces146_68 space147_69 notNLString148_70 newLine149_71 comment150_72 comments151_73 notComStr152_74 comEnd153_75 chars154_76 pos
                                   pegFile79_1 = runStateT pegFile4_77 d
                                   pragmas80_2 = runStateT pragmas5_78 d
                                   pragma81_3 = runStateT pragma6_79 d
                                   pragmaStr82_4 = runStateT pragmaStr7_80 d
                                   pragmaItems83_5 = runStateT pragmaItems8_81 d
                                   pragmaEnd84_6 = runStateT pragmaEnd9_82 d
                                   moduleDec85_7 = runStateT moduleDec10_83 d
                                   moduleName86_8 = runStateT moduleName11_84 d
                                   moduleDecStr87_9 = runStateT moduleDecStr12_85 d
                                   whr88_10 = runStateT whr13_86 d
                                   preImpPap89_11 = runStateT preImpPap14_87 d
                                   prePeg90_12 = runStateT prePeg15_88 d
                                   afterPeg91_13 = runStateT afterPeg16_89 d
                                   importPapillon92_14 = runStateT importPapillon17_90 d
                                   varToken93_15 = runStateT varToken18_91 d
                                   typToken94_16 = runStateT typToken19_92 d
                                   pap95_17 = runStateT pap20_93 d
                                   peg96_18 = runStateT peg21_94 d
                                   sourceType97_19 = runStateT sourceType22_95 d
                                   peg_98_20 = runStateT peg_23_96 d
                                   definition99_21 = runStateT definition24_97 d
                                   selection100_22 = runStateT selection25_98 d
                                   normalSelection101_23 = runStateT normalSelection26_99 d
                                   plainSelection102_24 = runStateT plainSelection27_100 d
                                   expressionHs103_25 = runStateT expressionHs28_101 d
                                   plainExpressionHs104_26 = runStateT plainExpressionHs29_102 d
                                   plainReadFromLs105_27 = runStateT plainReadFromLs30_103 d
                                   expression106_28 = runStateT expression31_104 d
                                   nameLeaf_107_29 = runStateT nameLeaf_32_105 d
                                   nameLeaf108_30 = runStateT nameLeaf33_106 d
                                   nameLeafNoCom109_31 = runStateT nameLeafNoCom34_107 d
                                   comForErr110_32 = runStateT comForErr35_108 d
                                   leaf111_33 = runStateT leaf36_109 d
                                   patOp112_34 = runStateT patOp37_110 d
                                   pat113_35 = runStateT pat38_111 d
                                   pat1114_36 = runStateT pat139_112 d
                                   patList115_37 = runStateT patList40_113 d
                                   opConName116_38 = runStateT opConName41_114 d
                                   charLit117_39 = runStateT charLit42_115 d
                                   stringLit118_40 = runStateT stringLit43_116 d
                                   escapeC119_41 = runStateT escapeC44_117 d
                                   pats120_42 = runStateT pats45_118 d
                                   readFromLs121_43 = runStateT readFromLs46_119 d
                                   readFrom122_44 = runStateT readFrom47_120 d
                                   selectCharsLs123_45 = runStateT selectCharsLs48_121 d
                                   selectChars124_46 = runStateT selectChars49_122 d
                                   test125_47 = runStateT test50_123 d
                                   hsExpLam126_48 = runStateT hsExpLam51_124 d
                                   hsExpTyp127_49 = runStateT hsExpTyp52_125 d
                                   hsExpOp128_50 = runStateT hsExpOp53_126 d
                                   hsOp129_51 = runStateT hsOp54_127 d
                                   opTail130_52 = runStateT opTail55_128 d
                                   hsExp131_53 = runStateT hsExp56_129 d
                                   hsExp1132_54 = runStateT hsExp157_130 d
                                   hsExpTpl133_55 = runStateT hsExpTpl58_131 d
                                   hsTypeArr134_56 = runStateT hsTypeArr59_132 d
                                   hsType135_57 = runStateT hsType60_133 d
                                   hsType1136_58 = runStateT hsType161_134 d
                                   hsTypeTpl137_59 = runStateT hsTypeTpl62_135 d
                                   typ138_60 = runStateT typ63_136 d
                                   variable139_61 = runStateT variable64_137 d
                                   tvtail140_62 = runStateT tvtail65_138 d
                                   integer141_63 = runStateT integer66_139 d
                                   alpha142_64 = runStateT alpha67_140 d
                                   upper143_65 = runStateT upper68_141 d
                                   lower144_66 = runStateT lower69_142 d
                                   digit145_67 = runStateT digit70_143 d
                                   spaces146_68 = runStateT spaces71_144 d
                                   space147_69 = runStateT space72_145 d
                                   notNLString148_70 = runStateT notNLString73_146 d
                                   newLine149_71 = runStateT newLine74_147 d
                                   comment150_72 = runStateT comment75_148 d
                                   comments151_73 = runStateT comments76_149 d
                                   notComStr152_74 = runStateT notComStr77_150 d
                                   comEnd153_75 = runStateT comEnd78_151 d
                                   chars154_76 = runStateT (case getToken s of
                                                                Just (c,
                                                                      s') -> do put (parse0_0 (updatePos c pos) s')
                                                                                return c
                                                                _ -> gets position >>= (throwError . mkParseError "" "end of input" "" undefined [])) d
                pegFile4_77 = foldl1 mplus [do pr <- StateT pragmas
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
                                               d172_152 <- get
                                               xx171_153 <- StateT char
                                               case xx171_153 of
                                                   '|' -> return ()
                                                   _ -> gets position >>= (throwError . mkParseError "'|'" "not match pattern: " "" d172_152 ["char"])
                                               let '|' = xx171_153
                                               return ()
                                               d174_154 <- get
                                               xx173_155 <- StateT char
                                               case xx173_155 of
                                                   ']' -> return ()
                                                   _ -> gets position >>= (throwError . mkParseError "']'" "not match pattern: " "" d174_154 ["char"])
                                               let ']' = xx173_155
                                               return ()
                                               d176_156 <- get
                                               xx175_157 <- StateT char
                                               case xx175_157 of
                                                   '\n' -> return ()
                                                   _ -> gets position >>= (throwError . mkParseError "'\\n'" "not match pattern: " "" d176_156 ["char"])
                                               let '\n' = xx175_157
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
                                               d192_158 <- get
                                               xx191_159 <- StateT char
                                               case xx191_159 of
                                                   '|' -> return ()
                                                   _ -> gets position >>= (throwError . mkParseError "'|'" "not match pattern: " "" d192_158 ["char"])
                                               let '|' = xx191_159
                                               return ()
                                               d194_160 <- get
                                               xx193_161 <- StateT char
                                               case xx193_161 of
                                                   ']' -> return ()
                                                   _ -> gets position >>= (throwError . mkParseError "']'" "not match pattern: " "" d194_160 ["char"])
                                               let ']' = xx193_161
                                               return ()
                                               d196_162 <- get
                                               xx195_163 <- StateT char
                                               case xx195_163 of
                                                   '\n' -> return ()
                                                   _ -> gets position >>= (throwError . mkParseError "'\\n'" "not match pattern: " "" d196_162 ["char"])
                                               let '\n' = xx195_163
                                               return ()
                                               atp <- StateT afterPeg
                                               return (mkPegFile pr md emp pp p atp)]
                pragmas5_78 = foldl1 mplus [do _ <- StateT spaces
                                               return ()
                                               pr <- StateT pragma
                                               prs <- StateT pragmas
                                               return (pr : prs),
                                            do _ <- StateT spaces
                                               return ()
                                               return []]
                pragma6_79 = foldl1 mplus [do d208_164 <- get
                                              xx207_165 <- StateT char
                                              case xx207_165 of
                                                  '{' -> return ()
                                                  _ -> gets position >>= (throwError . mkParseError "'{'" "not match pattern: " "" d208_164 ["char"])
                                              let '{' = xx207_165
                                              return ()
                                              d210_166 <- get
                                              xx209_167 <- StateT char
                                              case xx209_167 of
                                                  '-' -> return ()
                                                  _ -> gets position >>= (throwError . mkParseError "'-'" "not match pattern: " "" d210_166 ["char"])
                                              let '-' = xx209_167
                                              return ()
                                              d212_168 <- get
                                              xx211_169 <- StateT char
                                              case xx211_169 of
                                                  '#' -> return ()
                                                  _ -> gets position >>= (throwError . mkParseError "'#'" "not match pattern: " "" d212_168 ["char"])
                                              let '#' = xx211_169
                                              return ()
                                              _ <- StateT spaces
                                              return ()
                                              d216_170 <- get
                                              xx215_171 <- StateT char
                                              case xx215_171 of
                                                  'L' -> return ()
                                                  _ -> gets position >>= (throwError . mkParseError "'L'" "not match pattern: " "" d216_170 ["char"])
                                              let 'L' = xx215_171
                                              return ()
                                              d218_172 <- get
                                              xx217_173 <- StateT char
                                              case xx217_173 of
                                                  'A' -> return ()
                                                  _ -> gets position >>= (throwError . mkParseError "'A'" "not match pattern: " "" d218_172 ["char"])
                                              let 'A' = xx217_173
                                              return ()
                                              d220_174 <- get
                                              xx219_175 <- StateT char
                                              case xx219_175 of
                                                  'N' -> return ()
                                                  _ -> gets position >>= (throwError . mkParseError "'N'" "not match pattern: " "" d220_174 ["char"])
                                              let 'N' = xx219_175
                                              return ()
                                              d222_176 <- get
                                              xx221_177 <- StateT char
                                              case xx221_177 of
                                                  'G' -> return ()
                                                  _ -> gets position >>= (throwError . mkParseError "'G'" "not match pattern: " "" d222_176 ["char"])
                                              let 'G' = xx221_177
                                              return ()
                                              d224_178 <- get
                                              xx223_179 <- StateT char
                                              case xx223_179 of
                                                  'U' -> return ()
                                                  _ -> gets position >>= (throwError . mkParseError "'U'" "not match pattern: " "" d224_178 ["char"])
                                              let 'U' = xx223_179
                                              return ()
                                              d226_180 <- get
                                              xx225_181 <- StateT char
                                              case xx225_181 of
                                                  'A' -> return ()
                                                  _ -> gets position >>= (throwError . mkParseError "'A'" "not match pattern: " "" d226_180 ["char"])
                                              let 'A' = xx225_181
                                              return ()
                                              d228_182 <- get
                                              xx227_183 <- StateT char
                                              case xx227_183 of
                                                  'G' -> return ()
                                                  _ -> gets position >>= (throwError . mkParseError "'G'" "not match pattern: " "" d228_182 ["char"])
                                              let 'G' = xx227_183
                                              return ()
                                              d230_184 <- get
                                              xx229_185 <- StateT char
                                              case xx229_185 of
                                                  'E' -> return ()
                                                  _ -> gets position >>= (throwError . mkParseError "'E'" "not match pattern: " "" d230_184 ["char"])
                                              let 'E' = xx229_185
                                              return ()
                                              _ <- StateT spaces
                                              return ()
                                              s <- StateT pragmaItems
                                              _ <- StateT pragmaEnd
                                              return ()
                                              _ <- StateT spaces
                                              return ()
                                              return (LanguagePragma s),
                                           do d240_186 <- get
                                              xx239_187 <- StateT char
                                              case xx239_187 of
                                                  '{' -> return ()
                                                  _ -> gets position >>= (throwError . mkParseError "'{'" "not match pattern: " "" d240_186 ["char"])
                                              let '{' = xx239_187
                                              return ()
                                              d242_188 <- get
                                              xx241_189 <- StateT char
                                              case xx241_189 of
                                                  '-' -> return ()
                                                  _ -> gets position >>= (throwError . mkParseError "'-'" "not match pattern: " "" d242_188 ["char"])
                                              let '-' = xx241_189
                                              return ()
                                              d244_190 <- get
                                              xx243_191 <- StateT char
                                              case xx243_191 of
                                                  '#' -> return ()
                                                  _ -> gets position >>= (throwError . mkParseError "'#'" "not match pattern: " "" d244_190 ["char"])
                                              let '#' = xx243_191
                                              return ()
                                              _ <- StateT spaces
                                              return ()
                                              s <- StateT pragmaStr
                                              _ <- StateT pragmaEnd
                                              return ()
                                              return (OtherPragma s)]
                pragmaStr7_80 = foldl1 mplus [do ddd251_192 <- get
                                                 do err <- ((do _ <- StateT pragmaEnd
                                                                return ()) >> return False) `catchError` const (return True)
                                                    unless err (gets position >>= (throwError . mkParseError ('!' : "_:pragmaEnd") "not match: " "" ddd251_192 ["pragmaEnd"]))
                                                 put ddd251_192
                                                 c <- StateT char
                                                 s <- StateT pragmaStr
                                                 return (c : s),
                                              return ""]
                pragmaItems8_81 = foldl1 mplus [do t <- StateT typToken
                                                   d261_193 <- get
                                                   xx260_194 <- StateT char
                                                   case xx260_194 of
                                                       ',' -> return ()
                                                       _ -> gets position >>= (throwError . mkParseError "','" "not match pattern: " "" d261_193 ["char"])
                                                   let ',' = xx260_194
                                                   return ()
                                                   _ <- StateT spaces
                                                   return ()
                                                   i <- StateT pragmaItems
                                                   return (t : i),
                                                do t <- StateT typToken
                                                   return [t]]
                pragmaEnd9_82 = foldl1 mplus [do _ <- StateT spaces
                                                 return ()
                                                 d271_195 <- get
                                                 xx270_196 <- StateT char
                                                 case xx270_196 of
                                                     '#' -> return ()
                                                     _ -> gets position >>= (throwError . mkParseError "'#'" "not match pattern: " "" d271_195 ["char"])
                                                 let '#' = xx270_196
                                                 return ()
                                                 d273_197 <- get
                                                 xx272_198 <- StateT char
                                                 case xx272_198 of
                                                     '-' -> return ()
                                                     _ -> gets position >>= (throwError . mkParseError "'-'" "not match pattern: " "" d273_197 ["char"])
                                                 let '-' = xx272_198
                                                 return ()
                                                 d275_199 <- get
                                                 xx274_200 <- StateT char
                                                 case xx274_200 of
                                                     '}' -> return ()
                                                     _ -> gets position >>= (throwError . mkParseError "'}'" "not match pattern: " "" d275_199 ["char"])
                                                 let '}' = xx274_200
                                                 return ()
                                                 return ()]
                moduleDec10_83 = foldl1 mplus [do d277_201 <- get
                                                  xx276_202 <- StateT char
                                                  case xx276_202 of
                                                      'm' -> return ()
                                                      _ -> gets position >>= (throwError . mkParseError "'m'" "not match pattern: " "" d277_201 ["char"])
                                                  let 'm' = xx276_202
                                                  return ()
                                                  d279_203 <- get
                                                  xx278_204 <- StateT char
                                                  case xx278_204 of
                                                      'o' -> return ()
                                                      _ -> gets position >>= (throwError . mkParseError "'o'" "not match pattern: " "" d279_203 ["char"])
                                                  let 'o' = xx278_204
                                                  return ()
                                                  d281_205 <- get
                                                  xx280_206 <- StateT char
                                                  case xx280_206 of
                                                      'd' -> return ()
                                                      _ -> gets position >>= (throwError . mkParseError "'d'" "not match pattern: " "" d281_205 ["char"])
                                                  let 'd' = xx280_206
                                                  return ()
                                                  d283_207 <- get
                                                  xx282_208 <- StateT char
                                                  case xx282_208 of
                                                      'u' -> return ()
                                                      _ -> gets position >>= (throwError . mkParseError "'u'" "not match pattern: " "" d283_207 ["char"])
                                                  let 'u' = xx282_208
                                                  return ()
                                                  d285_209 <- get
                                                  xx284_210 <- StateT char
                                                  case xx284_210 of
                                                      'l' -> return ()
                                                      _ -> gets position >>= (throwError . mkParseError "'l'" "not match pattern: " "" d285_209 ["char"])
                                                  let 'l' = xx284_210
                                                  return ()
                                                  d287_211 <- get
                                                  xx286_212 <- StateT char
                                                  case xx286_212 of
                                                      'e' -> return ()
                                                      _ -> gets position >>= (throwError . mkParseError "'e'" "not match pattern: " "" d287_211 ["char"])
                                                  let 'e' = xx286_212
                                                  return ()
                                                  _ <- StateT spaces
                                                  return ()
                                                  n <- StateT moduleName
                                                  _ <- StateT spaces
                                                  return ()
                                                  d295_213 <- get
                                                  xx294_214 <- StateT char
                                                  case xx294_214 of
                                                      '(' -> return ()
                                                      _ -> gets position >>= (throwError . mkParseError "'('" "not match pattern: " "" d295_213 ["char"])
                                                  let '(' = xx294_214
                                                  return ()
                                                  _ <- StateT spaces
                                                  return ()
                                                  s <- StateT moduleDecStr
                                                  _ <- StateT whr
                                                  return ()
                                                  return (Just (n, Just s)),
                                               do d303_215 <- get
                                                  xx302_216 <- StateT char
                                                  case xx302_216 of
                                                      'm' -> return ()
                                                      _ -> gets position >>= (throwError . mkParseError "'m'" "not match pattern: " "" d303_215 ["char"])
                                                  let 'm' = xx302_216
                                                  return ()
                                                  d305_217 <- get
                                                  xx304_218 <- StateT char
                                                  case xx304_218 of
                                                      'o' -> return ()
                                                      _ -> gets position >>= (throwError . mkParseError "'o'" "not match pattern: " "" d305_217 ["char"])
                                                  let 'o' = xx304_218
                                                  return ()
                                                  d307_219 <- get
                                                  xx306_220 <- StateT char
                                                  case xx306_220 of
                                                      'd' -> return ()
                                                      _ -> gets position >>= (throwError . mkParseError "'d'" "not match pattern: " "" d307_219 ["char"])
                                                  let 'd' = xx306_220
                                                  return ()
                                                  d309_221 <- get
                                                  xx308_222 <- StateT char
                                                  case xx308_222 of
                                                      'u' -> return ()
                                                      _ -> gets position >>= (throwError . mkParseError "'u'" "not match pattern: " "" d309_221 ["char"])
                                                  let 'u' = xx308_222
                                                  return ()
                                                  d311_223 <- get
                                                  xx310_224 <- StateT char
                                                  case xx310_224 of
                                                      'l' -> return ()
                                                      _ -> gets position >>= (throwError . mkParseError "'l'" "not match pattern: " "" d311_223 ["char"])
                                                  let 'l' = xx310_224
                                                  return ()
                                                  d313_225 <- get
                                                  xx312_226 <- StateT char
                                                  case xx312_226 of
                                                      'e' -> return ()
                                                      _ -> gets position >>= (throwError . mkParseError "'e'" "not match pattern: " "" d313_225 ["char"])
                                                  let 'e' = xx312_226
                                                  return ()
                                                  _ <- StateT spaces
                                                  return ()
                                                  n <- StateT moduleName
                                                  _ <- StateT spaces
                                                  return ()
                                                  d321_227 <- get
                                                  xx320_228 <- StateT char
                                                  case xx320_228 of
                                                      'w' -> return ()
                                                      _ -> gets position >>= (throwError . mkParseError "'w'" "not match pattern: " "" d321_227 ["char"])
                                                  let 'w' = xx320_228
                                                  return ()
                                                  d323_229 <- get
                                                  xx322_230 <- StateT char
                                                  case xx322_230 of
                                                      'h' -> return ()
                                                      _ -> gets position >>= (throwError . mkParseError "'h'" "not match pattern: " "" d323_229 ["char"])
                                                  let 'h' = xx322_230
                                                  return ()
                                                  d325_231 <- get
                                                  xx324_232 <- StateT char
                                                  case xx324_232 of
                                                      'e' -> return ()
                                                      _ -> gets position >>= (throwError . mkParseError "'e'" "not match pattern: " "" d325_231 ["char"])
                                                  let 'e' = xx324_232
                                                  return ()
                                                  d327_233 <- get
                                                  xx326_234 <- StateT char
                                                  case xx326_234 of
                                                      'r' -> return ()
                                                      _ -> gets position >>= (throwError . mkParseError "'r'" "not match pattern: " "" d327_233 ["char"])
                                                  let 'r' = xx326_234
                                                  return ()
                                                  d329_235 <- get
                                                  xx328_236 <- StateT char
                                                  case xx328_236 of
                                                      'e' -> return ()
                                                      _ -> gets position >>= (throwError . mkParseError "'e'" "not match pattern: " "" d329_235 ["char"])
                                                  let 'e' = xx328_236
                                                  return ()
                                                  _ <- StateT spaces
                                                  return ()
                                                  return (Just (n, Nothing)),
                                               return Nothing]
                moduleName11_84 = foldl1 mplus [do t <- StateT typ
                                                   d335_237 <- get
                                                   xx334_238 <- StateT char
                                                   case xx334_238 of
                                                       '.' -> return ()
                                                       _ -> gets position >>= (throwError . mkParseError "'.'" "not match pattern: " "" d335_237 ["char"])
                                                   let '.' = xx334_238
                                                   return ()
                                                   n <- StateT moduleName
                                                   return (t : n),
                                                do t <- StateT typ
                                                   return [t]]
                moduleDecStr12_85 = foldl1 mplus [do ddd340_239 <- get
                                                     do err <- ((do _ <- StateT whr
                                                                    return ()) >> return False) `catchError` const (return True)
                                                        unless err (gets position >>= (throwError . mkParseError ('!' : "_:whr") "not match: " "" ddd340_239 ["whr"]))
                                                     put ddd340_239
                                                     c <- StateT char
                                                     s <- StateT moduleDecStr
                                                     return (c : s),
                                                  return ""]
                whr13_86 = foldl1 mplus [do _ <- StateT spaces
                                            return ()
                                            d350_240 <- get
                                            xx349_241 <- StateT char
                                            case xx349_241 of
                                                ')' -> return ()
                                                _ -> gets position >>= (throwError . mkParseError "')'" "not match pattern: " "" d350_240 ["char"])
                                            let ')' = xx349_241
                                            return ()
                                            _ <- StateT spaces
                                            return ()
                                            d354_242 <- get
                                            xx353_243 <- StateT char
                                            case xx353_243 of
                                                'w' -> return ()
                                                _ -> gets position >>= (throwError . mkParseError "'w'" "not match pattern: " "" d354_242 ["char"])
                                            let 'w' = xx353_243
                                            return ()
                                            d356_244 <- get
                                            xx355_245 <- StateT char
                                            case xx355_245 of
                                                'h' -> return ()
                                                _ -> gets position >>= (throwError . mkParseError "'h'" "not match pattern: " "" d356_244 ["char"])
                                            let 'h' = xx355_245
                                            return ()
                                            d358_246 <- get
                                            xx357_247 <- StateT char
                                            case xx357_247 of
                                                'e' -> return ()
                                                _ -> gets position >>= (throwError . mkParseError "'e'" "not match pattern: " "" d358_246 ["char"])
                                            let 'e' = xx357_247
                                            return ()
                                            d360_248 <- get
                                            xx359_249 <- StateT char
                                            case xx359_249 of
                                                'r' -> return ()
                                                _ -> gets position >>= (throwError . mkParseError "'r'" "not match pattern: " "" d360_248 ["char"])
                                            let 'r' = xx359_249
                                            return ()
                                            d362_250 <- get
                                            xx361_251 <- StateT char
                                            case xx361_251 of
                                                'e' -> return ()
                                                _ -> gets position >>= (throwError . mkParseError "'e'" "not match pattern: " "" d362_250 ["char"])
                                            let 'e' = xx361_251
                                            return ()
                                            return ()]
                preImpPap14_87 = foldl1 mplus [do ddd363_252 <- get
                                                  do err <- ((do _ <- StateT importPapillon
                                                                 return ()) >> return False) `catchError` const (return True)
                                                     unless err (gets position >>= (throwError . mkParseError ('!' : "_:importPapillon") "not match: " "" ddd363_252 ["importPapillon"]))
                                                  put ddd363_252
                                                  ddd366_253 <- get
                                                  do err <- ((do _ <- StateT pap
                                                                 return ()) >> return False) `catchError` const (return True)
                                                     unless err (gets position >>= (throwError . mkParseError ('!' : "_:pap") "not match: " "" ddd366_253 ["pap"]))
                                                  put ddd366_253
                                                  c <- StateT char
                                                  pip <- StateT preImpPap
                                                  return (cons c pip),
                                               return emp]
                prePeg15_88 = foldl1 mplus [do ddd373_254 <- get
                                               do err <- ((do _ <- StateT pap
                                                              return ()) >> return False) `catchError` const (return True)
                                                  unless err (gets position >>= (throwError . mkParseError ('!' : "_:pap") "not match: " "" ddd373_254 ["pap"]))
                                               put ddd373_254
                                               c <- StateT char
                                               pp <- StateT prePeg
                                               return (cons c pp),
                                            return emp]
                afterPeg16_89 = foldl1 mplus [do c <- StateT char
                                                 atp <- StateT afterPeg
                                                 return (cons c atp),
                                              return emp]
                importPapillon17_90 = foldl1 mplus [do d385_255 <- get
                                                       xx384_256 <- StateT varToken
                                                       case xx384_256 of
                                                           "import" -> return ()
                                                           _ -> gets position >>= (throwError . mkParseError "\"import\"" "not match pattern: " "" d385_255 ["varToken"])
                                                       let "import" = xx384_256
                                                       return ()
                                                       d387_257 <- get
                                                       xx386_258 <- StateT typToken
                                                       case xx386_258 of
                                                           "Text" -> return ()
                                                           _ -> gets position >>= (throwError . mkParseError "\"Text\"" "not match pattern: " "" d387_257 ["typToken"])
                                                       let "Text" = xx386_258
                                                       return ()
                                                       d389_259 <- get
                                                       xx388_260 <- StateT char
                                                       case xx388_260 of
                                                           '.' -> return ()
                                                           _ -> gets position >>= (throwError . mkParseError "'.'" "not match pattern: " "" d389_259 ["char"])
                                                       let '.' = xx388_260
                                                       return ()
                                                       _ <- StateT spaces
                                                       return ()
                                                       d393_261 <- get
                                                       xx392_262 <- StateT typToken
                                                       case xx392_262 of
                                                           "Papillon" -> return ()
                                                           _ -> gets position >>= (throwError . mkParseError "\"Papillon\"" "not match pattern: " "" d393_261 ["typToken"])
                                                       let "Papillon" = xx392_262
                                                       return ()
                                                       ddd394_263 <- get
                                                       do err <- ((do d396_264 <- get
                                                                      xx395_265 <- StateT char
                                                                      case xx395_265 of
                                                                          '.' -> return ()
                                                                          _ -> gets position >>= (throwError . mkParseError "'.'" "not match pattern: " "" d396_264 ["char"])
                                                                      let '.' = xx395_265
                                                                      return ()) >> return False) `catchError` const (return True)
                                                          unless err (gets position >>= (throwError . mkParseError ('!' : "'.':") "not match: " "" ddd394_263 ["char"]))
                                                       put ddd394_263
                                                       return ()]
                varToken18_91 = foldl1 mplus [do v <- StateT variable
                                                 _ <- StateT spaces
                                                 return ()
                                                 return v]
                typToken19_92 = foldl1 mplus [do t <- StateT typ
                                                 _ <- StateT spaces
                                                 return ()
                                                 return t]
                pap20_93 = foldl1 mplus [do d406_266 <- get
                                            xx405_267 <- StateT char
                                            case xx405_267 of
                                                '\n' -> return ()
                                                _ -> gets position >>= (throwError . mkParseError "'\\n'" "not match pattern: " "" d406_266 ["char"])
                                            let '\n' = xx405_267
                                            return ()
                                            d408_268 <- get
                                            xx407_269 <- StateT char
                                            case xx407_269 of
                                                '[' -> return ()
                                                _ -> gets position >>= (throwError . mkParseError "'['" "not match pattern: " "" d408_268 ["char"])
                                            let '[' = xx407_269
                                            return ()
                                            d410_270 <- get
                                            xx409_271 <- StateT char
                                            case xx409_271 of
                                                'p' -> return ()
                                                _ -> gets position >>= (throwError . mkParseError "'p'" "not match pattern: " "" d410_270 ["char"])
                                            let 'p' = xx409_271
                                            return ()
                                            d412_272 <- get
                                            xx411_273 <- StateT char
                                            case xx411_273 of
                                                'a' -> return ()
                                                _ -> gets position >>= (throwError . mkParseError "'a'" "not match pattern: " "" d412_272 ["char"])
                                            let 'a' = xx411_273
                                            return ()
                                            d414_274 <- get
                                            xx413_275 <- StateT char
                                            case xx413_275 of
                                                'p' -> return ()
                                                _ -> gets position >>= (throwError . mkParseError "'p'" "not match pattern: " "" d414_274 ["char"])
                                            let 'p' = xx413_275
                                            return ()
                                            d416_276 <- get
                                            xx415_277 <- StateT char
                                            case xx415_277 of
                                                'i' -> return ()
                                                _ -> gets position >>= (throwError . mkParseError "'i'" "not match pattern: " "" d416_276 ["char"])
                                            let 'i' = xx415_277
                                            return ()
                                            d418_278 <- get
                                            xx417_279 <- StateT char
                                            case xx417_279 of
                                                'l' -> return ()
                                                _ -> gets position >>= (throwError . mkParseError "'l'" "not match pattern: " "" d418_278 ["char"])
                                            let 'l' = xx417_279
                                            return ()
                                            d420_280 <- get
                                            xx419_281 <- StateT char
                                            case xx419_281 of
                                                'l' -> return ()
                                                _ -> gets position >>= (throwError . mkParseError "'l'" "not match pattern: " "" d420_280 ["char"])
                                            let 'l' = xx419_281
                                            return ()
                                            d422_282 <- get
                                            xx421_283 <- StateT char
                                            case xx421_283 of
                                                'o' -> return ()
                                                _ -> gets position >>= (throwError . mkParseError "'o'" "not match pattern: " "" d422_282 ["char"])
                                            let 'o' = xx421_283
                                            return ()
                                            d424_284 <- get
                                            xx423_285 <- StateT char
                                            case xx423_285 of
                                                'n' -> return ()
                                                _ -> gets position >>= (throwError . mkParseError "'n'" "not match pattern: " "" d424_284 ["char"])
                                            let 'n' = xx423_285
                                            return ()
                                            d426_286 <- get
                                            xx425_287 <- StateT char
                                            case xx425_287 of
                                                '|' -> return ()
                                                _ -> gets position >>= (throwError . mkParseError "'|'" "not match pattern: " "" d426_286 ["char"])
                                            let '|' = xx425_287
                                            return ()
                                            d428_288 <- get
                                            xx427_289 <- StateT char
                                            case xx427_289 of
                                                '\n' -> return ()
                                                _ -> gets position >>= (throwError . mkParseError "'\\n'" "not match pattern: " "" d428_288 ["char"])
                                            let '\n' = xx427_289
                                            return ()
                                            return ()]
                peg21_94 = foldl1 mplus [do _ <- StateT spaces
                                            return ()
                                            s <- StateT sourceType
                                            p <- StateT peg_
                                            return (mkTTPeg s p),
                                         do p <- StateT peg_
                                            return (mkTTPeg tString p)]
                sourceType22_95 = foldl1 mplus [do d438_290 <- get
                                                   xx437_291 <- StateT varToken
                                                   case xx437_291 of
                                                       "source" -> return ()
                                                       _ -> gets position >>= (throwError . mkParseError "\"source\"" "not match pattern: " "" d438_290 ["varToken"])
                                                   let "source" = xx437_291
                                                   return ()
                                                   d440_292 <- get
                                                   xx439_293 <- StateT char
                                                   case xx439_293 of
                                                       ':' -> return ()
                                                       _ -> gets position >>= (throwError . mkParseError "':'" "not match pattern: " "" d440_292 ["char"])
                                                   let ':' = xx439_293
                                                   return ()
                                                   _ <- StateT spaces
                                                   return ()
                                                   v <- StateT typToken
                                                   return v]
                peg_23_96 = foldl1 mplus [do _ <- StateT spaces
                                             return ()
                                             d <- StateT definition
                                             p <- StateT peg_
                                             return (cons d p),
                                          return emp]
                definition24_97 = foldl1 mplus [do v <- StateT variable
                                                   _ <- StateT spaces
                                                   return ()
                                                   d456_294 <- get
                                                   xx455_295 <- StateT char
                                                   case xx455_295 of
                                                       ':' -> return ()
                                                       _ -> gets position >>= (throwError . mkParseError "':'" "not match pattern: " "" d456_294 ["char"])
                                                   let ':' = xx455_295
                                                   return ()
                                                   d458_296 <- get
                                                   xx457_297 <- StateT char
                                                   case xx457_297 of
                                                       ':' -> return ()
                                                       _ -> gets position >>= (throwError . mkParseError "':'" "not match pattern: " "" d458_296 ["char"])
                                                   let ':' = xx457_297
                                                   return ()
                                                   _ <- StateT spaces
                                                   return ()
                                                   t <- StateT hsTypeArr
                                                   _ <- StateT spaces
                                                   return ()
                                                   d466_298 <- get
                                                   xx465_299 <- StateT char
                                                   case xx465_299 of
                                                       '=' -> return ()
                                                       _ -> gets position >>= (throwError . mkParseError "'='" "not match pattern: " "" d466_298 ["char"])
                                                   let '=' = xx465_299
                                                   return ()
                                                   _ <- StateT spaces
                                                   return ()
                                                   sel <- StateT selection
                                                   _ <- StateT spaces
                                                   return ()
                                                   d474_300 <- get
                                                   xx473_301 <- StateT char
                                                   case xx473_301 of
                                                       ';' -> return ()
                                                       _ -> gets position >>= (throwError . mkParseError "';'" "not match pattern: " "" d474_300 ["char"])
                                                   let ';' = xx473_301
                                                   return ()
                                                   return (mkDef v t sel),
                                                do v <- StateT variable
                                                   _ <- StateT spaces
                                                   return ()
                                                   d480_302 <- get
                                                   xx479_303 <- StateT char
                                                   case xx479_303 of
                                                       '<' -> return ()
                                                       _ -> gets position >>= (throwError . mkParseError "'<'" "not match pattern: " "" d480_302 ["char"])
                                                   let '<' = xx479_303
                                                   return ()
                                                   d482_304 <- get
                                                   xx481_305 <- StateT char
                                                   case xx481_305 of
                                                       '-' -> return ()
                                                       _ -> gets position >>= (throwError . mkParseError "'-'" "not match pattern: " "" d482_304 ["char"])
                                                   let '-' = xx481_305
                                                   return ()
                                                   _ <- StateT spaces
                                                   return ()
                                                   sel <- StateT plainSelection
                                                   _ <- StateT spaces
                                                   return ()
                                                   d490_306 <- get
                                                   xx489_307 <- StateT char
                                                   case xx489_307 of
                                                       ';' -> return ()
                                                       _ -> gets position >>= (throwError . mkParseError "';'" "not match pattern: " "" d490_306 ["char"])
                                                   let ';' = xx489_307
                                                   return ()
                                                   return (PlainDefinition v sel)]
                selection25_98 = foldl1 mplus [do s <- StateT normalSelection
                                                  return s,
                                               do s <- StateT plainSelection
                                                  return s]
                normalSelection26_99 = foldl1 mplus [do ex <- StateT expressionHs
                                                        _ <- StateT spaces
                                                        return ()
                                                        d500_308 <- get
                                                        xx499_309 <- StateT char
                                                        case xx499_309 of
                                                            '/' -> return ()
                                                            _ -> gets position >>= (throwError . mkParseError "'/'" "not match pattern: " "" d500_308 ["char"])
                                                        let '/' = xx499_309
                                                        return ()
                                                        _ <- StateT spaces
                                                        return ()
                                                        sel <- StateT normalSelection
                                                        return (Selection $ ex : expressions sel),
                                                     do ex <- StateT expressionHs
                                                        return (Selection [ex])]
                plainSelection27_100 = foldl1 mplus [do ex <- StateT plainExpressionHs
                                                        _ <- StateT spaces
                                                        return ()
                                                        d512_310 <- get
                                                        xx511_311 <- StateT char
                                                        case xx511_311 of
                                                            '/' -> return ()
                                                            _ -> gets position >>= (throwError . mkParseError "'/'" "not match pattern: " "" d512_310 ["char"])
                                                        let '/' = xx511_311
                                                        return ()
                                                        _ <- StateT spaces
                                                        return ()
                                                        sel <- StateT plainSelection
                                                        return (PlainSelection $ ex : plainExpressions sel),
                                                     do ex <- StateT plainExpressionHs
                                                        return (PlainSelection [ex])]
                expressionHs28_101 = foldl1 mplus [do e <- StateT expression
                                                      _ <- StateT spaces
                                                      return ()
                                                      d524_312 <- get
                                                      xx523_313 <- StateT char
                                                      case xx523_313 of
                                                          '{' -> return ()
                                                          _ -> gets position >>= (throwError . mkParseError "'{'" "not match pattern: " "" d524_312 ["char"])
                                                      let '{' = xx523_313
                                                      return ()
                                                      _ <- StateT spaces
                                                      return ()
                                                      h <- StateT hsExpLam
                                                      _ <- StateT spaces
                                                      return ()
                                                      d532_314 <- get
                                                      xx531_315 <- StateT char
                                                      case xx531_315 of
                                                          '}' -> return ()
                                                          _ -> gets position >>= (throwError . mkParseError "'}'" "not match pattern: " "" d532_314 ["char"])
                                                      let '}' = xx531_315
                                                      return ()
                                                      return (ExpressionHs e h)]
                plainExpressionHs29_102 = foldl1 mplus [do rfs <- list1_316 (foldl1 mplus [do rf <- StateT plainReadFromLs
                                                                                              _ <- StateT spaces
                                                                                              return ()
                                                                                              return rf])
                                                           return (PlainExpressionHs rfs)]
                plainReadFromLs30_103 = foldl1 mplus [do rf <- StateT readFromLs
                                                         return rf,
                                                      do rf <- StateT selectCharsLs
                                                         return rf]
                expression31_104 = foldl1 mplus [do l <- StateT nameLeaf_
                                                    _ <- StateT spaces
                                                    return ()
                                                    e <- StateT expression
                                                    return (cons l e),
                                                 return emp]
                nameLeaf_32_105 = foldl1 mplus [do d550_317 <- get
                                                   xx549_318 <- StateT char
                                                   case xx549_318 of
                                                       '!' -> return ()
                                                       _ -> gets position >>= (throwError . mkParseError "'!'" "not match pattern: " "" d550_317 ["char"])
                                                   let '!' = xx549_318
                                                   return ()
                                                   nl <- StateT nameLeafNoCom
                                                   _ <- StateT spaces
                                                   return ()
                                                   com <- optional3_319 (StateT comForErr)
                                                   return (NotAfter nl $ maybe "" id com),
                                                do d558_320 <- get
                                                   xx557_321 <- StateT char
                                                   let c = xx557_321
                                                   unless (isAmp c) (gets position >>= (throwError . mkParseError "isAmp c" "not match: " "" d558_320 ["char"]))
                                                   nl <- StateT nameLeaf
                                                   return (After nl),
                                                do nl <- StateT nameLeaf
                                                   return (Here nl)]
                nameLeaf33_106 = foldl1 mplus [do n <- StateT pat1
                                                  _ <- StateT spaces
                                                  return ()
                                                  com <- optional3_319 (StateT comForErr)
                                                  d570_322 <- get
                                                  xx569_323 <- StateT char
                                                  case xx569_323 of
                                                      ':' -> return ()
                                                      _ -> gets position >>= (throwError . mkParseError "':'" "not match pattern: " "" d570_322 ["char"])
                                                  let ':' = xx569_323
                                                  return ()
                                                  (rf, p) <- StateT leaf
                                                  return (NameLeaf (n, maybe "" id com) rf p),
                                               do n <- StateT pat1
                                                  _ <- StateT spaces
                                                  return ()
                                                  com <- optional3_319 (StateT comForErr)
                                                  return (NameLeaf (n,
                                                                    maybe "" id com) FromToken Nothing)]
                nameLeafNoCom34_107 = foldl1 mplus [do n <- StateT pat1
                                                       _ <- StateT spaces
                                                       return ()
                                                       com <- optional3_319 (StateT comForErr)
                                                       d586_324 <- get
                                                       xx585_325 <- StateT char
                                                       case xx585_325 of
                                                           ':' -> return ()
                                                           _ -> gets position >>= (throwError . mkParseError "':'" "not match pattern: " "" d586_324 ["char"])
                                                       let ':' = xx585_325
                                                       return ()
                                                       (rf, p) <- StateT leaf
                                                       return (NameLeaf (n, maybe "" id com) rf p),
                                                    do n <- StateT pat1
                                                       _ <- StateT spaces
                                                       return ()
                                                       return (NameLeaf (n, "") FromToken Nothing)]
                comForErr35_108 = foldl1 mplus [do d594_326 <- get
                                                   xx593_327 <- StateT char
                                                   case xx593_327 of
                                                       '{' -> return ()
                                                       _ -> gets position >>= (throwError . mkParseError "'{'" "not match pattern: " "" d594_326 ["char"])
                                                   let '{' = xx593_327
                                                   return ()
                                                   d596_328 <- get
                                                   xx595_329 <- StateT char
                                                   case xx595_329 of
                                                       '-' -> return ()
                                                       _ -> gets position >>= (throwError . mkParseError "'-'" "not match pattern: " "" d596_328 ["char"])
                                                   let '-' = xx595_329
                                                   return ()
                                                   d598_330 <- get
                                                   xx597_331 <- StateT char
                                                   case xx597_331 of
                                                       '#' -> return ()
                                                       _ -> gets position >>= (throwError . mkParseError "'#'" "not match pattern: " "" d598_330 ["char"])
                                                   let '#' = xx597_331
                                                   return ()
                                                   _ <- StateT spaces
                                                   return ()
                                                   d602_332 <- get
                                                   xx601_333 <- StateT char
                                                   case xx601_333 of
                                                       '"' -> return ()
                                                       _ -> gets position >>= (throwError . mkParseError "'\"'" "not match pattern: " "" d602_332 ["char"])
                                                   let '"' = xx601_333
                                                   return ()
                                                   s <- StateT stringLit
                                                   d606_334 <- get
                                                   xx605_335 <- StateT char
                                                   case xx605_335 of
                                                       '"' -> return ()
                                                       _ -> gets position >>= (throwError . mkParseError "'\"'" "not match pattern: " "" d606_334 ["char"])
                                                   let '"' = xx605_335
                                                   return ()
                                                   _ <- StateT spaces
                                                   return ()
                                                   d610_336 <- get
                                                   xx609_337 <- StateT char
                                                   case xx609_337 of
                                                       '#' -> return ()
                                                       _ -> gets position >>= (throwError . mkParseError "'#'" "not match pattern: " "" d610_336 ["char"])
                                                   let '#' = xx609_337
                                                   return ()
                                                   d612_338 <- get
                                                   xx611_339 <- StateT char
                                                   case xx611_339 of
                                                       '-' -> return ()
                                                       _ -> gets position >>= (throwError . mkParseError "'-'" "not match pattern: " "" d612_338 ["char"])
                                                   let '-' = xx611_339
                                                   return ()
                                                   d614_340 <- get
                                                   xx613_341 <- StateT char
                                                   case xx613_341 of
                                                       '}' -> return ()
                                                       _ -> gets position >>= (throwError . mkParseError "'}'" "not match pattern: " "" d614_340 ["char"])
                                                   let '}' = xx613_341
                                                   return ()
                                                   _ <- StateT spaces
                                                   return ()
                                                   return s]
                leaf36_109 = foldl1 mplus [do rf <- StateT readFromLs
                                              t <- StateT test
                                              return (rf, Just t),
                                           do rf <- StateT readFromLs
                                              return (rf, Nothing),
                                           do t <- StateT test
                                              return (FromToken, Just t)]
                patOp37_110 = foldl1 mplus [do p <- StateT pat
                                               o <- StateT opConName
                                               po <- StateT patOp
                                               return (uInfixP p o po),
                                            do p <- StateT pat
                                               _ <- StateT spaces
                                               return ()
                                               d636_342 <- get
                                               xx635_343 <- StateT char
                                               let q = xx635_343
                                               unless (isBQ q) (gets position >>= (throwError . mkParseError "isBQ q" "not match: " "" d636_342 ["char"]))
                                               t <- StateT typ
                                               d640_344 <- get
                                               xx639_345 <- StateT char
                                               let q_ = xx639_345
                                               unless (isBQ q_) (gets position >>= (throwError . mkParseError "isBQ q_" "not match: " "" d640_344 ["char"]))
                                               _ <- StateT spaces
                                               return ()
                                               po <- StateT patOp
                                               return (uInfixP p (mkName t) po),
                                            do p <- StateT pat
                                               return p]
                pat38_111 = foldl1 mplus [do t <- StateT typ
                                             _ <- StateT spaces
                                             return ()
                                             ps <- StateT pats
                                             return (conToPatQ t ps),
                                          do d654_346 <- get
                                             xx653_347 <- StateT char
                                             case xx653_347 of
                                                 '(' -> return ()
                                                 _ -> gets position >>= (throwError . mkParseError "'('" "not match pattern: " "" d654_346 ["char"])
                                             let '(' = xx653_347
                                             return ()
                                             o <- StateT opConName
                                             d658_348 <- get
                                             xx657_349 <- StateT char
                                             case xx657_349 of
                                                 ')' -> return ()
                                                 _ -> gets position >>= (throwError . mkParseError "')'" "not match pattern: " "" d658_348 ["char"])
                                             let ')' = xx657_349
                                             return ()
                                             _ <- StateT spaces
                                             return ()
                                             ps <- StateT pats
                                             return (conP o ps),
                                          do p <- StateT pat1
                                             return p]
                pat139_112 = foldl1 mplus [do t <- StateT typ
                                              return (conToPatQ t emp),
                                           do d668_350 <- get
                                              xx667_351 <- StateT variable
                                              case xx667_351 of
                                                  "_" -> return ()
                                                  _ -> gets position >>= (throwError . mkParseError "\"_\"" "not match pattern: " "" d668_350 ["variable"])
                                              let "_" = xx667_351
                                              return ()
                                              return wildP,
                                           do n <- StateT variable
                                              return (strToPatQ n),
                                           do i <- StateT integer
                                              return (litP (integerL i)),
                                           do d674_352 <- get
                                              xx673_353 <- StateT char
                                              case xx673_353 of
                                                  '-' -> return ()
                                                  _ -> gets position >>= (throwError . mkParseError "'-'" "not match pattern: " "" d674_352 ["char"])
                                              let '-' = xx673_353
                                              return ()
                                              _ <- StateT spaces
                                              return ()
                                              i <- StateT integer
                                              return (litP (integerL $ negate i)),
                                           do d680_354 <- get
                                              xx679_355 <- StateT char
                                              case xx679_355 of
                                                  '\'' -> return ()
                                                  _ -> gets position >>= (throwError . mkParseError "'\\''" "not match pattern: " "" d680_354 ["char"])
                                              let '\'' = xx679_355
                                              return ()
                                              c <- StateT charLit
                                              d684_356 <- get
                                              xx683_357 <- StateT char
                                              case xx683_357 of
                                                  '\'' -> return ()
                                                  _ -> gets position >>= (throwError . mkParseError "'\\''" "not match pattern: " "" d684_356 ["char"])
                                              let '\'' = xx683_357
                                              return ()
                                              return (charP c),
                                           do d686_358 <- get
                                              xx685_359 <- StateT char
                                              case xx685_359 of
                                                  '"' -> return ()
                                                  _ -> gets position >>= (throwError . mkParseError "'\"'" "not match pattern: " "" d686_358 ["char"])
                                              let '"' = xx685_359
                                              return ()
                                              s <- StateT stringLit
                                              d690_360 <- get
                                              xx689_361 <- StateT char
                                              case xx689_361 of
                                                  '"' -> return ()
                                                  _ -> gets position >>= (throwError . mkParseError "'\"'" "not match pattern: " "" d690_360 ["char"])
                                              let '"' = xx689_361
                                              return ()
                                              return (stringP s),
                                           do d692_362 <- get
                                              xx691_363 <- StateT char
                                              case xx691_363 of
                                                  '(' -> return ()
                                                  _ -> gets position >>= (throwError . mkParseError "'('" "not match pattern: " "" d692_362 ["char"])
                                              let '(' = xx691_363
                                              return ()
                                              p <- StateT patList
                                              d696_364 <- get
                                              xx695_365 <- StateT char
                                              case xx695_365 of
                                                  ')' -> return ()
                                                  _ -> gets position >>= (throwError . mkParseError "')'" "not match pattern: " "" d696_364 ["char"])
                                              let ')' = xx695_365
                                              return ()
                                              return (tupP p),
                                           do d698_366 <- get
                                              xx697_367 <- StateT char
                                              case xx697_367 of
                                                  '[' -> return ()
                                                  _ -> gets position >>= (throwError . mkParseError "'['" "not match pattern: " "" d698_366 ["char"])
                                              let '[' = xx697_367
                                              return ()
                                              p <- StateT patList
                                              d702_368 <- get
                                              xx701_369 <- StateT char
                                              case xx701_369 of
                                                  ']' -> return ()
                                                  _ -> gets position >>= (throwError . mkParseError "']'" "not match pattern: " "" d702_368 ["char"])
                                              let ']' = xx701_369
                                              return ()
                                              return (listP p)]
                patList40_113 = foldl1 mplus [do p <- StateT patOp
                                                 _ <- StateT spaces
                                                 return ()
                                                 d708_370 <- get
                                                 xx707_371 <- StateT char
                                                 case xx707_371 of
                                                     ',' -> return ()
                                                     _ -> gets position >>= (throwError . mkParseError "','" "not match pattern: " "" d708_370 ["char"])
                                                 let ',' = xx707_371
                                                 return ()
                                                 _ <- StateT spaces
                                                 return ()
                                                 ps <- StateT patList
                                                 return (p : ps),
                                              do p <- StateT patOp
                                                 return [p],
                                              return []]
                opConName41_114 = foldl1 mplus [do d716_372 <- get
                                                   xx715_373 <- StateT char
                                                   case xx715_373 of
                                                       ':' -> return ()
                                                       _ -> gets position >>= (throwError . mkParseError "':'" "not match pattern: " "" d716_372 ["char"])
                                                   let ':' = xx715_373
                                                   return ()
                                                   ot <- StateT opTail
                                                   return (mkName $ colon : ot)]
                charLit42_115 = foldl1 mplus [do d720_374 <- get
                                                 xx719_375 <- StateT char
                                                 let c = xx719_375
                                                 unless (isAlphaNumOt c) (gets position >>= (throwError . mkParseError "isAlphaNumOt c" "not match: " "" d720_374 ["char"]))
                                                 return c,
                                              do d722_376 <- get
                                                 xx721_377 <- StateT char
                                                 case xx721_377 of
                                                     '\\' -> return ()
                                                     _ -> gets position >>= (throwError . mkParseError "'\\\\'" "not match pattern: " "" d722_376 ["char"])
                                                 let '\\' = xx721_377
                                                 return ()
                                                 c <- StateT escapeC
                                                 return c]
                stringLit43_116 = foldl1 mplus [do d726_378 <- get
                                                   xx725_379 <- StateT char
                                                   let c = xx725_379
                                                   unless (isStrLitC c) (gets position >>= (throwError . mkParseError "isStrLitC c" "not match: " "" d726_378 ["char"]))
                                                   s <- StateT stringLit
                                                   return (cons c s),
                                                do d730_380 <- get
                                                   xx729_381 <- StateT char
                                                   case xx729_381 of
                                                       '\\' -> return ()
                                                       _ -> gets position >>= (throwError . mkParseError "'\\\\'" "not match pattern: " "" d730_380 ["char"])
                                                   let '\\' = xx729_381
                                                   return ()
                                                   c <- StateT escapeC
                                                   s <- StateT stringLit
                                                   return (c : s),
                                                return emp]
                escapeC44_117 = foldl1 mplus [do d736_382 <- get
                                                 xx735_383 <- StateT char
                                                 case xx735_383 of
                                                     '"' -> return ()
                                                     _ -> gets position >>= (throwError . mkParseError "'\"'" "not match pattern: " "" d736_382 ["char"])
                                                 let '"' = xx735_383
                                                 return ()
                                                 return '"',
                                              do d738_384 <- get
                                                 xx737_385 <- StateT char
                                                 case xx737_385 of
                                                     '\'' -> return ()
                                                     _ -> gets position >>= (throwError . mkParseError "'\\''" "not match pattern: " "" d738_384 ["char"])
                                                 let '\'' = xx737_385
                                                 return ()
                                                 return '\'',
                                              do d740_386 <- get
                                                 xx739_387 <- StateT char
                                                 case xx739_387 of
                                                     '\\' -> return ()
                                                     _ -> gets position >>= (throwError . mkParseError "'\\\\'" "not match pattern: " "" d740_386 ["char"])
                                                 let '\\' = xx739_387
                                                 return ()
                                                 return '\\',
                                              do d742_388 <- get
                                                 xx741_389 <- StateT char
                                                 case xx741_389 of
                                                     'n' -> return ()
                                                     _ -> gets position >>= (throwError . mkParseError "'n'" "not match pattern: " "" d742_388 ["char"])
                                                 let 'n' = xx741_389
                                                 return ()
                                                 return '\n',
                                              do d744_390 <- get
                                                 xx743_391 <- StateT char
                                                 case xx743_391 of
                                                     't' -> return ()
                                                     _ -> gets position >>= (throwError . mkParseError "'t'" "not match pattern: " "" d744_390 ["char"])
                                                 let 't' = xx743_391
                                                 return ()
                                                 return tab]
                pats45_118 = foldl1 mplus [do p <- StateT pat
                                              _ <- StateT spaces
                                              return ()
                                              ps <- StateT pats
                                              return (cons p ps),
                                           return emp]
                readFromLs46_119 = foldl1 mplus [do rf <- StateT readFrom
                                                    d754_392 <- get
                                                    xx753_393 <- StateT char
                                                    case xx753_393 of
                                                        '*' -> return ()
                                                        _ -> gets position >>= (throwError . mkParseError "'*'" "not match pattern: " "" d754_392 ["char"])
                                                    let '*' = xx753_393
                                                    return ()
                                                    return (FromList rf),
                                                 do rf <- StateT readFrom
                                                    d758_394 <- get
                                                    xx757_395 <- StateT char
                                                    case xx757_395 of
                                                        '+' -> return ()
                                                        _ -> gets position >>= (throwError . mkParseError "'+'" "not match pattern: " "" d758_394 ["char"])
                                                    let '+' = xx757_395
                                                    return ()
                                                    return (FromList1 rf),
                                                 do rf <- StateT readFrom
                                                    d762_396 <- get
                                                    xx761_397 <- StateT char
                                                    case xx761_397 of
                                                        '?' -> return ()
                                                        _ -> gets position >>= (throwError . mkParseError "'?'" "not match pattern: " "" d762_396 ["char"])
                                                    let '?' = xx761_397
                                                    return ()
                                                    return (FromOptional rf),
                                                 do rf <- StateT readFrom
                                                    return rf]
                readFrom47_120 = foldl1 mplus [do v <- StateT variable
                                                  return (FromVariable v),
                                               do d768_398 <- get
                                                  xx767_399 <- StateT char
                                                  case xx767_399 of
                                                      '(' -> return ()
                                                      _ -> gets position >>= (throwError . mkParseError "'('" "not match pattern: " "" d768_398 ["char"])
                                                  let '(' = xx767_399
                                                  return ()
                                                  s <- StateT selection
                                                  d772_400 <- get
                                                  xx771_401 <- StateT char
                                                  case xx771_401 of
                                                      ')' -> return ()
                                                      _ -> gets position >>= (throwError . mkParseError "')'" "not match pattern: " "" d772_400 ["char"])
                                                  let ')' = xx771_401
                                                  return ()
                                                  return (FromSelection s)]
                selectCharsLs48_121 = foldl1 mplus [do rf <- StateT selectChars
                                                       d776_402 <- get
                                                       xx775_403 <- StateT char
                                                       case xx775_403 of
                                                           '*' -> return ()
                                                           _ -> gets position >>= (throwError . mkParseError "'*'" "not match pattern: " "" d776_402 ["char"])
                                                       let '*' = xx775_403
                                                       return ()
                                                       return (FromList rf),
                                                    do rf <- StateT selectChars
                                                       d780_404 <- get
                                                       xx779_405 <- StateT char
                                                       case xx779_405 of
                                                           '+' -> return ()
                                                           _ -> gets position >>= (throwError . mkParseError "'+'" "not match pattern: " "" d780_404 ["char"])
                                                       let '+' = xx779_405
                                                       return ()
                                                       return (FromList1 rf),
                                                    do rf <- StateT selectChars
                                                       d784_406 <- get
                                                       xx783_407 <- StateT char
                                                       case xx783_407 of
                                                           '?' -> return ()
                                                           _ -> gets position >>= (throwError . mkParseError "'?'" "not match pattern: " "" d784_406 ["char"])
                                                       let '?' = xx783_407
                                                       return ()
                                                       return (FromOptional rf),
                                                    do rf <- StateT selectChars
                                                       return rf]
                selectChars49_122 = foldl1 mplus [do d788_408 <- get
                                                     xx787_409 <- StateT char
                                                     case xx787_409 of
                                                         '[' -> return ()
                                                         _ -> gets position >>= (throwError . mkParseError "'['" "not match pattern: " "" d788_408 ["char"])
                                                     let '[' = xx787_409
                                                     return ()
                                                     cs <- list12_410 (foldl1 mplus [do d792_411 <- get
                                                                                        xx791_412 <- StateT char
                                                                                        let c = xx791_412
                                                                                        unless (isLower c) (gets position >>= (throwError . mkParseError "isLower c" "not match: " "" d792_411 ["char"]))
                                                                                        return c])
                                                     d794_413 <- get
                                                     xx793_414 <- StateT char
                                                     case xx793_414 of
                                                         ']' -> return ()
                                                         _ -> gets position >>= (throwError . mkParseError "']'" "not match pattern: " "" d794_413 ["char"])
                                                     let ']' = xx793_414
                                                     return ()
                                                     return (FromTokenChars cs)]
                test50_123 = foldl1 mplus [do d796_415 <- get
                                              xx795_416 <- StateT char
                                              case xx795_416 of
                                                  '[' -> return ()
                                                  _ -> gets position >>= (throwError . mkParseError "'['" "not match pattern: " "" d796_415 ["char"])
                                              let '[' = xx795_416
                                              return ()
                                              h <- StateT hsExpLam
                                              _ <- StateT spaces
                                              return ()
                                              com <- optional3_319 (StateT comForErr)
                                              d804_417 <- get
                                              xx803_418 <- StateT char
                                              case xx803_418 of
                                                  ']' -> return ()
                                                  _ -> gets position >>= (throwError . mkParseError "']'" "not match pattern: " "" d804_417 ["char"])
                                              let ']' = xx803_418
                                              return ()
                                              return (h, maybe "" id com)]
                hsExpLam51_124 = foldl1 mplus [do d806_419 <- get
                                                  xx805_420 <- StateT char
                                                  case xx805_420 of
                                                      '\\' -> return ()
                                                      _ -> gets position >>= (throwError . mkParseError "'\\\\'" "not match pattern: " "" d806_419 ["char"])
                                                  let '\\' = xx805_420
                                                  return ()
                                                  _ <- StateT spaces
                                                  return ()
                                                  ps <- StateT pats
                                                  _ <- StateT spaces
                                                  return ()
                                                  d814_421 <- get
                                                  xx813_422 <- StateT char
                                                  case xx813_422 of
                                                      '-' -> return ()
                                                      _ -> gets position >>= (throwError . mkParseError "'-'" "not match pattern: " "" d814_421 ["char"])
                                                  let '-' = xx813_422
                                                  return ()
                                                  d816_423 <- get
                                                  xx815_424 <- StateT char
                                                  let c = xx815_424
                                                  unless (isGt c) (gets position >>= (throwError . mkParseError "isGt c" "not match: " "" d816_423 ["char"]))
                                                  _ <- StateT spaces
                                                  return ()
                                                  e <- StateT hsExpTyp
                                                  return (lamE ps e),
                                               do e <- StateT hsExpTyp
                                                  return e]
                hsExpTyp52_125 = foldl1 mplus [do eo <- StateT hsExpOp
                                                  d826_425 <- get
                                                  xx825_426 <- StateT char
                                                  case xx825_426 of
                                                      ':' -> return ()
                                                      _ -> gets position >>= (throwError . mkParseError "':'" "not match pattern: " "" d826_425 ["char"])
                                                  let ':' = xx825_426
                                                  return ()
                                                  d828_427 <- get
                                                  xx827_428 <- StateT char
                                                  case xx827_428 of
                                                      ':' -> return ()
                                                      _ -> gets position >>= (throwError . mkParseError "':'" "not match pattern: " "" d828_427 ["char"])
                                                  let ':' = xx827_428
                                                  return ()
                                                  _ <- StateT spaces
                                                  return ()
                                                  t <- StateT hsTypeArr
                                                  return (sigE eo t),
                                               do eo <- StateT hsExpOp
                                                  return eo]
                hsExpOp53_126 = foldl1 mplus [do l <- StateT hsExp
                                                 _ <- StateT spaces
                                                 return ()
                                                 o <- StateT hsOp
                                                 _ <- StateT spaces
                                                 return ()
                                                 r <- StateT hsExpOp
                                                 return (uInfixE (getEx l) o r),
                                              do e <- StateT hsExp
                                                 return (getEx e)]
                hsOp54_127 = foldl1 mplus [do d848_429 <- get
                                              xx847_430 <- StateT char
                                              let c = xx847_430
                                              unless (isOpHeadChar c) (gets position >>= (throwError . mkParseError "isOpHeadChar c" "not match: " "" d848_429 ["char"]))
                                              o <- StateT opTail
                                              return (varE (mkName (cons c o))),
                                           do d852_431 <- get
                                              xx851_432 <- StateT char
                                              case xx851_432 of
                                                  ':' -> return ()
                                                  _ -> gets position >>= (throwError . mkParseError "':'" "not match pattern: " "" d852_431 ["char"])
                                              let ':' = xx851_432
                                              return ()
                                              ddd853_433 <- get
                                              do err <- ((do d855_434 <- get
                                                             xx854_435 <- StateT char
                                                             case xx854_435 of
                                                                 ':' -> return ()
                                                                 _ -> gets position >>= (throwError . mkParseError "':'" "not match pattern: " "" d855_434 ["char"])
                                                             let ':' = xx854_435
                                                             return ()) >> return False) `catchError` const (return True)
                                                 unless err (gets position >>= (throwError . mkParseError ('!' : "':':") "not match: " "" ddd853_433 ["char"]))
                                              put ddd853_433
                                              o <- StateT opTail
                                              return (conE (mkName (':' : o))),
                                           do d859_436 <- get
                                              xx858_437 <- StateT char
                                              let c = xx858_437
                                              unless (isBQ c) (gets position >>= (throwError . mkParseError "isBQ c" "not match: " "" d859_436 ["char"]))
                                              v <- StateT variable
                                              d863_438 <- get
                                              xx862_439 <- StateT char
                                              let c_ = xx862_439
                                              unless (isBQ c_) (gets position >>= (throwError . mkParseError "isBQ c_" "not match: " "" d863_438 ["char"]))
                                              return (varE (mkName v)),
                                           do d865_440 <- get
                                              xx864_441 <- StateT char
                                              let c = xx864_441
                                              unless (isBQ c) (gets position >>= (throwError . mkParseError "isBQ c" "not match: " "" d865_440 ["char"]))
                                              t <- StateT typ
                                              d869_442 <- get
                                              xx868_443 <- StateT char
                                              let c_ = xx868_443
                                              unless (isBQ c_) (gets position >>= (throwError . mkParseError "isBQ c_" "not match: " "" d869_442 ["char"]))
                                              return (conE (mkName t))]
                opTail55_128 = foldl1 mplus [do d871_444 <- get
                                                xx870_445 <- StateT char
                                                let c = xx870_445
                                                unless (isOpTailChar c) (gets position >>= (throwError . mkParseError "isOpTailChar c" "not match: " "" d871_444 ["char"]))
                                                s <- StateT opTail
                                                return (cons c s),
                                             return emp]
                hsExp56_129 = foldl1 mplus [do e <- StateT hsExp1
                                               _ <- StateT spaces
                                               return ()
                                               h <- StateT hsExp
                                               return (applyExR e h),
                                            do e <- StateT hsExp1
                                               return (toEx e)]
                hsExp157_130 = foldl1 mplus [do d883_446 <- get
                                                xx882_447 <- StateT char
                                                case xx882_447 of
                                                    '(' -> return ()
                                                    _ -> gets position >>= (throwError . mkParseError "'('" "not match pattern: " "" d883_446 ["char"])
                                                let '(' = xx882_447
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
                                                d899_448 <- get
                                                xx898_449 <- StateT char
                                                case xx898_449 of
                                                    ')' -> return ()
                                                    _ -> gets position >>= (throwError . mkParseError "')'" "not match pattern: " "" d899_448 ["char"])
                                                let ')' = xx898_449
                                                return ()
                                                return (infixE l o r),
                                             do d901_450 <- get
                                                xx900_451 <- StateT char
                                                case xx900_451 of
                                                    '(' -> return ()
                                                    _ -> gets position >>= (throwError . mkParseError "'('" "not match pattern: " "" d901_450 ["char"])
                                                let '(' = xx900_451
                                                return ()
                                                et <- StateT hsExpTpl
                                                d905_452 <- get
                                                xx904_453 <- StateT char
                                                case xx904_453 of
                                                    ')' -> return ()
                                                    _ -> gets position >>= (throwError . mkParseError "')'" "not match pattern: " "" d905_452 ["char"])
                                                let ')' = xx904_453
                                                return ()
                                                return (tupE et),
                                             do d907_454 <- get
                                                xx906_455 <- StateT char
                                                case xx906_455 of
                                                    '[' -> return ()
                                                    _ -> gets position >>= (throwError . mkParseError "'['" "not match pattern: " "" d907_454 ["char"])
                                                let '[' = xx906_455
                                                return ()
                                                et <- StateT hsExpTpl
                                                d911_456 <- get
                                                xx910_457 <- StateT char
                                                case xx910_457 of
                                                    ']' -> return ()
                                                    _ -> gets position >>= (throwError . mkParseError "']'" "not match pattern: " "" d911_456 ["char"])
                                                let ']' = xx910_457
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
                                             do d921_458 <- get
                                                xx920_459 <- StateT char
                                                case xx920_459 of
                                                    '\'' -> return ()
                                                    _ -> gets position >>= (throwError . mkParseError "'\\''" "not match pattern: " "" d921_458 ["char"])
                                                let '\'' = xx920_459
                                                return ()
                                                c <- StateT charLit
                                                d925_460 <- get
                                                xx924_461 <- StateT char
                                                case xx924_461 of
                                                    '\'' -> return ()
                                                    _ -> gets position >>= (throwError . mkParseError "'\\''" "not match pattern: " "" d925_460 ["char"])
                                                let '\'' = xx924_461
                                                return ()
                                                return (litE (charL c)),
                                             do d927_462 <- get
                                                xx926_463 <- StateT char
                                                case xx926_463 of
                                                    '"' -> return ()
                                                    _ -> gets position >>= (throwError . mkParseError "'\"'" "not match pattern: " "" d927_462 ["char"])
                                                let '"' = xx926_463
                                                return ()
                                                s <- StateT stringLit
                                                d931_464 <- get
                                                xx930_465 <- StateT char
                                                case xx930_465 of
                                                    '"' -> return ()
                                                    _ -> gets position >>= (throwError . mkParseError "'\"'" "not match pattern: " "" d931_464 ["char"])
                                                let '"' = xx930_465
                                                return ()
                                                return (litE (stringL s)),
                                             do d933_466 <- get
                                                xx932_467 <- StateT char
                                                case xx932_467 of
                                                    '-' -> return ()
                                                    _ -> gets position >>= (throwError . mkParseError "'-'" "not match pattern: " "" d933_466 ["char"])
                                                let '-' = xx932_467
                                                return ()
                                                _ <- StateT spaces
                                                return ()
                                                e <- StateT hsExp1
                                                return (appE (varE $ mkName "negate") e)]
                hsExpTpl58_131 = foldl1 mplus [do e <- StateT hsExpLam
                                                  _ <- StateT spaces
                                                  return ()
                                                  d943_468 <- get
                                                  xx942_469 <- StateT char
                                                  let c = xx942_469
                                                  unless (isComma c) (gets position >>= (throwError . mkParseError "isComma c" "not match: " "" d943_468 ["char"]))
                                                  _ <- StateT spaces
                                                  return ()
                                                  et <- StateT hsExpTpl
                                                  return (cons e et),
                                               do e <- StateT hsExpLam
                                                  return (cons e emp),
                                               return emp]
                hsTypeArr59_132 = foldl1 mplus [do l <- StateT hsType
                                                   d953_470 <- get
                                                   xx952_471 <- StateT char
                                                   case xx952_471 of
                                                       '-' -> return ()
                                                       _ -> gets position >>= (throwError . mkParseError "'-'" "not match pattern: " "" d953_470 ["char"])
                                                   let '-' = xx952_471
                                                   return ()
                                                   d955_472 <- get
                                                   xx954_473 <- StateT char
                                                   let c = xx954_473
                                                   unless (isGt c) (gets position >>= (throwError . mkParseError "isGt c" "not match: " "" d955_472 ["char"]))
                                                   _ <- StateT spaces
                                                   return ()
                                                   r <- StateT hsTypeArr
                                                   return (appT (appT arrowT (getTyp l)) r),
                                                do t <- StateT hsType
                                                   return (getTyp t)]
                hsType60_133 = foldl1 mplus [do t <- StateT hsType1
                                                ts <- StateT hsType
                                                return (applyTyp (toTyp t) ts),
                                             do t <- StateT hsType1
                                                return (toTyp t)]
                hsType161_134 = foldl1 mplus [do d969_474 <- get
                                                 xx968_475 <- StateT char
                                                 case xx968_475 of
                                                     '[' -> return ()
                                                     _ -> gets position >>= (throwError . mkParseError "'['" "not match pattern: " "" d969_474 ["char"])
                                                 let '[' = xx968_475
                                                 return ()
                                                 d971_476 <- get
                                                 xx970_477 <- StateT char
                                                 case xx970_477 of
                                                     ']' -> return ()
                                                     _ -> gets position >>= (throwError . mkParseError "']'" "not match pattern: " "" d971_476 ["char"])
                                                 let ']' = xx970_477
                                                 return ()
                                                 _ <- StateT spaces
                                                 return ()
                                                 return listT,
                                              do d975_478 <- get
                                                 xx974_479 <- StateT char
                                                 case xx974_479 of
                                                     '[' -> return ()
                                                     _ -> gets position >>= (throwError . mkParseError "'['" "not match pattern: " "" d975_478 ["char"])
                                                 let '[' = xx974_479
                                                 return ()
                                                 t <- StateT hsTypeArr
                                                 d979_480 <- get
                                                 xx978_481 <- StateT char
                                                 case xx978_481 of
                                                     ']' -> return ()
                                                     _ -> gets position >>= (throwError . mkParseError "']'" "not match pattern: " "" d979_480 ["char"])
                                                 let ']' = xx978_481
                                                 return ()
                                                 _ <- StateT spaces
                                                 return ()
                                                 return (appT listT t),
                                              do d983_482 <- get
                                                 xx982_483 <- StateT char
                                                 case xx982_483 of
                                                     '(' -> return ()
                                                     _ -> gets position >>= (throwError . mkParseError "'('" "not match pattern: " "" d983_482 ["char"])
                                                 let '(' = xx982_483
                                                 return ()
                                                 _ <- StateT spaces
                                                 return ()
                                                 tt <- StateT hsTypeTpl
                                                 d989_484 <- get
                                                 xx988_485 <- StateT char
                                                 case xx988_485 of
                                                     ')' -> return ()
                                                     _ -> gets position >>= (throwError . mkParseError "')'" "not match pattern: " "" d989_484 ["char"])
                                                 let ')' = xx988_485
                                                 return ()
                                                 return (tupT tt),
                                              do t <- StateT typToken
                                                 return (conT (mkName t)),
                                              do d993_486 <- get
                                                 xx992_487 <- StateT char
                                                 case xx992_487 of
                                                     '(' -> return ()
                                                     _ -> gets position >>= (throwError . mkParseError "'('" "not match pattern: " "" d993_486 ["char"])
                                                 let '(' = xx992_487
                                                 return ()
                                                 d995_488 <- get
                                                 xx994_489 <- StateT char
                                                 case xx994_489 of
                                                     '-' -> return ()
                                                     _ -> gets position >>= (throwError . mkParseError "'-'" "not match pattern: " "" d995_488 ["char"])
                                                 let '-' = xx994_489
                                                 return ()
                                                 d997_490 <- get
                                                 xx996_491 <- StateT char
                                                 let c = xx996_491
                                                 unless (isGt c) (gets position >>= (throwError . mkParseError "isGt c" "not match: " "" d997_490 ["char"]))
                                                 d999_492 <- get
                                                 xx998_493 <- StateT char
                                                 case xx998_493 of
                                                     ')' -> return ()
                                                     _ -> gets position >>= (throwError . mkParseError "')'" "not match pattern: " "" d999_492 ["char"])
                                                 let ')' = xx998_493
                                                 return ()
                                                 _ <- StateT spaces
                                                 return ()
                                                 return arrowT]
                hsTypeTpl62_135 = foldl1 mplus [do t <- StateT hsTypeArr
                                                   d1005_494 <- get
                                                   xx1004_495 <- StateT char
                                                   let c = xx1004_495
                                                   unless (isComma c) (gets position >>= (throwError . mkParseError "isComma c" "not match: " "" d1005_494 ["char"]))
                                                   _ <- StateT spaces
                                                   return ()
                                                   tt <- StateT hsTypeTpl
                                                   return (cons t tt),
                                                do t <- StateT hsTypeArr
                                                   return (cons t emp),
                                                return emp]
                typ63_136 = foldl1 mplus [do u <- StateT upper
                                             t <- StateT tvtail
                                             return (cons u t)]
                variable64_137 = foldl1 mplus [do l <- StateT lower
                                                  t <- StateT tvtail
                                                  return (cons l t)]
                tvtail65_138 = foldl1 mplus [do a <- StateT alpha
                                                t <- StateT tvtail
                                                return (cons a t),
                                             return emp]
                integer66_139 = foldl1 mplus [do dh <- StateT digit
                                                 ds <- list1_316 (foldl1 mplus [do d <- StateT digit
                                                                                   return d])
                                                 return (read (cons dh ds))]
                alpha67_140 = foldl1 mplus [do u <- StateT upper
                                               return u,
                                            do l <- StateT lower
                                               return l,
                                            do d <- StateT digit
                                               return d,
                                            do d1037_496 <- get
                                               xx1036_497 <- StateT char
                                               case xx1036_497 of
                                                   '\'' -> return ()
                                                   _ -> gets position >>= (throwError . mkParseError "'\\''" "not match pattern: " "" d1037_496 ["char"])
                                               let '\'' = xx1036_497
                                               return ()
                                               return '\'']
                upper68_141 = foldl1 mplus [do d1039_498 <- get
                                               xx1038_499 <- StateT char
                                               let u = xx1038_499
                                               unless (isUpper u) (gets position >>= (throwError . mkParseError "isUpper u" "not match: " "" d1039_498 ["char"]))
                                               return u]
                lower69_142 = foldl1 mplus [do d1041_500 <- get
                                               xx1040_501 <- StateT char
                                               let l = xx1040_501
                                               unless (isLowerU l) (gets position >>= (throwError . mkParseError "isLowerU l" "not match: " "" d1041_500 ["char"]))
                                               return l]
                digit70_143 = foldl1 mplus [do d1043_502 <- get
                                               xx1042_503 <- StateT char
                                               let d = xx1042_503
                                               unless (isDigit d) (gets position >>= (throwError . mkParseError "isDigit d" "not match: " "" d1043_502 ["char"]))
                                               return d]
                spaces71_144 = foldl1 mplus [do _ <- StateT space
                                                return ()
                                                _ <- StateT spaces
                                                return ()
                                                return (),
                                             return ()]
                space72_145 = foldl1 mplus [do d1049_504 <- get
                                               xx1048_505 <- StateT char
                                               let s = xx1048_505
                                               unless (isSpace s) (gets position >>= (throwError . mkParseError "isSpace s" "not match: " "" d1049_504 ["char"]))
                                               return (),
                                            do d1051_506 <- get
                                               xx1050_507 <- StateT char
                                               case xx1050_507 of
                                                   '-' -> return ()
                                                   _ -> gets position >>= (throwError . mkParseError "'-'" "not match pattern: " "" d1051_506 ["char"])
                                               let '-' = xx1050_507
                                               return ()
                                               d1053_508 <- get
                                               xx1052_509 <- StateT char
                                               case xx1052_509 of
                                                   '-' -> return ()
                                                   _ -> gets position >>= (throwError . mkParseError "'-'" "not match pattern: " "" d1053_508 ["char"])
                                               let '-' = xx1052_509
                                               return ()
                                               _ <- StateT notNLString
                                               return ()
                                               _ <- StateT newLine
                                               return ()
                                               return (),
                                            do _ <- StateT comment
                                               return ()
                                               return ()]
                notNLString73_146 = foldl1 mplus [do ddd1060_510 <- get
                                                     do err <- ((do _ <- StateT newLine
                                                                    return ()) >> return False) `catchError` const (return True)
                                                        unless err (gets position >>= (throwError . mkParseError ('!' : "_:newLine") "not match: " "" ddd1060_510 ["newLine"]))
                                                     put ddd1060_510
                                                     c <- StateT char
                                                     s <- StateT notNLString
                                                     return (cons c s),
                                                  return emp]
                newLine74_147 = foldl1 mplus [do d1068_511 <- get
                                                 xx1067_512 <- StateT char
                                                 case xx1067_512 of
                                                     '\n' -> return ()
                                                     _ -> gets position >>= (throwError . mkParseError "'\\n'" "not match pattern: " "" d1068_511 ["char"])
                                                 let '\n' = xx1067_512
                                                 return ()
                                                 return ()]
                comment75_148 = foldl1 mplus [do d1070_513 <- get
                                                 xx1069_514 <- StateT char
                                                 case xx1069_514 of
                                                     '{' -> return ()
                                                     _ -> gets position >>= (throwError . mkParseError "'{'" "not match pattern: " "" d1070_513 ["char"])
                                                 let '{' = xx1069_514
                                                 return ()
                                                 d1072_515 <- get
                                                 xx1071_516 <- StateT char
                                                 case xx1071_516 of
                                                     '-' -> return ()
                                                     _ -> gets position >>= (throwError . mkParseError "'-'" "not match pattern: " "" d1072_515 ["char"])
                                                 let '-' = xx1071_516
                                                 return ()
                                                 ddd1073_517 <- get
                                                 do err <- ((do d1075_518 <- get
                                                                xx1074_519 <- StateT char
                                                                case xx1074_519 of
                                                                    '#' -> return ()
                                                                    _ -> gets position >>= (throwError . mkParseError "'#'" "not match pattern: " "" d1075_518 ["char"])
                                                                let '#' = xx1074_519
                                                                return ()) >> return False) `catchError` const (return True)
                                                    unless err (gets position >>= (throwError . mkParseError ('!' : "'#':") "not match: " "" ddd1073_517 ["char"]))
                                                 put ddd1073_517
                                                 _ <- StateT comments
                                                 return ()
                                                 _ <- StateT comEnd
                                                 return ()
                                                 return ()]
                comments76_149 = foldl1 mplus [do _ <- StateT notComStr
                                                  return ()
                                                  _ <- StateT comment
                                                  return ()
                                                  _ <- StateT comments
                                                  return ()
                                                  return (),
                                               do _ <- StateT notComStr
                                                  return ()
                                                  return ()]
                notComStr77_150 = foldl1 mplus [do ddd1088_520 <- get
                                                   do err <- ((do _ <- StateT comment
                                                                  return ()) >> return False) `catchError` const (return True)
                                                      unless err (gets position >>= (throwError . mkParseError ('!' : "_:comment") "not match: " "" ddd1088_520 ["comment"]))
                                                   put ddd1088_520
                                                   ddd1091_521 <- get
                                                   do err <- ((do _ <- StateT comEnd
                                                                  return ()) >> return False) `catchError` const (return True)
                                                      unless err (gets position >>= (throwError . mkParseError ('!' : "_:comEnd") "not match: " "" ddd1091_521 ["comEnd"]))
                                                   put ddd1091_521
                                                   _ <- StateT char
                                                   return ()
                                                   _ <- StateT notComStr
                                                   return ()
                                                   return (),
                                                return ()]
                comEnd78_151 = foldl1 mplus [do d1099_522 <- get
                                                xx1098_523 <- StateT char
                                                case xx1098_523 of
                                                    '-' -> return ()
                                                    _ -> gets position >>= (throwError . mkParseError "'-'" "not match pattern: " "" d1099_522 ["char"])
                                                let '-' = xx1098_523
                                                return ()
                                                d1101_524 <- get
                                                xx1100_525 <- StateT char
                                                case xx1100_525 of
                                                    '}' -> return ()
                                                    _ -> gets position >>= (throwError . mkParseError "'}'" "not match pattern: " "" d1101_524 ["char"])
                                                let '}' = xx1100_525
                                                return ()
                                                return ()]
                list1_316 :: forall m a . (MonadPlus m, Applicative m) =>
                                          m a -> m ([a])
                list12_410 :: forall m a . (MonadPlus m, Applicative m) =>
                                           m a -> m ([a])
                list1_316 p = list12_410 p `mplus` return []
                list12_410 p = ((:) <$> p) <*> list1_316 p
                optional3_319 :: forall m a . (MonadPlus m, Applicative m) =>
                                              m a -> m (Maybe a)
                optional3_319 p = (Just <$> p) `mplus` return Nothing

