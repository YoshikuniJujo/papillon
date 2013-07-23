{-# LANGUAGE FlexibleContexts, TemplateHaskell, UndecidableInstances, PackageImports, TypeFamilies, RankNTypes #-}
module Text.Papillon.Parser (
	Peg,
	Definition(..),
	Selection(..),
	ExpressionHs(..),
	NameLeaf(..),
	NameLeaf_(..),
	ReadFrom(..),

	getSelectionType,
	showNameLeaf,
	nameFromRF,

	parse,
	Source(..),
	SourceList(..),
	Derivs(pegFile, peg, char),
	ParseError(..),
	mkParseError,
	pePositionS,
	Pos(..),
	ListPos(..),

	PPragma(..),
	ModuleName,
	ExportList,
	Code
) where

import Text.Papillon.Papillon
import Control.Applicative
import "monads-tf" Control.Monad.State
import "monads-tf" Control.Monad.Error



import Text.Papillon.SyntaxTree
import Language.Haskell.TH (
	Name, TypeQ, PatQ, ExpQ, mkName,
	conT, listT, appT, arrowT, wildP, litP, conP, tupP, listP, uInfixP,
	litE, varE, conE, tupE, listE, sigE, appE, infixE, uInfixE, lamE,
	integerL, charL, stringL)
import Data.Char (isSpace, isDigit, isUpper, isLower)

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
                                         (([ExpressionHs], Derivs))),
              plainSelection :: (Either (ParseError (Pos String) Derivs)
                                        (([ExpressionHs], Derivs))),
              expressionHs :: (Either (ParseError (Pos String) Derivs)
                                      ((ExpressionHs, Derivs))),
              expressionHsSugar :: (Either (ParseError (Pos String) Derivs)
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
                             where d = Derivs pegFile80_1 pragmas81_2 pragma82_3 pragmaStr83_4 pragmaItems84_5 pragmaEnd85_6 moduleDec86_7 moduleName87_8 moduleDecStr88_9 whr89_10 preImpPap90_11 prePeg91_12 afterPeg92_13 importPapillon93_14 varToken94_15 typToken95_16 pap96_17 peg97_18 sourceType98_19 peg_99_20 definition100_21 selection101_22 normalSelection102_23 plainSelection103_24 expressionHs104_25 expressionHsSugar105_26 plainExpressionHs106_27 plainReadFromLs107_28 expression108_29 nameLeaf_109_30 nameLeaf110_31 nameLeafNoCom111_32 comForErr112_33 leaf113_34 patOp114_35 pat115_36 pat1116_37 patList117_38 opConName118_39 charLit119_40 stringLit120_41 escapeC121_42 pats122_43 readFromLs123_44 readFrom124_45 selectCharsLs125_46 selectChars126_47 test127_48 hsExpLam128_49 hsExpTyp129_50 hsExpOp130_51 hsOp131_52 opTail132_53 hsExp133_54 hsExp1134_55 hsExpTpl135_56 hsTypeArr136_57 hsType137_58 hsType1138_59 hsTypeTpl139_60 typ140_61 variable141_62 tvtail142_63 integer143_64 alpha144_65 upper145_66 lower146_67 digit147_68 spaces148_69 space149_70 notNLString150_71 newLine151_72 comment152_73 comments153_74 notComStr154_75 comEnd155_76 chars156_77 pos
                                   pegFile80_1 = runStateT pegFile4_78 d
                                   pragmas81_2 = runStateT pragmas5_79 d
                                   pragma82_3 = runStateT pragma6_80 d
                                   pragmaStr83_4 = runStateT pragmaStr7_81 d
                                   pragmaItems84_5 = runStateT pragmaItems8_82 d
                                   pragmaEnd85_6 = runStateT pragmaEnd9_83 d
                                   moduleDec86_7 = runStateT moduleDec10_84 d
                                   moduleName87_8 = runStateT moduleName11_85 d
                                   moduleDecStr88_9 = runStateT moduleDecStr12_86 d
                                   whr89_10 = runStateT whr13_87 d
                                   preImpPap90_11 = runStateT preImpPap14_88 d
                                   prePeg91_12 = runStateT prePeg15_89 d
                                   afterPeg92_13 = runStateT afterPeg16_90 d
                                   importPapillon93_14 = runStateT importPapillon17_91 d
                                   varToken94_15 = runStateT varToken18_92 d
                                   typToken95_16 = runStateT typToken19_93 d
                                   pap96_17 = runStateT pap20_94 d
                                   peg97_18 = runStateT peg21_95 d
                                   sourceType98_19 = runStateT sourceType22_96 d
                                   peg_99_20 = runStateT peg_23_97 d
                                   definition100_21 = runStateT definition24_98 d
                                   selection101_22 = runStateT selection25_99 d
                                   normalSelection102_23 = runStateT normalSelection26_100 d
                                   plainSelection103_24 = runStateT plainSelection27_101 d
                                   expressionHs104_25 = runStateT expressionHs28_102 d
                                   expressionHsSugar105_26 = runStateT expressionHsSugar29_103 d
                                   plainExpressionHs106_27 = runStateT plainExpressionHs30_104 d
                                   plainReadFromLs107_28 = runStateT plainReadFromLs31_105 d
                                   expression108_29 = runStateT expression32_106 d
                                   nameLeaf_109_30 = runStateT nameLeaf_33_107 d
                                   nameLeaf110_31 = runStateT nameLeaf34_108 d
                                   nameLeafNoCom111_32 = runStateT nameLeafNoCom35_109 d
                                   comForErr112_33 = runStateT comForErr36_110 d
                                   leaf113_34 = runStateT leaf37_111 d
                                   patOp114_35 = runStateT patOp38_112 d
                                   pat115_36 = runStateT pat39_113 d
                                   pat1116_37 = runStateT pat140_114 d
                                   patList117_38 = runStateT patList41_115 d
                                   opConName118_39 = runStateT opConName42_116 d
                                   charLit119_40 = runStateT charLit43_117 d
                                   stringLit120_41 = runStateT stringLit44_118 d
                                   escapeC121_42 = runStateT escapeC45_119 d
                                   pats122_43 = runStateT pats46_120 d
                                   readFromLs123_44 = runStateT readFromLs47_121 d
                                   readFrom124_45 = runStateT readFrom48_122 d
                                   selectCharsLs125_46 = runStateT selectCharsLs49_123 d
                                   selectChars126_47 = runStateT selectChars50_124 d
                                   test127_48 = runStateT test51_125 d
                                   hsExpLam128_49 = runStateT hsExpLam52_126 d
                                   hsExpTyp129_50 = runStateT hsExpTyp53_127 d
                                   hsExpOp130_51 = runStateT hsExpOp54_128 d
                                   hsOp131_52 = runStateT hsOp55_129 d
                                   opTail132_53 = runStateT opTail56_130 d
                                   hsExp133_54 = runStateT hsExp57_131 d
                                   hsExp1134_55 = runStateT hsExp158_132 d
                                   hsExpTpl135_56 = runStateT hsExpTpl59_133 d
                                   hsTypeArr136_57 = runStateT hsTypeArr60_134 d
                                   hsType137_58 = runStateT hsType61_135 d
                                   hsType1138_59 = runStateT hsType162_136 d
                                   hsTypeTpl139_60 = runStateT hsTypeTpl63_137 d
                                   typ140_61 = runStateT typ64_138 d
                                   variable141_62 = runStateT variable65_139 d
                                   tvtail142_63 = runStateT tvtail66_140 d
                                   integer143_64 = runStateT integer67_141 d
                                   alpha144_65 = runStateT alpha68_142 d
                                   upper145_66 = runStateT upper69_143 d
                                   lower146_67 = runStateT lower70_144 d
                                   digit147_68 = runStateT digit71_145 d
                                   spaces148_69 = runStateT spaces72_146 d
                                   space149_70 = runStateT space73_147 d
                                   notNLString150_71 = runStateT notNLString74_148 d
                                   newLine151_72 = runStateT newLine75_149 d
                                   comment152_73 = runStateT comment76_150 d
                                   comments153_74 = runStateT comments77_151 d
                                   notComStr154_75 = runStateT notComStr78_152 d
                                   comEnd155_76 = runStateT comEnd79_153 d
                                   chars156_77 = runStateT (case getToken s of
                                                                Just (c,
                                                                      s') -> do put (parse0_0 (updatePos c pos) s')
                                                                                return c
                                                                _ -> gets position >>= (throwError . mkParseError "" "end of input" "" undefined [])) d
                pegFile4_78 = foldl1 mplus [do pr <- StateT pragmas
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
                                               d174_154 <- get
                                               xx173_155 <- StateT char
                                               case xx173_155 of
                                                   '|' -> return ()
                                                   _ -> gets position >>= (throwError . mkParseError "'|'" "not match pattern: " "" d174_154 ["char"])
                                               let '|' = xx173_155
                                               return ()
                                               d176_156 <- get
                                               xx175_157 <- StateT char
                                               case xx175_157 of
                                                   ']' -> return ()
                                                   _ -> gets position >>= (throwError . mkParseError "']'" "not match pattern: " "" d176_156 ["char"])
                                               let ']' = xx175_157
                                               return ()
                                               d178_158 <- get
                                               xx177_159 <- StateT char
                                               case xx177_159 of
                                                   '\n' -> return ()
                                                   _ -> gets position >>= (throwError . mkParseError "'\\n'" "not match pattern: " "" d178_158 ["char"])
                                               let '\n' = xx177_159
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
                                               d194_160 <- get
                                               xx193_161 <- StateT char
                                               case xx193_161 of
                                                   '|' -> return ()
                                                   _ -> gets position >>= (throwError . mkParseError "'|'" "not match pattern: " "" d194_160 ["char"])
                                               let '|' = xx193_161
                                               return ()
                                               d196_162 <- get
                                               xx195_163 <- StateT char
                                               case xx195_163 of
                                                   ']' -> return ()
                                                   _ -> gets position >>= (throwError . mkParseError "']'" "not match pattern: " "" d196_162 ["char"])
                                               let ']' = xx195_163
                                               return ()
                                               d198_164 <- get
                                               xx197_165 <- StateT char
                                               case xx197_165 of
                                                   '\n' -> return ()
                                                   _ -> gets position >>= (throwError . mkParseError "'\\n'" "not match pattern: " "" d198_164 ["char"])
                                               let '\n' = xx197_165
                                               return ()
                                               atp <- StateT afterPeg
                                               return (mkPegFile pr md [] pp p atp)]
                pragmas5_79 = foldl1 mplus [do _ <- StateT spaces
                                               return ()
                                               pr <- StateT pragma
                                               prs <- StateT pragmas
                                               return (pr : prs),
                                            do _ <- StateT spaces
                                               return ()
                                               return []]
                pragma6_80 = foldl1 mplus [do d210_166 <- get
                                              xx209_167 <- StateT char
                                              case xx209_167 of
                                                  '{' -> return ()
                                                  _ -> gets position >>= (throwError . mkParseError "'{'" "not match pattern: " "" d210_166 ["char"])
                                              let '{' = xx209_167
                                              return ()
                                              d212_168 <- get
                                              xx211_169 <- StateT char
                                              case xx211_169 of
                                                  '-' -> return ()
                                                  _ -> gets position >>= (throwError . mkParseError "'-'" "not match pattern: " "" d212_168 ["char"])
                                              let '-' = xx211_169
                                              return ()
                                              d214_170 <- get
                                              xx213_171 <- StateT char
                                              case xx213_171 of
                                                  '#' -> return ()
                                                  _ -> gets position >>= (throwError . mkParseError "'#'" "not match pattern: " "" d214_170 ["char"])
                                              let '#' = xx213_171
                                              return ()
                                              _ <- StateT spaces
                                              return ()
                                              d218_172 <- get
                                              xx217_173 <- StateT char
                                              case xx217_173 of
                                                  'L' -> return ()
                                                  _ -> gets position >>= (throwError . mkParseError "'L'" "not match pattern: " "" d218_172 ["char"])
                                              let 'L' = xx217_173
                                              return ()
                                              d220_174 <- get
                                              xx219_175 <- StateT char
                                              case xx219_175 of
                                                  'A' -> return ()
                                                  _ -> gets position >>= (throwError . mkParseError "'A'" "not match pattern: " "" d220_174 ["char"])
                                              let 'A' = xx219_175
                                              return ()
                                              d222_176 <- get
                                              xx221_177 <- StateT char
                                              case xx221_177 of
                                                  'N' -> return ()
                                                  _ -> gets position >>= (throwError . mkParseError "'N'" "not match pattern: " "" d222_176 ["char"])
                                              let 'N' = xx221_177
                                              return ()
                                              d224_178 <- get
                                              xx223_179 <- StateT char
                                              case xx223_179 of
                                                  'G' -> return ()
                                                  _ -> gets position >>= (throwError . mkParseError "'G'" "not match pattern: " "" d224_178 ["char"])
                                              let 'G' = xx223_179
                                              return ()
                                              d226_180 <- get
                                              xx225_181 <- StateT char
                                              case xx225_181 of
                                                  'U' -> return ()
                                                  _ -> gets position >>= (throwError . mkParseError "'U'" "not match pattern: " "" d226_180 ["char"])
                                              let 'U' = xx225_181
                                              return ()
                                              d228_182 <- get
                                              xx227_183 <- StateT char
                                              case xx227_183 of
                                                  'A' -> return ()
                                                  _ -> gets position >>= (throwError . mkParseError "'A'" "not match pattern: " "" d228_182 ["char"])
                                              let 'A' = xx227_183
                                              return ()
                                              d230_184 <- get
                                              xx229_185 <- StateT char
                                              case xx229_185 of
                                                  'G' -> return ()
                                                  _ -> gets position >>= (throwError . mkParseError "'G'" "not match pattern: " "" d230_184 ["char"])
                                              let 'G' = xx229_185
                                              return ()
                                              d232_186 <- get
                                              xx231_187 <- StateT char
                                              case xx231_187 of
                                                  'E' -> return ()
                                                  _ -> gets position >>= (throwError . mkParseError "'E'" "not match pattern: " "" d232_186 ["char"])
                                              let 'E' = xx231_187
                                              return ()
                                              _ <- StateT spaces
                                              return ()
                                              s <- StateT pragmaItems
                                              _ <- StateT pragmaEnd
                                              return ()
                                              _ <- StateT spaces
                                              return ()
                                              return (LanguagePragma s),
                                           do d242_188 <- get
                                              xx241_189 <- StateT char
                                              case xx241_189 of
                                                  '{' -> return ()
                                                  _ -> gets position >>= (throwError . mkParseError "'{'" "not match pattern: " "" d242_188 ["char"])
                                              let '{' = xx241_189
                                              return ()
                                              d244_190 <- get
                                              xx243_191 <- StateT char
                                              case xx243_191 of
                                                  '-' -> return ()
                                                  _ -> gets position >>= (throwError . mkParseError "'-'" "not match pattern: " "" d244_190 ["char"])
                                              let '-' = xx243_191
                                              return ()
                                              d246_192 <- get
                                              xx245_193 <- StateT char
                                              case xx245_193 of
                                                  '#' -> return ()
                                                  _ -> gets position >>= (throwError . mkParseError "'#'" "not match pattern: " "" d246_192 ["char"])
                                              let '#' = xx245_193
                                              return ()
                                              _ <- StateT spaces
                                              return ()
                                              s <- StateT pragmaStr
                                              _ <- StateT pragmaEnd
                                              return ()
                                              return (OtherPragma s)]
                pragmaStr7_81 = foldl1 mplus [do ddd253_194 <- get
                                                 do err <- ((do _ <- StateT pragmaEnd
                                                                return ()) >> return False) `catchError` const (return True)
                                                    unless err (gets position >>= (throwError . mkParseError ('!' : "_:pragmaEnd") "not match: " "" ddd253_194 ["pragmaEnd"]))
                                                 put ddd253_194
                                                 c <- StateT char
                                                 s <- StateT pragmaStr
                                                 return (c : s),
                                              return ""]
                pragmaItems8_82 = foldl1 mplus [do t <- StateT typToken
                                                   d263_195 <- get
                                                   xx262_196 <- StateT char
                                                   case xx262_196 of
                                                       ',' -> return ()
                                                       _ -> gets position >>= (throwError . mkParseError "','" "not match pattern: " "" d263_195 ["char"])
                                                   let ',' = xx262_196
                                                   return ()
                                                   _ <- StateT spaces
                                                   return ()
                                                   i <- StateT pragmaItems
                                                   return (t : i),
                                                do t <- StateT typToken
                                                   return [t]]
                pragmaEnd9_83 = foldl1 mplus [do _ <- StateT spaces
                                                 return ()
                                                 d273_197 <- get
                                                 xx272_198 <- StateT char
                                                 case xx272_198 of
                                                     '#' -> return ()
                                                     _ -> gets position >>= (throwError . mkParseError "'#'" "not match pattern: " "" d273_197 ["char"])
                                                 let '#' = xx272_198
                                                 return ()
                                                 d275_199 <- get
                                                 xx274_200 <- StateT char
                                                 case xx274_200 of
                                                     '-' -> return ()
                                                     _ -> gets position >>= (throwError . mkParseError "'-'" "not match pattern: " "" d275_199 ["char"])
                                                 let '-' = xx274_200
                                                 return ()
                                                 d277_201 <- get
                                                 xx276_202 <- StateT char
                                                 case xx276_202 of
                                                     '}' -> return ()
                                                     _ -> gets position >>= (throwError . mkParseError "'}'" "not match pattern: " "" d277_201 ["char"])
                                                 let '}' = xx276_202
                                                 return ()
                                                 return ()]
                moduleDec10_84 = foldl1 mplus [do d279_203 <- get
                                                  xx278_204 <- StateT char
                                                  case xx278_204 of
                                                      'm' -> return ()
                                                      _ -> gets position >>= (throwError . mkParseError "'m'" "not match pattern: " "" d279_203 ["char"])
                                                  let 'm' = xx278_204
                                                  return ()
                                                  d281_205 <- get
                                                  xx280_206 <- StateT char
                                                  case xx280_206 of
                                                      'o' -> return ()
                                                      _ -> gets position >>= (throwError . mkParseError "'o'" "not match pattern: " "" d281_205 ["char"])
                                                  let 'o' = xx280_206
                                                  return ()
                                                  d283_207 <- get
                                                  xx282_208 <- StateT char
                                                  case xx282_208 of
                                                      'd' -> return ()
                                                      _ -> gets position >>= (throwError . mkParseError "'d'" "not match pattern: " "" d283_207 ["char"])
                                                  let 'd' = xx282_208
                                                  return ()
                                                  d285_209 <- get
                                                  xx284_210 <- StateT char
                                                  case xx284_210 of
                                                      'u' -> return ()
                                                      _ -> gets position >>= (throwError . mkParseError "'u'" "not match pattern: " "" d285_209 ["char"])
                                                  let 'u' = xx284_210
                                                  return ()
                                                  d287_211 <- get
                                                  xx286_212 <- StateT char
                                                  case xx286_212 of
                                                      'l' -> return ()
                                                      _ -> gets position >>= (throwError . mkParseError "'l'" "not match pattern: " "" d287_211 ["char"])
                                                  let 'l' = xx286_212
                                                  return ()
                                                  d289_213 <- get
                                                  xx288_214 <- StateT char
                                                  case xx288_214 of
                                                      'e' -> return ()
                                                      _ -> gets position >>= (throwError . mkParseError "'e'" "not match pattern: " "" d289_213 ["char"])
                                                  let 'e' = xx288_214
                                                  return ()
                                                  _ <- StateT spaces
                                                  return ()
                                                  n <- StateT moduleName
                                                  _ <- StateT spaces
                                                  return ()
                                                  d297_215 <- get
                                                  xx296_216 <- StateT char
                                                  case xx296_216 of
                                                      '(' -> return ()
                                                      _ -> gets position >>= (throwError . mkParseError "'('" "not match pattern: " "" d297_215 ["char"])
                                                  let '(' = xx296_216
                                                  return ()
                                                  _ <- StateT spaces
                                                  return ()
                                                  s <- StateT moduleDecStr
                                                  _ <- StateT whr
                                                  return ()
                                                  return (Just (n, Just s)),
                                               do d305_217 <- get
                                                  xx304_218 <- StateT char
                                                  case xx304_218 of
                                                      'm' -> return ()
                                                      _ -> gets position >>= (throwError . mkParseError "'m'" "not match pattern: " "" d305_217 ["char"])
                                                  let 'm' = xx304_218
                                                  return ()
                                                  d307_219 <- get
                                                  xx306_220 <- StateT char
                                                  case xx306_220 of
                                                      'o' -> return ()
                                                      _ -> gets position >>= (throwError . mkParseError "'o'" "not match pattern: " "" d307_219 ["char"])
                                                  let 'o' = xx306_220
                                                  return ()
                                                  d309_221 <- get
                                                  xx308_222 <- StateT char
                                                  case xx308_222 of
                                                      'd' -> return ()
                                                      _ -> gets position >>= (throwError . mkParseError "'d'" "not match pattern: " "" d309_221 ["char"])
                                                  let 'd' = xx308_222
                                                  return ()
                                                  d311_223 <- get
                                                  xx310_224 <- StateT char
                                                  case xx310_224 of
                                                      'u' -> return ()
                                                      _ -> gets position >>= (throwError . mkParseError "'u'" "not match pattern: " "" d311_223 ["char"])
                                                  let 'u' = xx310_224
                                                  return ()
                                                  d313_225 <- get
                                                  xx312_226 <- StateT char
                                                  case xx312_226 of
                                                      'l' -> return ()
                                                      _ -> gets position >>= (throwError . mkParseError "'l'" "not match pattern: " "" d313_225 ["char"])
                                                  let 'l' = xx312_226
                                                  return ()
                                                  d315_227 <- get
                                                  xx314_228 <- StateT char
                                                  case xx314_228 of
                                                      'e' -> return ()
                                                      _ -> gets position >>= (throwError . mkParseError "'e'" "not match pattern: " "" d315_227 ["char"])
                                                  let 'e' = xx314_228
                                                  return ()
                                                  _ <- StateT spaces
                                                  return ()
                                                  n <- StateT moduleName
                                                  _ <- StateT spaces
                                                  return ()
                                                  d323_229 <- get
                                                  xx322_230 <- StateT char
                                                  case xx322_230 of
                                                      'w' -> return ()
                                                      _ -> gets position >>= (throwError . mkParseError "'w'" "not match pattern: " "" d323_229 ["char"])
                                                  let 'w' = xx322_230
                                                  return ()
                                                  d325_231 <- get
                                                  xx324_232 <- StateT char
                                                  case xx324_232 of
                                                      'h' -> return ()
                                                      _ -> gets position >>= (throwError . mkParseError "'h'" "not match pattern: " "" d325_231 ["char"])
                                                  let 'h' = xx324_232
                                                  return ()
                                                  d327_233 <- get
                                                  xx326_234 <- StateT char
                                                  case xx326_234 of
                                                      'e' -> return ()
                                                      _ -> gets position >>= (throwError . mkParseError "'e'" "not match pattern: " "" d327_233 ["char"])
                                                  let 'e' = xx326_234
                                                  return ()
                                                  d329_235 <- get
                                                  xx328_236 <- StateT char
                                                  case xx328_236 of
                                                      'r' -> return ()
                                                      _ -> gets position >>= (throwError . mkParseError "'r'" "not match pattern: " "" d329_235 ["char"])
                                                  let 'r' = xx328_236
                                                  return ()
                                                  d331_237 <- get
                                                  xx330_238 <- StateT char
                                                  case xx330_238 of
                                                      'e' -> return ()
                                                      _ -> gets position >>= (throwError . mkParseError "'e'" "not match pattern: " "" d331_237 ["char"])
                                                  let 'e' = xx330_238
                                                  return ()
                                                  _ <- StateT spaces
                                                  return ()
                                                  return (Just (n, Nothing)),
                                               return Nothing]
                moduleName11_85 = foldl1 mplus [do t <- StateT typ
                                                   d337_239 <- get
                                                   xx336_240 <- StateT char
                                                   case xx336_240 of
                                                       '.' -> return ()
                                                       _ -> gets position >>= (throwError . mkParseError "'.'" "not match pattern: " "" d337_239 ["char"])
                                                   let '.' = xx336_240
                                                   return ()
                                                   n <- StateT moduleName
                                                   return (t : n),
                                                do t <- StateT typ
                                                   return [t]]
                moduleDecStr12_86 = foldl1 mplus [do ddd342_241 <- get
                                                     do err <- ((do _ <- StateT whr
                                                                    return ()) >> return False) `catchError` const (return True)
                                                        unless err (gets position >>= (throwError . mkParseError ('!' : "_:whr") "not match: " "" ddd342_241 ["whr"]))
                                                     put ddd342_241
                                                     c <- StateT char
                                                     s <- StateT moduleDecStr
                                                     return (c : s),
                                                  return ""]
                whr13_87 = foldl1 mplus [do _ <- StateT spaces
                                            return ()
                                            d352_242 <- get
                                            xx351_243 <- StateT char
                                            case xx351_243 of
                                                ')' -> return ()
                                                _ -> gets position >>= (throwError . mkParseError "')'" "not match pattern: " "" d352_242 ["char"])
                                            let ')' = xx351_243
                                            return ()
                                            _ <- StateT spaces
                                            return ()
                                            d356_244 <- get
                                            xx355_245 <- StateT char
                                            case xx355_245 of
                                                'w' -> return ()
                                                _ -> gets position >>= (throwError . mkParseError "'w'" "not match pattern: " "" d356_244 ["char"])
                                            let 'w' = xx355_245
                                            return ()
                                            d358_246 <- get
                                            xx357_247 <- StateT char
                                            case xx357_247 of
                                                'h' -> return ()
                                                _ -> gets position >>= (throwError . mkParseError "'h'" "not match pattern: " "" d358_246 ["char"])
                                            let 'h' = xx357_247
                                            return ()
                                            d360_248 <- get
                                            xx359_249 <- StateT char
                                            case xx359_249 of
                                                'e' -> return ()
                                                _ -> gets position >>= (throwError . mkParseError "'e'" "not match pattern: " "" d360_248 ["char"])
                                            let 'e' = xx359_249
                                            return ()
                                            d362_250 <- get
                                            xx361_251 <- StateT char
                                            case xx361_251 of
                                                'r' -> return ()
                                                _ -> gets position >>= (throwError . mkParseError "'r'" "not match pattern: " "" d362_250 ["char"])
                                            let 'r' = xx361_251
                                            return ()
                                            d364_252 <- get
                                            xx363_253 <- StateT char
                                            case xx363_253 of
                                                'e' -> return ()
                                                _ -> gets position >>= (throwError . mkParseError "'e'" "not match pattern: " "" d364_252 ["char"])
                                            let 'e' = xx363_253
                                            return ()
                                            return ()]
                preImpPap14_88 = foldl1 mplus [do ddd365_254 <- get
                                                  do err <- ((do _ <- StateT importPapillon
                                                                 return ()) >> return False) `catchError` const (return True)
                                                     unless err (gets position >>= (throwError . mkParseError ('!' : "_:importPapillon") "not match: " "" ddd365_254 ["importPapillon"]))
                                                  put ddd365_254
                                                  ddd368_255 <- get
                                                  do err <- ((do _ <- StateT pap
                                                                 return ()) >> return False) `catchError` const (return True)
                                                     unless err (gets position >>= (throwError . mkParseError ('!' : "_:pap") "not match: " "" ddd368_255 ["pap"]))
                                                  put ddd368_255
                                                  c <- StateT char
                                                  pip <- StateT preImpPap
                                                  return (c : pip),
                                               return ""]
                prePeg15_89 = foldl1 mplus [do ddd375_256 <- get
                                               do err <- ((do _ <- StateT pap
                                                              return ()) >> return False) `catchError` const (return True)
                                                  unless err (gets position >>= (throwError . mkParseError ('!' : "_:pap") "not match: " "" ddd375_256 ["pap"]))
                                               put ddd375_256
                                               c <- StateT char
                                               pp <- StateT prePeg
                                               return (c : pp),
                                            return ""]
                afterPeg16_90 = foldl1 mplus [do c <- StateT char
                                                 atp <- StateT afterPeg
                                                 return (c : atp),
                                              return ""]
                importPapillon17_91 = foldl1 mplus [do d387_257 <- get
                                                       xx386_258 <- StateT varToken
                                                       case xx386_258 of
                                                           "import" -> return ()
                                                           _ -> gets position >>= (throwError . mkParseError "\"import\"" "not match pattern: " "" d387_257 ["varToken"])
                                                       let "import" = xx386_258
                                                       return ()
                                                       d389_259 <- get
                                                       xx388_260 <- StateT typToken
                                                       case xx388_260 of
                                                           "Text" -> return ()
                                                           _ -> gets position >>= (throwError . mkParseError "\"Text\"" "not match pattern: " "" d389_259 ["typToken"])
                                                       let "Text" = xx388_260
                                                       return ()
                                                       d391_261 <- get
                                                       xx390_262 <- StateT char
                                                       case xx390_262 of
                                                           '.' -> return ()
                                                           _ -> gets position >>= (throwError . mkParseError "'.'" "not match pattern: " "" d391_261 ["char"])
                                                       let '.' = xx390_262
                                                       return ()
                                                       _ <- StateT spaces
                                                       return ()
                                                       d395_263 <- get
                                                       xx394_264 <- StateT typToken
                                                       case xx394_264 of
                                                           "Papillon" -> return ()
                                                           _ -> gets position >>= (throwError . mkParseError "\"Papillon\"" "not match pattern: " "" d395_263 ["typToken"])
                                                       let "Papillon" = xx394_264
                                                       return ()
                                                       ddd396_265 <- get
                                                       do err <- ((do d398_266 <- get
                                                                      xx397_267 <- StateT char
                                                                      case xx397_267 of
                                                                          '.' -> return ()
                                                                          _ -> gets position >>= (throwError . mkParseError "'.'" "not match pattern: " "" d398_266 ["char"])
                                                                      let '.' = xx397_267
                                                                      return ()) >> return False) `catchError` const (return True)
                                                          unless err (gets position >>= (throwError . mkParseError ('!' : "'.':") "not match: " "" ddd396_265 ["char"]))
                                                       put ddd396_265
                                                       return ()]
                varToken18_92 = foldl1 mplus [do v <- StateT variable
                                                 _ <- StateT spaces
                                                 return ()
                                                 return v]
                typToken19_93 = foldl1 mplus [do t <- StateT typ
                                                 _ <- StateT spaces
                                                 return ()
                                                 return t]
                pap20_94 = foldl1 mplus [do d408_268 <- get
                                            xx407_269 <- StateT char
                                            case xx407_269 of
                                                '\n' -> return ()
                                                _ -> gets position >>= (throwError . mkParseError "'\\n'" "not match pattern: " "" d408_268 ["char"])
                                            let '\n' = xx407_269
                                            return ()
                                            d410_270 <- get
                                            xx409_271 <- StateT char
                                            case xx409_271 of
                                                '[' -> return ()
                                                _ -> gets position >>= (throwError . mkParseError "'['" "not match pattern: " "" d410_270 ["char"])
                                            let '[' = xx409_271
                                            return ()
                                            d412_272 <- get
                                            xx411_273 <- StateT char
                                            case xx411_273 of
                                                'p' -> return ()
                                                _ -> gets position >>= (throwError . mkParseError "'p'" "not match pattern: " "" d412_272 ["char"])
                                            let 'p' = xx411_273
                                            return ()
                                            d414_274 <- get
                                            xx413_275 <- StateT char
                                            case xx413_275 of
                                                'a' -> return ()
                                                _ -> gets position >>= (throwError . mkParseError "'a'" "not match pattern: " "" d414_274 ["char"])
                                            let 'a' = xx413_275
                                            return ()
                                            d416_276 <- get
                                            xx415_277 <- StateT char
                                            case xx415_277 of
                                                'p' -> return ()
                                                _ -> gets position >>= (throwError . mkParseError "'p'" "not match pattern: " "" d416_276 ["char"])
                                            let 'p' = xx415_277
                                            return ()
                                            d418_278 <- get
                                            xx417_279 <- StateT char
                                            case xx417_279 of
                                                'i' -> return ()
                                                _ -> gets position >>= (throwError . mkParseError "'i'" "not match pattern: " "" d418_278 ["char"])
                                            let 'i' = xx417_279
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
                                                'l' -> return ()
                                                _ -> gets position >>= (throwError . mkParseError "'l'" "not match pattern: " "" d422_282 ["char"])
                                            let 'l' = xx421_283
                                            return ()
                                            d424_284 <- get
                                            xx423_285 <- StateT char
                                            case xx423_285 of
                                                'o' -> return ()
                                                _ -> gets position >>= (throwError . mkParseError "'o'" "not match pattern: " "" d424_284 ["char"])
                                            let 'o' = xx423_285
                                            return ()
                                            d426_286 <- get
                                            xx425_287 <- StateT char
                                            case xx425_287 of
                                                'n' -> return ()
                                                _ -> gets position >>= (throwError . mkParseError "'n'" "not match pattern: " "" d426_286 ["char"])
                                            let 'n' = xx425_287
                                            return ()
                                            d428_288 <- get
                                            xx427_289 <- StateT char
                                            case xx427_289 of
                                                '|' -> return ()
                                                _ -> gets position >>= (throwError . mkParseError "'|'" "not match pattern: " "" d428_288 ["char"])
                                            let '|' = xx427_289
                                            return ()
                                            d430_290 <- get
                                            xx429_291 <- StateT char
                                            case xx429_291 of
                                                '\n' -> return ()
                                                _ -> gets position >>= (throwError . mkParseError "'\\n'" "not match pattern: " "" d430_290 ["char"])
                                            let '\n' = xx429_291
                                            return ()
                                            return ()]
                peg21_95 = foldl1 mplus [do _ <- StateT spaces
                                            return ()
                                            s <- StateT sourceType
                                            p <- StateT peg_
                                            return (mkTTPeg s p),
                                         do p <- StateT peg_
                                            return (mkTTPeg tString p)]
                sourceType22_96 = foldl1 mplus [do d440_292 <- get
                                                   xx439_293 <- StateT varToken
                                                   case xx439_293 of
                                                       "source" -> return ()
                                                       _ -> gets position >>= (throwError . mkParseError "\"source\"" "not match pattern: " "" d440_292 ["varToken"])
                                                   let "source" = xx439_293
                                                   return ()
                                                   d442_294 <- get
                                                   xx441_295 <- StateT char
                                                   case xx441_295 of
                                                       ':' -> return ()
                                                       _ -> gets position >>= (throwError . mkParseError "':'" "not match pattern: " "" d442_294 ["char"])
                                                   let ':' = xx441_295
                                                   return ()
                                                   _ <- StateT spaces
                                                   return ()
                                                   v <- StateT typToken
                                                   return v]
                peg_23_97 = foldl1 mplus [do _ <- StateT spaces
                                             return ()
                                             d <- StateT definition
                                             p <- StateT peg_
                                             return (d : p),
                                          return []]
                definition24_98 = foldl1 mplus [do v <- StateT variable
                                                   _ <- StateT spaces
                                                   return ()
                                                   d458_296 <- get
                                                   xx457_297 <- StateT char
                                                   case xx457_297 of
                                                       ':' -> return ()
                                                       _ -> gets position >>= (throwError . mkParseError "':'" "not match pattern: " "" d458_296 ["char"])
                                                   let ':' = xx457_297
                                                   return ()
                                                   d460_298 <- get
                                                   xx459_299 <- StateT char
                                                   case xx459_299 of
                                                       ':' -> return ()
                                                       _ -> gets position >>= (throwError . mkParseError "':'" "not match pattern: " "" d460_298 ["char"])
                                                   let ':' = xx459_299
                                                   return ()
                                                   _ <- StateT spaces
                                                   return ()
                                                   t <- StateT hsTypeArr
                                                   _ <- StateT spaces
                                                   return ()
                                                   d468_300 <- get
                                                   xx467_301 <- StateT char
                                                   case xx467_301 of
                                                       '=' -> return ()
                                                       _ -> gets position >>= (throwError . mkParseError "'='" "not match pattern: " "" d468_300 ["char"])
                                                   let '=' = xx467_301
                                                   return ()
                                                   _ <- StateT spaces
                                                   return ()
                                                   sel <- StateT selection
                                                   _ <- StateT spaces
                                                   return ()
                                                   d476_302 <- get
                                                   xx475_303 <- StateT char
                                                   case xx475_303 of
                                                       ';' -> return ()
                                                       _ -> gets position >>= (throwError . mkParseError "';'" "not match pattern: " "" d476_302 ["char"])
                                                   let ';' = xx475_303
                                                   return ()
                                                   return (Definition v t sel),
                                                do v <- StateT variable
                                                   _ <- StateT spaces
                                                   return ()
                                                   d482_304 <- get
                                                   xx481_305 <- StateT char
                                                   case xx481_305 of
                                                       '<' -> return ()
                                                       _ -> gets position >>= (throwError . mkParseError "'<'" "not match pattern: " "" d482_304 ["char"])
                                                   let '<' = xx481_305
                                                   return ()
                                                   d484_306 <- get
                                                   xx483_307 <- StateT char
                                                   case xx483_307 of
                                                       '-' -> return ()
                                                       _ -> gets position >>= (throwError . mkParseError "'-'" "not match pattern: " "" d484_306 ["char"])
                                                   let '-' = xx483_307
                                                   return ()
                                                   _ <- StateT spaces
                                                   return ()
                                                   sel <- StateT plainSelection
                                                   _ <- StateT spaces
                                                   return ()
                                                   d492_308 <- get
                                                   xx491_309 <- StateT char
                                                   case xx491_309 of
                                                       ';' -> return ()
                                                       _ -> gets position >>= (throwError . mkParseError "';'" "not match pattern: " "" d492_308 ["char"])
                                                   let ';' = xx491_309
                                                   return ()
                                                   return (PlainDefinition v $ PlainSelection sel)]
                selection25_99 = foldl1 mplus [do s <- StateT normalSelection
                                                  return (Selection s),
                                               do s <- StateT plainSelection
                                                  return (PlainSelection s)]
                normalSelection26_100 = foldl1 mplus [do ex <- StateT expressionHs
                                                         _ <- StateT spaces
                                                         return ()
                                                         d502_310 <- get
                                                         xx501_311 <- StateT char
                                                         case xx501_311 of
                                                             '/' -> return ()
                                                             _ -> gets position >>= (throwError . mkParseError "'/'" "not match pattern: " "" d502_310 ["char"])
                                                         let '/' = xx501_311
                                                         return ()
                                                         _ <- StateT spaces
                                                         return ()
                                                         sel <- StateT normalSelection
                                                         return (ex : sel),
                                                      do ex <- StateT expressionHs
                                                         return [ex]]
                plainSelection27_101 = foldl1 mplus [do ex <- StateT plainExpressionHs
                                                        _ <- StateT spaces
                                                        return ()
                                                        d514_312 <- get
                                                        xx513_313 <- StateT char
                                                        case xx513_313 of
                                                            '/' -> return ()
                                                            _ -> gets position >>= (throwError . mkParseError "'/'" "not match pattern: " "" d514_312 ["char"])
                                                        let '/' = xx513_313
                                                        return ()
                                                        _ <- StateT spaces
                                                        return ()
                                                        sel <- StateT plainSelection
                                                        return (ex : sel),
                                                     do ex <- StateT plainExpressionHs
                                                        return [ex]]
                expressionHs28_102 = foldl1 mplus [do e <- StateT expression
                                                      _ <- StateT spaces
                                                      return ()
                                                      d526_314 <- get
                                                      xx525_315 <- StateT char
                                                      case xx525_315 of
                                                          '{' -> return ()
                                                          _ -> gets position >>= (throwError . mkParseError "'{'" "not match pattern: " "" d526_314 ["char"])
                                                      let '{' = xx525_315
                                                      return ()
                                                      _ <- StateT spaces
                                                      return ()
                                                      h <- StateT hsExpLam
                                                      _ <- StateT spaces
                                                      return ()
                                                      d534_316 <- get
                                                      xx533_317 <- StateT char
                                                      case xx533_317 of
                                                          '}' -> return ()
                                                          _ -> gets position >>= (throwError . mkParseError "'}'" "not match pattern: " "" d534_316 ["char"])
                                                      let '}' = xx533_317
                                                      return ()
                                                      return (ExpressionHs e h),
                                                   do e <- StateT expressionHsSugar
                                                      return e]
                expressionHsSugar29_103 = foldl1 mplus [do d538_318 <- get
                                                           xx537_319 <- StateT char
                                                           case xx537_319 of
                                                               '<' -> return ()
                                                               _ -> gets position >>= (throwError . mkParseError "'<'" "not match pattern: " "" d538_318 ["char"])
                                                           let '<' = xx537_319
                                                           return ()
                                                           _ <- StateT spaces
                                                           return ()
                                                           h <- StateT hsExpLam
                                                           _ <- StateT spaces
                                                           return ()
                                                           d546_320 <- get
                                                           xx545_321 <- StateT char
                                                           case xx545_321 of
                                                               '>' -> return ()
                                                               _ -> gets position >>= (throwError . mkParseError "'>'" "not match pattern: " "" d546_320 ["char"])
                                                           let '>' = xx545_321
                                                           return ()
                                                           return (ExpressionHsSugar h)]
                plainExpressionHs30_104 = foldl1 mplus [do rfs <- list1_322 (foldl1 mplus [do rf <- StateT plainReadFromLs
                                                                                              _ <- StateT spaces
                                                                                              return ()
                                                                                              return rf])
                                                           return (PlainExpressionHs rfs)]
                plainReadFromLs31_105 = foldl1 mplus [do rf <- StateT readFromLs
                                                         return rf,
                                                      do rf <- StateT selectCharsLs
                                                         return rf]
                expression32_106 = foldl1 mplus [do l <- StateT nameLeaf_
                                                    _ <- StateT spaces
                                                    return ()
                                                    e <- StateT expression
                                                    return (l : e),
                                                 return []]
                nameLeaf_33_107 = foldl1 mplus [do d564_323 <- get
                                                   xx563_324 <- StateT char
                                                   case xx563_324 of
                                                       '!' -> return ()
                                                       _ -> gets position >>= (throwError . mkParseError "'!'" "not match pattern: " "" d564_323 ["char"])
                                                   let '!' = xx563_324
                                                   return ()
                                                   nl <- StateT nameLeafNoCom
                                                   _ <- StateT spaces
                                                   return ()
                                                   com <- optional3_325 (StateT comForErr)
                                                   return (NotAfter nl $ maybe "" id com),
                                                do d572_326 <- get
                                                   xx571_327 <- StateT char
                                                   let c = xx571_327
                                                   unless (isAmp c) (gets position >>= (throwError . mkParseError "isAmp c" "not match: " "" d572_326 ["char"]))
                                                   nl <- StateT nameLeaf
                                                   return (After nl),
                                                do nl <- StateT nameLeaf
                                                   return (Here nl)]
                nameLeaf34_108 = foldl1 mplus [do n <- StateT pat1
                                                  _ <- StateT spaces
                                                  return ()
                                                  com <- optional3_325 (StateT comForErr)
                                                  d584_328 <- get
                                                  xx583_329 <- StateT char
                                                  case xx583_329 of
                                                      ':' -> return ()
                                                      _ -> gets position >>= (throwError . mkParseError "':'" "not match pattern: " "" d584_328 ["char"])
                                                  let ':' = xx583_329
                                                  return ()
                                                  (rf, p) <- StateT leaf
                                                  return (NameLeaf (n, maybe "" id com) rf p),
                                               do n <- StateT pat1
                                                  _ <- StateT spaces
                                                  return ()
                                                  com <- optional3_325 (StateT comForErr)
                                                  return (NameLeaf (n,
                                                                    maybe "" id com) FromToken Nothing)]
                nameLeafNoCom35_109 = foldl1 mplus [do n <- StateT pat1
                                                       _ <- StateT spaces
                                                       return ()
                                                       com <- optional3_325 (StateT comForErr)
                                                       d600_330 <- get
                                                       xx599_331 <- StateT char
                                                       case xx599_331 of
                                                           ':' -> return ()
                                                           _ -> gets position >>= (throwError . mkParseError "':'" "not match pattern: " "" d600_330 ["char"])
                                                       let ':' = xx599_331
                                                       return ()
                                                       (rf, p) <- StateT leaf
                                                       return (NameLeaf (n, maybe "" id com) rf p),
                                                    do n <- StateT pat1
                                                       _ <- StateT spaces
                                                       return ()
                                                       return (NameLeaf (n, "") FromToken Nothing)]
                comForErr36_110 = foldl1 mplus [do d608_332 <- get
                                                   xx607_333 <- StateT char
                                                   case xx607_333 of
                                                       '{' -> return ()
                                                       _ -> gets position >>= (throwError . mkParseError "'{'" "not match pattern: " "" d608_332 ["char"])
                                                   let '{' = xx607_333
                                                   return ()
                                                   d610_334 <- get
                                                   xx609_335 <- StateT char
                                                   case xx609_335 of
                                                       '-' -> return ()
                                                       _ -> gets position >>= (throwError . mkParseError "'-'" "not match pattern: " "" d610_334 ["char"])
                                                   let '-' = xx609_335
                                                   return ()
                                                   d612_336 <- get
                                                   xx611_337 <- StateT char
                                                   case xx611_337 of
                                                       '#' -> return ()
                                                       _ -> gets position >>= (throwError . mkParseError "'#'" "not match pattern: " "" d612_336 ["char"])
                                                   let '#' = xx611_337
                                                   return ()
                                                   _ <- StateT spaces
                                                   return ()
                                                   d616_338 <- get
                                                   xx615_339 <- StateT char
                                                   case xx615_339 of
                                                       '"' -> return ()
                                                       _ -> gets position >>= (throwError . mkParseError "'\"'" "not match pattern: " "" d616_338 ["char"])
                                                   let '"' = xx615_339
                                                   return ()
                                                   s <- StateT stringLit
                                                   d620_340 <- get
                                                   xx619_341 <- StateT char
                                                   case xx619_341 of
                                                       '"' -> return ()
                                                       _ -> gets position >>= (throwError . mkParseError "'\"'" "not match pattern: " "" d620_340 ["char"])
                                                   let '"' = xx619_341
                                                   return ()
                                                   _ <- StateT spaces
                                                   return ()
                                                   d624_342 <- get
                                                   xx623_343 <- StateT char
                                                   case xx623_343 of
                                                       '#' -> return ()
                                                       _ -> gets position >>= (throwError . mkParseError "'#'" "not match pattern: " "" d624_342 ["char"])
                                                   let '#' = xx623_343
                                                   return ()
                                                   d626_344 <- get
                                                   xx625_345 <- StateT char
                                                   case xx625_345 of
                                                       '-' -> return ()
                                                       _ -> gets position >>= (throwError . mkParseError "'-'" "not match pattern: " "" d626_344 ["char"])
                                                   let '-' = xx625_345
                                                   return ()
                                                   d628_346 <- get
                                                   xx627_347 <- StateT char
                                                   case xx627_347 of
                                                       '}' -> return ()
                                                       _ -> gets position >>= (throwError . mkParseError "'}'" "not match pattern: " "" d628_346 ["char"])
                                                   let '}' = xx627_347
                                                   return ()
                                                   _ <- StateT spaces
                                                   return ()
                                                   return s]
                leaf37_111 = foldl1 mplus [do rf <- StateT readFromLs
                                              t <- StateT test
                                              return (rf, Just t),
                                           do rf <- StateT readFromLs
                                              return (rf, Nothing),
                                           do t <- StateT test
                                              return (FromToken, Just t)]
                patOp38_112 = foldl1 mplus [do p <- StateT pat
                                               o <- StateT opConName
                                               po <- StateT patOp
                                               return (uInfixP p o po),
                                            do p <- StateT pat
                                               _ <- StateT spaces
                                               return ()
                                               d650_348 <- get
                                               xx649_349 <- StateT char
                                               let q = xx649_349
                                               unless (isBQ q) (gets position >>= (throwError . mkParseError "isBQ q" "not match: " "" d650_348 ["char"]))
                                               t <- StateT typ
                                               d654_350 <- get
                                               xx653_351 <- StateT char
                                               let q_ = xx653_351
                                               unless (isBQ q_) (gets position >>= (throwError . mkParseError "isBQ q_" "not match: " "" d654_350 ["char"]))
                                               _ <- StateT spaces
                                               return ()
                                               po <- StateT patOp
                                               return (uInfixP p (mkName t) po),
                                            do p <- StateT pat
                                               return p]
                pat39_113 = foldl1 mplus [do t <- StateT typ
                                             _ <- StateT spaces
                                             return ()
                                             ps <- StateT pats
                                             return (conToPatQ t ps),
                                          do d668_352 <- get
                                             xx667_353 <- StateT char
                                             case xx667_353 of
                                                 '(' -> return ()
                                                 _ -> gets position >>= (throwError . mkParseError "'('" "not match pattern: " "" d668_352 ["char"])
                                             let '(' = xx667_353
                                             return ()
                                             o <- StateT opConName
                                             d672_354 <- get
                                             xx671_355 <- StateT char
                                             case xx671_355 of
                                                 ')' -> return ()
                                                 _ -> gets position >>= (throwError . mkParseError "')'" "not match pattern: " "" d672_354 ["char"])
                                             let ')' = xx671_355
                                             return ()
                                             _ <- StateT spaces
                                             return ()
                                             ps <- StateT pats
                                             return (conP o ps),
                                          do p <- StateT pat1
                                             return p]
                pat140_114 = foldl1 mplus [do t <- StateT typ
                                              return (conToPatQ t []),
                                           do d682_356 <- get
                                              xx681_357 <- StateT variable
                                              case xx681_357 of
                                                  "_" -> return ()
                                                  _ -> gets position >>= (throwError . mkParseError "\"_\"" "not match pattern: " "" d682_356 ["variable"])
                                              let "_" = xx681_357
                                              return ()
                                              return wildP,
                                           do n <- StateT variable
                                              return (strToPatQ n),
                                           do i <- StateT integer
                                              return (litP (integerL i)),
                                           do d688_358 <- get
                                              xx687_359 <- StateT char
                                              case xx687_359 of
                                                  '-' -> return ()
                                                  _ -> gets position >>= (throwError . mkParseError "'-'" "not match pattern: " "" d688_358 ["char"])
                                              let '-' = xx687_359
                                              return ()
                                              _ <- StateT spaces
                                              return ()
                                              i <- StateT integer
                                              return (litP (integerL $ negate i)),
                                           do d694_360 <- get
                                              xx693_361 <- StateT char
                                              case xx693_361 of
                                                  '\'' -> return ()
                                                  _ -> gets position >>= (throwError . mkParseError "'\\''" "not match pattern: " "" d694_360 ["char"])
                                              let '\'' = xx693_361
                                              return ()
                                              c <- StateT charLit
                                              d698_362 <- get
                                              xx697_363 <- StateT char
                                              case xx697_363 of
                                                  '\'' -> return ()
                                                  _ -> gets position >>= (throwError . mkParseError "'\\''" "not match pattern: " "" d698_362 ["char"])
                                              let '\'' = xx697_363
                                              return ()
                                              return (litP $ charL c),
                                           do d700_364 <- get
                                              xx699_365 <- StateT char
                                              case xx699_365 of
                                                  '"' -> return ()
                                                  _ -> gets position >>= (throwError . mkParseError "'\"'" "not match pattern: " "" d700_364 ["char"])
                                              let '"' = xx699_365
                                              return ()
                                              s <- StateT stringLit
                                              d704_366 <- get
                                              xx703_367 <- StateT char
                                              case xx703_367 of
                                                  '"' -> return ()
                                                  _ -> gets position >>= (throwError . mkParseError "'\"'" "not match pattern: " "" d704_366 ["char"])
                                              let '"' = xx703_367
                                              return ()
                                              return (litP $ stringL s),
                                           do d706_368 <- get
                                              xx705_369 <- StateT char
                                              case xx705_369 of
                                                  '(' -> return ()
                                                  _ -> gets position >>= (throwError . mkParseError "'('" "not match pattern: " "" d706_368 ["char"])
                                              let '(' = xx705_369
                                              return ()
                                              p <- StateT patList
                                              d710_370 <- get
                                              xx709_371 <- StateT char
                                              case xx709_371 of
                                                  ')' -> return ()
                                                  _ -> gets position >>= (throwError . mkParseError "')'" "not match pattern: " "" d710_370 ["char"])
                                              let ')' = xx709_371
                                              return ()
                                              return (tupP p),
                                           do d712_372 <- get
                                              xx711_373 <- StateT char
                                              case xx711_373 of
                                                  '[' -> return ()
                                                  _ -> gets position >>= (throwError . mkParseError "'['" "not match pattern: " "" d712_372 ["char"])
                                              let '[' = xx711_373
                                              return ()
                                              p <- StateT patList
                                              d716_374 <- get
                                              xx715_375 <- StateT char
                                              case xx715_375 of
                                                  ']' -> return ()
                                                  _ -> gets position >>= (throwError . mkParseError "']'" "not match pattern: " "" d716_374 ["char"])
                                              let ']' = xx715_375
                                              return ()
                                              return (listP p)]
                patList41_115 = foldl1 mplus [do p <- StateT patOp
                                                 _ <- StateT spaces
                                                 return ()
                                                 d722_376 <- get
                                                 xx721_377 <- StateT char
                                                 case xx721_377 of
                                                     ',' -> return ()
                                                     _ -> gets position >>= (throwError . mkParseError "','" "not match pattern: " "" d722_376 ["char"])
                                                 let ',' = xx721_377
                                                 return ()
                                                 _ <- StateT spaces
                                                 return ()
                                                 ps <- StateT patList
                                                 return (p : ps),
                                              do p <- StateT patOp
                                                 return [p],
                                              return []]
                opConName42_116 = foldl1 mplus [do d730_378 <- get
                                                   xx729_379 <- StateT char
                                                   case xx729_379 of
                                                       ':' -> return ()
                                                       _ -> gets position >>= (throwError . mkParseError "':'" "not match pattern: " "" d730_378 ["char"])
                                                   let ':' = xx729_379
                                                   return ()
                                                   ot <- StateT opTail
                                                   return (mkName $ ':' : ot)]
                charLit43_117 = foldl1 mplus [do d734_380 <- get
                                                 xx733_381 <- StateT char
                                                 let c = xx733_381
                                                 unless (isAlphaNumOt c) (gets position >>= (throwError . mkParseError "isAlphaNumOt c" "not match: " "" d734_380 ["char"]))
                                                 return c,
                                              do d736_382 <- get
                                                 xx735_383 <- StateT char
                                                 case xx735_383 of
                                                     '\\' -> return ()
                                                     _ -> gets position >>= (throwError . mkParseError "'\\\\'" "not match pattern: " "" d736_382 ["char"])
                                                 let '\\' = xx735_383
                                                 return ()
                                                 c <- StateT escapeC
                                                 return c]
                stringLit44_118 = foldl1 mplus [do d740_384 <- get
                                                   xx739_385 <- StateT char
                                                   let c = xx739_385
                                                   unless (isStrLitC c) (gets position >>= (throwError . mkParseError "isStrLitC c" "not match: " "" d740_384 ["char"]))
                                                   s <- StateT stringLit
                                                   return (c : s),
                                                do d744_386 <- get
                                                   xx743_387 <- StateT char
                                                   case xx743_387 of
                                                       '\\' -> return ()
                                                       _ -> gets position >>= (throwError . mkParseError "'\\\\'" "not match pattern: " "" d744_386 ["char"])
                                                   let '\\' = xx743_387
                                                   return ()
                                                   c <- StateT escapeC
                                                   s <- StateT stringLit
                                                   return (c : s),
                                                return ""]
                escapeC45_119 = foldl1 mplus [do d750_388 <- get
                                                 xx749_389 <- StateT char
                                                 case xx749_389 of
                                                     '"' -> return ()
                                                     _ -> gets position >>= (throwError . mkParseError "'\"'" "not match pattern: " "" d750_388 ["char"])
                                                 let '"' = xx749_389
                                                 return ()
                                                 return '"',
                                              do d752_390 <- get
                                                 xx751_391 <- StateT char
                                                 case xx751_391 of
                                                     '\'' -> return ()
                                                     _ -> gets position >>= (throwError . mkParseError "'\\''" "not match pattern: " "" d752_390 ["char"])
                                                 let '\'' = xx751_391
                                                 return ()
                                                 return '\'',
                                              do d754_392 <- get
                                                 xx753_393 <- StateT char
                                                 case xx753_393 of
                                                     '\\' -> return ()
                                                     _ -> gets position >>= (throwError . mkParseError "'\\\\'" "not match pattern: " "" d754_392 ["char"])
                                                 let '\\' = xx753_393
                                                 return ()
                                                 return '\\',
                                              do d756_394 <- get
                                                 xx755_395 <- StateT char
                                                 case xx755_395 of
                                                     'n' -> return ()
                                                     _ -> gets position >>= (throwError . mkParseError "'n'" "not match pattern: " "" d756_394 ["char"])
                                                 let 'n' = xx755_395
                                                 return ()
                                                 return '\n',
                                              do d758_396 <- get
                                                 xx757_397 <- StateT char
                                                 case xx757_397 of
                                                     't' -> return ()
                                                     _ -> gets position >>= (throwError . mkParseError "'t'" "not match pattern: " "" d758_396 ["char"])
                                                 let 't' = xx757_397
                                                 return ()
                                                 return '\t']
                pats46_120 = foldl1 mplus [do p <- StateT pat
                                              _ <- StateT spaces
                                              return ()
                                              ps <- StateT pats
                                              return (p : ps),
                                           return []]
                readFromLs47_121 = foldl1 mplus [do rf <- StateT readFrom
                                                    d768_398 <- get
                                                    xx767_399 <- StateT char
                                                    case xx767_399 of
                                                        '*' -> return ()
                                                        _ -> gets position >>= (throwError . mkParseError "'*'" "not match pattern: " "" d768_398 ["char"])
                                                    let '*' = xx767_399
                                                    return ()
                                                    return (FromList rf),
                                                 do rf <- StateT readFrom
                                                    d772_400 <- get
                                                    xx771_401 <- StateT char
                                                    case xx771_401 of
                                                        '+' -> return ()
                                                        _ -> gets position >>= (throwError . mkParseError "'+'" "not match pattern: " "" d772_400 ["char"])
                                                    let '+' = xx771_401
                                                    return ()
                                                    return (FromList1 rf),
                                                 do rf <- StateT readFrom
                                                    d776_402 <- get
                                                    xx775_403 <- StateT char
                                                    case xx775_403 of
                                                        '?' -> return ()
                                                        _ -> gets position >>= (throwError . mkParseError "'?'" "not match pattern: " "" d776_402 ["char"])
                                                    let '?' = xx775_403
                                                    return ()
                                                    return (FromOptional rf),
                                                 do rf <- StateT readFrom
                                                    return rf]
                readFrom48_122 = foldl1 mplus [do v <- StateT variable
                                                  return (FromVariable v),
                                               do d782_404 <- get
                                                  xx781_405 <- StateT char
                                                  case xx781_405 of
                                                      '(' -> return ()
                                                      _ -> gets position >>= (throwError . mkParseError "'('" "not match pattern: " "" d782_404 ["char"])
                                                  let '(' = xx781_405
                                                  return ()
                                                  s <- StateT selection
                                                  d786_406 <- get
                                                  xx785_407 <- StateT char
                                                  case xx785_407 of
                                                      ')' -> return ()
                                                      _ -> gets position >>= (throwError . mkParseError "')'" "not match pattern: " "" d786_406 ["char"])
                                                  let ')' = xx785_407
                                                  return ()
                                                  return (FromSelection s),
                                               do e <- StateT expressionHsSugar
                                                  return (FromSelection $ Selection [e])]
                selectCharsLs49_123 = foldl1 mplus [do rf <- StateT selectChars
                                                       d792_408 <- get
                                                       xx791_409 <- StateT char
                                                       case xx791_409 of
                                                           '*' -> return ()
                                                           _ -> gets position >>= (throwError . mkParseError "'*'" "not match pattern: " "" d792_408 ["char"])
                                                       let '*' = xx791_409
                                                       return ()
                                                       return (FromList rf),
                                                    do rf <- StateT selectChars
                                                       d796_410 <- get
                                                       xx795_411 <- StateT char
                                                       case xx795_411 of
                                                           '+' -> return ()
                                                           _ -> gets position >>= (throwError . mkParseError "'+'" "not match pattern: " "" d796_410 ["char"])
                                                       let '+' = xx795_411
                                                       return ()
                                                       return (FromList1 rf),
                                                    do rf <- StateT selectChars
                                                       d800_412 <- get
                                                       xx799_413 <- StateT char
                                                       case xx799_413 of
                                                           '?' -> return ()
                                                           _ -> gets position >>= (throwError . mkParseError "'?'" "not match pattern: " "" d800_412 ["char"])
                                                       let '?' = xx799_413
                                                       return ()
                                                       return (FromOptional rf),
                                                    do rf <- StateT selectChars
                                                       return rf]
                selectChars50_124 = foldl1 mplus [do d804_414 <- get
                                                     xx803_415 <- StateT char
                                                     case xx803_415 of
                                                         '[' -> return ()
                                                         _ -> gets position >>= (throwError . mkParseError "'['" "not match pattern: " "" d804_414 ["char"])
                                                     let '[' = xx803_415
                                                     return ()
                                                     cs <- list12_416 (foldl1 mplus [do d808_417 <- get
                                                                                        xx807_418 <- StateT char
                                                                                        let c = xx807_418
                                                                                        unless (isLower c) (gets position >>= (throwError . mkParseError "isLower c" "not match: " "" d808_417 ["char"]))
                                                                                        return c])
                                                     d810_419 <- get
                                                     xx809_420 <- StateT char
                                                     case xx809_420 of
                                                         ']' -> return ()
                                                         _ -> gets position >>= (throwError . mkParseError "']'" "not match pattern: " "" d810_419 ["char"])
                                                     let ']' = xx809_420
                                                     return ()
                                                     return (FromTokenChars cs),
                                                  do d812_421 <- get
                                                     xx811_422 <- StateT char
                                                     case xx811_422 of
                                                         '[' -> return ()
                                                         _ -> gets position >>= (throwError . mkParseError "'['" "not match pattern: " "" d812_421 ["char"])
                                                     let '[' = xx811_422
                                                     return ()
                                                     d814_423 <- get
                                                     xx813_424 <- StateT char
                                                     let cb = xx813_424
                                                     unless (cb `notElem` "\\-") (gets position >>= (throwError . mkParseError "cb `notElem` \"\\\\-\"" "not match: " "" d814_423 ["char"]))
                                                     d816_425 <- get
                                                     xx815_426 <- StateT char
                                                     case xx815_426 of
                                                         '-' -> return ()
                                                         _ -> gets position >>= (throwError . mkParseError "'-'" "not match pattern: " "" d816_425 ["char"])
                                                     let '-' = xx815_426
                                                     return ()
                                                     d818_427 <- get
                                                     xx817_428 <- StateT char
                                                     let ce = xx817_428
                                                     unless (ce `notElem` "\\-") (gets position >>= (throwError . mkParseError "ce `notElem` \"\\\\-\"" "not match: " "" d818_427 ["char"]))
                                                     d820_429 <- get
                                                     xx819_430 <- StateT char
                                                     case xx819_430 of
                                                         ']' -> return ()
                                                         _ -> gets position >>= (throwError . mkParseError "']'" "not match pattern: " "" d820_429 ["char"])
                                                     let ']' = xx819_430
                                                     return ()
                                                     return (FromTokenChars [cb .. ce]),
                                                  do d822_431 <- get
                                                     xx821_432 <- StateT char
                                                     case xx821_432 of
                                                         '\'' -> return ()
                                                         _ -> gets position >>= (throwError . mkParseError "'\\''" "not match pattern: " "" d822_431 ["char"])
                                                     let '\'' = xx821_432
                                                     return ()
                                                     d824_433 <- get
                                                     xx823_434 <- StateT char
                                                     let c = xx823_434
                                                     unless (c `notElem` "\\'") (gets position >>= (throwError . mkParseError "c `notElem` \"\\\\'\"" "not match: " "" d824_433 ["char"]))
                                                     d826_435 <- get
                                                     xx825_436 <- StateT char
                                                     case xx825_436 of
                                                         '\'' -> return ()
                                                         _ -> gets position >>= (throwError . mkParseError "'\\''" "not match pattern: " "" d826_435 ["char"])
                                                     let '\'' = xx825_436
                                                     return ()
                                                     return (FromTokenChars [c])]
                test51_125 = foldl1 mplus [do d828_437 <- get
                                              xx827_438 <- StateT char
                                              case xx827_438 of
                                                  '[' -> return ()
                                                  _ -> gets position >>= (throwError . mkParseError "'['" "not match pattern: " "" d828_437 ["char"])
                                              let '[' = xx827_438
                                              return ()
                                              h <- StateT hsExpLam
                                              _ <- StateT spaces
                                              return ()
                                              com <- optional3_325 (StateT comForErr)
                                              d836_439 <- get
                                              xx835_440 <- StateT char
                                              case xx835_440 of
                                                  ']' -> return ()
                                                  _ -> gets position >>= (throwError . mkParseError "']'" "not match pattern: " "" d836_439 ["char"])
                                              let ']' = xx835_440
                                              return ()
                                              return (h, maybe "" id com)]
                hsExpLam52_126 = foldl1 mplus [do d838_441 <- get
                                                  xx837_442 <- StateT char
                                                  case xx837_442 of
                                                      '\\' -> return ()
                                                      _ -> gets position >>= (throwError . mkParseError "'\\\\'" "not match pattern: " "" d838_441 ["char"])
                                                  let '\\' = xx837_442
                                                  return ()
                                                  _ <- StateT spaces
                                                  return ()
                                                  ps <- StateT pats
                                                  _ <- StateT spaces
                                                  return ()
                                                  d846_443 <- get
                                                  xx845_444 <- StateT char
                                                  case xx845_444 of
                                                      '-' -> return ()
                                                      _ -> gets position >>= (throwError . mkParseError "'-'" "not match pattern: " "" d846_443 ["char"])
                                                  let '-' = xx845_444
                                                  return ()
                                                  d848_445 <- get
                                                  xx847_446 <- StateT char
                                                  let c = xx847_446
                                                  unless (isGt c) (gets position >>= (throwError . mkParseError "isGt c" "not match: " "" d848_445 ["char"]))
                                                  _ <- StateT spaces
                                                  return ()
                                                  e <- StateT hsExpTyp
                                                  return (lamE ps e),
                                               do e <- StateT hsExpTyp
                                                  return e]
                hsExpTyp53_127 = foldl1 mplus [do eo <- StateT hsExpOp
                                                  d858_447 <- get
                                                  xx857_448 <- StateT char
                                                  case xx857_448 of
                                                      ':' -> return ()
                                                      _ -> gets position >>= (throwError . mkParseError "':'" "not match pattern: " "" d858_447 ["char"])
                                                  let ':' = xx857_448
                                                  return ()
                                                  d860_449 <- get
                                                  xx859_450 <- StateT char
                                                  case xx859_450 of
                                                      ':' -> return ()
                                                      _ -> gets position >>= (throwError . mkParseError "':'" "not match pattern: " "" d860_449 ["char"])
                                                  let ':' = xx859_450
                                                  return ()
                                                  _ <- StateT spaces
                                                  return ()
                                                  t <- StateT hsTypeArr
                                                  return (sigE eo t),
                                               do eo <- StateT hsExpOp
                                                  return eo]
                hsExpOp54_128 = foldl1 mplus [do l <- StateT hsExp
                                                 _ <- StateT spaces
                                                 return ()
                                                 o <- StateT hsOp
                                                 _ <- StateT spaces
                                                 return ()
                                                 r <- StateT hsExpOp
                                                 return (uInfixE (getEx l) o r),
                                              do e <- StateT hsExp
                                                 return (getEx e)]
                hsOp55_129 = foldl1 mplus [do d880_451 <- get
                                              xx879_452 <- StateT char
                                              let c = xx879_452
                                              unless (isOpHeadChar c) (gets position >>= (throwError . mkParseError "isOpHeadChar c" "not match: " "" d880_451 ["char"]))
                                              o <- StateT opTail
                                              return (varE $ mkName $ c : o),
                                           do d884_453 <- get
                                              xx883_454 <- StateT char
                                              case xx883_454 of
                                                  ':' -> return ()
                                                  _ -> gets position >>= (throwError . mkParseError "':'" "not match pattern: " "" d884_453 ["char"])
                                              let ':' = xx883_454
                                              return ()
                                              ddd885_455 <- get
                                              do err <- ((do d887_456 <- get
                                                             xx886_457 <- StateT char
                                                             case xx886_457 of
                                                                 ':' -> return ()
                                                                 _ -> gets position >>= (throwError . mkParseError "':'" "not match pattern: " "" d887_456 ["char"])
                                                             let ':' = xx886_457
                                                             return ()) >> return False) `catchError` const (return True)
                                                 unless err (gets position >>= (throwError . mkParseError ('!' : "':':") "not match: " "" ddd885_455 ["char"]))
                                              put ddd885_455
                                              o <- StateT opTail
                                              return (conE $ mkName $ ':' : o),
                                           do d891_458 <- get
                                              xx890_459 <- StateT char
                                              let c = xx890_459
                                              unless (isBQ c) (gets position >>= (throwError . mkParseError "isBQ c" "not match: " "" d891_458 ["char"]))
                                              v <- StateT variable
                                              d895_460 <- get
                                              xx894_461 <- StateT char
                                              let c_ = xx894_461
                                              unless (isBQ c_) (gets position >>= (throwError . mkParseError "isBQ c_" "not match: " "" d895_460 ["char"]))
                                              return (varE $ mkName v),
                                           do d897_462 <- get
                                              xx896_463 <- StateT char
                                              let c = xx896_463
                                              unless (isBQ c) (gets position >>= (throwError . mkParseError "isBQ c" "not match: " "" d897_462 ["char"]))
                                              t <- StateT typ
                                              d901_464 <- get
                                              xx900_465 <- StateT char
                                              let c_ = xx900_465
                                              unless (isBQ c_) (gets position >>= (throwError . mkParseError "isBQ c_" "not match: " "" d901_464 ["char"]))
                                              return (conE $ mkName t)]
                opTail56_130 = foldl1 mplus [do d903_466 <- get
                                                xx902_467 <- StateT char
                                                let c = xx902_467
                                                unless (isOpTailChar c) (gets position >>= (throwError . mkParseError "isOpTailChar c" "not match: " "" d903_466 ["char"]))
                                                s <- StateT opTail
                                                return (c : s),
                                             return ""]
                hsExp57_131 = foldl1 mplus [do e <- StateT hsExp1
                                               _ <- StateT spaces
                                               return ()
                                               h <- StateT hsExp
                                               return (applyExR e h),
                                            do e <- StateT hsExp1
                                               return (toEx e)]
                hsExp158_132 = foldl1 mplus [do d915_468 <- get
                                                xx914_469 <- StateT char
                                                case xx914_469 of
                                                    '(' -> return ()
                                                    _ -> gets position >>= (throwError . mkParseError "'('" "not match pattern: " "" d915_468 ["char"])
                                                let '(' = xx914_469
                                                return ()
                                                l <- optional3_325 (foldl1 mplus [do e <- StateT hsExpTyp
                                                                                     return e])
                                                _ <- StateT spaces
                                                return ()
                                                o <- StateT hsOp
                                                _ <- StateT spaces
                                                return ()
                                                r <- optional3_325 (foldl1 mplus [do e <- StateT hsExpTyp
                                                                                     return e])
                                                d931_470 <- get
                                                xx930_471 <- StateT char
                                                case xx930_471 of
                                                    ')' -> return ()
                                                    _ -> gets position >>= (throwError . mkParseError "')'" "not match pattern: " "" d931_470 ["char"])
                                                let ')' = xx930_471
                                                return ()
                                                return (infixE l o r),
                                             do d933_472 <- get
                                                xx932_473 <- StateT char
                                                case xx932_473 of
                                                    '(' -> return ()
                                                    _ -> gets position >>= (throwError . mkParseError "'('" "not match pattern: " "" d933_472 ["char"])
                                                let '(' = xx932_473
                                                return ()
                                                et <- StateT hsExpTpl
                                                d937_474 <- get
                                                xx936_475 <- StateT char
                                                case xx936_475 of
                                                    ')' -> return ()
                                                    _ -> gets position >>= (throwError . mkParseError "')'" "not match pattern: " "" d937_474 ["char"])
                                                let ')' = xx936_475
                                                return ()
                                                return (tupE et),
                                             do d939_476 <- get
                                                xx938_477 <- StateT char
                                                case xx938_477 of
                                                    '[' -> return ()
                                                    _ -> gets position >>= (throwError . mkParseError "'['" "not match pattern: " "" d939_476 ["char"])
                                                let '[' = xx938_477
                                                return ()
                                                et <- StateT hsExpTpl
                                                d943_478 <- get
                                                xx942_479 <- StateT char
                                                case xx942_479 of
                                                    ']' -> return ()
                                                    _ -> gets position >>= (throwError . mkParseError "']'" "not match pattern: " "" d943_478 ["char"])
                                                let ']' = xx942_479
                                                return ()
                                                return (listE et),
                                             do v <- StateT variable
                                                return (varE $ mkName v),
                                             do t <- StateT typ
                                                return (conE $ mkName t),
                                             do i <- StateT integer
                                                _ <- StateT spaces
                                                return ()
                                                return (litE $ integerL i),
                                             do d953_480 <- get
                                                xx952_481 <- StateT char
                                                case xx952_481 of
                                                    '\'' -> return ()
                                                    _ -> gets position >>= (throwError . mkParseError "'\\''" "not match pattern: " "" d953_480 ["char"])
                                                let '\'' = xx952_481
                                                return ()
                                                c <- StateT charLit
                                                d957_482 <- get
                                                xx956_483 <- StateT char
                                                case xx956_483 of
                                                    '\'' -> return ()
                                                    _ -> gets position >>= (throwError . mkParseError "'\\''" "not match pattern: " "" d957_482 ["char"])
                                                let '\'' = xx956_483
                                                return ()
                                                return (litE $ charL c),
                                             do d959_484 <- get
                                                xx958_485 <- StateT char
                                                case xx958_485 of
                                                    '"' -> return ()
                                                    _ -> gets position >>= (throwError . mkParseError "'\"'" "not match pattern: " "" d959_484 ["char"])
                                                let '"' = xx958_485
                                                return ()
                                                s <- StateT stringLit
                                                d963_486 <- get
                                                xx962_487 <- StateT char
                                                case xx962_487 of
                                                    '"' -> return ()
                                                    _ -> gets position >>= (throwError . mkParseError "'\"'" "not match pattern: " "" d963_486 ["char"])
                                                let '"' = xx962_487
                                                return ()
                                                return (litE $ stringL s),
                                             do d965_488 <- get
                                                xx964_489 <- StateT char
                                                case xx964_489 of
                                                    '-' -> return ()
                                                    _ -> gets position >>= (throwError . mkParseError "'-'" "not match pattern: " "" d965_488 ["char"])
                                                let '-' = xx964_489
                                                return ()
                                                _ <- StateT spaces
                                                return ()
                                                e <- StateT hsExp1
                                                return (appE (varE $ mkName "negate") e)]
                hsExpTpl59_133 = foldl1 mplus [do e <- StateT hsExpLam
                                                  _ <- StateT spaces
                                                  return ()
                                                  d975_490 <- get
                                                  xx974_491 <- StateT char
                                                  let c = xx974_491
                                                  unless (isComma c) (gets position >>= (throwError . mkParseError "isComma c" "not match: " "" d975_490 ["char"]))
                                                  _ <- StateT spaces
                                                  return ()
                                                  et <- StateT hsExpTpl
                                                  return (e : et),
                                               do e <- StateT hsExpLam
                                                  return [e],
                                               return []]
                hsTypeArr60_134 = foldl1 mplus [do l <- StateT hsType
                                                   d985_492 <- get
                                                   xx984_493 <- StateT char
                                                   case xx984_493 of
                                                       '-' -> return ()
                                                       _ -> gets position >>= (throwError . mkParseError "'-'" "not match pattern: " "" d985_492 ["char"])
                                                   let '-' = xx984_493
                                                   return ()
                                                   d987_494 <- get
                                                   xx986_495 <- StateT char
                                                   let c = xx986_495
                                                   unless (isGt c) (gets position >>= (throwError . mkParseError "isGt c" "not match: " "" d987_494 ["char"]))
                                                   _ <- StateT spaces
                                                   return ()
                                                   r <- StateT hsTypeArr
                                                   return (appT (appT arrowT (getTyp l)) r),
                                                do t <- StateT hsType
                                                   return (getTyp t)]
                hsType61_135 = foldl1 mplus [do t <- StateT hsType1
                                                ts <- StateT hsType
                                                return (applyTyp (toTyp t) ts),
                                             do t <- StateT hsType1
                                                return (toTyp t)]
                hsType162_136 = foldl1 mplus [do d1001_496 <- get
                                                 xx1000_497 <- StateT char
                                                 case xx1000_497 of
                                                     '[' -> return ()
                                                     _ -> gets position >>= (throwError . mkParseError "'['" "not match pattern: " "" d1001_496 ["char"])
                                                 let '[' = xx1000_497
                                                 return ()
                                                 d1003_498 <- get
                                                 xx1002_499 <- StateT char
                                                 case xx1002_499 of
                                                     ']' -> return ()
                                                     _ -> gets position >>= (throwError . mkParseError "']'" "not match pattern: " "" d1003_498 ["char"])
                                                 let ']' = xx1002_499
                                                 return ()
                                                 _ <- StateT spaces
                                                 return ()
                                                 return listT,
                                              do d1007_500 <- get
                                                 xx1006_501 <- StateT char
                                                 case xx1006_501 of
                                                     '[' -> return ()
                                                     _ -> gets position >>= (throwError . mkParseError "'['" "not match pattern: " "" d1007_500 ["char"])
                                                 let '[' = xx1006_501
                                                 return ()
                                                 t <- StateT hsTypeArr
                                                 d1011_502 <- get
                                                 xx1010_503 <- StateT char
                                                 case xx1010_503 of
                                                     ']' -> return ()
                                                     _ -> gets position >>= (throwError . mkParseError "']'" "not match pattern: " "" d1011_502 ["char"])
                                                 let ']' = xx1010_503
                                                 return ()
                                                 _ <- StateT spaces
                                                 return ()
                                                 return (listT `appT` t),
                                              do d1015_504 <- get
                                                 xx1014_505 <- StateT char
                                                 case xx1014_505 of
                                                     '(' -> return ()
                                                     _ -> gets position >>= (throwError . mkParseError "'('" "not match pattern: " "" d1015_504 ["char"])
                                                 let '(' = xx1014_505
                                                 return ()
                                                 _ <- StateT spaces
                                                 return ()
                                                 tt <- StateT hsTypeTpl
                                                 d1021_506 <- get
                                                 xx1020_507 <- StateT char
                                                 case xx1020_507 of
                                                     ')' -> return ()
                                                     _ -> gets position >>= (throwError . mkParseError "')'" "not match pattern: " "" d1021_506 ["char"])
                                                 let ')' = xx1020_507
                                                 return ()
                                                 return (tupT tt),
                                              do t <- StateT typToken
                                                 return (conT $ mkName t),
                                              do d1025_508 <- get
                                                 xx1024_509 <- StateT char
                                                 case xx1024_509 of
                                                     '(' -> return ()
                                                     _ -> gets position >>= (throwError . mkParseError "'('" "not match pattern: " "" d1025_508 ["char"])
                                                 let '(' = xx1024_509
                                                 return ()
                                                 d1027_510 <- get
                                                 xx1026_511 <- StateT char
                                                 case xx1026_511 of
                                                     '-' -> return ()
                                                     _ -> gets position >>= (throwError . mkParseError "'-'" "not match pattern: " "" d1027_510 ["char"])
                                                 let '-' = xx1026_511
                                                 return ()
                                                 d1029_512 <- get
                                                 xx1028_513 <- StateT char
                                                 let c = xx1028_513
                                                 unless (isGt c) (gets position >>= (throwError . mkParseError "isGt c" "not match: " "" d1029_512 ["char"]))
                                                 d1031_514 <- get
                                                 xx1030_515 <- StateT char
                                                 case xx1030_515 of
                                                     ')' -> return ()
                                                     _ -> gets position >>= (throwError . mkParseError "')'" "not match pattern: " "" d1031_514 ["char"])
                                                 let ')' = xx1030_515
                                                 return ()
                                                 _ <- StateT spaces
                                                 return ()
                                                 return arrowT]
                hsTypeTpl63_137 = foldl1 mplus [do t <- StateT hsTypeArr
                                                   d1037_516 <- get
                                                   xx1036_517 <- StateT char
                                                   let c = xx1036_517
                                                   unless (isComma c) (gets position >>= (throwError . mkParseError "isComma c" "not match: " "" d1037_516 ["char"]))
                                                   _ <- StateT spaces
                                                   return ()
                                                   tt <- StateT hsTypeTpl
                                                   return (t : tt),
                                                do t <- StateT hsTypeArr
                                                   return [t],
                                                return []]
                typ64_138 = foldl1 mplus [do u <- StateT upper
                                             t <- StateT tvtail
                                             return (u : t)]
                variable65_139 = foldl1 mplus [do l <- StateT lower
                                                  t <- StateT tvtail
                                                  return (l : t)]
                tvtail66_140 = foldl1 mplus [do a <- StateT alpha
                                                t <- StateT tvtail
                                                return (a : t),
                                             return ""]
                integer67_141 = foldl1 mplus [do dh <- StateT digit
                                                 ds <- list1_322 (foldl1 mplus [do d <- StateT digit
                                                                                   return d])
                                                 return (read $ dh : ds)]
                alpha68_142 = foldl1 mplus [do u <- StateT upper
                                               return u,
                                            do l <- StateT lower
                                               return l,
                                            do d <- StateT digit
                                               return d,
                                            do d1069_518 <- get
                                               xx1068_519 <- StateT char
                                               case xx1068_519 of
                                                   '\'' -> return ()
                                                   _ -> gets position >>= (throwError . mkParseError "'\\''" "not match pattern: " "" d1069_518 ["char"])
                                               let '\'' = xx1068_519
                                               return ()
                                               return '\'']
                upper69_143 = foldl1 mplus [do d1071_520 <- get
                                               xx1070_521 <- StateT char
                                               let u = xx1070_521
                                               unless (isUpper u) (gets position >>= (throwError . mkParseError "isUpper u" "not match: " "" d1071_520 ["char"]))
                                               return u]
                lower70_144 = foldl1 mplus [do d1073_522 <- get
                                               xx1072_523 <- StateT char
                                               let l = xx1072_523
                                               unless (isLowerU l) (gets position >>= (throwError . mkParseError "isLowerU l" "not match: " "" d1073_522 ["char"]))
                                               return l]
                digit71_145 = foldl1 mplus [do d1075_524 <- get
                                               xx1074_525 <- StateT char
                                               let d = xx1074_525
                                               unless (isDigit d) (gets position >>= (throwError . mkParseError "isDigit d" "not match: " "" d1075_524 ["char"]))
                                               return d]
                spaces72_146 = foldl1 mplus [do _ <- StateT space
                                                return ()
                                                _ <- StateT spaces
                                                return ()
                                                return (),
                                             return ()]
                space73_147 = foldl1 mplus [do d1081_526 <- get
                                               xx1080_527 <- StateT char
                                               let s = xx1080_527
                                               unless (isSpace s) (gets position >>= (throwError . mkParseError "isSpace s" "not match: " "" d1081_526 ["char"]))
                                               return (),
                                            do d1083_528 <- get
                                               xx1082_529 <- StateT char
                                               case xx1082_529 of
                                                   '-' -> return ()
                                                   _ -> gets position >>= (throwError . mkParseError "'-'" "not match pattern: " "" d1083_528 ["char"])
                                               let '-' = xx1082_529
                                               return ()
                                               d1085_530 <- get
                                               xx1084_531 <- StateT char
                                               case xx1084_531 of
                                                   '-' -> return ()
                                                   _ -> gets position >>= (throwError . mkParseError "'-'" "not match pattern: " "" d1085_530 ["char"])
                                               let '-' = xx1084_531
                                               return ()
                                               _ <- StateT notNLString
                                               return ()
                                               _ <- StateT newLine
                                               return ()
                                               return (),
                                            do _ <- StateT comment
                                               return ()
                                               return ()]
                notNLString74_148 = foldl1 mplus [do ddd1092_532 <- get
                                                     do err <- ((do _ <- StateT newLine
                                                                    return ()) >> return False) `catchError` const (return True)
                                                        unless err (gets position >>= (throwError . mkParseError ('!' : "_:newLine") "not match: " "" ddd1092_532 ["newLine"]))
                                                     put ddd1092_532
                                                     c <- StateT char
                                                     s <- StateT notNLString
                                                     return (c : s),
                                                  return ""]
                newLine75_149 = foldl1 mplus [do d1100_533 <- get
                                                 xx1099_534 <- StateT char
                                                 case xx1099_534 of
                                                     '\n' -> return ()
                                                     _ -> gets position >>= (throwError . mkParseError "'\\n'" "not match pattern: " "" d1100_533 ["char"])
                                                 let '\n' = xx1099_534
                                                 return ()
                                                 return ()]
                comment76_150 = foldl1 mplus [do d1102_535 <- get
                                                 xx1101_536 <- StateT char
                                                 case xx1101_536 of
                                                     '{' -> return ()
                                                     _ -> gets position >>= (throwError . mkParseError "'{'" "not match pattern: " "" d1102_535 ["char"])
                                                 let '{' = xx1101_536
                                                 return ()
                                                 d1104_537 <- get
                                                 xx1103_538 <- StateT char
                                                 case xx1103_538 of
                                                     '-' -> return ()
                                                     _ -> gets position >>= (throwError . mkParseError "'-'" "not match pattern: " "" d1104_537 ["char"])
                                                 let '-' = xx1103_538
                                                 return ()
                                                 ddd1105_539 <- get
                                                 do err <- ((do d1107_540 <- get
                                                                xx1106_541 <- StateT char
                                                                case xx1106_541 of
                                                                    '#' -> return ()
                                                                    _ -> gets position >>= (throwError . mkParseError "'#'" "not match pattern: " "" d1107_540 ["char"])
                                                                let '#' = xx1106_541
                                                                return ()) >> return False) `catchError` const (return True)
                                                    unless err (gets position >>= (throwError . mkParseError ('!' : "'#':") "not match: " "" ddd1105_539 ["char"]))
                                                 put ddd1105_539
                                                 _ <- StateT comments
                                                 return ()
                                                 _ <- StateT comEnd
                                                 return ()
                                                 return ()]
                comments77_151 = foldl1 mplus [do _ <- StateT notComStr
                                                  return ()
                                                  _ <- StateT comment
                                                  return ()
                                                  _ <- StateT comments
                                                  return ()
                                                  return (),
                                               do _ <- StateT notComStr
                                                  return ()
                                                  return ()]
                notComStr78_152 = foldl1 mplus [do ddd1120_542 <- get
                                                   do err <- ((do _ <- StateT comment
                                                                  return ()) >> return False) `catchError` const (return True)
                                                      unless err (gets position >>= (throwError . mkParseError ('!' : "_:comment") "not match: " "" ddd1120_542 ["comment"]))
                                                   put ddd1120_542
                                                   ddd1123_543 <- get
                                                   do err <- ((do _ <- StateT comEnd
                                                                  return ()) >> return False) `catchError` const (return True)
                                                      unless err (gets position >>= (throwError . mkParseError ('!' : "_:comEnd") "not match: " "" ddd1123_543 ["comEnd"]))
                                                   put ddd1123_543
                                                   _ <- StateT char
                                                   return ()
                                                   _ <- StateT notComStr
                                                   return ()
                                                   return (),
                                                return ()]
                comEnd79_153 = foldl1 mplus [do d1131_544 <- get
                                                xx1130_545 <- StateT char
                                                case xx1130_545 of
                                                    '-' -> return ()
                                                    _ -> gets position >>= (throwError . mkParseError "'-'" "not match pattern: " "" d1131_544 ["char"])
                                                let '-' = xx1130_545
                                                return ()
                                                d1133_546 <- get
                                                xx1132_547 <- StateT char
                                                case xx1132_547 of
                                                    '}' -> return ()
                                                    _ -> gets position >>= (throwError . mkParseError "'}'" "not match pattern: " "" d1133_546 ["char"])
                                                let '}' = xx1132_547
                                                return ()
                                                return ()]
                list1_322 :: forall m a . (MonadPlus m, Applicative m) =>
                                          m a -> m ([a])
                list12_416 :: forall m a . (MonadPlus m, Applicative m) =>
                                           m a -> m ([a])
                list1_322 p = list12_416 p `mplus` return []
                list12_416 p = ((:) <$> p) <*> list1_322 p
                optional3_325 :: forall m a . (MonadPlus m, Applicative m) =>
                                              m a -> m (Maybe a)
                optional3_325 p = (Just <$> p) `mplus` return Nothing

