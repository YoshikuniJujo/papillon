{-# LANGUAGE FlexibleContexts, TemplateHaskell, UndecidableInstances, PackageImports, TypeFamilies, RankNTypes #-}
module Text.Papillon.Parser (
	HA(..),
	Lists(..),

	Peg,
	Definition,
	Selection(..),
	Expression,
	PlainExpression,
	NameLeaf,
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
import "monads-tf" Control.Monad.State
import "monads-tf" Control.Monad.Error
import Control.Applicative



import Text.Papillon.SyntaxTree
import Language.Haskell.TH (
	Name, TypeQ, PatQ, ExpQ, mkName,
	conT, tupleT, listT, appT, arrowT,
	wildP, litP, varP, conP, tupP, listP, uInfixP,
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
              peg :: (Either (ParseError (Pos String) Derivs)
                             (((TypeQ, Peg), Derivs))),
              sourceType :: (Either (ParseError (Pos String) Derivs)
                                    ((String, Derivs))),
              peg_ :: (Either (ParseError (Pos String) Derivs) ((Peg, Derivs))),
              definition :: (Either (ParseError (Pos String) Derivs)
                                    ((Definition, Derivs))),
              selection :: (Either (ParseError (Pos String) Derivs)
                                   ((Selection, Derivs))),
              normalSelection :: (Either (ParseError (Pos String) Derivs)
                                         (([Expression], Derivs))),
              plainSelection :: (Either (ParseError (Pos String) Derivs)
                                        (([PlainExpression], Derivs))),
              expressionHs :: (Either (ParseError (Pos String) Derivs)
                                      ((Expression, Derivs))),
              expressionHsSugar :: (Either (ParseError (Pos String) Derivs)
                                           ((Expression, Derivs))),
              plainExpressionHs :: (Either (ParseError (Pos String) Derivs)
                                           ((PlainExpression, Derivs))),
              plainHAReadFromLs :: (Either (ParseError (Pos String) Derivs)
                                           (((HA, ReadFrom), Derivs))),
              plainReadFromLs :: (Either (ParseError (Pos String) Derivs)
                                         ((ReadFrom, Derivs))),
              expression :: (Either (ParseError (Pos String) Derivs)
                                    (([(HA, NameLeaf)], Derivs))),
              nameLeaf_ :: (Either (ParseError (Pos String) Derivs)
                                   (((HA, NameLeaf), Derivs))),
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
                              (([PatQ], Derivs))),
              readFromLs :: (Either (ParseError (Pos String) Derivs)
                                    ((ReadFrom, Derivs))),
              readFrom :: (Either (ParseError (Pos String) Derivs)
                                  ((ReadFrom, Derivs))),
              selectCharsLs :: (Either (ParseError (Pos String) Derivs)
                                       ((ReadFrom, Derivs))),
              selectChars :: (Either (ParseError (Pos String) Derivs)
                                     ((ReadFrom, Derivs))),
              test :: (Either (ParseError (Pos String) Derivs)
                              (((ExpQ, String), Derivs))),
              hsExpLam :: (Either (ParseError (Pos String) Derivs)
                                  ((ExpQ, Derivs))),
              hsExpTyp :: (Either (ParseError (Pos String) Derivs)
                                  ((ExpQ, Derivs))),
              hsExpOp :: (Either (ParseError (Pos String) Derivs)
                                 ((ExpQ, Derivs))),
              hsOp :: (Either (ParseError (Pos String) Derivs) ((ExpQ, Derivs))),
              opTail :: (Either (ParseError (Pos String) Derivs)
                                ((String, Derivs))),
              hsExp :: (Either (ParseError (Pos String) Derivs)
                               (((ExpQ -> ExpQ) -> ExpQ, Derivs))),
              hsExp1 :: (Either (ParseError (Pos String) Derivs)
                                ((ExpQ, Derivs))),
              hsExpTpl :: (Either (ParseError (Pos String) Derivs)
                                  (([ExpQ], Derivs))),
              hsTypeArr :: (Either (ParseError (Pos String) Derivs)
                                   ((TypeQ, Derivs))),
              hsType :: (Either (ParseError (Pos String) Derivs)
                                (((TypeQ -> TypeQ) -> TypeQ, Derivs))),
              hsType1 :: (Either (ParseError (Pos String) Derivs)
                                 ((TypeQ, Derivs))),
              hsTypeTpl :: (Either (ParseError (Pos String) Derivs)
                                   (([TypeQ], Derivs))),
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
                             where d = Derivs pegFile81_1 pragmas82_2 pragma83_3 pragmaStr84_4 pragmaItems85_5 pragmaEnd86_6 moduleDec87_7 moduleName88_8 moduleDecStr89_9 whr90_10 preImpPap91_11 prePeg92_12 afterPeg93_13 importPapillon94_14 varToken95_15 typToken96_16 pap97_17 peg98_18 sourceType99_19 peg_100_20 definition101_21 selection102_22 normalSelection103_23 plainSelection104_24 expressionHs105_25 expressionHsSugar106_26 plainExpressionHs107_27 plainHAReadFromLs108_28 plainReadFromLs109_29 expression110_30 nameLeaf_111_31 nameLeaf112_32 nameLeafNoCom113_33 comForErr114_34 leaf115_35 patOp116_36 pat117_37 pat1118_38 patList119_39 opConName120_40 charLit121_41 stringLit122_42 escapeC123_43 pats124_44 readFromLs125_45 readFrom126_46 selectCharsLs127_47 selectChars128_48 test129_49 hsExpLam130_50 hsExpTyp131_51 hsExpOp132_52 hsOp133_53 opTail134_54 hsExp135_55 hsExp1136_56 hsExpTpl137_57 hsTypeArr138_58 hsType139_59 hsType1140_60 hsTypeTpl141_61 typ142_62 variable143_63 tvtail144_64 integer145_65 alpha146_66 upper147_67 lower148_68 digit149_69 spaces150_70 space151_71 notNLString152_72 newLine153_73 comment154_74 comments155_75 notComStr156_76 comEnd157_77 chars158_78 pos
                                   pegFile81_1 = runStateT pegFile4_79 d
                                   pragmas82_2 = runStateT pragmas5_80 d
                                   pragma83_3 = runStateT pragma6_81 d
                                   pragmaStr84_4 = runStateT pragmaStr7_82 d
                                   pragmaItems85_5 = runStateT pragmaItems8_83 d
                                   pragmaEnd86_6 = runStateT pragmaEnd9_84 d
                                   moduleDec87_7 = runStateT moduleDec10_85 d
                                   moduleName88_8 = runStateT moduleName11_86 d
                                   moduleDecStr89_9 = runStateT moduleDecStr12_87 d
                                   whr90_10 = runStateT whr13_88 d
                                   preImpPap91_11 = runStateT preImpPap14_89 d
                                   prePeg92_12 = runStateT prePeg15_90 d
                                   afterPeg93_13 = runStateT afterPeg16_91 d
                                   importPapillon94_14 = runStateT importPapillon17_92 d
                                   varToken95_15 = runStateT varToken18_93 d
                                   typToken96_16 = runStateT typToken19_94 d
                                   pap97_17 = runStateT pap20_95 d
                                   peg98_18 = runStateT peg21_96 d
                                   sourceType99_19 = runStateT sourceType22_97 d
                                   peg_100_20 = runStateT peg_23_98 d
                                   definition101_21 = runStateT definition24_99 d
                                   selection102_22 = runStateT selection25_100 d
                                   normalSelection103_23 = runStateT normalSelection26_101 d
                                   plainSelection104_24 = runStateT plainSelection27_102 d
                                   expressionHs105_25 = runStateT expressionHs28_103 d
                                   expressionHsSugar106_26 = runStateT expressionHsSugar29_104 d
                                   plainExpressionHs107_27 = runStateT plainExpressionHs30_105 d
                                   plainHAReadFromLs108_28 = runStateT plainHAReadFromLs31_106 d
                                   plainReadFromLs109_29 = runStateT plainReadFromLs32_107 d
                                   expression110_30 = runStateT expression33_108 d
                                   nameLeaf_111_31 = runStateT nameLeaf_34_109 d
                                   nameLeaf112_32 = runStateT nameLeaf35_110 d
                                   nameLeafNoCom113_33 = runStateT nameLeafNoCom36_111 d
                                   comForErr114_34 = runStateT comForErr37_112 d
                                   leaf115_35 = runStateT leaf38_113 d
                                   patOp116_36 = runStateT patOp39_114 d
                                   pat117_37 = runStateT pat40_115 d
                                   pat1118_38 = runStateT pat141_116 d
                                   patList119_39 = runStateT patList42_117 d
                                   opConName120_40 = runStateT opConName43_118 d
                                   charLit121_41 = runStateT charLit44_119 d
                                   stringLit122_42 = runStateT stringLit45_120 d
                                   escapeC123_43 = runStateT escapeC46_121 d
                                   pats124_44 = runStateT pats47_122 d
                                   readFromLs125_45 = runStateT readFromLs48_123 d
                                   readFrom126_46 = runStateT readFrom49_124 d
                                   selectCharsLs127_47 = runStateT selectCharsLs50_125 d
                                   selectChars128_48 = runStateT selectChars51_126 d
                                   test129_49 = runStateT test52_127 d
                                   hsExpLam130_50 = runStateT hsExpLam53_128 d
                                   hsExpTyp131_51 = runStateT hsExpTyp54_129 d
                                   hsExpOp132_52 = runStateT hsExpOp55_130 d
                                   hsOp133_53 = runStateT hsOp56_131 d
                                   opTail134_54 = runStateT opTail57_132 d
                                   hsExp135_55 = runStateT hsExp58_133 d
                                   hsExp1136_56 = runStateT hsExp159_134 d
                                   hsExpTpl137_57 = runStateT hsExpTpl60_135 d
                                   hsTypeArr138_58 = runStateT hsTypeArr61_136 d
                                   hsType139_59 = runStateT hsType62_137 d
                                   hsType1140_60 = runStateT hsType163_138 d
                                   hsTypeTpl141_61 = runStateT hsTypeTpl64_139 d
                                   typ142_62 = runStateT typ65_140 d
                                   variable143_63 = runStateT variable66_141 d
                                   tvtail144_64 = runStateT tvtail67_142 d
                                   integer145_65 = runStateT integer68_143 d
                                   alpha146_66 = runStateT alpha69_144 d
                                   upper147_67 = runStateT upper70_145 d
                                   lower148_68 = runStateT lower71_146 d
                                   digit149_69 = runStateT digit72_147 d
                                   spaces150_70 = runStateT spaces73_148 d
                                   space151_71 = runStateT space74_149 d
                                   notNLString152_72 = runStateT notNLString75_150 d
                                   newLine153_73 = runStateT newLine76_151 d
                                   comment154_74 = runStateT comment77_152 d
                                   comments155_75 = runStateT comments78_153 d
                                   notComStr156_76 = runStateT notComStr79_154 d
                                   comEnd157_77 = runStateT comEnd80_155 d
                                   chars158_78 = runStateT (case getToken s of
                                                                Just (c,
                                                                      s') -> do put (parse0_0 (updatePos c pos) s')
                                                                                return c
                                                                _ -> gets position >>= (throwError . mkParseError "" "end of input" "" undefined [])) d
                pegFile4_79 = foldl1 mplus [do pr <- StateT pragmas
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
                                               d176_156 <- get
                                               xx175_157 <- StateT char
                                               case xx175_157 of
                                                   '|' -> return ()
                                                   _ -> gets position >>= (throwError . mkParseError "'|'" "not match pattern: " "" d176_156 ["char"])
                                               let '|' = xx175_157
                                               return ()
                                               d178_158 <- get
                                               xx177_159 <- StateT char
                                               case xx177_159 of
                                                   ']' -> return ()
                                                   _ -> gets position >>= (throwError . mkParseError "']'" "not match pattern: " "" d178_158 ["char"])
                                               let ']' = xx177_159
                                               return ()
                                               d180_160 <- get
                                               xx179_161 <- StateT char
                                               case xx179_161 of
                                                   '\n' -> return ()
                                                   _ -> gets position >>= (throwError . mkParseError "'\\n'" "not match pattern: " "" d180_160 ["char"])
                                               let '\n' = xx179_161
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
                                               d196_162 <- get
                                               xx195_163 <- StateT char
                                               case xx195_163 of
                                                   '|' -> return ()
                                                   _ -> gets position >>= (throwError . mkParseError "'|'" "not match pattern: " "" d196_162 ["char"])
                                               let '|' = xx195_163
                                               return ()
                                               d198_164 <- get
                                               xx197_165 <- StateT char
                                               case xx197_165 of
                                                   ']' -> return ()
                                                   _ -> gets position >>= (throwError . mkParseError "']'" "not match pattern: " "" d198_164 ["char"])
                                               let ']' = xx197_165
                                               return ()
                                               d200_166 <- get
                                               xx199_167 <- StateT char
                                               case xx199_167 of
                                                   '\n' -> return ()
                                                   _ -> gets position >>= (throwError . mkParseError "'\\n'" "not match pattern: " "" d200_166 ["char"])
                                               let '\n' = xx199_167
                                               return ()
                                               atp <- StateT afterPeg
                                               return (mkPegFile pr md [] pp p atp)]
                pragmas5_80 = foldl1 mplus [do _ <- StateT spaces
                                               return ()
                                               pr <- StateT pragma
                                               prs <- StateT pragmas
                                               return (pr : prs),
                                            do _ <- StateT spaces
                                               return ()
                                               return []]
                pragma6_81 = foldl1 mplus [do d212_168 <- get
                                              xx211_169 <- StateT char
                                              case xx211_169 of
                                                  '{' -> return ()
                                                  _ -> gets position >>= (throwError . mkParseError "'{'" "not match pattern: " "" d212_168 ["char"])
                                              let '{' = xx211_169
                                              return ()
                                              d214_170 <- get
                                              xx213_171 <- StateT char
                                              case xx213_171 of
                                                  '-' -> return ()
                                                  _ -> gets position >>= (throwError . mkParseError "'-'" "not match pattern: " "" d214_170 ["char"])
                                              let '-' = xx213_171
                                              return ()
                                              d216_172 <- get
                                              xx215_173 <- StateT char
                                              case xx215_173 of
                                                  '#' -> return ()
                                                  _ -> gets position >>= (throwError . mkParseError "'#'" "not match pattern: " "" d216_172 ["char"])
                                              let '#' = xx215_173
                                              return ()
                                              _ <- StateT spaces
                                              return ()
                                              d220_174 <- get
                                              xx219_175 <- StateT char
                                              case xx219_175 of
                                                  'L' -> return ()
                                                  _ -> gets position >>= (throwError . mkParseError "'L'" "not match pattern: " "" d220_174 ["char"])
                                              let 'L' = xx219_175
                                              return ()
                                              d222_176 <- get
                                              xx221_177 <- StateT char
                                              case xx221_177 of
                                                  'A' -> return ()
                                                  _ -> gets position >>= (throwError . mkParseError "'A'" "not match pattern: " "" d222_176 ["char"])
                                              let 'A' = xx221_177
                                              return ()
                                              d224_178 <- get
                                              xx223_179 <- StateT char
                                              case xx223_179 of
                                                  'N' -> return ()
                                                  _ -> gets position >>= (throwError . mkParseError "'N'" "not match pattern: " "" d224_178 ["char"])
                                              let 'N' = xx223_179
                                              return ()
                                              d226_180 <- get
                                              xx225_181 <- StateT char
                                              case xx225_181 of
                                                  'G' -> return ()
                                                  _ -> gets position >>= (throwError . mkParseError "'G'" "not match pattern: " "" d226_180 ["char"])
                                              let 'G' = xx225_181
                                              return ()
                                              d228_182 <- get
                                              xx227_183 <- StateT char
                                              case xx227_183 of
                                                  'U' -> return ()
                                                  _ -> gets position >>= (throwError . mkParseError "'U'" "not match pattern: " "" d228_182 ["char"])
                                              let 'U' = xx227_183
                                              return ()
                                              d230_184 <- get
                                              xx229_185 <- StateT char
                                              case xx229_185 of
                                                  'A' -> return ()
                                                  _ -> gets position >>= (throwError . mkParseError "'A'" "not match pattern: " "" d230_184 ["char"])
                                              let 'A' = xx229_185
                                              return ()
                                              d232_186 <- get
                                              xx231_187 <- StateT char
                                              case xx231_187 of
                                                  'G' -> return ()
                                                  _ -> gets position >>= (throwError . mkParseError "'G'" "not match pattern: " "" d232_186 ["char"])
                                              let 'G' = xx231_187
                                              return ()
                                              d234_188 <- get
                                              xx233_189 <- StateT char
                                              case xx233_189 of
                                                  'E' -> return ()
                                                  _ -> gets position >>= (throwError . mkParseError "'E'" "not match pattern: " "" d234_188 ["char"])
                                              let 'E' = xx233_189
                                              return ()
                                              _ <- StateT spaces
                                              return ()
                                              s <- StateT pragmaItems
                                              _ <- StateT pragmaEnd
                                              return ()
                                              _ <- StateT spaces
                                              return ()
                                              return (LanguagePragma s),
                                           do d244_190 <- get
                                              xx243_191 <- StateT char
                                              case xx243_191 of
                                                  '{' -> return ()
                                                  _ -> gets position >>= (throwError . mkParseError "'{'" "not match pattern: " "" d244_190 ["char"])
                                              let '{' = xx243_191
                                              return ()
                                              d246_192 <- get
                                              xx245_193 <- StateT char
                                              case xx245_193 of
                                                  '-' -> return ()
                                                  _ -> gets position >>= (throwError . mkParseError "'-'" "not match pattern: " "" d246_192 ["char"])
                                              let '-' = xx245_193
                                              return ()
                                              d248_194 <- get
                                              xx247_195 <- StateT char
                                              case xx247_195 of
                                                  '#' -> return ()
                                                  _ -> gets position >>= (throwError . mkParseError "'#'" "not match pattern: " "" d248_194 ["char"])
                                              let '#' = xx247_195
                                              return ()
                                              _ <- StateT spaces
                                              return ()
                                              s <- StateT pragmaStr
                                              _ <- StateT pragmaEnd
                                              return ()
                                              return (OtherPragma s)]
                pragmaStr7_82 = foldl1 mplus [do ddd255_196 <- get
                                                 do err <- ((do _ <- StateT pragmaEnd
                                                                return ()) >> return False) `catchError` const (return True)
                                                    unless err (gets position >>= (throwError . mkParseError ('!' : "_:pragmaEnd") "not match: " "" ddd255_196 ["pragmaEnd"]))
                                                 put ddd255_196
                                                 c <- StateT char
                                                 s <- StateT pragmaStr
                                                 return (c : s),
                                              return ""]
                pragmaItems8_83 = foldl1 mplus [do t <- StateT typToken
                                                   d265_197 <- get
                                                   xx264_198 <- StateT char
                                                   case xx264_198 of
                                                       ',' -> return ()
                                                       _ -> gets position >>= (throwError . mkParseError "','" "not match pattern: " "" d265_197 ["char"])
                                                   let ',' = xx264_198
                                                   return ()
                                                   _ <- StateT spaces
                                                   return ()
                                                   i <- StateT pragmaItems
                                                   return (t : i),
                                                do t <- StateT typToken
                                                   return [t]]
                pragmaEnd9_84 = foldl1 mplus [do _ <- StateT spaces
                                                 return ()
                                                 d275_199 <- get
                                                 xx274_200 <- StateT char
                                                 case xx274_200 of
                                                     '#' -> return ()
                                                     _ -> gets position >>= (throwError . mkParseError "'#'" "not match pattern: " "" d275_199 ["char"])
                                                 let '#' = xx274_200
                                                 return ()
                                                 d277_201 <- get
                                                 xx276_202 <- StateT char
                                                 case xx276_202 of
                                                     '-' -> return ()
                                                     _ -> gets position >>= (throwError . mkParseError "'-'" "not match pattern: " "" d277_201 ["char"])
                                                 let '-' = xx276_202
                                                 return ()
                                                 d279_203 <- get
                                                 xx278_204 <- StateT char
                                                 case xx278_204 of
                                                     '}' -> return ()
                                                     _ -> gets position >>= (throwError . mkParseError "'}'" "not match pattern: " "" d279_203 ["char"])
                                                 let '}' = xx278_204
                                                 return ()
                                                 return ()]
                moduleDec10_85 = foldl1 mplus [do d281_205 <- get
                                                  xx280_206 <- StateT char
                                                  case xx280_206 of
                                                      'm' -> return ()
                                                      _ -> gets position >>= (throwError . mkParseError "'m'" "not match pattern: " "" d281_205 ["char"])
                                                  let 'm' = xx280_206
                                                  return ()
                                                  d283_207 <- get
                                                  xx282_208 <- StateT char
                                                  case xx282_208 of
                                                      'o' -> return ()
                                                      _ -> gets position >>= (throwError . mkParseError "'o'" "not match pattern: " "" d283_207 ["char"])
                                                  let 'o' = xx282_208
                                                  return ()
                                                  d285_209 <- get
                                                  xx284_210 <- StateT char
                                                  case xx284_210 of
                                                      'd' -> return ()
                                                      _ -> gets position >>= (throwError . mkParseError "'d'" "not match pattern: " "" d285_209 ["char"])
                                                  let 'd' = xx284_210
                                                  return ()
                                                  d287_211 <- get
                                                  xx286_212 <- StateT char
                                                  case xx286_212 of
                                                      'u' -> return ()
                                                      _ -> gets position >>= (throwError . mkParseError "'u'" "not match pattern: " "" d287_211 ["char"])
                                                  let 'u' = xx286_212
                                                  return ()
                                                  d289_213 <- get
                                                  xx288_214 <- StateT char
                                                  case xx288_214 of
                                                      'l' -> return ()
                                                      _ -> gets position >>= (throwError . mkParseError "'l'" "not match pattern: " "" d289_213 ["char"])
                                                  let 'l' = xx288_214
                                                  return ()
                                                  d291_215 <- get
                                                  xx290_216 <- StateT char
                                                  case xx290_216 of
                                                      'e' -> return ()
                                                      _ -> gets position >>= (throwError . mkParseError "'e'" "not match pattern: " "" d291_215 ["char"])
                                                  let 'e' = xx290_216
                                                  return ()
                                                  _ <- StateT spaces
                                                  return ()
                                                  n <- StateT moduleName
                                                  _ <- StateT spaces
                                                  return ()
                                                  d299_217 <- get
                                                  xx298_218 <- StateT char
                                                  case xx298_218 of
                                                      '(' -> return ()
                                                      _ -> gets position >>= (throwError . mkParseError "'('" "not match pattern: " "" d299_217 ["char"])
                                                  let '(' = xx298_218
                                                  return ()
                                                  _ <- StateT spaces
                                                  return ()
                                                  s <- StateT moduleDecStr
                                                  _ <- StateT whr
                                                  return ()
                                                  return (Just (n, Just s)),
                                               do d307_219 <- get
                                                  xx306_220 <- StateT char
                                                  case xx306_220 of
                                                      'm' -> return ()
                                                      _ -> gets position >>= (throwError . mkParseError "'m'" "not match pattern: " "" d307_219 ["char"])
                                                  let 'm' = xx306_220
                                                  return ()
                                                  d309_221 <- get
                                                  xx308_222 <- StateT char
                                                  case xx308_222 of
                                                      'o' -> return ()
                                                      _ -> gets position >>= (throwError . mkParseError "'o'" "not match pattern: " "" d309_221 ["char"])
                                                  let 'o' = xx308_222
                                                  return ()
                                                  d311_223 <- get
                                                  xx310_224 <- StateT char
                                                  case xx310_224 of
                                                      'd' -> return ()
                                                      _ -> gets position >>= (throwError . mkParseError "'d'" "not match pattern: " "" d311_223 ["char"])
                                                  let 'd' = xx310_224
                                                  return ()
                                                  d313_225 <- get
                                                  xx312_226 <- StateT char
                                                  case xx312_226 of
                                                      'u' -> return ()
                                                      _ -> gets position >>= (throwError . mkParseError "'u'" "not match pattern: " "" d313_225 ["char"])
                                                  let 'u' = xx312_226
                                                  return ()
                                                  d315_227 <- get
                                                  xx314_228 <- StateT char
                                                  case xx314_228 of
                                                      'l' -> return ()
                                                      _ -> gets position >>= (throwError . mkParseError "'l'" "not match pattern: " "" d315_227 ["char"])
                                                  let 'l' = xx314_228
                                                  return ()
                                                  d317_229 <- get
                                                  xx316_230 <- StateT char
                                                  case xx316_230 of
                                                      'e' -> return ()
                                                      _ -> gets position >>= (throwError . mkParseError "'e'" "not match pattern: " "" d317_229 ["char"])
                                                  let 'e' = xx316_230
                                                  return ()
                                                  _ <- StateT spaces
                                                  return ()
                                                  n <- StateT moduleName
                                                  _ <- StateT spaces
                                                  return ()
                                                  d325_231 <- get
                                                  xx324_232 <- StateT char
                                                  case xx324_232 of
                                                      'w' -> return ()
                                                      _ -> gets position >>= (throwError . mkParseError "'w'" "not match pattern: " "" d325_231 ["char"])
                                                  let 'w' = xx324_232
                                                  return ()
                                                  d327_233 <- get
                                                  xx326_234 <- StateT char
                                                  case xx326_234 of
                                                      'h' -> return ()
                                                      _ -> gets position >>= (throwError . mkParseError "'h'" "not match pattern: " "" d327_233 ["char"])
                                                  let 'h' = xx326_234
                                                  return ()
                                                  d329_235 <- get
                                                  xx328_236 <- StateT char
                                                  case xx328_236 of
                                                      'e' -> return ()
                                                      _ -> gets position >>= (throwError . mkParseError "'e'" "not match pattern: " "" d329_235 ["char"])
                                                  let 'e' = xx328_236
                                                  return ()
                                                  d331_237 <- get
                                                  xx330_238 <- StateT char
                                                  case xx330_238 of
                                                      'r' -> return ()
                                                      _ -> gets position >>= (throwError . mkParseError "'r'" "not match pattern: " "" d331_237 ["char"])
                                                  let 'r' = xx330_238
                                                  return ()
                                                  d333_239 <- get
                                                  xx332_240 <- StateT char
                                                  case xx332_240 of
                                                      'e' -> return ()
                                                      _ -> gets position >>= (throwError . mkParseError "'e'" "not match pattern: " "" d333_239 ["char"])
                                                  let 'e' = xx332_240
                                                  return ()
                                                  _ <- StateT spaces
                                                  return ()
                                                  return (Just (n, Nothing)),
                                               return Nothing]
                moduleName11_86 = foldl1 mplus [do t <- StateT typ
                                                   d339_241 <- get
                                                   xx338_242 <- StateT char
                                                   case xx338_242 of
                                                       '.' -> return ()
                                                       _ -> gets position >>= (throwError . mkParseError "'.'" "not match pattern: " "" d339_241 ["char"])
                                                   let '.' = xx338_242
                                                   return ()
                                                   n <- StateT moduleName
                                                   return (t : n),
                                                do t <- StateT typ
                                                   return [t]]
                moduleDecStr12_87 = foldl1 mplus [do ddd344_243 <- get
                                                     do err <- ((do _ <- StateT whr
                                                                    return ()) >> return False) `catchError` const (return True)
                                                        unless err (gets position >>= (throwError . mkParseError ('!' : "_:whr") "not match: " "" ddd344_243 ["whr"]))
                                                     put ddd344_243
                                                     c <- StateT char
                                                     s <- StateT moduleDecStr
                                                     return (c : s),
                                                  return ""]
                whr13_88 = foldl1 mplus [do _ <- StateT spaces
                                            return ()
                                            d354_244 <- get
                                            xx353_245 <- StateT char
                                            case xx353_245 of
                                                ')' -> return ()
                                                _ -> gets position >>= (throwError . mkParseError "')'" "not match pattern: " "" d354_244 ["char"])
                                            let ')' = xx353_245
                                            return ()
                                            _ <- StateT spaces
                                            return ()
                                            d358_246 <- get
                                            xx357_247 <- StateT char
                                            case xx357_247 of
                                                'w' -> return ()
                                                _ -> gets position >>= (throwError . mkParseError "'w'" "not match pattern: " "" d358_246 ["char"])
                                            let 'w' = xx357_247
                                            return ()
                                            d360_248 <- get
                                            xx359_249 <- StateT char
                                            case xx359_249 of
                                                'h' -> return ()
                                                _ -> gets position >>= (throwError . mkParseError "'h'" "not match pattern: " "" d360_248 ["char"])
                                            let 'h' = xx359_249
                                            return ()
                                            d362_250 <- get
                                            xx361_251 <- StateT char
                                            case xx361_251 of
                                                'e' -> return ()
                                                _ -> gets position >>= (throwError . mkParseError "'e'" "not match pattern: " "" d362_250 ["char"])
                                            let 'e' = xx361_251
                                            return ()
                                            d364_252 <- get
                                            xx363_253 <- StateT char
                                            case xx363_253 of
                                                'r' -> return ()
                                                _ -> gets position >>= (throwError . mkParseError "'r'" "not match pattern: " "" d364_252 ["char"])
                                            let 'r' = xx363_253
                                            return ()
                                            d366_254 <- get
                                            xx365_255 <- StateT char
                                            case xx365_255 of
                                                'e' -> return ()
                                                _ -> gets position >>= (throwError . mkParseError "'e'" "not match pattern: " "" d366_254 ["char"])
                                            let 'e' = xx365_255
                                            return ()
                                            return ()]
                preImpPap14_89 = foldl1 mplus [do ddd367_256 <- get
                                                  do err <- ((do _ <- StateT importPapillon
                                                                 return ()) >> return False) `catchError` const (return True)
                                                     unless err (gets position >>= (throwError . mkParseError ('!' : "_:importPapillon") "not match: " "" ddd367_256 ["importPapillon"]))
                                                  put ddd367_256
                                                  ddd370_257 <- get
                                                  do err <- ((do _ <- StateT pap
                                                                 return ()) >> return False) `catchError` const (return True)
                                                     unless err (gets position >>= (throwError . mkParseError ('!' : "_:pap") "not match: " "" ddd370_257 ["pap"]))
                                                  put ddd370_257
                                                  c <- StateT char
                                                  pip <- StateT preImpPap
                                                  return (c : pip),
                                               return ""]
                prePeg15_90 = foldl1 mplus [do ddd377_258 <- get
                                               do err <- ((do _ <- StateT pap
                                                              return ()) >> return False) `catchError` const (return True)
                                                  unless err (gets position >>= (throwError . mkParseError ('!' : "_:pap") "not match: " "" ddd377_258 ["pap"]))
                                               put ddd377_258
                                               c <- StateT char
                                               pp <- StateT prePeg
                                               return (c : pp),
                                            return ""]
                afterPeg16_91 = foldl1 mplus [do c <- StateT char
                                                 atp <- StateT afterPeg
                                                 return (c : atp),
                                              return ""]
                importPapillon17_92 = foldl1 mplus [do d389_259 <- get
                                                       xx388_260 <- StateT varToken
                                                       case xx388_260 of
                                                           "import" -> return ()
                                                           _ -> gets position >>= (throwError . mkParseError "\"import\"" "not match pattern: " "" d389_259 ["varToken"])
                                                       let "import" = xx388_260
                                                       return ()
                                                       d391_261 <- get
                                                       xx390_262 <- StateT typToken
                                                       case xx390_262 of
                                                           "Text" -> return ()
                                                           _ -> gets position >>= (throwError . mkParseError "\"Text\"" "not match pattern: " "" d391_261 ["typToken"])
                                                       let "Text" = xx390_262
                                                       return ()
                                                       d393_263 <- get
                                                       xx392_264 <- StateT char
                                                       case xx392_264 of
                                                           '.' -> return ()
                                                           _ -> gets position >>= (throwError . mkParseError "'.'" "not match pattern: " "" d393_263 ["char"])
                                                       let '.' = xx392_264
                                                       return ()
                                                       _ <- StateT spaces
                                                       return ()
                                                       d397_265 <- get
                                                       xx396_266 <- StateT typToken
                                                       case xx396_266 of
                                                           "Papillon" -> return ()
                                                           _ -> gets position >>= (throwError . mkParseError "\"Papillon\"" "not match pattern: " "" d397_265 ["typToken"])
                                                       let "Papillon" = xx396_266
                                                       return ()
                                                       ddd398_267 <- get
                                                       do err <- ((do d400_268 <- get
                                                                      xx399_269 <- StateT char
                                                                      case xx399_269 of
                                                                          '.' -> return ()
                                                                          _ -> gets position >>= (throwError . mkParseError "'.'" "not match pattern: " "" d400_268 ["char"])
                                                                      let '.' = xx399_269
                                                                      return ()) >> return False) `catchError` const (return True)
                                                          unless err (gets position >>= (throwError . mkParseError ('!' : "'.':") "not match: " "" ddd398_267 ["char"]))
                                                       put ddd398_267
                                                       return ()]
                varToken18_93 = foldl1 mplus [do v <- StateT variable
                                                 _ <- StateT spaces
                                                 return ()
                                                 return v]
                typToken19_94 = foldl1 mplus [do t <- StateT typ
                                                 _ <- StateT spaces
                                                 return ()
                                                 return t]
                pap20_95 = foldl1 mplus [do d410_270 <- get
                                            xx409_271 <- StateT char
                                            case xx409_271 of
                                                '\n' -> return ()
                                                _ -> gets position >>= (throwError . mkParseError "'\\n'" "not match pattern: " "" d410_270 ["char"])
                                            let '\n' = xx409_271
                                            return ()
                                            d412_272 <- get
                                            xx411_273 <- StateT char
                                            case xx411_273 of
                                                '[' -> return ()
                                                _ -> gets position >>= (throwError . mkParseError "'['" "not match pattern: " "" d412_272 ["char"])
                                            let '[' = xx411_273
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
                                                'a' -> return ()
                                                _ -> gets position >>= (throwError . mkParseError "'a'" "not match pattern: " "" d416_276 ["char"])
                                            let 'a' = xx415_277
                                            return ()
                                            d418_278 <- get
                                            xx417_279 <- StateT char
                                            case xx417_279 of
                                                'p' -> return ()
                                                _ -> gets position >>= (throwError . mkParseError "'p'" "not match pattern: " "" d418_278 ["char"])
                                            let 'p' = xx417_279
                                            return ()
                                            d420_280 <- get
                                            xx419_281 <- StateT char
                                            case xx419_281 of
                                                'i' -> return ()
                                                _ -> gets position >>= (throwError . mkParseError "'i'" "not match pattern: " "" d420_280 ["char"])
                                            let 'i' = xx419_281
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
                                                'l' -> return ()
                                                _ -> gets position >>= (throwError . mkParseError "'l'" "not match pattern: " "" d424_284 ["char"])
                                            let 'l' = xx423_285
                                            return ()
                                            d426_286 <- get
                                            xx425_287 <- StateT char
                                            case xx425_287 of
                                                'o' -> return ()
                                                _ -> gets position >>= (throwError . mkParseError "'o'" "not match pattern: " "" d426_286 ["char"])
                                            let 'o' = xx425_287
                                            return ()
                                            d428_288 <- get
                                            xx427_289 <- StateT char
                                            case xx427_289 of
                                                'n' -> return ()
                                                _ -> gets position >>= (throwError . mkParseError "'n'" "not match pattern: " "" d428_288 ["char"])
                                            let 'n' = xx427_289
                                            return ()
                                            d430_290 <- get
                                            xx429_291 <- StateT char
                                            case xx429_291 of
                                                '|' -> return ()
                                                _ -> gets position >>= (throwError . mkParseError "'|'" "not match pattern: " "" d430_290 ["char"])
                                            let '|' = xx429_291
                                            return ()
                                            d432_292 <- get
                                            xx431_293 <- StateT char
                                            case xx431_293 of
                                                '\n' -> return ()
                                                _ -> gets position >>= (throwError . mkParseError "'\\n'" "not match pattern: " "" d432_292 ["char"])
                                            let '\n' = xx431_293
                                            return ()
                                            return ()]
                peg21_96 = foldl1 mplus [do _ <- StateT spaces
                                            return ()
                                            s <- StateT sourceType
                                            p <- StateT peg_
                                            return (conT $ mkName s, p),
                                         do p <- StateT peg_
                                            return (conT $ mkName "String", p)]
                sourceType22_97 = foldl1 mplus [do d442_294 <- get
                                                   xx441_295 <- StateT varToken
                                                   case xx441_295 of
                                                       "source" -> return ()
                                                       _ -> gets position >>= (throwError . mkParseError "\"source\"" "not match pattern: " "" d442_294 ["varToken"])
                                                   let "source" = xx441_295
                                                   return ()
                                                   d444_296 <- get
                                                   xx443_297 <- StateT char
                                                   case xx443_297 of
                                                       ':' -> return ()
                                                       _ -> gets position >>= (throwError . mkParseError "':'" "not match pattern: " "" d444_296 ["char"])
                                                   let ':' = xx443_297
                                                   return ()
                                                   _ <- StateT spaces
                                                   return ()
                                                   v <- StateT typToken
                                                   return v]
                peg_23_98 = foldl1 mplus [do _ <- StateT spaces
                                             return ()
                                             d <- StateT definition
                                             p <- StateT peg_
                                             return (d : p),
                                          return []]
                definition24_99 = foldl1 mplus [do v <- StateT variable
                                                   _ <- StateT spaces
                                                   return ()
                                                   d460_298 <- get
                                                   xx459_299 <- StateT char
                                                   case xx459_299 of
                                                       ':' -> return ()
                                                       _ -> gets position >>= (throwError . mkParseError "':'" "not match pattern: " "" d460_298 ["char"])
                                                   let ':' = xx459_299
                                                   return ()
                                                   d462_300 <- get
                                                   xx461_301 <- StateT char
                                                   case xx461_301 of
                                                       ':' -> return ()
                                                       _ -> gets position >>= (throwError . mkParseError "':'" "not match pattern: " "" d462_300 ["char"])
                                                   let ':' = xx461_301
                                                   return ()
                                                   _ <- StateT spaces
                                                   return ()
                                                   t <- StateT hsTypeArr
                                                   _ <- StateT spaces
                                                   return ()
                                                   d470_302 <- get
                                                   xx469_303 <- StateT char
                                                   case xx469_303 of
                                                       '=' -> return ()
                                                       _ -> gets position >>= (throwError . mkParseError "'='" "not match pattern: " "" d470_302 ["char"])
                                                   let '=' = xx469_303
                                                   return ()
                                                   _ <- StateT spaces
                                                   return ()
                                                   sel <- StateT selection
                                                   _ <- StateT spaces
                                                   return ()
                                                   d478_304 <- get
                                                   xx477_305 <- StateT char
                                                   case xx477_305 of
                                                       ';' -> return ()
                                                       _ -> gets position >>= (throwError . mkParseError "';'" "not match pattern: " "" d478_304 ["char"])
                                                   let ';' = xx477_305
                                                   return ()
                                                   return (v, Just t, sel),
                                                do v <- StateT variable
                                                   _ <- StateT spaces
                                                   return ()
                                                   d484_306 <- get
                                                   xx483_307 <- StateT char
                                                   case xx483_307 of
                                                       '<' -> return ()
                                                       _ -> gets position >>= (throwError . mkParseError "'<'" "not match pattern: " "" d484_306 ["char"])
                                                   let '<' = xx483_307
                                                   return ()
                                                   d486_308 <- get
                                                   xx485_309 <- StateT char
                                                   case xx485_309 of
                                                       '-' -> return ()
                                                       _ -> gets position >>= (throwError . mkParseError "'-'" "not match pattern: " "" d486_308 ["char"])
                                                   let '-' = xx485_309
                                                   return ()
                                                   _ <- StateT spaces
                                                   return ()
                                                   sel <- StateT plainSelection
                                                   _ <- StateT spaces
                                                   return ()
                                                   d494_310 <- get
                                                   xx493_311 <- StateT char
                                                   case xx493_311 of
                                                       ';' -> return ()
                                                       _ -> gets position >>= (throwError . mkParseError "';'" "not match pattern: " "" d494_310 ["char"])
                                                   let ';' = xx493_311
                                                   return ()
                                                   return (v, Nothing, PlainSelection sel)]
                selection25_100 = foldl1 mplus [do s <- StateT normalSelection
                                                   return (Selection s),
                                                do s <- StateT plainSelection
                                                   return (PlainSelection s)]
                normalSelection26_101 = foldl1 mplus [do ex <- StateT expressionHs
                                                         _ <- StateT spaces
                                                         return ()
                                                         d504_312 <- get
                                                         xx503_313 <- StateT char
                                                         case xx503_313 of
                                                             '/' -> return ()
                                                             _ -> gets position >>= (throwError . mkParseError "'/'" "not match pattern: " "" d504_312 ["char"])
                                                         let '/' = xx503_313
                                                         return ()
                                                         _ <- StateT spaces
                                                         return ()
                                                         sel <- StateT normalSelection
                                                         return (ex : sel),
                                                      do ex <- StateT expressionHs
                                                         return [ex]]
                plainSelection27_102 = foldl1 mplus [do ex <- StateT plainExpressionHs
                                                        _ <- StateT spaces
                                                        return ()
                                                        d516_314 <- get
                                                        xx515_315 <- StateT char
                                                        case xx515_315 of
                                                            '/' -> return ()
                                                            _ -> gets position >>= (throwError . mkParseError "'/'" "not match pattern: " "" d516_314 ["char"])
                                                        let '/' = xx515_315
                                                        return ()
                                                        _ <- StateT spaces
                                                        return ()
                                                        sel <- StateT plainSelection
                                                        return (ex : sel),
                                                     do ex <- StateT plainExpressionHs
                                                        return [ex]]
                expressionHs28_103 = foldl1 mplus [do e <- StateT expression
                                                      _ <- StateT spaces
                                                      return ()
                                                      d528_316 <- get
                                                      xx527_317 <- StateT char
                                                      case xx527_317 of
                                                          '{' -> return ()
                                                          _ -> gets position >>= (throwError . mkParseError "'{'" "not match pattern: " "" d528_316 ["char"])
                                                      let '{' = xx527_317
                                                      return ()
                                                      _ <- StateT spaces
                                                      return ()
                                                      h <- StateT hsExpLam
                                                      _ <- StateT spaces
                                                      return ()
                                                      d536_318 <- get
                                                      xx535_319 <- StateT char
                                                      case xx535_319 of
                                                          '}' -> return ()
                                                          _ -> gets position >>= (throwError . mkParseError "'}'" "not match pattern: " "" d536_318 ["char"])
                                                      let '}' = xx535_319
                                                      return ()
                                                      return (False, const (e, h)),
                                                   do e <- StateT expressionHsSugar
                                                      return e]
                expressionHsSugar29_104 = foldl1 mplus [do d540_320 <- get
                                                           xx539_321 <- StateT char
                                                           case xx539_321 of
                                                               '<' -> return ()
                                                               _ -> gets position >>= (throwError . mkParseError "'<'" "not match pattern: " "" d540_320 ["char"])
                                                           let '<' = xx539_321
                                                           return ()
                                                           _ <- StateT spaces
                                                           return ()
                                                           h <- StateT hsExpLam
                                                           _ <- StateT spaces
                                                           return ()
                                                           d548_322 <- get
                                                           xx547_323 <- StateT char
                                                           case xx547_323 of
                                                               '>' -> return ()
                                                               _ -> gets position >>= (throwError . mkParseError "'>'" "not match pattern: " "" d548_322 ["char"])
                                                           let '>' = xx547_323
                                                           return ()
                                                           return (expressionSugar h)]
                plainExpressionHs30_105 = foldl1 mplus [do rfs <- list1_324 (foldl1 mplus [do rf <- StateT plainHAReadFromLs
                                                                                              _ <- StateT spaces
                                                                                              return ()
                                                                                              return rf])
                                                           return rfs]
                plainHAReadFromLs31_106 = foldl1 mplus [do rf <- StateT plainReadFromLs
                                                           return (Here, rf),
                                                        do d558_325 <- get
                                                           xx557_326 <- StateT char
                                                           case xx557_326 of
                                                               '&' -> return ()
                                                               _ -> gets position >>= (throwError . mkParseError "'&'" "not match pattern: " "" d558_325 ["char"])
                                                           let '&' = xx557_326
                                                           return ()
                                                           rf <- StateT plainReadFromLs
                                                           return (After, rf),
                                                        do d562_327 <- get
                                                           xx561_328 <- StateT char
                                                           case xx561_328 of
                                                               '!' -> return ()
                                                               _ -> gets position >>= (throwError . mkParseError "'!'" "not match pattern: " "" d562_327 ["char"])
                                                           let '!' = xx561_328
                                                           return ()
                                                           rf <- StateT plainReadFromLs
                                                           return (NotAfter "", rf)]
                plainReadFromLs32_107 = foldl1 mplus [do rf <- StateT readFromLs
                                                         return rf,
                                                      do rf <- StateT selectCharsLs
                                                         return rf]
                expression33_108 = foldl1 mplus [do l <- StateT nameLeaf_
                                                    _ <- StateT spaces
                                                    return ()
                                                    e <- StateT expression
                                                    return (l : e),
                                                 return []]
                nameLeaf_34_109 = foldl1 mplus [do d576_329 <- get
                                                   xx575_330 <- StateT char
                                                   case xx575_330 of
                                                       '!' -> return ()
                                                       _ -> gets position >>= (throwError . mkParseError "'!'" "not match pattern: " "" d576_329 ["char"])
                                                   let '!' = xx575_330
                                                   return ()
                                                   nl <- StateT nameLeafNoCom
                                                   _ <- StateT spaces
                                                   return ()
                                                   com <- optional3_331 (StateT comForErr)
                                                   return (NotAfter $ maybe "" id com, nl),
                                                do d584_332 <- get
                                                   xx583_333 <- StateT char
                                                   case xx583_333 of
                                                       '&' -> return ()
                                                       _ -> gets position >>= (throwError . mkParseError "'&'" "not match pattern: " "" d584_332 ["char"])
                                                   let '&' = xx583_333
                                                   return ()
                                                   nl <- StateT nameLeaf
                                                   return (After, nl),
                                                do nl <- StateT nameLeaf
                                                   return (Here, nl)]
                nameLeaf35_110 = foldl1 mplus [do n <- StateT pat1
                                                  _ <- StateT spaces
                                                  return ()
                                                  com <- optional3_331 (StateT comForErr)
                                                  d596_334 <- get
                                                  xx595_335 <- StateT char
                                                  case xx595_335 of
                                                      ':' -> return ()
                                                      _ -> gets position >>= (throwError . mkParseError "':'" "not match pattern: " "" d596_334 ["char"])
                                                  let ':' = xx595_335
                                                  return ()
                                                  (rf, p) <- StateT leaf
                                                  return ((n, maybe "" id com), rf, p),
                                               do n <- StateT pat1
                                                  _ <- StateT spaces
                                                  return ()
                                                  com <- optional3_331 (StateT comForErr)
                                                  return ((n, maybe "" id com),
                                                          (FromVariable Nothing),
                                                          Nothing)]
                nameLeafNoCom36_111 = foldl1 mplus [do n <- StateT pat1
                                                       _ <- StateT spaces
                                                       return ()
                                                       com <- optional3_331 (StateT comForErr)
                                                       d612_336 <- get
                                                       xx611_337 <- StateT char
                                                       case xx611_337 of
                                                           ':' -> return ()
                                                           _ -> gets position >>= (throwError . mkParseError "':'" "not match pattern: " "" d612_336 ["char"])
                                                       let ':' = xx611_337
                                                       return ()
                                                       (rf, p) <- StateT leaf
                                                       return ((n, maybe "" id com), rf, p),
                                                    do n <- StateT pat1
                                                       _ <- StateT spaces
                                                       return ()
                                                       return ((n, ""),
                                                               (FromVariable Nothing),
                                                               Nothing)]
                comForErr37_112 = foldl1 mplus [do d620_338 <- get
                                                   xx619_339 <- StateT char
                                                   case xx619_339 of
                                                       '{' -> return ()
                                                       _ -> gets position >>= (throwError . mkParseError "'{'" "not match pattern: " "" d620_338 ["char"])
                                                   let '{' = xx619_339
                                                   return ()
                                                   d622_340 <- get
                                                   xx621_341 <- StateT char
                                                   case xx621_341 of
                                                       '-' -> return ()
                                                       _ -> gets position >>= (throwError . mkParseError "'-'" "not match pattern: " "" d622_340 ["char"])
                                                   let '-' = xx621_341
                                                   return ()
                                                   d624_342 <- get
                                                   xx623_343 <- StateT char
                                                   case xx623_343 of
                                                       '#' -> return ()
                                                       _ -> gets position >>= (throwError . mkParseError "'#'" "not match pattern: " "" d624_342 ["char"])
                                                   let '#' = xx623_343
                                                   return ()
                                                   _ <- StateT spaces
                                                   return ()
                                                   d628_344 <- get
                                                   xx627_345 <- StateT char
                                                   case xx627_345 of
                                                       '"' -> return ()
                                                       _ -> gets position >>= (throwError . mkParseError "'\"'" "not match pattern: " "" d628_344 ["char"])
                                                   let '"' = xx627_345
                                                   return ()
                                                   s <- StateT stringLit
                                                   d632_346 <- get
                                                   xx631_347 <- StateT char
                                                   case xx631_347 of
                                                       '"' -> return ()
                                                       _ -> gets position >>= (throwError . mkParseError "'\"'" "not match pattern: " "" d632_346 ["char"])
                                                   let '"' = xx631_347
                                                   return ()
                                                   _ <- StateT spaces
                                                   return ()
                                                   d636_348 <- get
                                                   xx635_349 <- StateT char
                                                   case xx635_349 of
                                                       '#' -> return ()
                                                       _ -> gets position >>= (throwError . mkParseError "'#'" "not match pattern: " "" d636_348 ["char"])
                                                   let '#' = xx635_349
                                                   return ()
                                                   d638_350 <- get
                                                   xx637_351 <- StateT char
                                                   case xx637_351 of
                                                       '-' -> return ()
                                                       _ -> gets position >>= (throwError . mkParseError "'-'" "not match pattern: " "" d638_350 ["char"])
                                                   let '-' = xx637_351
                                                   return ()
                                                   d640_352 <- get
                                                   xx639_353 <- StateT char
                                                   case xx639_353 of
                                                       '}' -> return ()
                                                       _ -> gets position >>= (throwError . mkParseError "'}'" "not match pattern: " "" d640_352 ["char"])
                                                   let '}' = xx639_353
                                                   return ()
                                                   _ <- StateT spaces
                                                   return ()
                                                   return s]
                leaf38_113 = foldl1 mplus [do rf <- StateT readFromLs
                                              t <- StateT test
                                              return (rf, Just t),
                                           do rf <- StateT readFromLs
                                              return (rf, Nothing),
                                           do t <- StateT test
                                              return (FromVariable Nothing, Just t)]
                patOp39_114 = foldl1 mplus [do p <- StateT pat
                                               o <- StateT opConName
                                               po <- StateT patOp
                                               return (uInfixP p o po),
                                            do p <- StateT pat
                                               _ <- StateT spaces
                                               return ()
                                               d662_354 <- get
                                               xx661_355 <- StateT char
                                               case xx661_355 of
                                                   '`' -> return ()
                                                   _ -> gets position >>= (throwError . mkParseError "'`'" "not match pattern: " "" d662_354 ["char"])
                                               let '`' = xx661_355
                                               return ()
                                               t <- StateT typ
                                               d666_356 <- get
                                               xx665_357 <- StateT char
                                               case xx665_357 of
                                                   '`' -> return ()
                                                   _ -> gets position >>= (throwError . mkParseError "'`'" "not match pattern: " "" d666_356 ["char"])
                                               let '`' = xx665_357
                                               return ()
                                               _ <- StateT spaces
                                               return ()
                                               po <- StateT patOp
                                               return (uInfixP p (mkName t) po),
                                            do p <- StateT pat
                                               return p]
                pat40_115 = foldl1 mplus [do t <- StateT typ
                                             _ <- StateT spaces
                                             return ()
                                             ps <- StateT pats
                                             return (conP (mkName t) ps),
                                          do d680_358 <- get
                                             xx679_359 <- StateT char
                                             case xx679_359 of
                                                 '(' -> return ()
                                                 _ -> gets position >>= (throwError . mkParseError "'('" "not match pattern: " "" d680_358 ["char"])
                                             let '(' = xx679_359
                                             return ()
                                             o <- StateT opConName
                                             d684_360 <- get
                                             xx683_361 <- StateT char
                                             case xx683_361 of
                                                 ')' -> return ()
                                                 _ -> gets position >>= (throwError . mkParseError "')'" "not match pattern: " "" d684_360 ["char"])
                                             let ')' = xx683_361
                                             return ()
                                             _ <- StateT spaces
                                             return ()
                                             ps <- StateT pats
                                             return (conP o ps),
                                          do p <- StateT pat1
                                             return p]
                pat141_116 = foldl1 mplus [do t <- StateT typ
                                              return (conP (mkName t) []),
                                           do d694_362 <- get
                                              xx693_363 <- StateT variable
                                              case xx693_363 of
                                                  "_" -> return ()
                                                  _ -> gets position >>= (throwError . mkParseError "\"_\"" "not match pattern: " "" d694_362 ["variable"])
                                              let "_" = xx693_363
                                              return ()
                                              return wildP,
                                           do n <- StateT variable
                                              return (varP $ mkName n),
                                           do i <- StateT integer
                                              return (litP (integerL i)),
                                           do d700_364 <- get
                                              xx699_365 <- StateT char
                                              case xx699_365 of
                                                  '-' -> return ()
                                                  _ -> gets position >>= (throwError . mkParseError "'-'" "not match pattern: " "" d700_364 ["char"])
                                              let '-' = xx699_365
                                              return ()
                                              _ <- StateT spaces
                                              return ()
                                              i <- StateT integer
                                              return (litP (integerL $ negate i)),
                                           do d706_366 <- get
                                              xx705_367 <- StateT char
                                              case xx705_367 of
                                                  '\'' -> return ()
                                                  _ -> gets position >>= (throwError . mkParseError "'\\''" "not match pattern: " "" d706_366 ["char"])
                                              let '\'' = xx705_367
                                              return ()
                                              c <- StateT charLit
                                              d710_368 <- get
                                              xx709_369 <- StateT char
                                              case xx709_369 of
                                                  '\'' -> return ()
                                                  _ -> gets position >>= (throwError . mkParseError "'\\''" "not match pattern: " "" d710_368 ["char"])
                                              let '\'' = xx709_369
                                              return ()
                                              return (litP $ charL c),
                                           do d712_370 <- get
                                              xx711_371 <- StateT char
                                              case xx711_371 of
                                                  '"' -> return ()
                                                  _ -> gets position >>= (throwError . mkParseError "'\"'" "not match pattern: " "" d712_370 ["char"])
                                              let '"' = xx711_371
                                              return ()
                                              s <- StateT stringLit
                                              d716_372 <- get
                                              xx715_373 <- StateT char
                                              case xx715_373 of
                                                  '"' -> return ()
                                                  _ -> gets position >>= (throwError . mkParseError "'\"'" "not match pattern: " "" d716_372 ["char"])
                                              let '"' = xx715_373
                                              return ()
                                              return (litP $ stringL s),
                                           do d718_374 <- get
                                              xx717_375 <- StateT char
                                              case xx717_375 of
                                                  '(' -> return ()
                                                  _ -> gets position >>= (throwError . mkParseError "'('" "not match pattern: " "" d718_374 ["char"])
                                              let '(' = xx717_375
                                              return ()
                                              p <- StateT patList
                                              d722_376 <- get
                                              xx721_377 <- StateT char
                                              case xx721_377 of
                                                  ')' -> return ()
                                                  _ -> gets position >>= (throwError . mkParseError "')'" "not match pattern: " "" d722_376 ["char"])
                                              let ')' = xx721_377
                                              return ()
                                              return (tupP p),
                                           do d724_378 <- get
                                              xx723_379 <- StateT char
                                              case xx723_379 of
                                                  '[' -> return ()
                                                  _ -> gets position >>= (throwError . mkParseError "'['" "not match pattern: " "" d724_378 ["char"])
                                              let '[' = xx723_379
                                              return ()
                                              p <- StateT patList
                                              d728_380 <- get
                                              xx727_381 <- StateT char
                                              case xx727_381 of
                                                  ']' -> return ()
                                                  _ -> gets position >>= (throwError . mkParseError "']'" "not match pattern: " "" d728_380 ["char"])
                                              let ']' = xx727_381
                                              return ()
                                              return (listP p)]
                patList42_117 = foldl1 mplus [do p <- StateT patOp
                                                 _ <- StateT spaces
                                                 return ()
                                                 d734_382 <- get
                                                 xx733_383 <- StateT char
                                                 case xx733_383 of
                                                     ',' -> return ()
                                                     _ -> gets position >>= (throwError . mkParseError "','" "not match pattern: " "" d734_382 ["char"])
                                                 let ',' = xx733_383
                                                 return ()
                                                 _ <- StateT spaces
                                                 return ()
                                                 ps <- StateT patList
                                                 return (p : ps),
                                              do p <- StateT patOp
                                                 return [p],
                                              return []]
                opConName43_118 = foldl1 mplus [do d742_384 <- get
                                                   xx741_385 <- StateT char
                                                   case xx741_385 of
                                                       ':' -> return ()
                                                       _ -> gets position >>= (throwError . mkParseError "':'" "not match pattern: " "" d742_384 ["char"])
                                                   let ':' = xx741_385
                                                   return ()
                                                   ot <- StateT opTail
                                                   return (mkName $ ':' : ot)]
                charLit44_119 = foldl1 mplus [do d746_386 <- get
                                                 xx745_387 <- StateT char
                                                 let c = xx745_387
                                                 unless (c `notElem` "\\'") (gets position >>= (throwError . mkParseError "c `notElem` \"\\\\'\"" "not match: " "" d746_386 ["char"]))
                                                 return c,
                                              do d748_388 <- get
                                                 xx747_389 <- StateT char
                                                 case xx747_389 of
                                                     '\\' -> return ()
                                                     _ -> gets position >>= (throwError . mkParseError "'\\\\'" "not match pattern: " "" d748_388 ["char"])
                                                 let '\\' = xx747_389
                                                 return ()
                                                 c <- StateT escapeC
                                                 return c]
                stringLit45_120 = foldl1 mplus [do d752_390 <- get
                                                   xx751_391 <- StateT char
                                                   let c = xx751_391
                                                   unless (c `notElem` "\"\\") (gets position >>= (throwError . mkParseError "c `notElem` \"\\\"\\\\\"" "not match: " "" d752_390 ["char"]))
                                                   s <- StateT stringLit
                                                   return (c : s),
                                                do d756_392 <- get
                                                   xx755_393 <- StateT char
                                                   case xx755_393 of
                                                       '\\' -> return ()
                                                       _ -> gets position >>= (throwError . mkParseError "'\\\\'" "not match pattern: " "" d756_392 ["char"])
                                                   let '\\' = xx755_393
                                                   return ()
                                                   c <- StateT escapeC
                                                   s <- StateT stringLit
                                                   return (c : s),
                                                return ""]
                escapeC46_121 = foldl1 mplus [do d762_394 <- get
                                                 xx761_395 <- StateT char
                                                 case xx761_395 of
                                                     '"' -> return ()
                                                     _ -> gets position >>= (throwError . mkParseError "'\"'" "not match pattern: " "" d762_394 ["char"])
                                                 let '"' = xx761_395
                                                 return ()
                                                 return '"',
                                              do d764_396 <- get
                                                 xx763_397 <- StateT char
                                                 case xx763_397 of
                                                     '\'' -> return ()
                                                     _ -> gets position >>= (throwError . mkParseError "'\\''" "not match pattern: " "" d764_396 ["char"])
                                                 let '\'' = xx763_397
                                                 return ()
                                                 return '\'',
                                              do d766_398 <- get
                                                 xx765_399 <- StateT char
                                                 case xx765_399 of
                                                     '\\' -> return ()
                                                     _ -> gets position >>= (throwError . mkParseError "'\\\\'" "not match pattern: " "" d766_398 ["char"])
                                                 let '\\' = xx765_399
                                                 return ()
                                                 return '\\',
                                              do d768_400 <- get
                                                 xx767_401 <- StateT char
                                                 case xx767_401 of
                                                     'n' -> return ()
                                                     _ -> gets position >>= (throwError . mkParseError "'n'" "not match pattern: " "" d768_400 ["char"])
                                                 let 'n' = xx767_401
                                                 return ()
                                                 return '\n',
                                              do d770_402 <- get
                                                 xx769_403 <- StateT char
                                                 case xx769_403 of
                                                     't' -> return ()
                                                     _ -> gets position >>= (throwError . mkParseError "'t'" "not match pattern: " "" d770_402 ["char"])
                                                 let 't' = xx769_403
                                                 return ()
                                                 return '\t']
                pats47_122 = foldl1 mplus [do p <- StateT pat
                                              _ <- StateT spaces
                                              return ()
                                              ps <- StateT pats
                                              return (p : ps),
                                           return []]
                readFromLs48_123 = foldl1 mplus [do rf <- StateT readFrom
                                                    d780_404 <- get
                                                    xx779_405 <- StateT char
                                                    case xx779_405 of
                                                        '*' -> return ()
                                                        _ -> gets position >>= (throwError . mkParseError "'*'" "not match pattern: " "" d780_404 ["char"])
                                                    let '*' = xx779_405
                                                    return ()
                                                    return (FromL List rf),
                                                 do rf <- StateT readFrom
                                                    d784_406 <- get
                                                    xx783_407 <- StateT char
                                                    case xx783_407 of
                                                        '+' -> return ()
                                                        _ -> gets position >>= (throwError . mkParseError "'+'" "not match pattern: " "" d784_406 ["char"])
                                                    let '+' = xx783_407
                                                    return ()
                                                    return (FromL List1 rf),
                                                 do rf <- StateT readFrom
                                                    d788_408 <- get
                                                    xx787_409 <- StateT char
                                                    case xx787_409 of
                                                        '?' -> return ()
                                                        _ -> gets position >>= (throwError . mkParseError "'?'" "not match pattern: " "" d788_408 ["char"])
                                                    let '?' = xx787_409
                                                    return ()
                                                    return (FromL Optional rf),
                                                 do rf <- StateT readFrom
                                                    return rf]
                readFrom49_124 = foldl1 mplus [do v <- StateT variable
                                                  return (FromVariable $ Just v),
                                               do d794_410 <- get
                                                  xx793_411 <- StateT char
                                                  case xx793_411 of
                                                      '(' -> return ()
                                                      _ -> gets position >>= (throwError . mkParseError "'('" "not match pattern: " "" d794_410 ["char"])
                                                  let '(' = xx793_411
                                                  return ()
                                                  s <- StateT selection
                                                  d798_412 <- get
                                                  xx797_413 <- StateT char
                                                  case xx797_413 of
                                                      ')' -> return ()
                                                      _ -> gets position >>= (throwError . mkParseError "')'" "not match pattern: " "" d798_412 ["char"])
                                                  let ')' = xx797_413
                                                  return ()
                                                  return (FromSelection s),
                                               do e <- StateT expressionHsSugar
                                                  return (FromSelection $ Selection [e])]
                selectCharsLs50_125 = foldl1 mplus [do rf <- StateT selectChars
                                                       d804_414 <- get
                                                       xx803_415 <- StateT char
                                                       case xx803_415 of
                                                           '*' -> return ()
                                                           _ -> gets position >>= (throwError . mkParseError "'*'" "not match pattern: " "" d804_414 ["char"])
                                                       let '*' = xx803_415
                                                       return ()
                                                       return (FromL List rf),
                                                    do rf <- StateT selectChars
                                                       d808_416 <- get
                                                       xx807_417 <- StateT char
                                                       case xx807_417 of
                                                           '+' -> return ()
                                                           _ -> gets position >>= (throwError . mkParseError "'+'" "not match pattern: " "" d808_416 ["char"])
                                                       let '+' = xx807_417
                                                       return ()
                                                       return (FromL List1 rf),
                                                    do rf <- StateT selectChars
                                                       d812_418 <- get
                                                       xx811_419 <- StateT char
                                                       case xx811_419 of
                                                           '?' -> return ()
                                                           _ -> gets position >>= (throwError . mkParseError "'?'" "not match pattern: " "" d812_418 ["char"])
                                                       let '?' = xx811_419
                                                       return ()
                                                       return (FromL Optional rf),
                                                    do rf <- StateT selectChars
                                                       return rf]
                selectChars51_126 = foldl1 mplus [do d816_420 <- get
                                                     xx815_421 <- StateT char
                                                     case xx815_421 of
                                                         '[' -> return ()
                                                         _ -> gets position >>= (throwError . mkParseError "'['" "not match pattern: " "" d816_420 ["char"])
                                                     let '[' = xx815_421
                                                     return ()
                                                     cs <- list12_422 (foldl1 mplus [do d820_423 <- get
                                                                                        xx819_424 <- StateT char
                                                                                        let c = xx819_424
                                                                                        unless (isLower c) (gets position >>= (throwError . mkParseError "isLower c" "not match: " "" d820_423 ["char"]))
                                                                                        return c])
                                                     d822_425 <- get
                                                     xx821_426 <- StateT char
                                                     case xx821_426 of
                                                         ']' -> return ()
                                                         _ -> gets position >>= (throwError . mkParseError "']'" "not match pattern: " "" d822_425 ["char"])
                                                     let ']' = xx821_426
                                                     return ()
                                                     return (fromTokenChars cs),
                                                  do d824_427 <- get
                                                     xx823_428 <- StateT char
                                                     case xx823_428 of
                                                         '[' -> return ()
                                                         _ -> gets position >>= (throwError . mkParseError "'['" "not match pattern: " "" d824_427 ["char"])
                                                     let '[' = xx823_428
                                                     return ()
                                                     d826_429 <- get
                                                     xx825_430 <- StateT char
                                                     let cb = xx825_430
                                                     unless (cb `notElem` "\\-") (gets position >>= (throwError . mkParseError "cb `notElem` \"\\\\-\"" "not match: " "" d826_429 ["char"]))
                                                     d828_431 <- get
                                                     xx827_432 <- StateT char
                                                     case xx827_432 of
                                                         '-' -> return ()
                                                         _ -> gets position >>= (throwError . mkParseError "'-'" "not match pattern: " "" d828_431 ["char"])
                                                     let '-' = xx827_432
                                                     return ()
                                                     d830_433 <- get
                                                     xx829_434 <- StateT char
                                                     let ce = xx829_434
                                                     unless (ce `notElem` "\\-") (gets position >>= (throwError . mkParseError "ce `notElem` \"\\\\-\"" "not match: " "" d830_433 ["char"]))
                                                     d832_435 <- get
                                                     xx831_436 <- StateT char
                                                     case xx831_436 of
                                                         ']' -> return ()
                                                         _ -> gets position >>= (throwError . mkParseError "']'" "not match pattern: " "" d832_435 ["char"])
                                                     let ']' = xx831_436
                                                     return ()
                                                     return (fromTokenChars [cb .. ce]),
                                                  do d834_437 <- get
                                                     xx833_438 <- StateT char
                                                     case xx833_438 of
                                                         '\'' -> return ()
                                                         _ -> gets position >>= (throwError . mkParseError "'\\''" "not match pattern: " "" d834_437 ["char"])
                                                     let '\'' = xx833_438
                                                     return ()
                                                     d836_439 <- get
                                                     xx835_440 <- StateT char
                                                     let c = xx835_440
                                                     unless (c `notElem` "\\'") (gets position >>= (throwError . mkParseError "c `notElem` \"\\\\'\"" "not match: " "" d836_439 ["char"]))
                                                     d838_441 <- get
                                                     xx837_442 <- StateT char
                                                     case xx837_442 of
                                                         '\'' -> return ()
                                                         _ -> gets position >>= (throwError . mkParseError "'\\''" "not match pattern: " "" d838_441 ["char"])
                                                     let '\'' = xx837_442
                                                     return ()
                                                     return (fromTokenChars [c])]
                test52_127 = foldl1 mplus [do d840_443 <- get
                                              xx839_444 <- StateT char
                                              case xx839_444 of
                                                  '[' -> return ()
                                                  _ -> gets position >>= (throwError . mkParseError "'['" "not match pattern: " "" d840_443 ["char"])
                                              let '[' = xx839_444
                                              return ()
                                              h <- StateT hsExpLam
                                              _ <- StateT spaces
                                              return ()
                                              com <- optional3_331 (StateT comForErr)
                                              d848_445 <- get
                                              xx847_446 <- StateT char
                                              case xx847_446 of
                                                  ']' -> return ()
                                                  _ -> gets position >>= (throwError . mkParseError "']'" "not match pattern: " "" d848_445 ["char"])
                                              let ']' = xx847_446
                                              return ()
                                              return (h, maybe "" id com)]
                hsExpLam53_128 = foldl1 mplus [do d850_447 <- get
                                                  xx849_448 <- StateT char
                                                  case xx849_448 of
                                                      '\\' -> return ()
                                                      _ -> gets position >>= (throwError . mkParseError "'\\\\'" "not match pattern: " "" d850_447 ["char"])
                                                  let '\\' = xx849_448
                                                  return ()
                                                  _ <- StateT spaces
                                                  return ()
                                                  ps <- StateT pats
                                                  _ <- StateT spaces
                                                  return ()
                                                  d858_449 <- get
                                                  xx857_450 <- StateT char
                                                  case xx857_450 of
                                                      '-' -> return ()
                                                      _ -> gets position >>= (throwError . mkParseError "'-'" "not match pattern: " "" d858_449 ["char"])
                                                  let '-' = xx857_450
                                                  return ()
                                                  d860_451 <- get
                                                  xx859_452 <- StateT char
                                                  case xx859_452 of
                                                      '>' -> return ()
                                                      _ -> gets position >>= (throwError . mkParseError "'>'" "not match pattern: " "" d860_451 ["char"])
                                                  let '>' = xx859_452
                                                  return ()
                                                  _ <- StateT spaces
                                                  return ()
                                                  e <- StateT hsExpTyp
                                                  return (lamE ps e),
                                               do e <- StateT hsExpTyp
                                                  return e]
                hsExpTyp54_129 = foldl1 mplus [do eo <- StateT hsExpOp
                                                  d870_453 <- get
                                                  xx869_454 <- StateT char
                                                  case xx869_454 of
                                                      ':' -> return ()
                                                      _ -> gets position >>= (throwError . mkParseError "':'" "not match pattern: " "" d870_453 ["char"])
                                                  let ':' = xx869_454
                                                  return ()
                                                  d872_455 <- get
                                                  xx871_456 <- StateT char
                                                  case xx871_456 of
                                                      ':' -> return ()
                                                      _ -> gets position >>= (throwError . mkParseError "':'" "not match pattern: " "" d872_455 ["char"])
                                                  let ':' = xx871_456
                                                  return ()
                                                  _ <- StateT spaces
                                                  return ()
                                                  t <- StateT hsTypeArr
                                                  return (sigE eo t),
                                               do eo <- StateT hsExpOp
                                                  return eo]
                hsExpOp55_130 = foldl1 mplus [do l <- StateT hsExp
                                                 _ <- StateT spaces
                                                 return ()
                                                 o <- StateT hsOp
                                                 _ <- StateT spaces
                                                 return ()
                                                 r <- StateT hsExpOp
                                                 return (uInfixE (l id) o r),
                                              do e <- StateT hsExp
                                                 return (e id)]
                hsOp56_131 = foldl1 mplus [do d892_457 <- get
                                              xx891_458 <- StateT char
                                              let c = xx891_458
                                              unless (c `elem` "+*/-!|&.^=<>$") (gets position >>= (throwError . mkParseError "c `elem` \"+*/-!|&.^=<>$\"" "not match: " "" d892_457 ["char"]))
                                              o <- StateT opTail
                                              return (varE $ mkName $ c : o),
                                           do d896_459 <- get
                                              xx895_460 <- StateT char
                                              case xx895_460 of
                                                  ':' -> return ()
                                                  _ -> gets position >>= (throwError . mkParseError "':'" "not match pattern: " "" d896_459 ["char"])
                                              let ':' = xx895_460
                                              return ()
                                              ddd897_461 <- get
                                              do err <- ((do d899_462 <- get
                                                             xx898_463 <- StateT char
                                                             case xx898_463 of
                                                                 ':' -> return ()
                                                                 _ -> gets position >>= (throwError . mkParseError "':'" "not match pattern: " "" d899_462 ["char"])
                                                             let ':' = xx898_463
                                                             return ()) >> return False) `catchError` const (return True)
                                                 unless err (gets position >>= (throwError . mkParseError ('!' : "':':") "not match: " "" ddd897_461 ["char"]))
                                              put ddd897_461
                                              o <- StateT opTail
                                              return (conE $ mkName $ ':' : o),
                                           do d903_464 <- get
                                              xx902_465 <- StateT char
                                              case xx902_465 of
                                                  '`' -> return ()
                                                  _ -> gets position >>= (throwError . mkParseError "'`'" "not match pattern: " "" d903_464 ["char"])
                                              let '`' = xx902_465
                                              return ()
                                              v <- StateT variable
                                              d907_466 <- get
                                              xx906_467 <- StateT char
                                              case xx906_467 of
                                                  '`' -> return ()
                                                  _ -> gets position >>= (throwError . mkParseError "'`'" "not match pattern: " "" d907_466 ["char"])
                                              let '`' = xx906_467
                                              return ()
                                              return (varE $ mkName v),
                                           do d909_468 <- get
                                              xx908_469 <- StateT char
                                              case xx908_469 of
                                                  '`' -> return ()
                                                  _ -> gets position >>= (throwError . mkParseError "'`'" "not match pattern: " "" d909_468 ["char"])
                                              let '`' = xx908_469
                                              return ()
                                              t <- StateT typ
                                              d913_470 <- get
                                              xx912_471 <- StateT char
                                              case xx912_471 of
                                                  '`' -> return ()
                                                  _ -> gets position >>= (throwError . mkParseError "'`'" "not match pattern: " "" d913_470 ["char"])
                                              let '`' = xx912_471
                                              return ()
                                              return (conE $ mkName t)]
                opTail57_132 = foldl1 mplus [do d915_472 <- get
                                                xx914_473 <- StateT char
                                                let c = xx914_473
                                                unless (c `elem` ":+*/-!|&.^=<>$") (gets position >>= (throwError . mkParseError "c `elem` \":+*/-!|&.^=<>$\"" "not match: " "" d915_472 ["char"]))
                                                s <- StateT opTail
                                                return (c : s),
                                             return ""]
                hsExp58_133 = foldl1 mplus [do e <- StateT hsExp1
                                               _ <- StateT spaces
                                               return ()
                                               h <- StateT hsExp
                                               return (\f -> h (f e `appE`)),
                                            do e <- StateT hsExp1
                                               return (\f -> f e)]
                hsExp159_134 = foldl1 mplus [do d927_474 <- get
                                                xx926_475 <- StateT char
                                                case xx926_475 of
                                                    '(' -> return ()
                                                    _ -> gets position >>= (throwError . mkParseError "'('" "not match pattern: " "" d927_474 ["char"])
                                                let '(' = xx926_475
                                                return ()
                                                l <- optional3_331 (foldl1 mplus [do e <- StateT hsExpTyp
                                                                                     return e])
                                                _ <- StateT spaces
                                                return ()
                                                o <- StateT hsOp
                                                _ <- StateT spaces
                                                return ()
                                                r <- optional3_331 (foldl1 mplus [do e <- StateT hsExpTyp
                                                                                     return e])
                                                d943_476 <- get
                                                xx942_477 <- StateT char
                                                case xx942_477 of
                                                    ')' -> return ()
                                                    _ -> gets position >>= (throwError . mkParseError "')'" "not match pattern: " "" d943_476 ["char"])
                                                let ')' = xx942_477
                                                return ()
                                                return (infixE l o r),
                                             do d945_478 <- get
                                                xx944_479 <- StateT char
                                                case xx944_479 of
                                                    '(' -> return ()
                                                    _ -> gets position >>= (throwError . mkParseError "'('" "not match pattern: " "" d945_478 ["char"])
                                                let '(' = xx944_479
                                                return ()
                                                et <- StateT hsExpTpl
                                                d949_480 <- get
                                                xx948_481 <- StateT char
                                                case xx948_481 of
                                                    ')' -> return ()
                                                    _ -> gets position >>= (throwError . mkParseError "')'" "not match pattern: " "" d949_480 ["char"])
                                                let ')' = xx948_481
                                                return ()
                                                return (tupE et),
                                             do d951_482 <- get
                                                xx950_483 <- StateT char
                                                case xx950_483 of
                                                    '[' -> return ()
                                                    _ -> gets position >>= (throwError . mkParseError "'['" "not match pattern: " "" d951_482 ["char"])
                                                let '[' = xx950_483
                                                return ()
                                                et <- StateT hsExpTpl
                                                d955_484 <- get
                                                xx954_485 <- StateT char
                                                case xx954_485 of
                                                    ']' -> return ()
                                                    _ -> gets position >>= (throwError . mkParseError "']'" "not match pattern: " "" d955_484 ["char"])
                                                let ']' = xx954_485
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
                                             do d965_486 <- get
                                                xx964_487 <- StateT char
                                                case xx964_487 of
                                                    '\'' -> return ()
                                                    _ -> gets position >>= (throwError . mkParseError "'\\''" "not match pattern: " "" d965_486 ["char"])
                                                let '\'' = xx964_487
                                                return ()
                                                c <- StateT charLit
                                                d969_488 <- get
                                                xx968_489 <- StateT char
                                                case xx968_489 of
                                                    '\'' -> return ()
                                                    _ -> gets position >>= (throwError . mkParseError "'\\''" "not match pattern: " "" d969_488 ["char"])
                                                let '\'' = xx968_489
                                                return ()
                                                return (litE $ charL c),
                                             do d971_490 <- get
                                                xx970_491 <- StateT char
                                                case xx970_491 of
                                                    '"' -> return ()
                                                    _ -> gets position >>= (throwError . mkParseError "'\"'" "not match pattern: " "" d971_490 ["char"])
                                                let '"' = xx970_491
                                                return ()
                                                s <- StateT stringLit
                                                d975_492 <- get
                                                xx974_493 <- StateT char
                                                case xx974_493 of
                                                    '"' -> return ()
                                                    _ -> gets position >>= (throwError . mkParseError "'\"'" "not match pattern: " "" d975_492 ["char"])
                                                let '"' = xx974_493
                                                return ()
                                                return (litE $ stringL s),
                                             do d977_494 <- get
                                                xx976_495 <- StateT char
                                                case xx976_495 of
                                                    '-' -> return ()
                                                    _ -> gets position >>= (throwError . mkParseError "'-'" "not match pattern: " "" d977_494 ["char"])
                                                let '-' = xx976_495
                                                return ()
                                                _ <- StateT spaces
                                                return ()
                                                e <- StateT hsExp1
                                                return (appE (varE $ mkName "negate") e)]
                hsExpTpl60_135 = foldl1 mplus [do e <- StateT hsExpLam
                                                  _ <- StateT spaces
                                                  return ()
                                                  d987_496 <- get
                                                  xx986_497 <- StateT char
                                                  case xx986_497 of
                                                      ',' -> return ()
                                                      _ -> gets position >>= (throwError . mkParseError "','" "not match pattern: " "" d987_496 ["char"])
                                                  let ',' = xx986_497
                                                  return ()
                                                  _ <- StateT spaces
                                                  return ()
                                                  et <- StateT hsExpTpl
                                                  return (e : et),
                                               do e <- StateT hsExpLam
                                                  return [e],
                                               return []]
                hsTypeArr61_136 = foldl1 mplus [do l <- StateT hsType
                                                   d997_498 <- get
                                                   xx996_499 <- StateT char
                                                   case xx996_499 of
                                                       '-' -> return ()
                                                       _ -> gets position >>= (throwError . mkParseError "'-'" "not match pattern: " "" d997_498 ["char"])
                                                   let '-' = xx996_499
                                                   return ()
                                                   d999_500 <- get
                                                   xx998_501 <- StateT char
                                                   case xx998_501 of
                                                       '>' -> return ()
                                                       _ -> gets position >>= (throwError . mkParseError "'>'" "not match pattern: " "" d999_500 ["char"])
                                                   let '>' = xx998_501
                                                   return ()
                                                   _ <- StateT spaces
                                                   return ()
                                                   r <- StateT hsTypeArr
                                                   return (appT (appT arrowT $ l id) r),
                                                do t <- StateT hsType
                                                   return (t id)]
                hsType62_137 = foldl1 mplus [do t <- StateT hsType1
                                                ts <- StateT hsType
                                                return (\f -> ts (f t `appT`)),
                                             do t <- StateT hsType1
                                                return ($ t)]
                hsType163_138 = foldl1 mplus [do d1013_502 <- get
                                                 xx1012_503 <- StateT char
                                                 case xx1012_503 of
                                                     '[' -> return ()
                                                     _ -> gets position >>= (throwError . mkParseError "'['" "not match pattern: " "" d1013_502 ["char"])
                                                 let '[' = xx1012_503
                                                 return ()
                                                 d1015_504 <- get
                                                 xx1014_505 <- StateT char
                                                 case xx1014_505 of
                                                     ']' -> return ()
                                                     _ -> gets position >>= (throwError . mkParseError "']'" "not match pattern: " "" d1015_504 ["char"])
                                                 let ']' = xx1014_505
                                                 return ()
                                                 _ <- StateT spaces
                                                 return ()
                                                 return listT,
                                              do d1019_506 <- get
                                                 xx1018_507 <- StateT char
                                                 case xx1018_507 of
                                                     '[' -> return ()
                                                     _ -> gets position >>= (throwError . mkParseError "'['" "not match pattern: " "" d1019_506 ["char"])
                                                 let '[' = xx1018_507
                                                 return ()
                                                 t <- StateT hsTypeArr
                                                 d1023_508 <- get
                                                 xx1022_509 <- StateT char
                                                 case xx1022_509 of
                                                     ']' -> return ()
                                                     _ -> gets position >>= (throwError . mkParseError "']'" "not match pattern: " "" d1023_508 ["char"])
                                                 let ']' = xx1022_509
                                                 return ()
                                                 _ <- StateT spaces
                                                 return ()
                                                 return (listT `appT` t),
                                              do d1027_510 <- get
                                                 xx1026_511 <- StateT char
                                                 case xx1026_511 of
                                                     '(' -> return ()
                                                     _ -> gets position >>= (throwError . mkParseError "'('" "not match pattern: " "" d1027_510 ["char"])
                                                 let '(' = xx1026_511
                                                 return ()
                                                 _ <- StateT spaces
                                                 return ()
                                                 tt <- StateT hsTypeTpl
                                                 d1033_512 <- get
                                                 xx1032_513 <- StateT char
                                                 case xx1032_513 of
                                                     ')' -> return ()
                                                     _ -> gets position >>= (throwError . mkParseError "')'" "not match pattern: " "" d1033_512 ["char"])
                                                 let ')' = xx1032_513
                                                 return ()
                                                 _ <- StateT spaces
                                                 return ()
                                                 return (foldl appT (tupleT $ length tt) tt),
                                              do t <- StateT typToken
                                                 return (conT $ mkName t),
                                              do d1039_514 <- get
                                                 xx1038_515 <- StateT char
                                                 case xx1038_515 of
                                                     '(' -> return ()
                                                     _ -> gets position >>= (throwError . mkParseError "'('" "not match pattern: " "" d1039_514 ["char"])
                                                 let '(' = xx1038_515
                                                 return ()
                                                 d1041_516 <- get
                                                 xx1040_517 <- StateT char
                                                 case xx1040_517 of
                                                     '-' -> return ()
                                                     _ -> gets position >>= (throwError . mkParseError "'-'" "not match pattern: " "" d1041_516 ["char"])
                                                 let '-' = xx1040_517
                                                 return ()
                                                 d1043_518 <- get
                                                 xx1042_519 <- StateT char
                                                 case xx1042_519 of
                                                     '>' -> return ()
                                                     _ -> gets position >>= (throwError . mkParseError "'>'" "not match pattern: " "" d1043_518 ["char"])
                                                 let '>' = xx1042_519
                                                 return ()
                                                 d1045_520 <- get
                                                 xx1044_521 <- StateT char
                                                 case xx1044_521 of
                                                     ')' -> return ()
                                                     _ -> gets position >>= (throwError . mkParseError "')'" "not match pattern: " "" d1045_520 ["char"])
                                                 let ')' = xx1044_521
                                                 return ()
                                                 _ <- StateT spaces
                                                 return ()
                                                 return arrowT]
                hsTypeTpl64_139 = foldl1 mplus [do t <- StateT hsTypeArr
                                                   d1051_522 <- get
                                                   xx1050_523 <- StateT char
                                                   case xx1050_523 of
                                                       ',' -> return ()
                                                       _ -> gets position >>= (throwError . mkParseError "','" "not match pattern: " "" d1051_522 ["char"])
                                                   let ',' = xx1050_523
                                                   return ()
                                                   _ <- StateT spaces
                                                   return ()
                                                   tt <- StateT hsTypeTpl
                                                   return (t : tt),
                                                do t <- StateT hsTypeArr
                                                   return [t],
                                                return []]
                typ65_140 = foldl1 mplus [do u <- StateT upper
                                             t <- StateT tvtail
                                             return (u : t)]
                variable66_141 = foldl1 mplus [do l <- StateT lower
                                                  t <- StateT tvtail
                                                  return (l : t)]
                tvtail67_142 = foldl1 mplus [do a <- StateT alpha
                                                t <- StateT tvtail
                                                return (a : t),
                                             return ""]
                integer68_143 = foldl1 mplus [do dh <- StateT digit
                                                 ds <- list1_324 (foldl1 mplus [do d <- StateT digit
                                                                                   return d])
                                                 return (read $ dh : ds)]
                alpha69_144 = foldl1 mplus [do u <- StateT upper
                                               return u,
                                            do l <- StateT lower
                                               return l,
                                            do d <- StateT digit
                                               return d,
                                            do d1083_524 <- get
                                               xx1082_525 <- StateT char
                                               case xx1082_525 of
                                                   '\'' -> return ()
                                                   _ -> gets position >>= (throwError . mkParseError "'\\''" "not match pattern: " "" d1083_524 ["char"])
                                               let '\'' = xx1082_525
                                               return ()
                                               return '\'']
                upper70_145 = foldl1 mplus [do d1085_526 <- get
                                               xx1084_527 <- StateT char
                                               let u = xx1084_527
                                               unless (isUpper u) (gets position >>= (throwError . mkParseError "isUpper u" "not match: " "" d1085_526 ["char"]))
                                               return u]
                lower71_146 = foldl1 mplus [do d1087_528 <- get
                                               xx1086_529 <- StateT char
                                               let l = xx1086_529
                                               unless (isLower l || l == '_') (gets position >>= (throwError . mkParseError "isLower l || l == '_'" "not match: " "" d1087_528 ["char"]))
                                               return l]
                digit72_147 = foldl1 mplus [do d1089_530 <- get
                                               xx1088_531 <- StateT char
                                               let d = xx1088_531
                                               unless (isDigit d) (gets position >>= (throwError . mkParseError "isDigit d" "not match: " "" d1089_530 ["char"]))
                                               return d]
                spaces73_148 = foldl1 mplus [do _ <- StateT space
                                                return ()
                                                _ <- StateT spaces
                                                return ()
                                                return (),
                                             return ()]
                space74_149 = foldl1 mplus [do d1095_532 <- get
                                               xx1094_533 <- StateT char
                                               let s = xx1094_533
                                               unless (isSpace s) (gets position >>= (throwError . mkParseError "isSpace s" "not match: " "" d1095_532 ["char"]))
                                               return (),
                                            do d1097_534 <- get
                                               xx1096_535 <- StateT char
                                               case xx1096_535 of
                                                   '-' -> return ()
                                                   _ -> gets position >>= (throwError . mkParseError "'-'" "not match pattern: " "" d1097_534 ["char"])
                                               let '-' = xx1096_535
                                               return ()
                                               d1099_536 <- get
                                               xx1098_537 <- StateT char
                                               case xx1098_537 of
                                                   '-' -> return ()
                                                   _ -> gets position >>= (throwError . mkParseError "'-'" "not match pattern: " "" d1099_536 ["char"])
                                               let '-' = xx1098_537
                                               return ()
                                               _ <- StateT notNLString
                                               return ()
                                               _ <- StateT newLine
                                               return ()
                                               return (),
                                            do _ <- StateT comment
                                               return ()
                                               return ()]
                notNLString75_150 = foldl1 mplus [do ddd1106_538 <- get
                                                     do err <- ((do _ <- StateT newLine
                                                                    return ()) >> return False) `catchError` const (return True)
                                                        unless err (gets position >>= (throwError . mkParseError ('!' : "_:newLine") "not match: " "" ddd1106_538 ["newLine"]))
                                                     put ddd1106_538
                                                     c <- StateT char
                                                     s <- StateT notNLString
                                                     return (c : s),
                                                  return ""]
                newLine76_151 = foldl1 mplus [do d1114_539 <- get
                                                 xx1113_540 <- StateT char
                                                 case xx1113_540 of
                                                     '\n' -> return ()
                                                     _ -> gets position >>= (throwError . mkParseError "'\\n'" "not match pattern: " "" d1114_539 ["char"])
                                                 let '\n' = xx1113_540
                                                 return ()
                                                 return ()]
                comment77_152 = foldl1 mplus [do d1116_541 <- get
                                                 xx1115_542 <- StateT char
                                                 case xx1115_542 of
                                                     '{' -> return ()
                                                     _ -> gets position >>= (throwError . mkParseError "'{'" "not match pattern: " "" d1116_541 ["char"])
                                                 let '{' = xx1115_542
                                                 return ()
                                                 d1118_543 <- get
                                                 xx1117_544 <- StateT char
                                                 case xx1117_544 of
                                                     '-' -> return ()
                                                     _ -> gets position >>= (throwError . mkParseError "'-'" "not match pattern: " "" d1118_543 ["char"])
                                                 let '-' = xx1117_544
                                                 return ()
                                                 ddd1119_545 <- get
                                                 do err <- ((do d1121_546 <- get
                                                                xx1120_547 <- StateT char
                                                                case xx1120_547 of
                                                                    '#' -> return ()
                                                                    _ -> gets position >>= (throwError . mkParseError "'#'" "not match pattern: " "" d1121_546 ["char"])
                                                                let '#' = xx1120_547
                                                                return ()) >> return False) `catchError` const (return True)
                                                    unless err (gets position >>= (throwError . mkParseError ('!' : "'#':") "not match: " "" ddd1119_545 ["char"]))
                                                 put ddd1119_545
                                                 _ <- StateT comments
                                                 return ()
                                                 _ <- StateT comEnd
                                                 return ()
                                                 return ()]
                comments78_153 = foldl1 mplus [do _ <- StateT notComStr
                                                  return ()
                                                  _ <- StateT comment
                                                  return ()
                                                  _ <- StateT comments
                                                  return ()
                                                  return (),
                                               do _ <- StateT notComStr
                                                  return ()
                                                  return ()]
                notComStr79_154 = foldl1 mplus [do ddd1134_548 <- get
                                                   do err <- ((do _ <- StateT comment
                                                                  return ()) >> return False) `catchError` const (return True)
                                                      unless err (gets position >>= (throwError . mkParseError ('!' : "_:comment") "not match: " "" ddd1134_548 ["comment"]))
                                                   put ddd1134_548
                                                   ddd1137_549 <- get
                                                   do err <- ((do _ <- StateT comEnd
                                                                  return ()) >> return False) `catchError` const (return True)
                                                      unless err (gets position >>= (throwError . mkParseError ('!' : "_:comEnd") "not match: " "" ddd1137_549 ["comEnd"]))
                                                   put ddd1137_549
                                                   _ <- StateT char
                                                   return ()
                                                   _ <- StateT notComStr
                                                   return ()
                                                   return (),
                                                return ()]
                comEnd80_155 = foldl1 mplus [do d1145_550 <- get
                                                xx1144_551 <- StateT char
                                                case xx1144_551 of
                                                    '-' -> return ()
                                                    _ -> gets position >>= (throwError . mkParseError "'-'" "not match pattern: " "" d1145_550 ["char"])
                                                let '-' = xx1144_551
                                                return ()
                                                d1147_552 <- get
                                                xx1146_553 <- StateT char
                                                case xx1146_553 of
                                                    '}' -> return ()
                                                    _ -> gets position >>= (throwError . mkParseError "'}'" "not match pattern: " "" d1147_552 ["char"])
                                                let '}' = xx1146_553
                                                return ()
                                                return ()]
                list1_324 :: forall m a . (MonadPlus m, Applicative m) =>
                                          m a -> m ([a])
                list12_422 :: forall m a . (MonadPlus m, Applicative m) =>
                                           m a -> m ([a])
                list1_324 p = list12_422 p `mplus` return []
                list12_422 p = ((:) <$> p) <*> list1_324 p
                optional3_331 :: forall m a . (MonadPlus m, Applicative m) =>
                                              m a -> m (Maybe a)
                optional3_331 p = (Just <$> p) `mplus` return Nothing

