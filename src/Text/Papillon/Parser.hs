{-# LANGUAGE FlexibleContexts, TemplateHaskell, UndecidableInstances, PackageImports, TypeFamilies, RankNTypes #-}
module Text.Papillon.Parser (
	Lookahead(..),
	Lists(..),

	Peg,
	Definition,
	Selection,
	Expression,
	PlainExpression,
	Check,
	ReadFrom(..),

	PegQ,

	selectionType,
	pprCheck,
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
	Exports,
	Code,

	runError,

	dvCharsN
) where

import Text.Papillon.Papillon
import "monads-tf" Control.Monad.State
import "monads-tf" Control.Monad.Identity
import "monads-tf" Control.Monad.Error
import Control.Applicative



import Text.Papillon.SyntaxTree
import Language.Haskell.TH {- (
	Name, TypeQ, PatQ, ExpQ, mkName,
	conT, tupleT, listT, appT, arrowT,
	wildP, litP, varP, conP, tupP, listP, uInfixP,
	litE, varE, conE, tupE, listE, sigE, appE, infixE, uInfixE, lamE,
	integerL, charL, stringL) -}
import Data.Char (isSpace, isDigit, isUpper, isLower)

data Derivs
    = Derivs {pegFile :: (ErrorT (ParseError (Pos String) Derivs)
                                 Identity
                                 ((PegFileQ, Derivs))),
              pragmas :: (ErrorT (ParseError (Pos String) Derivs)
                                 Identity
                                 (([PPragma], Derivs))),
              pragma :: (ErrorT (ParseError (Pos String) Derivs)
                                Identity
                                ((PPragma, Derivs))),
              pragmaStr :: (ErrorT (ParseError (Pos String) Derivs)
                                   Identity
                                   ((String, Derivs))),
              pragmaItems :: (ErrorT (ParseError (Pos String) Derivs)
                                     Identity
                                     (([String], Derivs))),
              pragmaEnd :: (ErrorT (ParseError (Pos String) Derivs)
                                   Identity
                                   (((), Derivs))),
              moduleDec :: (ErrorT (ParseError (Pos String) Derivs)
                                   Identity
                                   ((Maybe (([String], Maybe String)), Derivs))),
              moduleName :: (ErrorT (ParseError (Pos String) Derivs)
                                    Identity
                                    ((ModuleName, Derivs))),
              moduleDecStr :: (ErrorT (ParseError (Pos String) Derivs)
                                      Identity
                                      ((String, Derivs))),
              whr :: (ErrorT (ParseError (Pos String) Derivs)
                             Identity
                             (((), Derivs))),
              preImpPap :: (ErrorT (ParseError (Pos String) Derivs)
                                   Identity
                                   ((String, Derivs))),
              prePeg :: (ErrorT (ParseError (Pos String) Derivs)
                                Identity
                                ((String, Derivs))),
              afterPeg :: (ErrorT (ParseError (Pos String) Derivs)
                                  Identity
                                  ((String, Derivs))),
              importPapillon :: (ErrorT (ParseError (Pos String) Derivs)
                                        Identity
                                        (((), Derivs))),
              varToken :: (ErrorT (ParseError (Pos String) Derivs)
                                  Identity
                                  ((String, Derivs))),
              typToken :: (ErrorT (ParseError (Pos String) Derivs)
                                  Identity
                                  ((String, Derivs))),
              pap :: (ErrorT (ParseError (Pos String) Derivs)
                             Identity
                             (((), Derivs))),
              peg :: (ErrorT (ParseError (Pos String) Derivs)
                             Identity
                             ((STPegQ, Derivs))),
              sourceType :: (ErrorT (ParseError (Pos String) Derivs)
                                    Identity
                                    ((String, Derivs))),
              peg_ :: (ErrorT (ParseError (Pos String) Derivs)
                              Identity
                              ((PegQ, Derivs))),
              definition :: (ErrorT (ParseError (Pos String) Derivs)
                                    Identity
                                    ((DefinitionQ, Derivs))),
              selection :: (ErrorT (ParseError (Pos String) Derivs)
                                   Identity
                                   ((SelectionQ, Derivs))),
              normalSelection :: (ErrorT (ParseError (Pos String) Derivs)
                                         Identity
                                         (([ExpressionQ], Derivs))),
              plainSelection :: (ErrorT (ParseError (Pos String) Derivs)
                                        Identity
                                        (([PlainExpressionQ], Derivs))),
              expressionHs :: (ErrorT (ParseError (Pos String) Derivs)
                                      Identity
                                      ((ExpressionQ, Derivs))),
              expressionHsSugar :: (ErrorT (ParseError (Pos String) Derivs)
                                           Identity
                                           ((ExpressionQ, Derivs))),
              plainExpressionHs :: (ErrorT (ParseError (Pos String) Derivs)
                                           Identity
                                           ((PlainExpressionQ, Derivs))),
              plainHAReadFromLs :: (ErrorT (ParseError (Pos String) Derivs)
                                           Identity
                                           (((Lookahead, ReadFromQ), Derivs))),
              plainReadFromLs :: (ErrorT (ParseError (Pos String) Derivs)
                                         Identity
                                         ((ReadFromQ, Derivs))),
              expression :: (ErrorT (ParseError (Pos String) Derivs)
                                    Identity
                                    (([(Lookahead, CheckQ)], Derivs))),
              nameLeaf_ :: (ErrorT (ParseError (Pos String) Derivs)
                                   Identity
                                   (((Lookahead, CheckQ), Derivs))),
              nameLeaf :: (ErrorT (ParseError (Pos String) Derivs)
                                  Identity
                                  ((CheckQ, Derivs))),
              nameLeafNoCom :: (ErrorT (ParseError (Pos String) Derivs)
                                       Identity
                                       ((CheckQ, Derivs))),
              comForErr :: (ErrorT (ParseError (Pos String) Derivs)
                                   Identity
                                   ((String, Derivs))),
              leaf :: (ErrorT (ParseError (Pos String) Derivs)
                              Identity
                              (((ReadFromQ, Maybe ((Exp, String))), Derivs))),
              patOp :: (ErrorT (ParseError (Pos String) Derivs)
                               Identity
                               ((Pat, Derivs))),
              pat :: (ErrorT (ParseError (Pos String) Derivs)
                             Identity
                             ((Pat, Derivs))),
              pat1 :: (ErrorT (ParseError (Pos String) Derivs)
                              Identity
                              ((Pat, Derivs))),
              patList :: (ErrorT (ParseError (Pos String) Derivs)
                                 Identity
                                 (([Pat], Derivs))),
              opConName :: (ErrorT (ParseError (Pos String) Derivs)
                                   Identity
                                   ((Name, Derivs))),
              charLit :: (ErrorT (ParseError (Pos String) Derivs)
                                 Identity
                                 ((Char, Derivs))),
              stringLit :: (ErrorT (ParseError (Pos String) Derivs)
                                   Identity
                                   ((String, Derivs))),
              escapeC :: (ErrorT (ParseError (Pos String) Derivs)
                                 Identity
                                 ((Char, Derivs))),
              pats :: (ErrorT (ParseError (Pos String) Derivs)
                              Identity
                              (([Pat], Derivs))),
              readFromLs :: (ErrorT (ParseError (Pos String) Derivs)
                                    Identity
                                    ((ReadFromQ, Derivs))),
              readFrom :: (ErrorT (ParseError (Pos String) Derivs)
                                  Identity
                                  ((ReadFromQ, Derivs))),
              selectCharsLs :: (ErrorT (ParseError (Pos String) Derivs)
                                       Identity
                                       ((ReadFromQ, Derivs))),
              selectChars :: (ErrorT (ParseError (Pos String) Derivs)
                                     Identity
                                     ((ReadFromQ, Derivs))),
              test :: (ErrorT (ParseError (Pos String) Derivs)
                              Identity
                              (((Exp, String), Derivs))),
              hsExpLam :: (ErrorT (ParseError (Pos String) Derivs)
                                  Identity
                                  ((Exp, Derivs))),
              hsExpTyp :: (ErrorT (ParseError (Pos String) Derivs)
                                  Identity
                                  ((Exp, Derivs))),
              hsExpOp :: (ErrorT (ParseError (Pos String) Derivs)
                                 Identity
                                 ((Exp, Derivs))),
              hsOp :: (ErrorT (ParseError (Pos String) Derivs)
                              Identity
                              ((Exp, Derivs))),
              opTail :: (ErrorT (ParseError (Pos String) Derivs)
                                Identity
                                ((String, Derivs))),
              hsExp :: (ErrorT (ParseError (Pos String) Derivs)
                               Identity
                               (((Exp -> Exp) -> Exp, Derivs))),
              hsExp1 :: (ErrorT (ParseError (Pos String) Derivs)
                                Identity
                                ((Exp, Derivs))),
              hsExpTpl :: (ErrorT (ParseError (Pos String) Derivs)
                                  Identity
                                  (([Exp], Derivs))),
              hsTypeArr :: (ErrorT (ParseError (Pos String) Derivs)
                                   Identity
                                   ((Type, Derivs))),
              hsType :: (ErrorT (ParseError (Pos String) Derivs)
                                Identity
                                (((Type -> Type) -> Type, Derivs))),
              hsType1 :: (ErrorT (ParseError (Pos String) Derivs)
                                 Identity
                                 ((Type, Derivs))),
              hsTypeTpl :: (ErrorT (ParseError (Pos String) Derivs)
                                   Identity
                                   (([Type], Derivs))),
              typ :: (ErrorT (ParseError (Pos String) Derivs)
                             Identity
                             ((String, Derivs))),
              variable :: (ErrorT (ParseError (Pos String) Derivs)
                                  Identity
                                  ((String, Derivs))),
              tvtail :: (ErrorT (ParseError (Pos String) Derivs)
                                Identity
                                ((String, Derivs))),
              integer :: (ErrorT (ParseError (Pos String) Derivs)
                                 Identity
                                 ((Integer, Derivs))),
              alpha :: (ErrorT (ParseError (Pos String) Derivs)
                               Identity
                               ((Char, Derivs))),
              upper :: (ErrorT (ParseError (Pos String) Derivs)
                               Identity
                               ((Char, Derivs))),
              lower :: (ErrorT (ParseError (Pos String) Derivs)
                               Identity
                               ((Char, Derivs))),
              digit :: (ErrorT (ParseError (Pos String) Derivs)
                               Identity
                               ((Char, Derivs))),
              spaces :: (ErrorT (ParseError (Pos String) Derivs)
                                Identity
                                (((), Derivs))),
              space :: (ErrorT (ParseError (Pos String) Derivs)
                               Identity
                               (((), Derivs))),
              notNLString :: (ErrorT (ParseError (Pos String) Derivs)
                                     Identity
                                     ((String, Derivs))),
              newLine :: (ErrorT (ParseError (Pos String) Derivs)
                                 Identity
                                 (((), Derivs))),
              comment :: (ErrorT (ParseError (Pos String) Derivs)
                                 Identity
                                 (((), Derivs))),
              comments :: (ErrorT (ParseError (Pos String) Derivs)
                                  Identity
                                  (((), Derivs))),
              notComStr :: (ErrorT (ParseError (Pos String) Derivs)
                                   Identity
                                   (((), Derivs))),
              comEnd :: (ErrorT (ParseError (Pos String) Derivs)
                                Identity
                                (((), Derivs))),
              char :: (ErrorT (ParseError (Pos String) Derivs)
                              Identity
                              ((Token String, Derivs))),
              position :: (Pos String)}
parse :: String -> Derivs
parse = parse1160_0 initialPos
          where parse1160_0 pos1159_1 s1161_2 = d646_3
                                where d646_3 = Derivs pegFile0_4 pragmas1_5 pragma2_6 pragmaStr3_7 pragmaItems4_8 pragmaEnd5_9 moduleDec6_10 moduleName7_11 moduleDecStr8_12 whr9_13 preImpPap10_14 prePeg11_15 afterPeg12_16 importPapillon13_17 varToken14_18 typToken15_19 pap16_20 peg17_21 sourceType18_22 peg_19_23 definition20_24 selection21_25 normalSelection22_26 plainSelection23_27 expressionHs24_28 expressionHsSugar25_29 plainExpressionHs26_30 plainHAReadFromLs27_31 plainReadFromLs28_32 expression29_33 nameLeaf_30_34 nameLeaf31_35 nameLeafNoCom32_36 comForErr33_37 leaf34_38 patOp35_39 pat36_40 pat137_41 patList38_42 opConName39_43 charLit40_44 stringLit41_45 escapeC42_46 pats43_47 readFromLs44_48 readFrom45_49 selectCharsLs46_50 selectChars47_51 test48_52 hsExpLam49_53 hsExpTyp50_54 hsExpOp51_55 hsOp52_56 opTail53_57 hsExp54_58 hsExp155_59 hsExpTpl56_60 hsTypeArr57_61 hsType58_62 hsType159_63 hsTypeTpl60_64 typ61_65 variable62_66 tvtail63_67 integer64_68 alpha65_69 upper66_70 lower67_71 digit68_72 spaces69_73 space70_74 notNLString71_75 newLine72_76 comment73_77 comments74_78 notComStr75_79 comEnd76_80 chars1162_81 pos1159_1
                                      pegFile0_4 = runStateT pegFile77_82 d646_3
                                      pragmas1_5 = runStateT pragmas78_83 d646_3
                                      pragma2_6 = runStateT pragma79_84 d646_3
                                      pragmaStr3_7 = runStateT pragmaStr80_85 d646_3
                                      pragmaItems4_8 = runStateT pragmaItems81_86 d646_3
                                      pragmaEnd5_9 = runStateT pragmaEnd82_87 d646_3
                                      moduleDec6_10 = runStateT moduleDec83_88 d646_3
                                      moduleName7_11 = runStateT moduleName84_89 d646_3
                                      moduleDecStr8_12 = runStateT moduleDecStr85_90 d646_3
                                      whr9_13 = runStateT whr86_91 d646_3
                                      preImpPap10_14 = runStateT preImpPap87_92 d646_3
                                      prePeg11_15 = runStateT prePeg88_93 d646_3
                                      afterPeg12_16 = runStateT afterPeg89_94 d646_3
                                      importPapillon13_17 = runStateT importPapillon90_95 d646_3
                                      varToken14_18 = runStateT varToken91_96 d646_3
                                      typToken15_19 = runStateT typToken92_97 d646_3
                                      pap16_20 = runStateT pap93_98 d646_3
                                      peg17_21 = runStateT peg94_99 d646_3
                                      sourceType18_22 = runStateT sourceType95_100 d646_3
                                      peg_19_23 = runStateT peg_96_101 d646_3
                                      definition20_24 = runStateT definition97_102 d646_3
                                      selection21_25 = runStateT selection98_103 d646_3
                                      normalSelection22_26 = runStateT normalSelection99_104 d646_3
                                      plainSelection23_27 = runStateT plainSelection100_105 d646_3
                                      expressionHs24_28 = runStateT expressionHs101_106 d646_3
                                      expressionHsSugar25_29 = runStateT expressionHsSugar102_107 d646_3
                                      plainExpressionHs26_30 = runStateT plainExpressionHs103_108 d646_3
                                      plainHAReadFromLs27_31 = runStateT plainHAReadFromLs104_109 d646_3
                                      plainReadFromLs28_32 = runStateT plainReadFromLs105_110 d646_3
                                      expression29_33 = runStateT expression106_111 d646_3
                                      nameLeaf_30_34 = runStateT nameLeaf_107_112 d646_3
                                      nameLeaf31_35 = runStateT nameLeaf108_113 d646_3
                                      nameLeafNoCom32_36 = runStateT nameLeafNoCom109_114 d646_3
                                      comForErr33_37 = runStateT comForErr110_115 d646_3
                                      leaf34_38 = runStateT leaf111_116 d646_3
                                      patOp35_39 = runStateT patOp112_117 d646_3
                                      pat36_40 = runStateT pat113_118 d646_3
                                      pat137_41 = runStateT pat1114_119 d646_3
                                      patList38_42 = runStateT patList115_120 d646_3
                                      opConName39_43 = runStateT opConName116_121 d646_3
                                      charLit40_44 = runStateT charLit117_122 d646_3
                                      stringLit41_45 = runStateT stringLit118_123 d646_3
                                      escapeC42_46 = runStateT escapeC119_124 d646_3
                                      pats43_47 = runStateT pats120_125 d646_3
                                      readFromLs44_48 = runStateT readFromLs121_126 d646_3
                                      readFrom45_49 = runStateT readFrom122_127 d646_3
                                      selectCharsLs46_50 = runStateT selectCharsLs123_128 d646_3
                                      selectChars47_51 = runStateT selectChars124_129 d646_3
                                      test48_52 = runStateT test125_130 d646_3
                                      hsExpLam49_53 = runStateT hsExpLam126_131 d646_3
                                      hsExpTyp50_54 = runStateT hsExpTyp127_132 d646_3
                                      hsExpOp51_55 = runStateT hsExpOp128_133 d646_3
                                      hsOp52_56 = runStateT hsOp129_134 d646_3
                                      opTail53_57 = runStateT opTail130_135 d646_3
                                      hsExp54_58 = runStateT hsExp131_136 d646_3
                                      hsExp155_59 = runStateT hsExp1132_137 d646_3
                                      hsExpTpl56_60 = runStateT hsExpTpl133_138 d646_3
                                      hsTypeArr57_61 = runStateT hsTypeArr134_139 d646_3
                                      hsType58_62 = runStateT hsType135_140 d646_3
                                      hsType159_63 = runStateT hsType1136_141 d646_3
                                      hsTypeTpl60_64 = runStateT hsTypeTpl137_142 d646_3
                                      typ61_65 = runStateT typ138_143 d646_3
                                      variable62_66 = runStateT variable139_144 d646_3
                                      tvtail63_67 = runStateT tvtail140_145 d646_3
                                      integer64_68 = runStateT integer141_146 d646_3
                                      alpha65_69 = runStateT alpha142_147 d646_3
                                      upper66_70 = runStateT upper143_148 d646_3
                                      lower67_71 = runStateT lower144_149 d646_3
                                      digit68_72 = runStateT digit145_150 d646_3
                                      spaces69_73 = runStateT spaces146_151 d646_3
                                      space70_74 = runStateT space147_152 d646_3
                                      notNLString71_75 = runStateT notNLString148_153 d646_3
                                      newLine72_76 = runStateT newLine149_154 d646_3
                                      comment73_77 = runStateT comment150_155 d646_3
                                      comments74_78 = runStateT comments151_156 d646_3
                                      notComStr75_79 = runStateT notComStr152_157 d646_3
                                      comEnd76_80 = runStateT comEnd153_158 d646_3
                                      chars1162_81 = runStateT (case getToken s1161_2 of
                                                                    Just (c1157_159,
                                                                          s'1158_160) -> do put (parse1160_0 (updatePos c1157_159 pos1159_1) s'1158_160)
                                                                                            return c1157_159
                                                                    _ -> gets position >>= (throwError . mkParseError "" "end of input" "" undefined [])) d646_3
                pegFile77_82 = foldl1 mplus [do pr <- StateT pragmas
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
                                                d654_161 <- get
                                                t165_162 <- StateT char
                                                case t165_162 of
                                                    '|' -> return ()
                                                    _ -> gets position >>= (throwError . mkParseError "'|'" "not match pattern: " "" d654_161 ["char"])
                                                let '|' = t165_162
                                                return ()
                                                d655_163 <- get
                                                t166_164 <- StateT char
                                                case t166_164 of
                                                    ']' -> return ()
                                                    _ -> gets position >>= (throwError . mkParseError "']'" "not match pattern: " "" d655_163 ["char"])
                                                let ']' = t166_164
                                                return ()
                                                d656_165 <- get
                                                t167_166 <- StateT char
                                                case t167_166 of
                                                    '\n' -> return ()
                                                    _ -> gets position >>= (throwError . mkParseError "'\\n'" "not match pattern: " "" d656_165 ["char"])
                                                let '\n' = t167_166
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
                                                d664_167 <- get
                                                t175_168 <- StateT char
                                                case t175_168 of
                                                    '|' -> return ()
                                                    _ -> gets position >>= (throwError . mkParseError "'|'" "not match pattern: " "" d664_167 ["char"])
                                                let '|' = t175_168
                                                return ()
                                                d665_169 <- get
                                                t176_170 <- StateT char
                                                case t176_170 of
                                                    ']' -> return ()
                                                    _ -> gets position >>= (throwError . mkParseError "']'" "not match pattern: " "" d665_169 ["char"])
                                                let ']' = t176_170
                                                return ()
                                                d666_171 <- get
                                                t177_172 <- StateT char
                                                case t177_172 of
                                                    '\n' -> return ()
                                                    _ -> gets position >>= (throwError . mkParseError "'\\n'" "not match pattern: " "" d666_171 ["char"])
                                                let '\n' = t177_172
                                                return ()
                                                atp <- StateT afterPeg
                                                return (mkPegFile pr md [] pp p atp)]
                pragmas78_83 = foldl1 mplus [do _ <- StateT spaces
                                                return ()
                                                pr <- StateT pragma
                                                prs <- StateT pragmas
                                                return (pr : prs),
                                             do _ <- StateT spaces
                                                return ()
                                                return []]
                pragma79_84 = foldl1 mplus [do d672_173 <- get
                                               t183_174 <- StateT char
                                               case t183_174 of
                                                   '{' -> return ()
                                                   _ -> gets position >>= (throwError . mkParseError "'{'" "not match pattern: " "" d672_173 ["char"])
                                               let '{' = t183_174
                                               return ()
                                               d673_175 <- get
                                               t184_176 <- StateT char
                                               case t184_176 of
                                                   '-' -> return ()
                                                   _ -> gets position >>= (throwError . mkParseError "'-'" "not match pattern: " "" d673_175 ["char"])
                                               let '-' = t184_176
                                               return ()
                                               d674_177 <- get
                                               t185_178 <- StateT char
                                               case t185_178 of
                                                   '#' -> return ()
                                                   _ -> gets position >>= (throwError . mkParseError "'#'" "not match pattern: " "" d674_177 ["char"])
                                               let '#' = t185_178
                                               return ()
                                               _ <- StateT spaces
                                               return ()
                                               d676_179 <- get
                                               t187_180 <- StateT char
                                               case t187_180 of
                                                   'L' -> return ()
                                                   _ -> gets position >>= (throwError . mkParseError "'L'" "not match pattern: " "" d676_179 ["char"])
                                               let 'L' = t187_180
                                               return ()
                                               d677_181 <- get
                                               t188_182 <- StateT char
                                               case t188_182 of
                                                   'A' -> return ()
                                                   _ -> gets position >>= (throwError . mkParseError "'A'" "not match pattern: " "" d677_181 ["char"])
                                               let 'A' = t188_182
                                               return ()
                                               d678_183 <- get
                                               t189_184 <- StateT char
                                               case t189_184 of
                                                   'N' -> return ()
                                                   _ -> gets position >>= (throwError . mkParseError "'N'" "not match pattern: " "" d678_183 ["char"])
                                               let 'N' = t189_184
                                               return ()
                                               d679_185 <- get
                                               t190_186 <- StateT char
                                               case t190_186 of
                                                   'G' -> return ()
                                                   _ -> gets position >>= (throwError . mkParseError "'G'" "not match pattern: " "" d679_185 ["char"])
                                               let 'G' = t190_186
                                               return ()
                                               d680_187 <- get
                                               t191_188 <- StateT char
                                               case t191_188 of
                                                   'U' -> return ()
                                                   _ -> gets position >>= (throwError . mkParseError "'U'" "not match pattern: " "" d680_187 ["char"])
                                               let 'U' = t191_188
                                               return ()
                                               d681_189 <- get
                                               t192_190 <- StateT char
                                               case t192_190 of
                                                   'A' -> return ()
                                                   _ -> gets position >>= (throwError . mkParseError "'A'" "not match pattern: " "" d681_189 ["char"])
                                               let 'A' = t192_190
                                               return ()
                                               d682_191 <- get
                                               t193_192 <- StateT char
                                               case t193_192 of
                                                   'G' -> return ()
                                                   _ -> gets position >>= (throwError . mkParseError "'G'" "not match pattern: " "" d682_191 ["char"])
                                               let 'G' = t193_192
                                               return ()
                                               d683_193 <- get
                                               t194_194 <- StateT char
                                               case t194_194 of
                                                   'E' -> return ()
                                                   _ -> gets position >>= (throwError . mkParseError "'E'" "not match pattern: " "" d683_193 ["char"])
                                               let 'E' = t194_194
                                               return ()
                                               _ <- StateT spaces
                                               return ()
                                               s <- StateT pragmaItems
                                               _ <- StateT pragmaEnd
                                               return ()
                                               _ <- StateT spaces
                                               return ()
                                               return (LanguagePragma s),
                                            do d688_195 <- get
                                               t199_196 <- StateT char
                                               case t199_196 of
                                                   '{' -> return ()
                                                   _ -> gets position >>= (throwError . mkParseError "'{'" "not match pattern: " "" d688_195 ["char"])
                                               let '{' = t199_196
                                               return ()
                                               d689_197 <- get
                                               t200_198 <- StateT char
                                               case t200_198 of
                                                   '-' -> return ()
                                                   _ -> gets position >>= (throwError . mkParseError "'-'" "not match pattern: " "" d689_197 ["char"])
                                               let '-' = t200_198
                                               return ()
                                               d690_199 <- get
                                               t201_200 <- StateT char
                                               case t201_200 of
                                                   '#' -> return ()
                                                   _ -> gets position >>= (throwError . mkParseError "'#'" "not match pattern: " "" d690_199 ["char"])
                                               let '#' = t201_200
                                               return ()
                                               _ <- StateT spaces
                                               return ()
                                               s <- StateT pragmaStr
                                               _ <- StateT pragmaEnd
                                               return ()
                                               return (OtherPragma s)]
                pragmaStr80_85 = foldl1 mplus [do d695_201 <- get
                                                  do err1146_202 <- ((do _ <- StateT pragmaEnd
                                                                         return ()) >> return False) `catchError` const (return True)
                                                     unless err1146_202 (gets position >>= (throwError . mkParseError "!_:pragmaEnd" "not match: " "" d695_201 ["pragmaEnd"]))
                                                  put d695_201
                                                  c <- StateT char
                                                  s <- StateT pragmaStr
                                                  return (c : s),
                                               return ""]
                pragmaItems81_86 = foldl1 mplus [do t <- StateT typToken
                                                    d699_203 <- get
                                                    t209_204 <- StateT char
                                                    case t209_204 of
                                                        ',' -> return ()
                                                        _ -> gets position >>= (throwError . mkParseError "','" "not match pattern: " "" d699_203 ["char"])
                                                    let ',' = t209_204
                                                    return ()
                                                    _ <- StateT spaces
                                                    return ()
                                                    i <- StateT pragmaItems
                                                    return (t : i),
                                                 do t <- StateT typToken
                                                    return [t]]
                pragmaEnd82_87 = foldl1 mplus [do _ <- StateT spaces
                                                  return ()
                                                  d704_205 <- get
                                                  t214_206 <- StateT char
                                                  case t214_206 of
                                                      '#' -> return ()
                                                      _ -> gets position >>= (throwError . mkParseError "'#'" "not match pattern: " "" d704_205 ["char"])
                                                  let '#' = t214_206
                                                  return ()
                                                  d705_207 <- get
                                                  t215_208 <- StateT char
                                                  case t215_208 of
                                                      '-' -> return ()
                                                      _ -> gets position >>= (throwError . mkParseError "'-'" "not match pattern: " "" d705_207 ["char"])
                                                  let '-' = t215_208
                                                  return ()
                                                  d706_209 <- get
                                                  t216_210 <- StateT char
                                                  case t216_210 of
                                                      '}' -> return ()
                                                      _ -> gets position >>= (throwError . mkParseError "'}'" "not match pattern: " "" d706_209 ["char"])
                                                  let '}' = t216_210
                                                  return ()
                                                  return ()]
                moduleDec83_88 = foldl1 mplus [do d707_211 <- get
                                                  t217_212 <- StateT char
                                                  case t217_212 of
                                                      'm' -> return ()
                                                      _ -> gets position >>= (throwError . mkParseError "'m'" "not match pattern: " "" d707_211 ["char"])
                                                  let 'm' = t217_212
                                                  return ()
                                                  d708_213 <- get
                                                  t218_214 <- StateT char
                                                  case t218_214 of
                                                      'o' -> return ()
                                                      _ -> gets position >>= (throwError . mkParseError "'o'" "not match pattern: " "" d708_213 ["char"])
                                                  let 'o' = t218_214
                                                  return ()
                                                  d709_215 <- get
                                                  t219_216 <- StateT char
                                                  case t219_216 of
                                                      'd' -> return ()
                                                      _ -> gets position >>= (throwError . mkParseError "'d'" "not match pattern: " "" d709_215 ["char"])
                                                  let 'd' = t219_216
                                                  return ()
                                                  d710_217 <- get
                                                  t220_218 <- StateT char
                                                  case t220_218 of
                                                      'u' -> return ()
                                                      _ -> gets position >>= (throwError . mkParseError "'u'" "not match pattern: " "" d710_217 ["char"])
                                                  let 'u' = t220_218
                                                  return ()
                                                  d711_219 <- get
                                                  t221_220 <- StateT char
                                                  case t221_220 of
                                                      'l' -> return ()
                                                      _ -> gets position >>= (throwError . mkParseError "'l'" "not match pattern: " "" d711_219 ["char"])
                                                  let 'l' = t221_220
                                                  return ()
                                                  d712_221 <- get
                                                  t222_222 <- StateT char
                                                  case t222_222 of
                                                      'e' -> return ()
                                                      _ -> gets position >>= (throwError . mkParseError "'e'" "not match pattern: " "" d712_221 ["char"])
                                                  let 'e' = t222_222
                                                  return ()
                                                  _ <- StateT spaces
                                                  return ()
                                                  n <- StateT moduleName
                                                  _ <- StateT spaces
                                                  return ()
                                                  d716_223 <- get
                                                  t226_224 <- StateT char
                                                  case t226_224 of
                                                      '(' -> return ()
                                                      _ -> gets position >>= (throwError . mkParseError "'('" "not match pattern: " "" d716_223 ["char"])
                                                  let '(' = t226_224
                                                  return ()
                                                  _ <- StateT spaces
                                                  return ()
                                                  s <- StateT moduleDecStr
                                                  _ <- StateT whr
                                                  return ()
                                                  return (Just (n, Just s)),
                                               do d720_225 <- get
                                                  t230_226 <- StateT char
                                                  case t230_226 of
                                                      'm' -> return ()
                                                      _ -> gets position >>= (throwError . mkParseError "'m'" "not match pattern: " "" d720_225 ["char"])
                                                  let 'm' = t230_226
                                                  return ()
                                                  d721_227 <- get
                                                  t231_228 <- StateT char
                                                  case t231_228 of
                                                      'o' -> return ()
                                                      _ -> gets position >>= (throwError . mkParseError "'o'" "not match pattern: " "" d721_227 ["char"])
                                                  let 'o' = t231_228
                                                  return ()
                                                  d722_229 <- get
                                                  t232_230 <- StateT char
                                                  case t232_230 of
                                                      'd' -> return ()
                                                      _ -> gets position >>= (throwError . mkParseError "'d'" "not match pattern: " "" d722_229 ["char"])
                                                  let 'd' = t232_230
                                                  return ()
                                                  d723_231 <- get
                                                  t233_232 <- StateT char
                                                  case t233_232 of
                                                      'u' -> return ()
                                                      _ -> gets position >>= (throwError . mkParseError "'u'" "not match pattern: " "" d723_231 ["char"])
                                                  let 'u' = t233_232
                                                  return ()
                                                  d724_233 <- get
                                                  t234_234 <- StateT char
                                                  case t234_234 of
                                                      'l' -> return ()
                                                      _ -> gets position >>= (throwError . mkParseError "'l'" "not match pattern: " "" d724_233 ["char"])
                                                  let 'l' = t234_234
                                                  return ()
                                                  d725_235 <- get
                                                  t235_236 <- StateT char
                                                  case t235_236 of
                                                      'e' -> return ()
                                                      _ -> gets position >>= (throwError . mkParseError "'e'" "not match pattern: " "" d725_235 ["char"])
                                                  let 'e' = t235_236
                                                  return ()
                                                  _ <- StateT spaces
                                                  return ()
                                                  n <- StateT moduleName
                                                  _ <- StateT spaces
                                                  return ()
                                                  d729_237 <- get
                                                  t239_238 <- StateT char
                                                  case t239_238 of
                                                      'w' -> return ()
                                                      _ -> gets position >>= (throwError . mkParseError "'w'" "not match pattern: " "" d729_237 ["char"])
                                                  let 'w' = t239_238
                                                  return ()
                                                  d730_239 <- get
                                                  t240_240 <- StateT char
                                                  case t240_240 of
                                                      'h' -> return ()
                                                      _ -> gets position >>= (throwError . mkParseError "'h'" "not match pattern: " "" d730_239 ["char"])
                                                  let 'h' = t240_240
                                                  return ()
                                                  d731_241 <- get
                                                  t241_242 <- StateT char
                                                  case t241_242 of
                                                      'e' -> return ()
                                                      _ -> gets position >>= (throwError . mkParseError "'e'" "not match pattern: " "" d731_241 ["char"])
                                                  let 'e' = t241_242
                                                  return ()
                                                  d732_243 <- get
                                                  t242_244 <- StateT char
                                                  case t242_244 of
                                                      'r' -> return ()
                                                      _ -> gets position >>= (throwError . mkParseError "'r'" "not match pattern: " "" d732_243 ["char"])
                                                  let 'r' = t242_244
                                                  return ()
                                                  d733_245 <- get
                                                  t243_246 <- StateT char
                                                  case t243_246 of
                                                      'e' -> return ()
                                                      _ -> gets position >>= (throwError . mkParseError "'e'" "not match pattern: " "" d733_245 ["char"])
                                                  let 'e' = t243_246
                                                  return ()
                                                  _ <- StateT spaces
                                                  return ()
                                                  return (Just (n, Nothing)),
                                               return Nothing]
                moduleName84_89 = foldl1 mplus [do t <- StateT typ
                                                   d736_247 <- get
                                                   t246_248 <- StateT char
                                                   case t246_248 of
                                                       '.' -> return ()
                                                       _ -> gets position >>= (throwError . mkParseError "'.'" "not match pattern: " "" d736_247 ["char"])
                                                   let '.' = t246_248
                                                   return ()
                                                   n <- StateT moduleName
                                                   return (t : n),
                                                do t <- StateT typ
                                                   return [t]]
                moduleDecStr85_90 = foldl1 mplus [do d740_249 <- get
                                                     do err1147_250 <- ((do _ <- StateT whr
                                                                            return ()) >> return False) `catchError` const (return True)
                                                        unless err1147_250 (gets position >>= (throwError . mkParseError "!_:whr" "not match: " "" d740_249 ["whr"]))
                                                     put d740_249
                                                     c <- StateT char
                                                     s <- StateT moduleDecStr
                                                     return (c : s),
                                                  return ""]
                whr86_91 = foldl1 mplus [do _ <- StateT spaces
                                            return ()
                                            d744_251 <- get
                                            t253_252 <- StateT char
                                            case t253_252 of
                                                ')' -> return ()
                                                _ -> gets position >>= (throwError . mkParseError "')'" "not match pattern: " "" d744_251 ["char"])
                                            let ')' = t253_252
                                            return ()
                                            _ <- StateT spaces
                                            return ()
                                            d746_253 <- get
                                            t255_254 <- StateT char
                                            case t255_254 of
                                                'w' -> return ()
                                                _ -> gets position >>= (throwError . mkParseError "'w'" "not match pattern: " "" d746_253 ["char"])
                                            let 'w' = t255_254
                                            return ()
                                            d747_255 <- get
                                            t256_256 <- StateT char
                                            case t256_256 of
                                                'h' -> return ()
                                                _ -> gets position >>= (throwError . mkParseError "'h'" "not match pattern: " "" d747_255 ["char"])
                                            let 'h' = t256_256
                                            return ()
                                            d748_257 <- get
                                            t257_258 <- StateT char
                                            case t257_258 of
                                                'e' -> return ()
                                                _ -> gets position >>= (throwError . mkParseError "'e'" "not match pattern: " "" d748_257 ["char"])
                                            let 'e' = t257_258
                                            return ()
                                            d749_259 <- get
                                            t258_260 <- StateT char
                                            case t258_260 of
                                                'r' -> return ()
                                                _ -> gets position >>= (throwError . mkParseError "'r'" "not match pattern: " "" d749_259 ["char"])
                                            let 'r' = t258_260
                                            return ()
                                            d750_261 <- get
                                            t259_262 <- StateT char
                                            case t259_262 of
                                                'e' -> return ()
                                                _ -> gets position >>= (throwError . mkParseError "'e'" "not match pattern: " "" d750_261 ["char"])
                                            let 'e' = t259_262
                                            return ()
                                            return ()]
                preImpPap87_92 = foldl1 mplus [do d752_263 <- get
                                                  do err1148_264 <- ((do _ <- StateT importPapillon
                                                                         return ()) >> return False) `catchError` const (return True)
                                                     unless err1148_264 (gets position >>= (throwError . mkParseError "!_:importPapillon" "not match: " "" d752_263 ["importPapillon"]))
                                                  put d752_263
                                                  d754_265 <- get
                                                  do err1149_266 <- ((do _ <- StateT pap
                                                                         return ()) >> return False) `catchError` const (return True)
                                                     unless err1149_266 (gets position >>= (throwError . mkParseError "!_:pap" "not match: " "" d754_265 ["pap"]))
                                                  put d754_265
                                                  c <- StateT char
                                                  pip <- StateT preImpPap
                                                  return (c : pip),
                                               return ""]
                prePeg88_93 = foldl1 mplus [do d758_267 <- get
                                               do err1150_268 <- ((do _ <- StateT pap
                                                                      return ()) >> return False) `catchError` const (return True)
                                                  unless err1150_268 (gets position >>= (throwError . mkParseError "!_:pap" "not match: " "" d758_267 ["pap"]))
                                               put d758_267
                                               c <- StateT char
                                               pp <- StateT prePeg
                                               return (c : pp),
                                            return ""]
                afterPeg89_94 = foldl1 mplus [do c <- StateT char
                                                 atp <- StateT afterPeg
                                                 return (c : atp),
                                              return ""]
                importPapillon90_95 = foldl1 mplus [do d763_269 <- get
                                                       t269_270 <- StateT varToken
                                                       case t269_270 of
                                                           "import" -> return ()
                                                           _ -> gets position >>= (throwError . mkParseError "\"import\"" "not match pattern: " "" d763_269 ["varToken"])
                                                       let "import" = t269_270
                                                       return ()
                                                       d764_271 <- get
                                                       t270_272 <- StateT typToken
                                                       case t270_272 of
                                                           "Text" -> return ()
                                                           _ -> gets position >>= (throwError . mkParseError "\"Text\"" "not match pattern: " "" d764_271 ["typToken"])
                                                       let "Text" = t270_272
                                                       return ()
                                                       d765_273 <- get
                                                       t271_274 <- StateT char
                                                       case t271_274 of
                                                           '.' -> return ()
                                                           _ -> gets position >>= (throwError . mkParseError "'.'" "not match pattern: " "" d765_273 ["char"])
                                                       let '.' = t271_274
                                                       return ()
                                                       _ <- StateT spaces
                                                       return ()
                                                       d767_275 <- get
                                                       t273_276 <- StateT typToken
                                                       case t273_276 of
                                                           "Papillon" -> return ()
                                                           _ -> gets position >>= (throwError . mkParseError "\"Papillon\"" "not match pattern: " "" d767_275 ["typToken"])
                                                       let "Papillon" = t273_276
                                                       return ()
                                                       d769_277 <- get
                                                       do err1151_278 <- ((do d768_279 <- get
                                                                              t274_280 <- StateT char
                                                                              case t274_280 of
                                                                                  '.' -> return ()
                                                                                  _ -> gets position >>= (throwError . mkParseError "'.'" "not match pattern: " "" d768_279 ["char"])
                                                                              let '.' = t274_280
                                                                              return ()) >> return False) `catchError` const (return True)
                                                          unless err1151_278 (gets position >>= (throwError . mkParseError "!'.':" "not match: " "" d769_277 ["char"]))
                                                       put d769_277
                                                       return ()]
                varToken91_96 = foldl1 mplus [do v <- StateT variable
                                                 _ <- StateT spaces
                                                 return ()
                                                 return v]
                typToken92_97 = foldl1 mplus [do t <- StateT typ
                                                 _ <- StateT spaces
                                                 return ()
                                                 return t]
                pap93_98 = foldl1 mplus [do d774_281 <- get
                                            t279_282 <- StateT char
                                            case t279_282 of
                                                '\n' -> return ()
                                                _ -> gets position >>= (throwError . mkParseError "'\\n'" "not match pattern: " "" d774_281 ["char"])
                                            let '\n' = t279_282
                                            return ()
                                            d775_283 <- get
                                            t280_284 <- StateT char
                                            case t280_284 of
                                                '[' -> return ()
                                                _ -> gets position >>= (throwError . mkParseError "'['" "not match pattern: " "" d775_283 ["char"])
                                            let '[' = t280_284
                                            return ()
                                            d776_285 <- get
                                            t281_286 <- StateT char
                                            case t281_286 of
                                                'p' -> return ()
                                                _ -> gets position >>= (throwError . mkParseError "'p'" "not match pattern: " "" d776_285 ["char"])
                                            let 'p' = t281_286
                                            return ()
                                            d777_287 <- get
                                            t282_288 <- StateT char
                                            case t282_288 of
                                                'a' -> return ()
                                                _ -> gets position >>= (throwError . mkParseError "'a'" "not match pattern: " "" d777_287 ["char"])
                                            let 'a' = t282_288
                                            return ()
                                            d778_289 <- get
                                            t283_290 <- StateT char
                                            case t283_290 of
                                                'p' -> return ()
                                                _ -> gets position >>= (throwError . mkParseError "'p'" "not match pattern: " "" d778_289 ["char"])
                                            let 'p' = t283_290
                                            return ()
                                            d779_291 <- get
                                            t284_292 <- StateT char
                                            case t284_292 of
                                                'i' -> return ()
                                                _ -> gets position >>= (throwError . mkParseError "'i'" "not match pattern: " "" d779_291 ["char"])
                                            let 'i' = t284_292
                                            return ()
                                            d780_293 <- get
                                            t285_294 <- StateT char
                                            case t285_294 of
                                                'l' -> return ()
                                                _ -> gets position >>= (throwError . mkParseError "'l'" "not match pattern: " "" d780_293 ["char"])
                                            let 'l' = t285_294
                                            return ()
                                            d781_295 <- get
                                            t286_296 <- StateT char
                                            case t286_296 of
                                                'l' -> return ()
                                                _ -> gets position >>= (throwError . mkParseError "'l'" "not match pattern: " "" d781_295 ["char"])
                                            let 'l' = t286_296
                                            return ()
                                            d782_297 <- get
                                            t287_298 <- StateT char
                                            case t287_298 of
                                                'o' -> return ()
                                                _ -> gets position >>= (throwError . mkParseError "'o'" "not match pattern: " "" d782_297 ["char"])
                                            let 'o' = t287_298
                                            return ()
                                            d783_299 <- get
                                            t288_300 <- StateT char
                                            case t288_300 of
                                                'n' -> return ()
                                                _ -> gets position >>= (throwError . mkParseError "'n'" "not match pattern: " "" d783_299 ["char"])
                                            let 'n' = t288_300
                                            return ()
                                            d784_301 <- get
                                            t289_302 <- StateT char
                                            case t289_302 of
                                                '|' -> return ()
                                                _ -> gets position >>= (throwError . mkParseError "'|'" "not match pattern: " "" d784_301 ["char"])
                                            let '|' = t289_302
                                            return ()
                                            d785_303 <- get
                                            t290_304 <- StateT char
                                            case t290_304 of
                                                '\n' -> return ()
                                                _ -> gets position >>= (throwError . mkParseError "'\\n'" "not match pattern: " "" d785_303 ["char"])
                                            let '\n' = t290_304
                                            return ()
                                            return ()]
                peg94_99 = foldl1 mplus [do _ <- StateT spaces
                                            return ()
                                            s <- StateT sourceType
                                            p <- StateT peg_
                                            return (stPegQ (ConT $ mkName s) p),
                                         do p <- StateT peg_
                                            return (stPegQ (ConT $ mkName "String") p)]
                sourceType95_100 = foldl1 mplus [do d790_305 <- get
                                                    t295_306 <- StateT varToken
                                                    case t295_306 of
                                                        "source" -> return ()
                                                        _ -> gets position >>= (throwError . mkParseError "\"source\"" "not match pattern: " "" d790_305 ["varToken"])
                                                    let "source" = t295_306
                                                    return ()
                                                    d791_307 <- get
                                                    t296_308 <- StateT char
                                                    case t296_308 of
                                                        ':' -> return ()
                                                        _ -> gets position >>= (throwError . mkParseError "':'" "not match pattern: " "" d791_307 ["char"])
                                                    let ':' = t296_308
                                                    return ()
                                                    _ <- StateT spaces
                                                    return ()
                                                    v <- StateT typToken
                                                    return v]
                peg_96_101 = foldl1 mplus [do _ <- StateT spaces
                                              return ()
                                              d <- StateT definition
                                              p <- StateT peg_
                                              return (d : p),
                                           return []]
                definition97_102 = foldl1 mplus [do v <- StateT variable
                                                    _ <- StateT spaces
                                                    return ()
                                                    d799_309 <- get
                                                    t304_310 <- StateT char
                                                    case t304_310 of
                                                        ':' -> return ()
                                                        _ -> gets position >>= (throwError . mkParseError "':'" "not match pattern: " "" d799_309 ["char"])
                                                    let ':' = t304_310
                                                    return ()
                                                    d800_311 <- get
                                                    t305_312 <- StateT char
                                                    case t305_312 of
                                                        ':' -> return ()
                                                        _ -> gets position >>= (throwError . mkParseError "':'" "not match pattern: " "" d800_311 ["char"])
                                                    let ':' = t305_312
                                                    return ()
                                                    _ <- StateT spaces
                                                    return ()
                                                    t <- StateT hsTypeArr
                                                    _ <- StateT spaces
                                                    return ()
                                                    d804_313 <- get
                                                    t309_314 <- StateT char
                                                    case t309_314 of
                                                        '=' -> return ()
                                                        _ -> gets position >>= (throwError . mkParseError "'='" "not match pattern: " "" d804_313 ["char"])
                                                    let '=' = t309_314
                                                    return ()
                                                    _ <- StateT spaces
                                                    return ()
                                                    sel <- StateT selection
                                                    _ <- StateT spaces
                                                    return ()
                                                    d808_315 <- get
                                                    t313_316 <- StateT char
                                                    case t313_316 of
                                                        ';' -> return ()
                                                        _ -> gets position >>= (throwError . mkParseError "';'" "not match pattern: " "" d808_315 ["char"])
                                                    let ';' = t313_316
                                                    return ()
                                                    return (definitionQ v (Just t) sel),
                                                 do v <- StateT variable
                                                    _ <- StateT spaces
                                                    return ()
                                                    d811_317 <- get
                                                    t316_318 <- StateT char
                                                    case t316_318 of
                                                        '<' -> return ()
                                                        _ -> gets position >>= (throwError . mkParseError "'<'" "not match pattern: " "" d811_317 ["char"])
                                                    let '<' = t316_318
                                                    return ()
                                                    d812_319 <- get
                                                    t317_320 <- StateT char
                                                    case t317_320 of
                                                        '-' -> return ()
                                                        _ -> gets position >>= (throwError . mkParseError "'-'" "not match pattern: " "" d812_319 ["char"])
                                                    let '-' = t317_320
                                                    return ()
                                                    _ <- StateT spaces
                                                    return ()
                                                    sel <- StateT plainSelection
                                                    _ <- StateT spaces
                                                    return ()
                                                    d816_321 <- get
                                                    t321_322 <- StateT char
                                                    case t321_322 of
                                                        ';' -> return ()
                                                        _ -> gets position >>= (throwError . mkParseError "';'" "not match pattern: " "" d816_321 ["char"])
                                                    let ';' = t321_322
                                                    return ()
                                                    return (definitionQ v Nothing $ plainSelectionQ sel)]
                selection98_103 = foldl1 mplus [do s <- StateT normalSelection
                                                   return (normalSelectionQ s),
                                                do s <- StateT plainSelection
                                                   return (plainSelectionQ s)]
                normalSelection99_104 = foldl1 mplus [do ex <- StateT expressionHs
                                                         _ <- StateT spaces
                                                         return ()
                                                         d821_323 <- get
                                                         t326_324 <- StateT char
                                                         case t326_324 of
                                                             '/' -> return ()
                                                             _ -> gets position >>= (throwError . mkParseError "'/'" "not match pattern: " "" d821_323 ["char"])
                                                         let '/' = t326_324
                                                         return ()
                                                         _ <- StateT spaces
                                                         return ()
                                                         sel <- StateT normalSelection
                                                         return (ex : sel),
                                                      do ex <- StateT expressionHs
                                                         return [ex]]
                plainSelection100_105 = foldl1 mplus [do ex <- StateT plainExpressionHs
                                                         _ <- StateT spaces
                                                         return ()
                                                         d827_325 <- get
                                                         t332_326 <- StateT char
                                                         case t332_326 of
                                                             '/' -> return ()
                                                             _ -> gets position >>= (throwError . mkParseError "'/'" "not match pattern: " "" d827_325 ["char"])
                                                         let '/' = t332_326
                                                         return ()
                                                         _ <- StateT spaces
                                                         return ()
                                                         sel <- StateT plainSelection
                                                         return (ex : sel),
                                                      do ex <- StateT plainExpressionHs
                                                         return [ex]]
                expressionHs101_106 = foldl1 mplus [do e <- StateT expression
                                                       _ <- StateT spaces
                                                       return ()
                                                       d833_327 <- get
                                                       t338_328 <- StateT char
                                                       case t338_328 of
                                                           '{' -> return ()
                                                           _ -> gets position >>= (throwError . mkParseError "'{'" "not match pattern: " "" d833_327 ["char"])
                                                       let '{' = t338_328
                                                       return ()
                                                       _ <- StateT spaces
                                                       return ()
                                                       h <- StateT hsExpLam
                                                       _ <- StateT spaces
                                                       return ()
                                                       d837_329 <- get
                                                       t342_330 <- StateT char
                                                       case t342_330 of
                                                           '}' -> return ()
                                                           _ -> gets position >>= (throwError . mkParseError "'}'" "not match pattern: " "" d837_329 ["char"])
                                                       let '}' = t342_330
                                                       return ()
                                                       return (expressionQ (e, h)),
                                                    do e <- StateT expressionHsSugar
                                                       return e]
                expressionHsSugar102_107 = foldl1 mplus [do d839_331 <- get
                                                            t344_332 <- StateT char
                                                            case t344_332 of
                                                                '<' -> return ()
                                                                _ -> gets position >>= (throwError . mkParseError "'<'" "not match pattern: " "" d839_331 ["char"])
                                                            let '<' = t344_332
                                                            return ()
                                                            _ <- StateT spaces
                                                            return ()
                                                            h <- StateT hsExpLam
                                                            _ <- StateT spaces
                                                            return ()
                                                            d843_333 <- get
                                                            t348_334 <- StateT char
                                                            case t348_334 of
                                                                '>' -> return ()
                                                                _ -> gets position >>= (throwError . mkParseError "'>'" "not match pattern: " "" d843_333 ["char"])
                                                            let '>' = t348_334
                                                            return ()
                                                            return (expressionSugar h)]
                plainExpressionHs103_108 = foldl1 mplus [do rfs <- list155_335 (foldl1 mplus [do rf <- StateT plainHAReadFromLs
                                                                                                 _ <- StateT spaces
                                                                                                 return ()
                                                                                                 return rf])
                                                            return (plainExpressionQ rfs)]
                plainHAReadFromLs104_109 = foldl1 mplus [do rf <- StateT plainReadFromLs
                                                            return (Here, rf),
                                                         do d848_336 <- get
                                                            t353_337 <- StateT char
                                                            case t353_337 of
                                                                '&' -> return ()
                                                                _ -> gets position >>= (throwError . mkParseError "'&'" "not match pattern: " "" d848_336 ["char"])
                                                            let '&' = t353_337
                                                            return ()
                                                            rf <- StateT plainReadFromLs
                                                            return (Ahead, rf),
                                                         do d850_338 <- get
                                                            t355_339 <- StateT char
                                                            case t355_339 of
                                                                '!' -> return ()
                                                                _ -> gets position >>= (throwError . mkParseError "'!'" "not match pattern: " "" d850_338 ["char"])
                                                            let '!' = t355_339
                                                            return ()
                                                            rf <- StateT plainReadFromLs
                                                            return (NAhead "", rf)]
                plainReadFromLs105_110 = foldl1 mplus [do rf <- StateT readFromLs
                                                          return rf,
                                                       do rf <- StateT selectCharsLs
                                                          return rf]
                expression106_111 = foldl1 mplus [do l <- StateT nameLeaf_
                                                     _ <- StateT spaces
                                                     return ()
                                                     e <- StateT expression
                                                     return (l : e),
                                                  return []]
                nameLeaf_107_112 = foldl1 mplus [do d857_340 <- get
                                                    t362_341 <- StateT char
                                                    case t362_341 of
                                                        '!' -> return ()
                                                        _ -> gets position >>= (throwError . mkParseError "'!'" "not match pattern: " "" d857_340 ["char"])
                                                    let '!' = t362_341
                                                    return ()
                                                    nl <- StateT nameLeafNoCom
                                                    _ <- StateT spaces
                                                    return ()
                                                    com <- optional154_342 (StateT comForErr)
                                                    return (NAhead $ maybe "" id com, nl),
                                                 do d861_343 <- get
                                                    t366_344 <- StateT char
                                                    case t366_344 of
                                                        '&' -> return ()
                                                        _ -> gets position >>= (throwError . mkParseError "'&'" "not match pattern: " "" d861_343 ["char"])
                                                    let '&' = t366_344
                                                    return ()
                                                    nl <- StateT nameLeaf
                                                    return (Ahead, nl),
                                                 do nl <- StateT nameLeaf
                                                    return (Here, nl)]
                nameLeaf108_113 = foldl1 mplus [do n <- StateT pat1
                                                   _ <- StateT spaces
                                                   return ()
                                                   com <- optional154_342 (StateT comForErr)
                                                   d867_345 <- get
                                                   t372_346 <- StateT char
                                                   case t372_346 of
                                                       ':' -> return ()
                                                       _ -> gets position >>= (throwError . mkParseError "':'" "not match pattern: " "" d867_345 ["char"])
                                                   let ':' = t372_346
                                                   return ()
                                                   (rf, p) <- StateT leaf
                                                   return (check (n, maybe "" id com) rf p),
                                                do n <- StateT pat1
                                                   _ <- StateT spaces
                                                   return ()
                                                   com <- optional154_342 (StateT comForErr)
                                                   return (check (n,
                                                                  maybe "" id com) (FromVariable Nothing) Nothing)]
                nameLeafNoCom109_114 = foldl1 mplus [do n <- StateT pat1
                                                        _ <- StateT spaces
                                                        return ()
                                                        com <- optional154_342 (StateT comForErr)
                                                        d875_347 <- get
                                                        t380_348 <- StateT char
                                                        case t380_348 of
                                                            ':' -> return ()
                                                            _ -> gets position >>= (throwError . mkParseError "':'" "not match pattern: " "" d875_347 ["char"])
                                                        let ':' = t380_348
                                                        return ()
                                                        (rf, p) <- StateT leaf
                                                        return (check (n, maybe "" id com) rf p),
                                                     do n <- StateT pat1
                                                        _ <- StateT spaces
                                                        return ()
                                                        return (check (n,
                                                                       "") (FromVariable Nothing) Nothing)]
                comForErr110_115 = foldl1 mplus [do d879_349 <- get
                                                    t384_350 <- StateT char
                                                    case t384_350 of
                                                        '{' -> return ()
                                                        _ -> gets position >>= (throwError . mkParseError "'{'" "not match pattern: " "" d879_349 ["char"])
                                                    let '{' = t384_350
                                                    return ()
                                                    d880_351 <- get
                                                    t385_352 <- StateT char
                                                    case t385_352 of
                                                        '-' -> return ()
                                                        _ -> gets position >>= (throwError . mkParseError "'-'" "not match pattern: " "" d880_351 ["char"])
                                                    let '-' = t385_352
                                                    return ()
                                                    d881_353 <- get
                                                    t386_354 <- StateT char
                                                    case t386_354 of
                                                        '#' -> return ()
                                                        _ -> gets position >>= (throwError . mkParseError "'#'" "not match pattern: " "" d881_353 ["char"])
                                                    let '#' = t386_354
                                                    return ()
                                                    _ <- StateT spaces
                                                    return ()
                                                    d883_355 <- get
                                                    t388_356 <- StateT char
                                                    case t388_356 of
                                                        '"' -> return ()
                                                        _ -> gets position >>= (throwError . mkParseError "'\"'" "not match pattern: " "" d883_355 ["char"])
                                                    let '"' = t388_356
                                                    return ()
                                                    s <- StateT stringLit
                                                    d885_357 <- get
                                                    t390_358 <- StateT char
                                                    case t390_358 of
                                                        '"' -> return ()
                                                        _ -> gets position >>= (throwError . mkParseError "'\"'" "not match pattern: " "" d885_357 ["char"])
                                                    let '"' = t390_358
                                                    return ()
                                                    _ <- StateT spaces
                                                    return ()
                                                    d887_359 <- get
                                                    t392_360 <- StateT char
                                                    case t392_360 of
                                                        '#' -> return ()
                                                        _ -> gets position >>= (throwError . mkParseError "'#'" "not match pattern: " "" d887_359 ["char"])
                                                    let '#' = t392_360
                                                    return ()
                                                    d888_361 <- get
                                                    t393_362 <- StateT char
                                                    case t393_362 of
                                                        '-' -> return ()
                                                        _ -> gets position >>= (throwError . mkParseError "'-'" "not match pattern: " "" d888_361 ["char"])
                                                    let '-' = t393_362
                                                    return ()
                                                    d889_363 <- get
                                                    t394_364 <- StateT char
                                                    case t394_364 of
                                                        '}' -> return ()
                                                        _ -> gets position >>= (throwError . mkParseError "'}'" "not match pattern: " "" d889_363 ["char"])
                                                    let '}' = t394_364
                                                    return ()
                                                    _ <- StateT spaces
                                                    return ()
                                                    return s]
                leaf111_116 = foldl1 mplus [do rf <- StateT readFromLs
                                               t <- StateT test
                                               return (rf, Just t),
                                            do rf <- StateT readFromLs
                                               return (rf, Nothing),
                                            do t <- StateT test
                                               return (FromVariable Nothing, Just t)]
                patOp112_117 = foldl1 mplus [do p <- StateT pat
                                                o <- StateT opConName
                                                po <- StateT patOp
                                                return (UInfixP p o po),
                                             do p <- StateT pat
                                                _ <- StateT spaces
                                                return ()
                                                d900_365 <- get
                                                t405_366 <- StateT char
                                                case t405_366 of
                                                    '`' -> return ()
                                                    _ -> gets position >>= (throwError . mkParseError "'`'" "not match pattern: " "" d900_365 ["char"])
                                                let '`' = t405_366
                                                return ()
                                                t <- StateT typ
                                                d902_367 <- get
                                                t407_368 <- StateT char
                                                case t407_368 of
                                                    '`' -> return ()
                                                    _ -> gets position >>= (throwError . mkParseError "'`'" "not match pattern: " "" d902_367 ["char"])
                                                let '`' = t407_368
                                                return ()
                                                _ <- StateT spaces
                                                return ()
                                                po <- StateT patOp
                                                return (UInfixP p (mkName t) po),
                                             do p <- StateT pat
                                                return p]
                pat113_118 = foldl1 mplus [do t <- StateT typ
                                              _ <- StateT spaces
                                              return ()
                                              ps <- StateT pats
                                              return (ConP (mkName t) ps),
                                           do d909_369 <- get
                                              t414_370 <- StateT char
                                              case t414_370 of
                                                  '(' -> return ()
                                                  _ -> gets position >>= (throwError . mkParseError "'('" "not match pattern: " "" d909_369 ["char"])
                                              let '(' = t414_370
                                              return ()
                                              o <- StateT opConName
                                              d911_371 <- get
                                              t416_372 <- StateT char
                                              case t416_372 of
                                                  ')' -> return ()
                                                  _ -> gets position >>= (throwError . mkParseError "')'" "not match pattern: " "" d911_371 ["char"])
                                              let ')' = t416_372
                                              return ()
                                              _ <- StateT spaces
                                              return ()
                                              ps <- StateT pats
                                              return (ConP o ps),
                                           do p <- StateT pat1
                                              return p]
                pat1114_119 = foldl1 mplus [do t <- StateT typ
                                               return (ConP (mkName t) []),
                                            do d916_373 <- get
                                               t421_374 <- StateT variable
                                               case t421_374 of
                                                   "_" -> return ()
                                                   _ -> gets position >>= (throwError . mkParseError "\"_\"" "not match pattern: " "" d916_373 ["variable"])
                                               let "_" = t421_374
                                               return ()
                                               return WildP,
                                            do n <- StateT variable
                                               return (VarP $ mkName n),
                                            do i <- StateT integer
                                               return (LitP (IntegerL i)),
                                            do d919_375 <- get
                                               t424_376 <- StateT char
                                               case t424_376 of
                                                   '-' -> return ()
                                                   _ -> gets position >>= (throwError . mkParseError "'-'" "not match pattern: " "" d919_375 ["char"])
                                               let '-' = t424_376
                                               return ()
                                               _ <- StateT spaces
                                               return ()
                                               i <- StateT integer
                                               return (LitP (IntegerL $ negate i)),
                                            do d922_377 <- get
                                               t427_378 <- StateT char
                                               case t427_378 of
                                                   '\'' -> return ()
                                                   _ -> gets position >>= (throwError . mkParseError "'\\''" "not match pattern: " "" d922_377 ["char"])
                                               let '\'' = t427_378
                                               return ()
                                               c <- StateT charLit
                                               d924_379 <- get
                                               t429_380 <- StateT char
                                               case t429_380 of
                                                   '\'' -> return ()
                                                   _ -> gets position >>= (throwError . mkParseError "'\\''" "not match pattern: " "" d924_379 ["char"])
                                               let '\'' = t429_380
                                               return ()
                                               return (LitP $ CharL c),
                                            do d925_381 <- get
                                               t430_382 <- StateT char
                                               case t430_382 of
                                                   '"' -> return ()
                                                   _ -> gets position >>= (throwError . mkParseError "'\"'" "not match pattern: " "" d925_381 ["char"])
                                               let '"' = t430_382
                                               return ()
                                               s <- StateT stringLit
                                               d927_383 <- get
                                               t432_384 <- StateT char
                                               case t432_384 of
                                                   '"' -> return ()
                                                   _ -> gets position >>= (throwError . mkParseError "'\"'" "not match pattern: " "" d927_383 ["char"])
                                               let '"' = t432_384
                                               return ()
                                               return (LitP $ StringL s),
                                            do d928_385 <- get
                                               t433_386 <- StateT char
                                               case t433_386 of
                                                   '(' -> return ()
                                                   _ -> gets position >>= (throwError . mkParseError "'('" "not match pattern: " "" d928_385 ["char"])
                                               let '(' = t433_386
                                               return ()
                                               p <- StateT patList
                                               d930_387 <- get
                                               t435_388 <- StateT char
                                               case t435_388 of
                                                   ')' -> return ()
                                                   _ -> gets position >>= (throwError . mkParseError "')'" "not match pattern: " "" d930_387 ["char"])
                                               let ')' = t435_388
                                               return ()
                                               return (TupP p),
                                            do d931_389 <- get
                                               t436_390 <- StateT char
                                               case t436_390 of
                                                   '[' -> return ()
                                                   _ -> gets position >>= (throwError . mkParseError "'['" "not match pattern: " "" d931_389 ["char"])
                                               let '[' = t436_390
                                               return ()
                                               p <- StateT patList
                                               d933_391 <- get
                                               t438_392 <- StateT char
                                               case t438_392 of
                                                   ']' -> return ()
                                                   _ -> gets position >>= (throwError . mkParseError "']'" "not match pattern: " "" d933_391 ["char"])
                                               let ']' = t438_392
                                               return ()
                                               return (ListP p)]
                patList115_120 = foldl1 mplus [do p <- StateT patOp
                                                  _ <- StateT spaces
                                                  return ()
                                                  d936_393 <- get
                                                  t441_394 <- StateT char
                                                  case t441_394 of
                                                      ',' -> return ()
                                                      _ -> gets position >>= (throwError . mkParseError "','" "not match pattern: " "" d936_393 ["char"])
                                                  let ',' = t441_394
                                                  return ()
                                                  _ <- StateT spaces
                                                  return ()
                                                  ps <- StateT patList
                                                  return (p : ps),
                                               do p <- StateT patOp
                                                  return [p],
                                               return []]
                opConName116_121 = foldl1 mplus [do d940_395 <- get
                                                    t445_396 <- StateT char
                                                    case t445_396 of
                                                        ':' -> return ()
                                                        _ -> gets position >>= (throwError . mkParseError "':'" "not match pattern: " "" d940_395 ["char"])
                                                    let ':' = t445_396
                                                    return ()
                                                    ot <- StateT opTail
                                                    return (mkName $ ':' : ot)]
                charLit117_122 = foldl1 mplus [do d942_397 <- get
                                                  t447_398 <- StateT char
                                                  let c = t447_398
                                                  unless (c `notElem` "\\'") (gets position >>= (throwError . mkParseError "c `notElem` \"\\\\'\"" "not match: " "" d942_397 ["char"]))
                                                  return c,
                                               do d943_399 <- get
                                                  t448_400 <- StateT char
                                                  case t448_400 of
                                                      '\\' -> return ()
                                                      _ -> gets position >>= (throwError . mkParseError "'\\\\'" "not match pattern: " "" d943_399 ["char"])
                                                  let '\\' = t448_400
                                                  return ()
                                                  c <- StateT escapeC
                                                  return c]
                stringLit118_123 = foldl1 mplus [do d945_401 <- get
                                                    t450_402 <- StateT char
                                                    let c = t450_402
                                                    unless (c `notElem` "\"\\") (gets position >>= (throwError . mkParseError "c `notElem` \"\\\"\\\\\"" "not match: " "" d945_401 ["char"]))
                                                    s <- StateT stringLit
                                                    return (c : s),
                                                 do d947_403 <- get
                                                    t452_404 <- StateT char
                                                    case t452_404 of
                                                        '\\' -> return ()
                                                        _ -> gets position >>= (throwError . mkParseError "'\\\\'" "not match pattern: " "" d947_403 ["char"])
                                                    let '\\' = t452_404
                                                    return ()
                                                    c <- StateT escapeC
                                                    s <- StateT stringLit
                                                    return (c : s),
                                                 return ""]
                escapeC119_124 = foldl1 mplus [do d950_405 <- get
                                                  t455_406 <- StateT char
                                                  case t455_406 of
                                                      '"' -> return ()
                                                      _ -> gets position >>= (throwError . mkParseError "'\"'" "not match pattern: " "" d950_405 ["char"])
                                                  let '"' = t455_406
                                                  return ()
                                                  return '"',
                                               do d951_407 <- get
                                                  t456_408 <- StateT char
                                                  case t456_408 of
                                                      '\'' -> return ()
                                                      _ -> gets position >>= (throwError . mkParseError "'\\''" "not match pattern: " "" d951_407 ["char"])
                                                  let '\'' = t456_408
                                                  return ()
                                                  return '\'',
                                               do d952_409 <- get
                                                  t457_410 <- StateT char
                                                  case t457_410 of
                                                      '\\' -> return ()
                                                      _ -> gets position >>= (throwError . mkParseError "'\\\\'" "not match pattern: " "" d952_409 ["char"])
                                                  let '\\' = t457_410
                                                  return ()
                                                  return '\\',
                                               do d953_411 <- get
                                                  t458_412 <- StateT char
                                                  case t458_412 of
                                                      'n' -> return ()
                                                      _ -> gets position >>= (throwError . mkParseError "'n'" "not match pattern: " "" d953_411 ["char"])
                                                  let 'n' = t458_412
                                                  return ()
                                                  return '\n',
                                               do d954_413 <- get
                                                  t459_414 <- StateT char
                                                  case t459_414 of
                                                      't' -> return ()
                                                      _ -> gets position >>= (throwError . mkParseError "'t'" "not match pattern: " "" d954_413 ["char"])
                                                  let 't' = t459_414
                                                  return ()
                                                  return '\t']
                pats120_125 = foldl1 mplus [do p <- StateT pat
                                               _ <- StateT spaces
                                               return ()
                                               ps <- StateT pats
                                               return (p : ps),
                                            return []]
                readFromLs121_126 = foldl1 mplus [do rf <- StateT readFrom
                                                     d959_415 <- get
                                                     t464_416 <- StateT char
                                                     case t464_416 of
                                                         '*' -> return ()
                                                         _ -> gets position >>= (throwError . mkParseError "'*'" "not match pattern: " "" d959_415 ["char"])
                                                     let '*' = t464_416
                                                     return ()
                                                     return (FromL List rf),
                                                  do rf <- StateT readFrom
                                                     d961_417 <- get
                                                     t466_418 <- StateT char
                                                     case t466_418 of
                                                         '+' -> return ()
                                                         _ -> gets position >>= (throwError . mkParseError "'+'" "not match pattern: " "" d961_417 ["char"])
                                                     let '+' = t466_418
                                                     return ()
                                                     return (FromL List1 rf),
                                                  do rf <- StateT readFrom
                                                     d963_419 <- get
                                                     t468_420 <- StateT char
                                                     case t468_420 of
                                                         '?' -> return ()
                                                         _ -> gets position >>= (throwError . mkParseError "'?'" "not match pattern: " "" d963_419 ["char"])
                                                     let '?' = t468_420
                                                     return ()
                                                     return (FromL Optional rf),
                                                  do rf <- StateT readFrom
                                                     return rf]
                readFrom122_127 = foldl1 mplus [do v <- StateT variable
                                                   return (FromVariable $ Just v),
                                                do d966_421 <- get
                                                   t471_422 <- StateT char
                                                   case t471_422 of
                                                       '(' -> return ()
                                                       _ -> gets position >>= (throwError . mkParseError "'('" "not match pattern: " "" d966_421 ["char"])
                                                   let '(' = t471_422
                                                   return ()
                                                   s <- StateT selection
                                                   d968_423 <- get
                                                   t473_424 <- StateT char
                                                   case t473_424 of
                                                       ')' -> return ()
                                                       _ -> gets position >>= (throwError . mkParseError "')'" "not match pattern: " "" d968_423 ["char"])
                                                   let ')' = t473_424
                                                   return ()
                                                   return (fromSelectionQ s),
                                                do e <- StateT expressionHsSugar
                                                   return (fromSelectionQ $ normalSelectionQ [e])]
                selectCharsLs123_128 = foldl1 mplus [do rf <- StateT selectChars
                                                        d971_425 <- get
                                                        t476_426 <- StateT char
                                                        case t476_426 of
                                                            '*' -> return ()
                                                            _ -> gets position >>= (throwError . mkParseError "'*'" "not match pattern: " "" d971_425 ["char"])
                                                        let '*' = t476_426
                                                        return ()
                                                        return (FromL List rf),
                                                     do rf <- StateT selectChars
                                                        d973_427 <- get
                                                        t478_428 <- StateT char
                                                        case t478_428 of
                                                            '+' -> return ()
                                                            _ -> gets position >>= (throwError . mkParseError "'+'" "not match pattern: " "" d973_427 ["char"])
                                                        let '+' = t478_428
                                                        return ()
                                                        return (FromL List1 rf),
                                                     do rf <- StateT selectChars
                                                        d975_429 <- get
                                                        t480_430 <- StateT char
                                                        case t480_430 of
                                                            '?' -> return ()
                                                            _ -> gets position >>= (throwError . mkParseError "'?'" "not match pattern: " "" d975_429 ["char"])
                                                        let '?' = t480_430
                                                        return ()
                                                        return (FromL Optional rf),
                                                     do rf <- StateT selectChars
                                                        return rf]
                selectChars124_129 = foldl1 mplus [do d977_431 <- get
                                                      t482_432 <- StateT char
                                                      case t482_432 of
                                                          '[' -> return ()
                                                          _ -> gets position >>= (throwError . mkParseError "'['" "not match pattern: " "" d977_431 ["char"])
                                                      let '[' = t482_432
                                                      return ()
                                                      cs <- list1156_433 (foldl1 mplus [do d979_434 <- get
                                                                                           t484_435 <- StateT char
                                                                                           let c = t484_435
                                                                                           unless (isLower c) (gets position >>= (throwError . mkParseError "isLower c" "not match: " "" d979_434 ["char"]))
                                                                                           return c])
                                                      d980_436 <- get
                                                      t485_437 <- StateT char
                                                      case t485_437 of
                                                          ']' -> return ()
                                                          _ -> gets position >>= (throwError . mkParseError "']'" "not match pattern: " "" d980_436 ["char"])
                                                      let ']' = t485_437
                                                      return ()
                                                      return (fromTokenChars cs),
                                                   do d981_438 <- get
                                                      t486_439 <- StateT char
                                                      case t486_439 of
                                                          '[' -> return ()
                                                          _ -> gets position >>= (throwError . mkParseError "'['" "not match pattern: " "" d981_438 ["char"])
                                                      let '[' = t486_439
                                                      return ()
                                                      d982_440 <- get
                                                      t487_441 <- StateT char
                                                      let cb = t487_441
                                                      unless (cb `notElem` "\\-") (gets position >>= (throwError . mkParseError "cb `notElem` \"\\\\-\"" "not match: " "" d982_440 ["char"]))
                                                      d983_442 <- get
                                                      t488_443 <- StateT char
                                                      case t488_443 of
                                                          '-' -> return ()
                                                          _ -> gets position >>= (throwError . mkParseError "'-'" "not match pattern: " "" d983_442 ["char"])
                                                      let '-' = t488_443
                                                      return ()
                                                      d984_444 <- get
                                                      t489_445 <- StateT char
                                                      let ce = t489_445
                                                      unless (ce `notElem` "\\-") (gets position >>= (throwError . mkParseError "ce `notElem` \"\\\\-\"" "not match: " "" d984_444 ["char"]))
                                                      d985_446 <- get
                                                      t490_447 <- StateT char
                                                      case t490_447 of
                                                          ']' -> return ()
                                                          _ -> gets position >>= (throwError . mkParseError "']'" "not match pattern: " "" d985_446 ["char"])
                                                      let ']' = t490_447
                                                      return ()
                                                      return (fromTokenChars [cb .. ce]),
                                                   do d986_448 <- get
                                                      t491_449 <- StateT char
                                                      case t491_449 of
                                                          '\'' -> return ()
                                                          _ -> gets position >>= (throwError . mkParseError "'\\''" "not match pattern: " "" d986_448 ["char"])
                                                      let '\'' = t491_449
                                                      return ()
                                                      d987_450 <- get
                                                      t492_451 <- StateT char
                                                      let c = t492_451
                                                      unless (c `notElem` "\\'") (gets position >>= (throwError . mkParseError "c `notElem` \"\\\\'\"" "not match: " "" d987_450 ["char"]))
                                                      d988_452 <- get
                                                      t493_453 <- StateT char
                                                      case t493_453 of
                                                          '\'' -> return ()
                                                          _ -> gets position >>= (throwError . mkParseError "'\\''" "not match pattern: " "" d988_452 ["char"])
                                                      let '\'' = t493_453
                                                      return ()
                                                      return (fromTokenChars [c])]
                test125_130 = foldl1 mplus [do d989_454 <- get
                                               t494_455 <- StateT char
                                               case t494_455 of
                                                   '[' -> return ()
                                                   _ -> gets position >>= (throwError . mkParseError "'['" "not match pattern: " "" d989_454 ["char"])
                                               let '[' = t494_455
                                               return ()
                                               h <- StateT hsExpLam
                                               _ <- StateT spaces
                                               return ()
                                               com <- optional154_342 (StateT comForErr)
                                               d993_456 <- get
                                               t498_457 <- StateT char
                                               case t498_457 of
                                                   ']' -> return ()
                                                   _ -> gets position >>= (throwError . mkParseError "']'" "not match pattern: " "" d993_456 ["char"])
                                               let ']' = t498_457
                                               return ()
                                               return (h, maybe "" id com)]
                hsExpLam126_131 = foldl1 mplus [do d994_458 <- get
                                                   t499_459 <- StateT char
                                                   case t499_459 of
                                                       '\\' -> return ()
                                                       _ -> gets position >>= (throwError . mkParseError "'\\\\'" "not match pattern: " "" d994_458 ["char"])
                                                   let '\\' = t499_459
                                                   return ()
                                                   _ <- StateT spaces
                                                   return ()
                                                   ps <- StateT pats
                                                   _ <- StateT spaces
                                                   return ()
                                                   d998_460 <- get
                                                   t503_461 <- StateT char
                                                   case t503_461 of
                                                       '-' -> return ()
                                                       _ -> gets position >>= (throwError . mkParseError "'-'" "not match pattern: " "" d998_460 ["char"])
                                                   let '-' = t503_461
                                                   return ()
                                                   d999_462 <- get
                                                   t504_463 <- StateT char
                                                   case t504_463 of
                                                       '>' -> return ()
                                                       _ -> gets position >>= (throwError . mkParseError "'>'" "not match pattern: " "" d999_462 ["char"])
                                                   let '>' = t504_463
                                                   return ()
                                                   _ <- StateT spaces
                                                   return ()
                                                   e <- StateT hsExpTyp
                                                   return (LamE ps e),
                                                do e <- StateT hsExpTyp
                                                   return e]
                hsExpTyp127_132 = foldl1 mplus [do eo <- StateT hsExpOp
                                                   d1004_464 <- get
                                                   t509_465 <- StateT char
                                                   case t509_465 of
                                                       ':' -> return ()
                                                       _ -> gets position >>= (throwError . mkParseError "':'" "not match pattern: " "" d1004_464 ["char"])
                                                   let ':' = t509_465
                                                   return ()
                                                   d1005_466 <- get
                                                   t510_467 <- StateT char
                                                   case t510_467 of
                                                       ':' -> return ()
                                                       _ -> gets position >>= (throwError . mkParseError "':'" "not match pattern: " "" d1005_466 ["char"])
                                                   let ':' = t510_467
                                                   return ()
                                                   _ <- StateT spaces
                                                   return ()
                                                   t <- StateT hsTypeArr
                                                   return (SigE eo t),
                                                do eo <- StateT hsExpOp
                                                   return eo]
                hsExpOp128_133 = foldl1 mplus [do l <- StateT hsExp
                                                  _ <- StateT spaces
                                                  return ()
                                                  o <- StateT hsOp
                                                  _ <- StateT spaces
                                                  return ()
                                                  r <- StateT hsExpOp
                                                  return (UInfixE (l id) o r),
                                               do e <- StateT hsExp
                                                  return (e id)]
                hsOp129_134 = foldl1 mplus [do d1015_468 <- get
                                               t520_469 <- StateT char
                                               let c = t520_469
                                               unless (c `elem` "+*/-!|&.^=<>$") (gets position >>= (throwError . mkParseError "c `elem` \"+*/-!|&.^=<>$\"" "not match: " "" d1015_468 ["char"]))
                                               o <- StateT opTail
                                               return (VarE $ mkName $ c : o),
                                            do d1017_470 <- get
                                               t522_471 <- StateT char
                                               case t522_471 of
                                                   ':' -> return ()
                                                   _ -> gets position >>= (throwError . mkParseError "':'" "not match pattern: " "" d1017_470 ["char"])
                                               let ':' = t522_471
                                               return ()
                                               d1019_472 <- get
                                               do err1152_473 <- ((do d1018_474 <- get
                                                                      t523_475 <- StateT char
                                                                      case t523_475 of
                                                                          ':' -> return ()
                                                                          _ -> gets position >>= (throwError . mkParseError "':'" "not match pattern: " "" d1018_474 ["char"])
                                                                      let ':' = t523_475
                                                                      return ()) >> return False) `catchError` const (return True)
                                                  unless err1152_473 (gets position >>= (throwError . mkParseError "!':':" "not match: " "" d1019_472 ["char"]))
                                               put d1019_472
                                               o <- StateT opTail
                                               return (ConE $ mkName $ ':' : o),
                                            do d1021_476 <- get
                                               t525_477 <- StateT char
                                               case t525_477 of
                                                   '`' -> return ()
                                                   _ -> gets position >>= (throwError . mkParseError "'`'" "not match pattern: " "" d1021_476 ["char"])
                                               let '`' = t525_477
                                               return ()
                                               v <- StateT variable
                                               d1023_478 <- get
                                               t527_479 <- StateT char
                                               case t527_479 of
                                                   '`' -> return ()
                                                   _ -> gets position >>= (throwError . mkParseError "'`'" "not match pattern: " "" d1023_478 ["char"])
                                               let '`' = t527_479
                                               return ()
                                               return (VarE $ mkName v),
                                            do d1024_480 <- get
                                               t528_481 <- StateT char
                                               case t528_481 of
                                                   '`' -> return ()
                                                   _ -> gets position >>= (throwError . mkParseError "'`'" "not match pattern: " "" d1024_480 ["char"])
                                               let '`' = t528_481
                                               return ()
                                               t <- StateT typ
                                               d1026_482 <- get
                                               t530_483 <- StateT char
                                               case t530_483 of
                                                   '`' -> return ()
                                                   _ -> gets position >>= (throwError . mkParseError "'`'" "not match pattern: " "" d1026_482 ["char"])
                                               let '`' = t530_483
                                               return ()
                                               return (ConE $ mkName t)]
                opTail130_135 = foldl1 mplus [do d1027_484 <- get
                                                 t531_485 <- StateT char
                                                 let c = t531_485
                                                 unless (c `elem` ":+*/-!|&.^=<>$") (gets position >>= (throwError . mkParseError "c `elem` \":+*/-!|&.^=<>$\"" "not match: " "" d1027_484 ["char"]))
                                                 s <- StateT opTail
                                                 return (c : s),
                                              return ""]
                hsExp131_136 = foldl1 mplus [do e <- StateT hsExp1
                                                _ <- StateT spaces
                                                return ()
                                                h <- StateT hsExp
                                                return (\f -> h (f e `AppE`)),
                                             do e <- StateT hsExp1
                                                return (\f -> f e)]
                hsExp1132_137 = foldl1 mplus [do d1033_486 <- get
                                                 t537_487 <- StateT char
                                                 case t537_487 of
                                                     '(' -> return ()
                                                     _ -> gets position >>= (throwError . mkParseError "'('" "not match pattern: " "" d1033_486 ["char"])
                                                 let '(' = t537_487
                                                 return ()
                                                 l <- optional154_342 (foldl1 mplus [do e <- StateT hsExpTyp
                                                                                        return e])
                                                 _ <- StateT spaces
                                                 return ()
                                                 o <- StateT hsOp
                                                 _ <- StateT spaces
                                                 return ()
                                                 r <- optional154_342 (foldl1 mplus [do e <- StateT hsExpTyp
                                                                                        return e])
                                                 d1041_488 <- get
                                                 t545_489 <- StateT char
                                                 case t545_489 of
                                                     ')' -> return ()
                                                     _ -> gets position >>= (throwError . mkParseError "')'" "not match pattern: " "" d1041_488 ["char"])
                                                 let ')' = t545_489
                                                 return ()
                                                 return (InfixE l o r),
                                              do d1042_490 <- get
                                                 t546_491 <- StateT char
                                                 case t546_491 of
                                                     '(' -> return ()
                                                     _ -> gets position >>= (throwError . mkParseError "'('" "not match pattern: " "" d1042_490 ["char"])
                                                 let '(' = t546_491
                                                 return ()
                                                 et <- StateT hsExpTpl
                                                 d1044_492 <- get
                                                 t548_493 <- StateT char
                                                 case t548_493 of
                                                     ')' -> return ()
                                                     _ -> gets position >>= (throwError . mkParseError "')'" "not match pattern: " "" d1044_492 ["char"])
                                                 let ')' = t548_493
                                                 return ()
                                                 return (TupE et),
                                              do d1045_494 <- get
                                                 t549_495 <- StateT char
                                                 case t549_495 of
                                                     '[' -> return ()
                                                     _ -> gets position >>= (throwError . mkParseError "'['" "not match pattern: " "" d1045_494 ["char"])
                                                 let '[' = t549_495
                                                 return ()
                                                 et <- StateT hsExpTpl
                                                 d1047_496 <- get
                                                 t551_497 <- StateT char
                                                 case t551_497 of
                                                     ']' -> return ()
                                                     _ -> gets position >>= (throwError . mkParseError "']'" "not match pattern: " "" d1047_496 ["char"])
                                                 let ']' = t551_497
                                                 return ()
                                                 return (ListE et),
                                              do v <- StateT variable
                                                 return (VarE $ mkName v),
                                              do t <- StateT typ
                                                 return (ConE $ mkName t),
                                              do i <- StateT integer
                                                 _ <- StateT spaces
                                                 return ()
                                                 return (LitE $ integerL i),
                                              do d1052_498 <- get
                                                 t556_499 <- StateT char
                                                 case t556_499 of
                                                     '\'' -> return ()
                                                     _ -> gets position >>= (throwError . mkParseError "'\\''" "not match pattern: " "" d1052_498 ["char"])
                                                 let '\'' = t556_499
                                                 return ()
                                                 c <- StateT charLit
                                                 d1054_500 <- get
                                                 t558_501 <- StateT char
                                                 case t558_501 of
                                                     '\'' -> return ()
                                                     _ -> gets position >>= (throwError . mkParseError "'\\''" "not match pattern: " "" d1054_500 ["char"])
                                                 let '\'' = t558_501
                                                 return ()
                                                 return (LitE $ charL c),
                                              do d1055_502 <- get
                                                 t559_503 <- StateT char
                                                 case t559_503 of
                                                     '"' -> return ()
                                                     _ -> gets position >>= (throwError . mkParseError "'\"'" "not match pattern: " "" d1055_502 ["char"])
                                                 let '"' = t559_503
                                                 return ()
                                                 s <- StateT stringLit
                                                 d1057_504 <- get
                                                 t561_505 <- StateT char
                                                 case t561_505 of
                                                     '"' -> return ()
                                                     _ -> gets position >>= (throwError . mkParseError "'\"'" "not match pattern: " "" d1057_504 ["char"])
                                                 let '"' = t561_505
                                                 return ()
                                                 return (LitE $ stringL s),
                                              do d1058_506 <- get
                                                 t562_507 <- StateT char
                                                 case t562_507 of
                                                     '-' -> return ()
                                                     _ -> gets position >>= (throwError . mkParseError "'-'" "not match pattern: " "" d1058_506 ["char"])
                                                 let '-' = t562_507
                                                 return ()
                                                 _ <- StateT spaces
                                                 return ()
                                                 e <- StateT hsExp1
                                                 return (AppE (VarE $ mkName "negate") e)]
                hsExpTpl133_138 = foldl1 mplus [do e <- StateT hsExpLam
                                                   _ <- StateT spaces
                                                   return ()
                                                   d1063_508 <- get
                                                   t567_509 <- StateT char
                                                   case t567_509 of
                                                       ',' -> return ()
                                                       _ -> gets position >>= (throwError . mkParseError "','" "not match pattern: " "" d1063_508 ["char"])
                                                   let ',' = t567_509
                                                   return ()
                                                   _ <- StateT spaces
                                                   return ()
                                                   et <- StateT hsExpTpl
                                                   return (e : et),
                                                do e <- StateT hsExpLam
                                                   return [e],
                                                return []]
                hsTypeArr134_139 = foldl1 mplus [do l <- StateT hsType
                                                    d1068_510 <- get
                                                    t572_511 <- StateT char
                                                    case t572_511 of
                                                        '-' -> return ()
                                                        _ -> gets position >>= (throwError . mkParseError "'-'" "not match pattern: " "" d1068_510 ["char"])
                                                    let '-' = t572_511
                                                    return ()
                                                    d1069_512 <- get
                                                    t573_513 <- StateT char
                                                    case t573_513 of
                                                        '>' -> return ()
                                                        _ -> gets position >>= (throwError . mkParseError "'>'" "not match pattern: " "" d1069_512 ["char"])
                                                    let '>' = t573_513
                                                    return ()
                                                    _ <- StateT spaces
                                                    return ()
                                                    r <- StateT hsTypeArr
                                                    return (AppT (AppT ArrowT $ l id) r),
                                                 do t <- StateT hsType
                                                    return (t id)]
                hsType135_140 = foldl1 mplus [do t <- StateT hsType1
                                                 ts <- StateT hsType
                                                 return (\f -> ts (f t `AppT`)),
                                              do t <- StateT hsType1
                                                 return ($ t)]
                hsType1136_141 = foldl1 mplus [do d1076_514 <- get
                                                  t580_515 <- StateT char
                                                  case t580_515 of
                                                      '[' -> return ()
                                                      _ -> gets position >>= (throwError . mkParseError "'['" "not match pattern: " "" d1076_514 ["char"])
                                                  let '[' = t580_515
                                                  return ()
                                                  d1077_516 <- get
                                                  t581_517 <- StateT char
                                                  case t581_517 of
                                                      ']' -> return ()
                                                      _ -> gets position >>= (throwError . mkParseError "']'" "not match pattern: " "" d1077_516 ["char"])
                                                  let ']' = t581_517
                                                  return ()
                                                  _ <- StateT spaces
                                                  return ()
                                                  return ListT,
                                               do d1079_518 <- get
                                                  t583_519 <- StateT char
                                                  case t583_519 of
                                                      '[' -> return ()
                                                      _ -> gets position >>= (throwError . mkParseError "'['" "not match pattern: " "" d1079_518 ["char"])
                                                  let '[' = t583_519
                                                  return ()
                                                  t <- StateT hsTypeArr
                                                  d1081_520 <- get
                                                  t585_521 <- StateT char
                                                  case t585_521 of
                                                      ']' -> return ()
                                                      _ -> gets position >>= (throwError . mkParseError "']'" "not match pattern: " "" d1081_520 ["char"])
                                                  let ']' = t585_521
                                                  return ()
                                                  _ <- StateT spaces
                                                  return ()
                                                  return (ListT `AppT` t),
                                               do d1083_522 <- get
                                                  t587_523 <- StateT char
                                                  case t587_523 of
                                                      '(' -> return ()
                                                      _ -> gets position >>= (throwError . mkParseError "'('" "not match pattern: " "" d1083_522 ["char"])
                                                  let '(' = t587_523
                                                  return ()
                                                  _ <- StateT spaces
                                                  return ()
                                                  tt <- StateT hsTypeTpl
                                                  d1086_524 <- get
                                                  t590_525 <- StateT char
                                                  case t590_525 of
                                                      ')' -> return ()
                                                      _ -> gets position >>= (throwError . mkParseError "')'" "not match pattern: " "" d1086_524 ["char"])
                                                  let ')' = t590_525
                                                  return ()
                                                  _ <- StateT spaces
                                                  return ()
                                                  return (foldl AppT (TupleT $ length tt) tt),
                                               do t <- StateT typToken
                                                  return (ConT $ mkName t),
                                               do d1089_526 <- get
                                                  t593_527 <- StateT char
                                                  case t593_527 of
                                                      '(' -> return ()
                                                      _ -> gets position >>= (throwError . mkParseError "'('" "not match pattern: " "" d1089_526 ["char"])
                                                  let '(' = t593_527
                                                  return ()
                                                  d1090_528 <- get
                                                  t594_529 <- StateT char
                                                  case t594_529 of
                                                      '-' -> return ()
                                                      _ -> gets position >>= (throwError . mkParseError "'-'" "not match pattern: " "" d1090_528 ["char"])
                                                  let '-' = t594_529
                                                  return ()
                                                  d1091_530 <- get
                                                  t595_531 <- StateT char
                                                  case t595_531 of
                                                      '>' -> return ()
                                                      _ -> gets position >>= (throwError . mkParseError "'>'" "not match pattern: " "" d1091_530 ["char"])
                                                  let '>' = t595_531
                                                  return ()
                                                  d1092_532 <- get
                                                  t596_533 <- StateT char
                                                  case t596_533 of
                                                      ')' -> return ()
                                                      _ -> gets position >>= (throwError . mkParseError "')'" "not match pattern: " "" d1092_532 ["char"])
                                                  let ')' = t596_533
                                                  return ()
                                                  _ <- StateT spaces
                                                  return ()
                                                  return ArrowT]
                hsTypeTpl137_142 = foldl1 mplus [do t <- StateT hsTypeArr
                                                    d1095_534 <- get
                                                    t599_535 <- StateT char
                                                    case t599_535 of
                                                        ',' -> return ()
                                                        _ -> gets position >>= (throwError . mkParseError "','" "not match pattern: " "" d1095_534 ["char"])
                                                    let ',' = t599_535
                                                    return ()
                                                    _ <- StateT spaces
                                                    return ()
                                                    tt <- StateT hsTypeTpl
                                                    return (t : tt),
                                                 do t <- StateT hsTypeArr
                                                    return [t],
                                                 return []]
                typ138_143 = foldl1 mplus [do u <- StateT upper
                                              t <- StateT tvtail
                                              return (u : t)]
                variable139_144 = foldl1 mplus [do l <- StateT lower
                                                   t <- StateT tvtail
                                                   return (l : t)]
                tvtail140_145 = foldl1 mplus [do a <- StateT alpha
                                                 t <- StateT tvtail
                                                 return (a : t),
                                              return ""]
                integer141_146 = foldl1 mplus [do dh <- StateT digit
                                                  ds <- list155_335 (foldl1 mplus [do d <- StateT digit
                                                                                      return d])
                                                  return (read $ dh : ds)]
                alpha142_147 = foldl1 mplus [do u <- StateT upper
                                                return u,
                                             do l <- StateT lower
                                                return l,
                                             do d <- StateT digit
                                                return d,
                                             do d1111_536 <- get
                                                t615_537 <- StateT char
                                                case t615_537 of
                                                    '\'' -> return ()
                                                    _ -> gets position >>= (throwError . mkParseError "'\\''" "not match pattern: " "" d1111_536 ["char"])
                                                let '\'' = t615_537
                                                return ()
                                                return '\'']
                upper143_148 = foldl1 mplus [do d1112_538 <- get
                                                t616_539 <- StateT char
                                                let u = t616_539
                                                unless (isUpper u) (gets position >>= (throwError . mkParseError "isUpper u" "not match: " "" d1112_538 ["char"]))
                                                return u]
                lower144_149 = foldl1 mplus [do d1113_540 <- get
                                                t617_541 <- StateT char
                                                let l = t617_541
                                                unless (isLower l || l == '_') (gets position >>= (throwError . mkParseError "isLower l || l == '_'" "not match: " "" d1113_540 ["char"]))
                                                return l]
                digit145_150 = foldl1 mplus [do d1114_542 <- get
                                                t618_543 <- StateT char
                                                let d = t618_543
                                                unless (isDigit d) (gets position >>= (throwError . mkParseError "isDigit d" "not match: " "" d1114_542 ["char"]))
                                                return d]
                spaces146_151 = foldl1 mplus [do _ <- StateT space
                                                 return ()
                                                 _ <- StateT spaces
                                                 return ()
                                                 return (),
                                              return ()]
                space147_152 = foldl1 mplus [do d1117_544 <- get
                                                t621_545 <- StateT char
                                                let s = t621_545
                                                unless (isSpace s) (gets position >>= (throwError . mkParseError "isSpace s" "not match: " "" d1117_544 ["char"]))
                                                return (),
                                             do d1118_546 <- get
                                                t622_547 <- StateT char
                                                case t622_547 of
                                                    '-' -> return ()
                                                    _ -> gets position >>= (throwError . mkParseError "'-'" "not match pattern: " "" d1118_546 ["char"])
                                                let '-' = t622_547
                                                return ()
                                                d1119_548 <- get
                                                t623_549 <- StateT char
                                                case t623_549 of
                                                    '-' -> return ()
                                                    _ -> gets position >>= (throwError . mkParseError "'-'" "not match pattern: " "" d1119_548 ["char"])
                                                let '-' = t623_549
                                                return ()
                                                _ <- StateT notNLString
                                                return ()
                                                _ <- StateT newLine
                                                return ()
                                                return (),
                                             do _ <- StateT comment
                                                return ()
                                                return ()]
                notNLString148_153 = foldl1 mplus [do d1124_550 <- get
                                                      do err1153_551 <- ((do _ <- StateT newLine
                                                                             return ()) >> return False) `catchError` const (return True)
                                                         unless err1153_551 (gets position >>= (throwError . mkParseError "!_:newLine" "not match: " "" d1124_550 ["newLine"]))
                                                      put d1124_550
                                                      c <- StateT char
                                                      s <- StateT notNLString
                                                      return (c : s),
                                                   return ""]
                newLine149_154 = foldl1 mplus [do d1127_552 <- get
                                                  t630_553 <- StateT char
                                                  case t630_553 of
                                                      '\n' -> return ()
                                                      _ -> gets position >>= (throwError . mkParseError "'\\n'" "not match pattern: " "" d1127_552 ["char"])
                                                  let '\n' = t630_553
                                                  return ()
                                                  return ()]
                comment150_155 = foldl1 mplus [do d1128_554 <- get
                                                  t631_555 <- StateT char
                                                  case t631_555 of
                                                      '{' -> return ()
                                                      _ -> gets position >>= (throwError . mkParseError "'{'" "not match pattern: " "" d1128_554 ["char"])
                                                  let '{' = t631_555
                                                  return ()
                                                  d1129_556 <- get
                                                  t632_557 <- StateT char
                                                  case t632_557 of
                                                      '-' -> return ()
                                                      _ -> gets position >>= (throwError . mkParseError "'-'" "not match pattern: " "" d1129_556 ["char"])
                                                  let '-' = t632_557
                                                  return ()
                                                  d1131_558 <- get
                                                  do err1154_559 <- ((do d1130_560 <- get
                                                                         t633_561 <- StateT char
                                                                         case t633_561 of
                                                                             '#' -> return ()
                                                                             _ -> gets position >>= (throwError . mkParseError "'#'" "not match pattern: " "" d1130_560 ["char"])
                                                                         let '#' = t633_561
                                                                         return ()) >> return False) `catchError` const (return True)
                                                     unless err1154_559 (gets position >>= (throwError . mkParseError "!'#':" "not match: " "" d1131_558 ["char"]))
                                                  put d1131_558
                                                  _ <- StateT comments
                                                  return ()
                                                  _ <- StateT comEnd
                                                  return ()
                                                  return ()]
                comments151_156 = foldl1 mplus [do _ <- StateT notComStr
                                                   return ()
                                                   _ <- StateT comment
                                                   return ()
                                                   _ <- StateT comments
                                                   return ()
                                                   return (),
                                                do _ <- StateT notComStr
                                                   return ()
                                                   return ()]
                notComStr152_157 = foldl1 mplus [do d1139_562 <- get
                                                    do err1155_563 <- ((do _ <- StateT comment
                                                                           return ()) >> return False) `catchError` const (return True)
                                                       unless err1155_563 (gets position >>= (throwError . mkParseError "!_:comment" "not match: " "" d1139_562 ["comment"]))
                                                    put d1139_562
                                                    d1141_564 <- get
                                                    do err1156_565 <- ((do _ <- StateT comEnd
                                                                           return ()) >> return False) `catchError` const (return True)
                                                       unless err1156_565 (gets position >>= (throwError . mkParseError "!_:comEnd" "not match: " "" d1141_564 ["comEnd"]))
                                                    put d1141_564
                                                    _ <- StateT char
                                                    return ()
                                                    _ <- StateT notComStr
                                                    return ()
                                                    return (),
                                                 return ()]
                comEnd153_158 = foldl1 mplus [do d1144_566 <- get
                                                 t644_567 <- StateT char
                                                 case t644_567 of
                                                     '-' -> return ()
                                                     _ -> gets position >>= (throwError . mkParseError "'-'" "not match pattern: " "" d1144_566 ["char"])
                                                 let '-' = t644_567
                                                 return ()
                                                 d1145_568 <- get
                                                 t645_569 <- StateT char
                                                 case t645_569 of
                                                     '}' -> return ()
                                                     _ -> gets position >>= (throwError . mkParseError "'}'" "not match pattern: " "" d1145_568 ["char"])
                                                 let '}' = t645_569
                                                 return ()
                                                 return ()]
                list155_335 :: forall m a . (MonadPlus m, Applicative m) =>
                                            m a -> m ([a])
                list1156_433 :: forall m a . (MonadPlus m, Applicative m) =>
                                             m a -> m ([a])
                list155_335 p = list1156_433 p `mplus` return []
                list1156_433 p = ((:) <$> p) <*> list155_335 p
                optional154_342 :: forall m a . (MonadPlus m, Applicative m) =>
                                                m a -> m (Maybe a)
                optional154_342 p = (Just <$> p) `mplus` return Nothing

