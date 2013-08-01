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
              monadType :: (ErrorT (ParseError (Pos String) Derivs)
                                   Identity
                                   ((Type, Derivs))),
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
parse = parse1176_0 initialPos
          where parse1176_0 pos1175_1 s1177_2 = d655_3
                                where d655_3 = Derivs pegFile0_4 pragmas1_5 pragma2_6 pragmaStr3_7 pragmaItems4_8 pragmaEnd5_9 moduleDec6_10 moduleName7_11 moduleDecStr8_12 whr9_13 preImpPap10_14 prePeg11_15 afterPeg12_16 importPapillon13_17 varToken14_18 typToken15_19 pap16_20 peg17_21 monadType18_22 sourceType19_23 peg_20_24 definition21_25 selection22_26 normalSelection23_27 plainSelection24_28 expressionHs25_29 expressionHsSugar26_30 plainExpressionHs27_31 plainHAReadFromLs28_32 plainReadFromLs29_33 expression30_34 nameLeaf_31_35 nameLeaf32_36 nameLeafNoCom33_37 comForErr34_38 leaf35_39 patOp36_40 pat37_41 pat138_42 patList39_43 opConName40_44 charLit41_45 stringLit42_46 escapeC43_47 pats44_48 readFromLs45_49 readFrom46_50 selectCharsLs47_51 selectChars48_52 test49_53 hsExpLam50_54 hsExpTyp51_55 hsExpOp52_56 hsOp53_57 opTail54_58 hsExp55_59 hsExp156_60 hsExpTpl57_61 hsTypeArr58_62 hsType59_63 hsType160_64 hsTypeTpl61_65 typ62_66 variable63_67 tvtail64_68 integer65_69 alpha66_70 upper67_71 lower68_72 digit69_73 spaces70_74 space71_75 notNLString72_76 newLine73_77 comment74_78 comments75_79 notComStr76_80 comEnd77_81 chars1178_82 pos1175_1
                                      pegFile0_4 = runStateT pegFile78_83 d655_3
                                      pragmas1_5 = runStateT pragmas79_84 d655_3
                                      pragma2_6 = runStateT pragma80_85 d655_3
                                      pragmaStr3_7 = runStateT pragmaStr81_86 d655_3
                                      pragmaItems4_8 = runStateT pragmaItems82_87 d655_3
                                      pragmaEnd5_9 = runStateT pragmaEnd83_88 d655_3
                                      moduleDec6_10 = runStateT moduleDec84_89 d655_3
                                      moduleName7_11 = runStateT moduleName85_90 d655_3
                                      moduleDecStr8_12 = runStateT moduleDecStr86_91 d655_3
                                      whr9_13 = runStateT whr87_92 d655_3
                                      preImpPap10_14 = runStateT preImpPap88_93 d655_3
                                      prePeg11_15 = runStateT prePeg89_94 d655_3
                                      afterPeg12_16 = runStateT afterPeg90_95 d655_3
                                      importPapillon13_17 = runStateT importPapillon91_96 d655_3
                                      varToken14_18 = runStateT varToken92_97 d655_3
                                      typToken15_19 = runStateT typToken93_98 d655_3
                                      pap16_20 = runStateT pap94_99 d655_3
                                      peg17_21 = runStateT peg95_100 d655_3
                                      monadType18_22 = runStateT monadType96_101 d655_3
                                      sourceType19_23 = runStateT sourceType97_102 d655_3
                                      peg_20_24 = runStateT peg_98_103 d655_3
                                      definition21_25 = runStateT definition99_104 d655_3
                                      selection22_26 = runStateT selection100_105 d655_3
                                      normalSelection23_27 = runStateT normalSelection101_106 d655_3
                                      plainSelection24_28 = runStateT plainSelection102_107 d655_3
                                      expressionHs25_29 = runStateT expressionHs103_108 d655_3
                                      expressionHsSugar26_30 = runStateT expressionHsSugar104_109 d655_3
                                      plainExpressionHs27_31 = runStateT plainExpressionHs105_110 d655_3
                                      plainHAReadFromLs28_32 = runStateT plainHAReadFromLs106_111 d655_3
                                      plainReadFromLs29_33 = runStateT plainReadFromLs107_112 d655_3
                                      expression30_34 = runStateT expression108_113 d655_3
                                      nameLeaf_31_35 = runStateT nameLeaf_109_114 d655_3
                                      nameLeaf32_36 = runStateT nameLeaf110_115 d655_3
                                      nameLeafNoCom33_37 = runStateT nameLeafNoCom111_116 d655_3
                                      comForErr34_38 = runStateT comForErr112_117 d655_3
                                      leaf35_39 = runStateT leaf113_118 d655_3
                                      patOp36_40 = runStateT patOp114_119 d655_3
                                      pat37_41 = runStateT pat115_120 d655_3
                                      pat138_42 = runStateT pat1116_121 d655_3
                                      patList39_43 = runStateT patList117_122 d655_3
                                      opConName40_44 = runStateT opConName118_123 d655_3
                                      charLit41_45 = runStateT charLit119_124 d655_3
                                      stringLit42_46 = runStateT stringLit120_125 d655_3
                                      escapeC43_47 = runStateT escapeC121_126 d655_3
                                      pats44_48 = runStateT pats122_127 d655_3
                                      readFromLs45_49 = runStateT readFromLs123_128 d655_3
                                      readFrom46_50 = runStateT readFrom124_129 d655_3
                                      selectCharsLs47_51 = runStateT selectCharsLs125_130 d655_3
                                      selectChars48_52 = runStateT selectChars126_131 d655_3
                                      test49_53 = runStateT test127_132 d655_3
                                      hsExpLam50_54 = runStateT hsExpLam128_133 d655_3
                                      hsExpTyp51_55 = runStateT hsExpTyp129_134 d655_3
                                      hsExpOp52_56 = runStateT hsExpOp130_135 d655_3
                                      hsOp53_57 = runStateT hsOp131_136 d655_3
                                      opTail54_58 = runStateT opTail132_137 d655_3
                                      hsExp55_59 = runStateT hsExp133_138 d655_3
                                      hsExp156_60 = runStateT hsExp1134_139 d655_3
                                      hsExpTpl57_61 = runStateT hsExpTpl135_140 d655_3
                                      hsTypeArr58_62 = runStateT hsTypeArr136_141 d655_3
                                      hsType59_63 = runStateT hsType137_142 d655_3
                                      hsType160_64 = runStateT hsType1138_143 d655_3
                                      hsTypeTpl61_65 = runStateT hsTypeTpl139_144 d655_3
                                      typ62_66 = runStateT typ140_145 d655_3
                                      variable63_67 = runStateT variable141_146 d655_3
                                      tvtail64_68 = runStateT tvtail142_147 d655_3
                                      integer65_69 = runStateT integer143_148 d655_3
                                      alpha66_70 = runStateT alpha144_149 d655_3
                                      upper67_71 = runStateT upper145_150 d655_3
                                      lower68_72 = runStateT lower146_151 d655_3
                                      digit69_73 = runStateT digit147_152 d655_3
                                      spaces70_74 = runStateT spaces148_153 d655_3
                                      space71_75 = runStateT space149_154 d655_3
                                      notNLString72_76 = runStateT notNLString150_155 d655_3
                                      newLine73_77 = runStateT newLine151_156 d655_3
                                      comment74_78 = runStateT comment152_157 d655_3
                                      comments75_79 = runStateT comments153_158 d655_3
                                      notComStr76_80 = runStateT notComStr154_159 d655_3
                                      comEnd77_81 = runStateT comEnd155_160 d655_3
                                      chars1178_82 = runStateT (case getToken s1177_2 of
                                                                    Just (c1173_161,
                                                                          s'1174_162) -> do put (parse1176_0 (updatePos c1173_161 pos1175_1) s'1174_162)
                                                                                            return c1173_161
                                                                    _ -> gets position >>= (throwError . mkParseError "" "end of input" "" undefined [])) d655_3
                pegFile78_83 = foldl1 mplus [do pr <- StateT pragmas
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
                                                d663_163 <- get
                                                t167_164 <- StateT char
                                                case t167_164 of
                                                    '|' -> return ()
                                                    _ -> gets position >>= (throwError . mkParseError "'|'" "not match pattern: " "" d663_163 ["char"])
                                                let '|' = t167_164
                                                return ()
                                                d664_165 <- get
                                                t168_166 <- StateT char
                                                case t168_166 of
                                                    ']' -> return ()
                                                    _ -> gets position >>= (throwError . mkParseError "']'" "not match pattern: " "" d664_165 ["char"])
                                                let ']' = t168_166
                                                return ()
                                                d665_167 <- get
                                                t169_168 <- StateT char
                                                case t169_168 of
                                                    '\n' -> return ()
                                                    _ -> gets position >>= (throwError . mkParseError "'\\n'" "not match pattern: " "" d665_167 ["char"])
                                                let '\n' = t169_168
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
                                                d673_169 <- get
                                                t177_170 <- StateT char
                                                case t177_170 of
                                                    '|' -> return ()
                                                    _ -> gets position >>= (throwError . mkParseError "'|'" "not match pattern: " "" d673_169 ["char"])
                                                let '|' = t177_170
                                                return ()
                                                d674_171 <- get
                                                t178_172 <- StateT char
                                                case t178_172 of
                                                    ']' -> return ()
                                                    _ -> gets position >>= (throwError . mkParseError "']'" "not match pattern: " "" d674_171 ["char"])
                                                let ']' = t178_172
                                                return ()
                                                d675_173 <- get
                                                t179_174 <- StateT char
                                                case t179_174 of
                                                    '\n' -> return ()
                                                    _ -> gets position >>= (throwError . mkParseError "'\\n'" "not match pattern: " "" d675_173 ["char"])
                                                let '\n' = t179_174
                                                return ()
                                                atp <- StateT afterPeg
                                                return (mkPegFile pr md [] pp p atp)]
                pragmas79_84 = foldl1 mplus [do _ <- StateT spaces
                                                return ()
                                                pr <- StateT pragma
                                                prs <- StateT pragmas
                                                return (pr : prs),
                                             do _ <- StateT spaces
                                                return ()
                                                return []]
                pragma80_85 = foldl1 mplus [do d681_175 <- get
                                               t185_176 <- StateT char
                                               case t185_176 of
                                                   '{' -> return ()
                                                   _ -> gets position >>= (throwError . mkParseError "'{'" "not match pattern: " "" d681_175 ["char"])
                                               let '{' = t185_176
                                               return ()
                                               d682_177 <- get
                                               t186_178 <- StateT char
                                               case t186_178 of
                                                   '-' -> return ()
                                                   _ -> gets position >>= (throwError . mkParseError "'-'" "not match pattern: " "" d682_177 ["char"])
                                               let '-' = t186_178
                                               return ()
                                               d683_179 <- get
                                               t187_180 <- StateT char
                                               case t187_180 of
                                                   '#' -> return ()
                                                   _ -> gets position >>= (throwError . mkParseError "'#'" "not match pattern: " "" d683_179 ["char"])
                                               let '#' = t187_180
                                               return ()
                                               _ <- StateT spaces
                                               return ()
                                               d685_181 <- get
                                               t189_182 <- StateT char
                                               case t189_182 of
                                                   'L' -> return ()
                                                   _ -> gets position >>= (throwError . mkParseError "'L'" "not match pattern: " "" d685_181 ["char"])
                                               let 'L' = t189_182
                                               return ()
                                               d686_183 <- get
                                               t190_184 <- StateT char
                                               case t190_184 of
                                                   'A' -> return ()
                                                   _ -> gets position >>= (throwError . mkParseError "'A'" "not match pattern: " "" d686_183 ["char"])
                                               let 'A' = t190_184
                                               return ()
                                               d687_185 <- get
                                               t191_186 <- StateT char
                                               case t191_186 of
                                                   'N' -> return ()
                                                   _ -> gets position >>= (throwError . mkParseError "'N'" "not match pattern: " "" d687_185 ["char"])
                                               let 'N' = t191_186
                                               return ()
                                               d688_187 <- get
                                               t192_188 <- StateT char
                                               case t192_188 of
                                                   'G' -> return ()
                                                   _ -> gets position >>= (throwError . mkParseError "'G'" "not match pattern: " "" d688_187 ["char"])
                                               let 'G' = t192_188
                                               return ()
                                               d689_189 <- get
                                               t193_190 <- StateT char
                                               case t193_190 of
                                                   'U' -> return ()
                                                   _ -> gets position >>= (throwError . mkParseError "'U'" "not match pattern: " "" d689_189 ["char"])
                                               let 'U' = t193_190
                                               return ()
                                               d690_191 <- get
                                               t194_192 <- StateT char
                                               case t194_192 of
                                                   'A' -> return ()
                                                   _ -> gets position >>= (throwError . mkParseError "'A'" "not match pattern: " "" d690_191 ["char"])
                                               let 'A' = t194_192
                                               return ()
                                               d691_193 <- get
                                               t195_194 <- StateT char
                                               case t195_194 of
                                                   'G' -> return ()
                                                   _ -> gets position >>= (throwError . mkParseError "'G'" "not match pattern: " "" d691_193 ["char"])
                                               let 'G' = t195_194
                                               return ()
                                               d692_195 <- get
                                               t196_196 <- StateT char
                                               case t196_196 of
                                                   'E' -> return ()
                                                   _ -> gets position >>= (throwError . mkParseError "'E'" "not match pattern: " "" d692_195 ["char"])
                                               let 'E' = t196_196
                                               return ()
                                               _ <- StateT spaces
                                               return ()
                                               s <- StateT pragmaItems
                                               _ <- StateT pragmaEnd
                                               return ()
                                               _ <- StateT spaces
                                               return ()
                                               return (LanguagePragma s),
                                            do d697_197 <- get
                                               t201_198 <- StateT char
                                               case t201_198 of
                                                   '{' -> return ()
                                                   _ -> gets position >>= (throwError . mkParseError "'{'" "not match pattern: " "" d697_197 ["char"])
                                               let '{' = t201_198
                                               return ()
                                               d698_199 <- get
                                               t202_200 <- StateT char
                                               case t202_200 of
                                                   '-' -> return ()
                                                   _ -> gets position >>= (throwError . mkParseError "'-'" "not match pattern: " "" d698_199 ["char"])
                                               let '-' = t202_200
                                               return ()
                                               d699_201 <- get
                                               t203_202 <- StateT char
                                               case t203_202 of
                                                   '#' -> return ()
                                                   _ -> gets position >>= (throwError . mkParseError "'#'" "not match pattern: " "" d699_201 ["char"])
                                               let '#' = t203_202
                                               return ()
                                               _ <- StateT spaces
                                               return ()
                                               s <- StateT pragmaStr
                                               _ <- StateT pragmaEnd
                                               return ()
                                               return (OtherPragma s)]
                pragmaStr81_86 = foldl1 mplus [do d704_203 <- get
                                                  do err1162_204 <- ((do _ <- StateT pragmaEnd
                                                                         return ()) >> return False) `catchError` const (return True)
                                                     unless err1162_204 (gets position >>= (throwError . mkParseError "!_:pragmaEnd" "not match: " "" d704_203 ["pragmaEnd"]))
                                                  put d704_203
                                                  c <- StateT char
                                                  s <- StateT pragmaStr
                                                  return (c : s),
                                               return ""]
                pragmaItems82_87 = foldl1 mplus [do t <- StateT typToken
                                                    d708_205 <- get
                                                    t211_206 <- StateT char
                                                    case t211_206 of
                                                        ',' -> return ()
                                                        _ -> gets position >>= (throwError . mkParseError "','" "not match pattern: " "" d708_205 ["char"])
                                                    let ',' = t211_206
                                                    return ()
                                                    _ <- StateT spaces
                                                    return ()
                                                    i <- StateT pragmaItems
                                                    return (t : i),
                                                 do t <- StateT typToken
                                                    return [t]]
                pragmaEnd83_88 = foldl1 mplus [do _ <- StateT spaces
                                                  return ()
                                                  d713_207 <- get
                                                  t216_208 <- StateT char
                                                  case t216_208 of
                                                      '#' -> return ()
                                                      _ -> gets position >>= (throwError . mkParseError "'#'" "not match pattern: " "" d713_207 ["char"])
                                                  let '#' = t216_208
                                                  return ()
                                                  d714_209 <- get
                                                  t217_210 <- StateT char
                                                  case t217_210 of
                                                      '-' -> return ()
                                                      _ -> gets position >>= (throwError . mkParseError "'-'" "not match pattern: " "" d714_209 ["char"])
                                                  let '-' = t217_210
                                                  return ()
                                                  d715_211 <- get
                                                  t218_212 <- StateT char
                                                  case t218_212 of
                                                      '}' -> return ()
                                                      _ -> gets position >>= (throwError . mkParseError "'}'" "not match pattern: " "" d715_211 ["char"])
                                                  let '}' = t218_212
                                                  return ()
                                                  return ()]
                moduleDec84_89 = foldl1 mplus [do d716_213 <- get
                                                  t219_214 <- StateT char
                                                  case t219_214 of
                                                      'm' -> return ()
                                                      _ -> gets position >>= (throwError . mkParseError "'m'" "not match pattern: " "" d716_213 ["char"])
                                                  let 'm' = t219_214
                                                  return ()
                                                  d717_215 <- get
                                                  t220_216 <- StateT char
                                                  case t220_216 of
                                                      'o' -> return ()
                                                      _ -> gets position >>= (throwError . mkParseError "'o'" "not match pattern: " "" d717_215 ["char"])
                                                  let 'o' = t220_216
                                                  return ()
                                                  d718_217 <- get
                                                  t221_218 <- StateT char
                                                  case t221_218 of
                                                      'd' -> return ()
                                                      _ -> gets position >>= (throwError . mkParseError "'d'" "not match pattern: " "" d718_217 ["char"])
                                                  let 'd' = t221_218
                                                  return ()
                                                  d719_219 <- get
                                                  t222_220 <- StateT char
                                                  case t222_220 of
                                                      'u' -> return ()
                                                      _ -> gets position >>= (throwError . mkParseError "'u'" "not match pattern: " "" d719_219 ["char"])
                                                  let 'u' = t222_220
                                                  return ()
                                                  d720_221 <- get
                                                  t223_222 <- StateT char
                                                  case t223_222 of
                                                      'l' -> return ()
                                                      _ -> gets position >>= (throwError . mkParseError "'l'" "not match pattern: " "" d720_221 ["char"])
                                                  let 'l' = t223_222
                                                  return ()
                                                  d721_223 <- get
                                                  t224_224 <- StateT char
                                                  case t224_224 of
                                                      'e' -> return ()
                                                      _ -> gets position >>= (throwError . mkParseError "'e'" "not match pattern: " "" d721_223 ["char"])
                                                  let 'e' = t224_224
                                                  return ()
                                                  _ <- StateT spaces
                                                  return ()
                                                  n <- StateT moduleName
                                                  _ <- StateT spaces
                                                  return ()
                                                  d725_225 <- get
                                                  t228_226 <- StateT char
                                                  case t228_226 of
                                                      '(' -> return ()
                                                      _ -> gets position >>= (throwError . mkParseError "'('" "not match pattern: " "" d725_225 ["char"])
                                                  let '(' = t228_226
                                                  return ()
                                                  _ <- StateT spaces
                                                  return ()
                                                  s <- StateT moduleDecStr
                                                  _ <- StateT whr
                                                  return ()
                                                  return (Just (n, Just s)),
                                               do d729_227 <- get
                                                  t232_228 <- StateT char
                                                  case t232_228 of
                                                      'm' -> return ()
                                                      _ -> gets position >>= (throwError . mkParseError "'m'" "not match pattern: " "" d729_227 ["char"])
                                                  let 'm' = t232_228
                                                  return ()
                                                  d730_229 <- get
                                                  t233_230 <- StateT char
                                                  case t233_230 of
                                                      'o' -> return ()
                                                      _ -> gets position >>= (throwError . mkParseError "'o'" "not match pattern: " "" d730_229 ["char"])
                                                  let 'o' = t233_230
                                                  return ()
                                                  d731_231 <- get
                                                  t234_232 <- StateT char
                                                  case t234_232 of
                                                      'd' -> return ()
                                                      _ -> gets position >>= (throwError . mkParseError "'d'" "not match pattern: " "" d731_231 ["char"])
                                                  let 'd' = t234_232
                                                  return ()
                                                  d732_233 <- get
                                                  t235_234 <- StateT char
                                                  case t235_234 of
                                                      'u' -> return ()
                                                      _ -> gets position >>= (throwError . mkParseError "'u'" "not match pattern: " "" d732_233 ["char"])
                                                  let 'u' = t235_234
                                                  return ()
                                                  d733_235 <- get
                                                  t236_236 <- StateT char
                                                  case t236_236 of
                                                      'l' -> return ()
                                                      _ -> gets position >>= (throwError . mkParseError "'l'" "not match pattern: " "" d733_235 ["char"])
                                                  let 'l' = t236_236
                                                  return ()
                                                  d734_237 <- get
                                                  t237_238 <- StateT char
                                                  case t237_238 of
                                                      'e' -> return ()
                                                      _ -> gets position >>= (throwError . mkParseError "'e'" "not match pattern: " "" d734_237 ["char"])
                                                  let 'e' = t237_238
                                                  return ()
                                                  _ <- StateT spaces
                                                  return ()
                                                  n <- StateT moduleName
                                                  _ <- StateT spaces
                                                  return ()
                                                  d738_239 <- get
                                                  t241_240 <- StateT char
                                                  case t241_240 of
                                                      'w' -> return ()
                                                      _ -> gets position >>= (throwError . mkParseError "'w'" "not match pattern: " "" d738_239 ["char"])
                                                  let 'w' = t241_240
                                                  return ()
                                                  d739_241 <- get
                                                  t242_242 <- StateT char
                                                  case t242_242 of
                                                      'h' -> return ()
                                                      _ -> gets position >>= (throwError . mkParseError "'h'" "not match pattern: " "" d739_241 ["char"])
                                                  let 'h' = t242_242
                                                  return ()
                                                  d740_243 <- get
                                                  t243_244 <- StateT char
                                                  case t243_244 of
                                                      'e' -> return ()
                                                      _ -> gets position >>= (throwError . mkParseError "'e'" "not match pattern: " "" d740_243 ["char"])
                                                  let 'e' = t243_244
                                                  return ()
                                                  d741_245 <- get
                                                  t244_246 <- StateT char
                                                  case t244_246 of
                                                      'r' -> return ()
                                                      _ -> gets position >>= (throwError . mkParseError "'r'" "not match pattern: " "" d741_245 ["char"])
                                                  let 'r' = t244_246
                                                  return ()
                                                  d742_247 <- get
                                                  t245_248 <- StateT char
                                                  case t245_248 of
                                                      'e' -> return ()
                                                      _ -> gets position >>= (throwError . mkParseError "'e'" "not match pattern: " "" d742_247 ["char"])
                                                  let 'e' = t245_248
                                                  return ()
                                                  _ <- StateT spaces
                                                  return ()
                                                  return (Just (n, Nothing)),
                                               return Nothing]
                moduleName85_90 = foldl1 mplus [do t <- StateT typ
                                                   d745_249 <- get
                                                   t248_250 <- StateT char
                                                   case t248_250 of
                                                       '.' -> return ()
                                                       _ -> gets position >>= (throwError . mkParseError "'.'" "not match pattern: " "" d745_249 ["char"])
                                                   let '.' = t248_250
                                                   return ()
                                                   n <- StateT moduleName
                                                   return (t : n),
                                                do t <- StateT typ
                                                   return [t]]
                moduleDecStr86_91 = foldl1 mplus [do d749_251 <- get
                                                     do err1163_252 <- ((do _ <- StateT whr
                                                                            return ()) >> return False) `catchError` const (return True)
                                                        unless err1163_252 (gets position >>= (throwError . mkParseError "!_:whr" "not match: " "" d749_251 ["whr"]))
                                                     put d749_251
                                                     c <- StateT char
                                                     s <- StateT moduleDecStr
                                                     return (c : s),
                                                  return ""]
                whr87_92 = foldl1 mplus [do _ <- StateT spaces
                                            return ()
                                            d753_253 <- get
                                            t255_254 <- StateT char
                                            case t255_254 of
                                                ')' -> return ()
                                                _ -> gets position >>= (throwError . mkParseError "')'" "not match pattern: " "" d753_253 ["char"])
                                            let ')' = t255_254
                                            return ()
                                            _ <- StateT spaces
                                            return ()
                                            d755_255 <- get
                                            t257_256 <- StateT char
                                            case t257_256 of
                                                'w' -> return ()
                                                _ -> gets position >>= (throwError . mkParseError "'w'" "not match pattern: " "" d755_255 ["char"])
                                            let 'w' = t257_256
                                            return ()
                                            d756_257 <- get
                                            t258_258 <- StateT char
                                            case t258_258 of
                                                'h' -> return ()
                                                _ -> gets position >>= (throwError . mkParseError "'h'" "not match pattern: " "" d756_257 ["char"])
                                            let 'h' = t258_258
                                            return ()
                                            d757_259 <- get
                                            t259_260 <- StateT char
                                            case t259_260 of
                                                'e' -> return ()
                                                _ -> gets position >>= (throwError . mkParseError "'e'" "not match pattern: " "" d757_259 ["char"])
                                            let 'e' = t259_260
                                            return ()
                                            d758_261 <- get
                                            t260_262 <- StateT char
                                            case t260_262 of
                                                'r' -> return ()
                                                _ -> gets position >>= (throwError . mkParseError "'r'" "not match pattern: " "" d758_261 ["char"])
                                            let 'r' = t260_262
                                            return ()
                                            d759_263 <- get
                                            t261_264 <- StateT char
                                            case t261_264 of
                                                'e' -> return ()
                                                _ -> gets position >>= (throwError . mkParseError "'e'" "not match pattern: " "" d759_263 ["char"])
                                            let 'e' = t261_264
                                            return ()
                                            return ()]
                preImpPap88_93 = foldl1 mplus [do d761_265 <- get
                                                  do err1164_266 <- ((do _ <- StateT importPapillon
                                                                         return ()) >> return False) `catchError` const (return True)
                                                     unless err1164_266 (gets position >>= (throwError . mkParseError "!_:importPapillon" "not match: " "" d761_265 ["importPapillon"]))
                                                  put d761_265
                                                  d763_267 <- get
                                                  do err1165_268 <- ((do _ <- StateT pap
                                                                         return ()) >> return False) `catchError` const (return True)
                                                     unless err1165_268 (gets position >>= (throwError . mkParseError "!_:pap" "not match: " "" d763_267 ["pap"]))
                                                  put d763_267
                                                  c <- StateT char
                                                  pip <- StateT preImpPap
                                                  return (c : pip),
                                               return ""]
                prePeg89_94 = foldl1 mplus [do d767_269 <- get
                                               do err1166_270 <- ((do _ <- StateT pap
                                                                      return ()) >> return False) `catchError` const (return True)
                                                  unless err1166_270 (gets position >>= (throwError . mkParseError "!_:pap" "not match: " "" d767_269 ["pap"]))
                                               put d767_269
                                               c <- StateT char
                                               pp <- StateT prePeg
                                               return (c : pp),
                                            return ""]
                afterPeg90_95 = foldl1 mplus [do c <- StateT char
                                                 atp <- StateT afterPeg
                                                 return (c : atp),
                                              return ""]
                importPapillon91_96 = foldl1 mplus [do d772_271 <- get
                                                       t271_272 <- StateT varToken
                                                       case t271_272 of
                                                           "import" -> return ()
                                                           _ -> gets position >>= (throwError . mkParseError "\"import\"" "not match pattern: " "" d772_271 ["varToken"])
                                                       let "import" = t271_272
                                                       return ()
                                                       d773_273 <- get
                                                       t272_274 <- StateT typToken
                                                       case t272_274 of
                                                           "Text" -> return ()
                                                           _ -> gets position >>= (throwError . mkParseError "\"Text\"" "not match pattern: " "" d773_273 ["typToken"])
                                                       let "Text" = t272_274
                                                       return ()
                                                       d774_275 <- get
                                                       t273_276 <- StateT char
                                                       case t273_276 of
                                                           '.' -> return ()
                                                           _ -> gets position >>= (throwError . mkParseError "'.'" "not match pattern: " "" d774_275 ["char"])
                                                       let '.' = t273_276
                                                       return ()
                                                       _ <- StateT spaces
                                                       return ()
                                                       d776_277 <- get
                                                       t275_278 <- StateT typToken
                                                       case t275_278 of
                                                           "Papillon" -> return ()
                                                           _ -> gets position >>= (throwError . mkParseError "\"Papillon\"" "not match pattern: " "" d776_277 ["typToken"])
                                                       let "Papillon" = t275_278
                                                       return ()
                                                       d778_279 <- get
                                                       do err1167_280 <- ((do d777_281 <- get
                                                                              t276_282 <- StateT char
                                                                              case t276_282 of
                                                                                  '.' -> return ()
                                                                                  _ -> gets position >>= (throwError . mkParseError "'.'" "not match pattern: " "" d777_281 ["char"])
                                                                              let '.' = t276_282
                                                                              return ()) >> return False) `catchError` const (return True)
                                                          unless err1167_280 (gets position >>= (throwError . mkParseError "!'.':" "not match: " "" d778_279 ["char"]))
                                                       put d778_279
                                                       return ()]
                varToken92_97 = foldl1 mplus [do v <- StateT variable
                                                 _ <- StateT spaces
                                                 return ()
                                                 return v]
                typToken93_98 = foldl1 mplus [do t <- StateT typ
                                                 _ <- StateT spaces
                                                 return ()
                                                 return t]
                pap94_99 = foldl1 mplus [do d783_283 <- get
                                            t281_284 <- StateT char
                                            case t281_284 of
                                                '\n' -> return ()
                                                _ -> gets position >>= (throwError . mkParseError "'\\n'" "not match pattern: " "" d783_283 ["char"])
                                            let '\n' = t281_284
                                            return ()
                                            d784_285 <- get
                                            t282_286 <- StateT char
                                            case t282_286 of
                                                '[' -> return ()
                                                _ -> gets position >>= (throwError . mkParseError "'['" "not match pattern: " "" d784_285 ["char"])
                                            let '[' = t282_286
                                            return ()
                                            d785_287 <- get
                                            t283_288 <- StateT char
                                            case t283_288 of
                                                'p' -> return ()
                                                _ -> gets position >>= (throwError . mkParseError "'p'" "not match pattern: " "" d785_287 ["char"])
                                            let 'p' = t283_288
                                            return ()
                                            d786_289 <- get
                                            t284_290 <- StateT char
                                            case t284_290 of
                                                'a' -> return ()
                                                _ -> gets position >>= (throwError . mkParseError "'a'" "not match pattern: " "" d786_289 ["char"])
                                            let 'a' = t284_290
                                            return ()
                                            d787_291 <- get
                                            t285_292 <- StateT char
                                            case t285_292 of
                                                'p' -> return ()
                                                _ -> gets position >>= (throwError . mkParseError "'p'" "not match pattern: " "" d787_291 ["char"])
                                            let 'p' = t285_292
                                            return ()
                                            d788_293 <- get
                                            t286_294 <- StateT char
                                            case t286_294 of
                                                'i' -> return ()
                                                _ -> gets position >>= (throwError . mkParseError "'i'" "not match pattern: " "" d788_293 ["char"])
                                            let 'i' = t286_294
                                            return ()
                                            d789_295 <- get
                                            t287_296 <- StateT char
                                            case t287_296 of
                                                'l' -> return ()
                                                _ -> gets position >>= (throwError . mkParseError "'l'" "not match pattern: " "" d789_295 ["char"])
                                            let 'l' = t287_296
                                            return ()
                                            d790_297 <- get
                                            t288_298 <- StateT char
                                            case t288_298 of
                                                'l' -> return ()
                                                _ -> gets position >>= (throwError . mkParseError "'l'" "not match pattern: " "" d790_297 ["char"])
                                            let 'l' = t288_298
                                            return ()
                                            d791_299 <- get
                                            t289_300 <- StateT char
                                            case t289_300 of
                                                'o' -> return ()
                                                _ -> gets position >>= (throwError . mkParseError "'o'" "not match pattern: " "" d791_299 ["char"])
                                            let 'o' = t289_300
                                            return ()
                                            d792_301 <- get
                                            t290_302 <- StateT char
                                            case t290_302 of
                                                'n' -> return ()
                                                _ -> gets position >>= (throwError . mkParseError "'n'" "not match pattern: " "" d792_301 ["char"])
                                            let 'n' = t290_302
                                            return ()
                                            d793_303 <- get
                                            t291_304 <- StateT char
                                            case t291_304 of
                                                '|' -> return ()
                                                _ -> gets position >>= (throwError . mkParseError "'|'" "not match pattern: " "" d793_303 ["char"])
                                            let '|' = t291_304
                                            return ()
                                            d794_305 <- get
                                            t292_306 <- StateT char
                                            case t292_306 of
                                                '\n' -> return ()
                                                _ -> gets position >>= (throwError . mkParseError "'\\n'" "not match pattern: " "" d794_305 ["char"])
                                            let '\n' = t292_306
                                            return ()
                                            return ()]
                peg95_100 = foldl1 mplus [do mt <- optional156_307 (StateT monadType)
                                             _ <- StateT spaces
                                             return ()
                                             s <- StateT sourceType
                                             p <- StateT peg_
                                             return (stPegQ mt (ConT $ mkName s) p),
                                          do mt <- optional156_307 (StateT monadType)
                                             p <- StateT peg_
                                             return (stPegQ mt (ConT $ mkName "String") p)]
                monadType96_101 = foldl1 mplus [do _ <- StateT spaces
                                                   return ()
                                                   d802_308 <- get
                                                   t300_309 <- StateT varToken
                                                   case t300_309 of
                                                       "monad" -> return ()
                                                       _ -> gets position >>= (throwError . mkParseError "\"monad\"" "not match pattern: " "" d802_308 ["varToken"])
                                                   let "monad" = t300_309
                                                   return ()
                                                   d803_310 <- get
                                                   t301_311 <- StateT char
                                                   case t301_311 of
                                                       ':' -> return ()
                                                       _ -> gets position >>= (throwError . mkParseError "':'" "not match pattern: " "" d803_310 ["char"])
                                                   let ':' = t301_311
                                                   return ()
                                                   _ <- StateT spaces
                                                   return ()
                                                   t <- StateT hsTypeArr
                                                   return t]
                sourceType97_102 = foldl1 mplus [do d806_312 <- get
                                                    t304_313 <- StateT varToken
                                                    case t304_313 of
                                                        "source" -> return ()
                                                        _ -> gets position >>= (throwError . mkParseError "\"source\"" "not match pattern: " "" d806_312 ["varToken"])
                                                    let "source" = t304_313
                                                    return ()
                                                    d807_314 <- get
                                                    t305_315 <- StateT char
                                                    case t305_315 of
                                                        ':' -> return ()
                                                        _ -> gets position >>= (throwError . mkParseError "':'" "not match pattern: " "" d807_314 ["char"])
                                                    let ':' = t305_315
                                                    return ()
                                                    _ <- StateT spaces
                                                    return ()
                                                    v <- StateT typToken
                                                    return v]
                peg_98_103 = foldl1 mplus [do _ <- StateT spaces
                                              return ()
                                              d <- StateT definition
                                              p <- StateT peg_
                                              return (d : p),
                                           return []]
                definition99_104 = foldl1 mplus [do v <- StateT variable
                                                    _ <- StateT spaces
                                                    return ()
                                                    d815_316 <- get
                                                    t313_317 <- StateT char
                                                    case t313_317 of
                                                        ':' -> return ()
                                                        _ -> gets position >>= (throwError . mkParseError "':'" "not match pattern: " "" d815_316 ["char"])
                                                    let ':' = t313_317
                                                    return ()
                                                    d816_318 <- get
                                                    t314_319 <- StateT char
                                                    case t314_319 of
                                                        ':' -> return ()
                                                        _ -> gets position >>= (throwError . mkParseError "':'" "not match pattern: " "" d816_318 ["char"])
                                                    let ':' = t314_319
                                                    return ()
                                                    _ <- StateT spaces
                                                    return ()
                                                    t <- StateT hsTypeArr
                                                    _ <- StateT spaces
                                                    return ()
                                                    d820_320 <- get
                                                    t318_321 <- StateT char
                                                    case t318_321 of
                                                        '=' -> return ()
                                                        _ -> gets position >>= (throwError . mkParseError "'='" "not match pattern: " "" d820_320 ["char"])
                                                    let '=' = t318_321
                                                    return ()
                                                    _ <- StateT spaces
                                                    return ()
                                                    sel <- StateT selection
                                                    _ <- StateT spaces
                                                    return ()
                                                    d824_322 <- get
                                                    t322_323 <- StateT char
                                                    case t322_323 of
                                                        ';' -> return ()
                                                        _ -> gets position >>= (throwError . mkParseError "';'" "not match pattern: " "" d824_322 ["char"])
                                                    let ';' = t322_323
                                                    return ()
                                                    return (definitionQ v (Just t) sel),
                                                 do v <- StateT variable
                                                    _ <- StateT spaces
                                                    return ()
                                                    d827_324 <- get
                                                    t325_325 <- StateT char
                                                    case t325_325 of
                                                        '<' -> return ()
                                                        _ -> gets position >>= (throwError . mkParseError "'<'" "not match pattern: " "" d827_324 ["char"])
                                                    let '<' = t325_325
                                                    return ()
                                                    d828_326 <- get
                                                    t326_327 <- StateT char
                                                    case t326_327 of
                                                        '-' -> return ()
                                                        _ -> gets position >>= (throwError . mkParseError "'-'" "not match pattern: " "" d828_326 ["char"])
                                                    let '-' = t326_327
                                                    return ()
                                                    _ <- StateT spaces
                                                    return ()
                                                    sel <- StateT plainSelection
                                                    _ <- StateT spaces
                                                    return ()
                                                    d832_328 <- get
                                                    t330_329 <- StateT char
                                                    case t330_329 of
                                                        ';' -> return ()
                                                        _ -> gets position >>= (throwError . mkParseError "';'" "not match pattern: " "" d832_328 ["char"])
                                                    let ';' = t330_329
                                                    return ()
                                                    return (definitionQ v Nothing $ plainSelectionQ sel)]
                selection100_105 = foldl1 mplus [do s <- StateT normalSelection
                                                    return (normalSelectionQ s),
                                                 do s <- StateT plainSelection
                                                    return (plainSelectionQ s)]
                normalSelection101_106 = foldl1 mplus [do ex <- StateT expressionHs
                                                          _ <- StateT spaces
                                                          return ()
                                                          d837_330 <- get
                                                          t335_331 <- StateT char
                                                          case t335_331 of
                                                              '/' -> return ()
                                                              _ -> gets position >>= (throwError . mkParseError "'/'" "not match pattern: " "" d837_330 ["char"])
                                                          let '/' = t335_331
                                                          return ()
                                                          _ <- StateT spaces
                                                          return ()
                                                          sel <- StateT normalSelection
                                                          return (ex : sel),
                                                       do ex <- StateT expressionHs
                                                          return [ex]]
                plainSelection102_107 = foldl1 mplus [do ex <- StateT plainExpressionHs
                                                         _ <- StateT spaces
                                                         return ()
                                                         d843_332 <- get
                                                         t341_333 <- StateT char
                                                         case t341_333 of
                                                             '/' -> return ()
                                                             _ -> gets position >>= (throwError . mkParseError "'/'" "not match pattern: " "" d843_332 ["char"])
                                                         let '/' = t341_333
                                                         return ()
                                                         _ <- StateT spaces
                                                         return ()
                                                         sel <- StateT plainSelection
                                                         return (ex : sel),
                                                      do ex <- StateT plainExpressionHs
                                                         return [ex]]
                expressionHs103_108 = foldl1 mplus [do e <- StateT expression
                                                       _ <- StateT spaces
                                                       return ()
                                                       d849_334 <- get
                                                       t347_335 <- StateT char
                                                       case t347_335 of
                                                           '{' -> return ()
                                                           _ -> gets position >>= (throwError . mkParseError "'{'" "not match pattern: " "" d849_334 ["char"])
                                                       let '{' = t347_335
                                                       return ()
                                                       _ <- StateT spaces
                                                       return ()
                                                       h <- StateT hsExpLam
                                                       _ <- StateT spaces
                                                       return ()
                                                       d853_336 <- get
                                                       t351_337 <- StateT char
                                                       case t351_337 of
                                                           '}' -> return ()
                                                           _ -> gets position >>= (throwError . mkParseError "'}'" "not match pattern: " "" d853_336 ["char"])
                                                       let '}' = t351_337
                                                       return ()
                                                       return (expressionQ (e, h)),
                                                    do e <- StateT expressionHsSugar
                                                       return e]
                expressionHsSugar104_109 = foldl1 mplus [do d855_338 <- get
                                                            t353_339 <- StateT char
                                                            case t353_339 of
                                                                '<' -> return ()
                                                                _ -> gets position >>= (throwError . mkParseError "'<'" "not match pattern: " "" d855_338 ["char"])
                                                            let '<' = t353_339
                                                            return ()
                                                            _ <- StateT spaces
                                                            return ()
                                                            h <- StateT hsExpLam
                                                            _ <- StateT spaces
                                                            return ()
                                                            d859_340 <- get
                                                            t357_341 <- StateT char
                                                            case t357_341 of
                                                                '>' -> return ()
                                                                _ -> gets position >>= (throwError . mkParseError "'>'" "not match pattern: " "" d859_340 ["char"])
                                                            let '>' = t357_341
                                                            return ()
                                                            return (expressionSugar h)]
                plainExpressionHs105_110 = foldl1 mplus [do rfs <- list157_342 (foldl1 mplus [do rf <- StateT plainHAReadFromLs
                                                                                                 _ <- StateT spaces
                                                                                                 return ()
                                                                                                 return rf])
                                                            return (plainExpressionQ rfs)]
                plainHAReadFromLs106_111 = foldl1 mplus [do rf <- StateT plainReadFromLs
                                                            return (Here, rf),
                                                         do d864_343 <- get
                                                            t362_344 <- StateT char
                                                            case t362_344 of
                                                                '&' -> return ()
                                                                _ -> gets position >>= (throwError . mkParseError "'&'" "not match pattern: " "" d864_343 ["char"])
                                                            let '&' = t362_344
                                                            return ()
                                                            rf <- StateT plainReadFromLs
                                                            return (Ahead, rf),
                                                         do d866_345 <- get
                                                            t364_346 <- StateT char
                                                            case t364_346 of
                                                                '!' -> return ()
                                                                _ -> gets position >>= (throwError . mkParseError "'!'" "not match pattern: " "" d866_345 ["char"])
                                                            let '!' = t364_346
                                                            return ()
                                                            rf <- StateT plainReadFromLs
                                                            return (NAhead "", rf)]
                plainReadFromLs107_112 = foldl1 mplus [do rf <- StateT readFromLs
                                                          return rf,
                                                       do rf <- StateT selectCharsLs
                                                          return rf]
                expression108_113 = foldl1 mplus [do l <- StateT nameLeaf_
                                                     _ <- StateT spaces
                                                     return ()
                                                     e <- StateT expression
                                                     return (l : e),
                                                  return []]
                nameLeaf_109_114 = foldl1 mplus [do d873_347 <- get
                                                    t371_348 <- StateT char
                                                    case t371_348 of
                                                        '!' -> return ()
                                                        _ -> gets position >>= (throwError . mkParseError "'!'" "not match pattern: " "" d873_347 ["char"])
                                                    let '!' = t371_348
                                                    return ()
                                                    nl <- StateT nameLeafNoCom
                                                    _ <- StateT spaces
                                                    return ()
                                                    com <- optional156_307 (StateT comForErr)
                                                    return (NAhead $ maybe "" id com, nl),
                                                 do d877_349 <- get
                                                    t375_350 <- StateT char
                                                    case t375_350 of
                                                        '&' -> return ()
                                                        _ -> gets position >>= (throwError . mkParseError "'&'" "not match pattern: " "" d877_349 ["char"])
                                                    let '&' = t375_350
                                                    return ()
                                                    nl <- StateT nameLeaf
                                                    return (Ahead, nl),
                                                 do nl <- StateT nameLeaf
                                                    return (Here, nl)]
                nameLeaf110_115 = foldl1 mplus [do n <- StateT pat1
                                                   _ <- StateT spaces
                                                   return ()
                                                   com <- optional156_307 (StateT comForErr)
                                                   d883_351 <- get
                                                   t381_352 <- StateT char
                                                   case t381_352 of
                                                       ':' -> return ()
                                                       _ -> gets position >>= (throwError . mkParseError "':'" "not match pattern: " "" d883_351 ["char"])
                                                   let ':' = t381_352
                                                   return ()
                                                   (rf, p) <- StateT leaf
                                                   return (check (n, maybe "" id com) rf p),
                                                do n <- StateT pat1
                                                   _ <- StateT spaces
                                                   return ()
                                                   com <- optional156_307 (StateT comForErr)
                                                   return (check (n,
                                                                  maybe "" id com) (FromVariable Nothing) Nothing)]
                nameLeafNoCom111_116 = foldl1 mplus [do n <- StateT pat1
                                                        _ <- StateT spaces
                                                        return ()
                                                        com <- optional156_307 (StateT comForErr)
                                                        d891_353 <- get
                                                        t389_354 <- StateT char
                                                        case t389_354 of
                                                            ':' -> return ()
                                                            _ -> gets position >>= (throwError . mkParseError "':'" "not match pattern: " "" d891_353 ["char"])
                                                        let ':' = t389_354
                                                        return ()
                                                        (rf, p) <- StateT leaf
                                                        return (check (n, maybe "" id com) rf p),
                                                     do n <- StateT pat1
                                                        _ <- StateT spaces
                                                        return ()
                                                        return (check (n,
                                                                       "") (FromVariable Nothing) Nothing)]
                comForErr112_117 = foldl1 mplus [do d895_355 <- get
                                                    t393_356 <- StateT char
                                                    case t393_356 of
                                                        '{' -> return ()
                                                        _ -> gets position >>= (throwError . mkParseError "'{'" "not match pattern: " "" d895_355 ["char"])
                                                    let '{' = t393_356
                                                    return ()
                                                    d896_357 <- get
                                                    t394_358 <- StateT char
                                                    case t394_358 of
                                                        '-' -> return ()
                                                        _ -> gets position >>= (throwError . mkParseError "'-'" "not match pattern: " "" d896_357 ["char"])
                                                    let '-' = t394_358
                                                    return ()
                                                    d897_359 <- get
                                                    t395_360 <- StateT char
                                                    case t395_360 of
                                                        '#' -> return ()
                                                        _ -> gets position >>= (throwError . mkParseError "'#'" "not match pattern: " "" d897_359 ["char"])
                                                    let '#' = t395_360
                                                    return ()
                                                    _ <- StateT spaces
                                                    return ()
                                                    d899_361 <- get
                                                    t397_362 <- StateT char
                                                    case t397_362 of
                                                        '"' -> return ()
                                                        _ -> gets position >>= (throwError . mkParseError "'\"'" "not match pattern: " "" d899_361 ["char"])
                                                    let '"' = t397_362
                                                    return ()
                                                    s <- StateT stringLit
                                                    d901_363 <- get
                                                    t399_364 <- StateT char
                                                    case t399_364 of
                                                        '"' -> return ()
                                                        _ -> gets position >>= (throwError . mkParseError "'\"'" "not match pattern: " "" d901_363 ["char"])
                                                    let '"' = t399_364
                                                    return ()
                                                    _ <- StateT spaces
                                                    return ()
                                                    d903_365 <- get
                                                    t401_366 <- StateT char
                                                    case t401_366 of
                                                        '#' -> return ()
                                                        _ -> gets position >>= (throwError . mkParseError "'#'" "not match pattern: " "" d903_365 ["char"])
                                                    let '#' = t401_366
                                                    return ()
                                                    d904_367 <- get
                                                    t402_368 <- StateT char
                                                    case t402_368 of
                                                        '-' -> return ()
                                                        _ -> gets position >>= (throwError . mkParseError "'-'" "not match pattern: " "" d904_367 ["char"])
                                                    let '-' = t402_368
                                                    return ()
                                                    d905_369 <- get
                                                    t403_370 <- StateT char
                                                    case t403_370 of
                                                        '}' -> return ()
                                                        _ -> gets position >>= (throwError . mkParseError "'}'" "not match pattern: " "" d905_369 ["char"])
                                                    let '}' = t403_370
                                                    return ()
                                                    _ <- StateT spaces
                                                    return ()
                                                    return s]
                leaf113_118 = foldl1 mplus [do rf <- StateT readFromLs
                                               t <- StateT test
                                               return (rf, Just t),
                                            do rf <- StateT readFromLs
                                               return (rf, Nothing),
                                            do t <- StateT test
                                               return (FromVariable Nothing, Just t)]
                patOp114_119 = foldl1 mplus [do p <- StateT pat
                                                o <- StateT opConName
                                                po <- StateT patOp
                                                return (UInfixP p o po),
                                             do p <- StateT pat
                                                _ <- StateT spaces
                                                return ()
                                                d916_371 <- get
                                                t414_372 <- StateT char
                                                case t414_372 of
                                                    '`' -> return ()
                                                    _ -> gets position >>= (throwError . mkParseError "'`'" "not match pattern: " "" d916_371 ["char"])
                                                let '`' = t414_372
                                                return ()
                                                t <- StateT typ
                                                d918_373 <- get
                                                t416_374 <- StateT char
                                                case t416_374 of
                                                    '`' -> return ()
                                                    _ -> gets position >>= (throwError . mkParseError "'`'" "not match pattern: " "" d918_373 ["char"])
                                                let '`' = t416_374
                                                return ()
                                                _ <- StateT spaces
                                                return ()
                                                po <- StateT patOp
                                                return (UInfixP p (mkName t) po),
                                             do p <- StateT pat
                                                return p]
                pat115_120 = foldl1 mplus [do t <- StateT typ
                                              _ <- StateT spaces
                                              return ()
                                              ps <- StateT pats
                                              return (ConP (mkName t) ps),
                                           do d925_375 <- get
                                              t423_376 <- StateT char
                                              case t423_376 of
                                                  '(' -> return ()
                                                  _ -> gets position >>= (throwError . mkParseError "'('" "not match pattern: " "" d925_375 ["char"])
                                              let '(' = t423_376
                                              return ()
                                              o <- StateT opConName
                                              d927_377 <- get
                                              t425_378 <- StateT char
                                              case t425_378 of
                                                  ')' -> return ()
                                                  _ -> gets position >>= (throwError . mkParseError "')'" "not match pattern: " "" d927_377 ["char"])
                                              let ')' = t425_378
                                              return ()
                                              _ <- StateT spaces
                                              return ()
                                              ps <- StateT pats
                                              return (ConP o ps),
                                           do p <- StateT pat1
                                              return p]
                pat1116_121 = foldl1 mplus [do t <- StateT typ
                                               return (ConP (mkName t) []),
                                            do d932_379 <- get
                                               t430_380 <- StateT variable
                                               case t430_380 of
                                                   "_" -> return ()
                                                   _ -> gets position >>= (throwError . mkParseError "\"_\"" "not match pattern: " "" d932_379 ["variable"])
                                               let "_" = t430_380
                                               return ()
                                               return WildP,
                                            do n <- StateT variable
                                               return (VarP $ mkName n),
                                            do i <- StateT integer
                                               return (LitP (IntegerL i)),
                                            do d935_381 <- get
                                               t433_382 <- StateT char
                                               case t433_382 of
                                                   '-' -> return ()
                                                   _ -> gets position >>= (throwError . mkParseError "'-'" "not match pattern: " "" d935_381 ["char"])
                                               let '-' = t433_382
                                               return ()
                                               _ <- StateT spaces
                                               return ()
                                               i <- StateT integer
                                               return (LitP (IntegerL $ negate i)),
                                            do d938_383 <- get
                                               t436_384 <- StateT char
                                               case t436_384 of
                                                   '\'' -> return ()
                                                   _ -> gets position >>= (throwError . mkParseError "'\\''" "not match pattern: " "" d938_383 ["char"])
                                               let '\'' = t436_384
                                               return ()
                                               c <- StateT charLit
                                               d940_385 <- get
                                               t438_386 <- StateT char
                                               case t438_386 of
                                                   '\'' -> return ()
                                                   _ -> gets position >>= (throwError . mkParseError "'\\''" "not match pattern: " "" d940_385 ["char"])
                                               let '\'' = t438_386
                                               return ()
                                               return (LitP $ CharL c),
                                            do d941_387 <- get
                                               t439_388 <- StateT char
                                               case t439_388 of
                                                   '"' -> return ()
                                                   _ -> gets position >>= (throwError . mkParseError "'\"'" "not match pattern: " "" d941_387 ["char"])
                                               let '"' = t439_388
                                               return ()
                                               s <- StateT stringLit
                                               d943_389 <- get
                                               t441_390 <- StateT char
                                               case t441_390 of
                                                   '"' -> return ()
                                                   _ -> gets position >>= (throwError . mkParseError "'\"'" "not match pattern: " "" d943_389 ["char"])
                                               let '"' = t441_390
                                               return ()
                                               return (LitP $ StringL s),
                                            do d944_391 <- get
                                               t442_392 <- StateT char
                                               case t442_392 of
                                                   '(' -> return ()
                                                   _ -> gets position >>= (throwError . mkParseError "'('" "not match pattern: " "" d944_391 ["char"])
                                               let '(' = t442_392
                                               return ()
                                               p <- StateT patList
                                               d946_393 <- get
                                               t444_394 <- StateT char
                                               case t444_394 of
                                                   ')' -> return ()
                                                   _ -> gets position >>= (throwError . mkParseError "')'" "not match pattern: " "" d946_393 ["char"])
                                               let ')' = t444_394
                                               return ()
                                               return (TupP p),
                                            do d947_395 <- get
                                               t445_396 <- StateT char
                                               case t445_396 of
                                                   '[' -> return ()
                                                   _ -> gets position >>= (throwError . mkParseError "'['" "not match pattern: " "" d947_395 ["char"])
                                               let '[' = t445_396
                                               return ()
                                               p <- StateT patList
                                               d949_397 <- get
                                               t447_398 <- StateT char
                                               case t447_398 of
                                                   ']' -> return ()
                                                   _ -> gets position >>= (throwError . mkParseError "']'" "not match pattern: " "" d949_397 ["char"])
                                               let ']' = t447_398
                                               return ()
                                               return (ListP p)]
                patList117_122 = foldl1 mplus [do p <- StateT patOp
                                                  _ <- StateT spaces
                                                  return ()
                                                  d952_399 <- get
                                                  t450_400 <- StateT char
                                                  case t450_400 of
                                                      ',' -> return ()
                                                      _ -> gets position >>= (throwError . mkParseError "','" "not match pattern: " "" d952_399 ["char"])
                                                  let ',' = t450_400
                                                  return ()
                                                  _ <- StateT spaces
                                                  return ()
                                                  ps <- StateT patList
                                                  return (p : ps),
                                               do p <- StateT patOp
                                                  return [p],
                                               return []]
                opConName118_123 = foldl1 mplus [do d956_401 <- get
                                                    t454_402 <- StateT char
                                                    case t454_402 of
                                                        ':' -> return ()
                                                        _ -> gets position >>= (throwError . mkParseError "':'" "not match pattern: " "" d956_401 ["char"])
                                                    let ':' = t454_402
                                                    return ()
                                                    ot <- StateT opTail
                                                    return (mkName $ ':' : ot)]
                charLit119_124 = foldl1 mplus [do d958_403 <- get
                                                  t456_404 <- StateT char
                                                  let c = t456_404
                                                  unless (c `notElem` "\\'") (gets position >>= (throwError . mkParseError "c `notElem` \"\\\\'\"" "not match: " "" d958_403 ["char"]))
                                                  return c,
                                               do d959_405 <- get
                                                  t457_406 <- StateT char
                                                  case t457_406 of
                                                      '\\' -> return ()
                                                      _ -> gets position >>= (throwError . mkParseError "'\\\\'" "not match pattern: " "" d959_405 ["char"])
                                                  let '\\' = t457_406
                                                  return ()
                                                  c <- StateT escapeC
                                                  return c]
                stringLit120_125 = foldl1 mplus [do d961_407 <- get
                                                    t459_408 <- StateT char
                                                    let c = t459_408
                                                    unless (c `notElem` "\"\\") (gets position >>= (throwError . mkParseError "c `notElem` \"\\\"\\\\\"" "not match: " "" d961_407 ["char"]))
                                                    s <- StateT stringLit
                                                    return (c : s),
                                                 do d963_409 <- get
                                                    t461_410 <- StateT char
                                                    case t461_410 of
                                                        '\\' -> return ()
                                                        _ -> gets position >>= (throwError . mkParseError "'\\\\'" "not match pattern: " "" d963_409 ["char"])
                                                    let '\\' = t461_410
                                                    return ()
                                                    c <- StateT escapeC
                                                    s <- StateT stringLit
                                                    return (c : s),
                                                 return ""]
                escapeC121_126 = foldl1 mplus [do d966_411 <- get
                                                  t464_412 <- StateT char
                                                  case t464_412 of
                                                      '"' -> return ()
                                                      _ -> gets position >>= (throwError . mkParseError "'\"'" "not match pattern: " "" d966_411 ["char"])
                                                  let '"' = t464_412
                                                  return ()
                                                  return '"',
                                               do d967_413 <- get
                                                  t465_414 <- StateT char
                                                  case t465_414 of
                                                      '\'' -> return ()
                                                      _ -> gets position >>= (throwError . mkParseError "'\\''" "not match pattern: " "" d967_413 ["char"])
                                                  let '\'' = t465_414
                                                  return ()
                                                  return '\'',
                                               do d968_415 <- get
                                                  t466_416 <- StateT char
                                                  case t466_416 of
                                                      '\\' -> return ()
                                                      _ -> gets position >>= (throwError . mkParseError "'\\\\'" "not match pattern: " "" d968_415 ["char"])
                                                  let '\\' = t466_416
                                                  return ()
                                                  return '\\',
                                               do d969_417 <- get
                                                  t467_418 <- StateT char
                                                  case t467_418 of
                                                      'n' -> return ()
                                                      _ -> gets position >>= (throwError . mkParseError "'n'" "not match pattern: " "" d969_417 ["char"])
                                                  let 'n' = t467_418
                                                  return ()
                                                  return '\n',
                                               do d970_419 <- get
                                                  t468_420 <- StateT char
                                                  case t468_420 of
                                                      't' -> return ()
                                                      _ -> gets position >>= (throwError . mkParseError "'t'" "not match pattern: " "" d970_419 ["char"])
                                                  let 't' = t468_420
                                                  return ()
                                                  return '\t']
                pats122_127 = foldl1 mplus [do p <- StateT pat
                                               _ <- StateT spaces
                                               return ()
                                               ps <- StateT pats
                                               return (p : ps),
                                            return []]
                readFromLs123_128 = foldl1 mplus [do rf <- StateT readFrom
                                                     d975_421 <- get
                                                     t473_422 <- StateT char
                                                     case t473_422 of
                                                         '*' -> return ()
                                                         _ -> gets position >>= (throwError . mkParseError "'*'" "not match pattern: " "" d975_421 ["char"])
                                                     let '*' = t473_422
                                                     return ()
                                                     return (FromL List rf),
                                                  do rf <- StateT readFrom
                                                     d977_423 <- get
                                                     t475_424 <- StateT char
                                                     case t475_424 of
                                                         '+' -> return ()
                                                         _ -> gets position >>= (throwError . mkParseError "'+'" "not match pattern: " "" d977_423 ["char"])
                                                     let '+' = t475_424
                                                     return ()
                                                     return (FromL List1 rf),
                                                  do rf <- StateT readFrom
                                                     d979_425 <- get
                                                     t477_426 <- StateT char
                                                     case t477_426 of
                                                         '?' -> return ()
                                                         _ -> gets position >>= (throwError . mkParseError "'?'" "not match pattern: " "" d979_425 ["char"])
                                                     let '?' = t477_426
                                                     return ()
                                                     return (FromL Optional rf),
                                                  do rf <- StateT readFrom
                                                     return rf]
                readFrom124_129 = foldl1 mplus [do v <- StateT variable
                                                   return (FromVariable $ Just v),
                                                do d982_427 <- get
                                                   t480_428 <- StateT char
                                                   case t480_428 of
                                                       '(' -> return ()
                                                       _ -> gets position >>= (throwError . mkParseError "'('" "not match pattern: " "" d982_427 ["char"])
                                                   let '(' = t480_428
                                                   return ()
                                                   s <- StateT selection
                                                   d984_429 <- get
                                                   t482_430 <- StateT char
                                                   case t482_430 of
                                                       ')' -> return ()
                                                       _ -> gets position >>= (throwError . mkParseError "')'" "not match pattern: " "" d984_429 ["char"])
                                                   let ')' = t482_430
                                                   return ()
                                                   return (fromSelectionQ s),
                                                do e <- StateT expressionHsSugar
                                                   return (fromSelectionQ $ normalSelectionQ [e])]
                selectCharsLs125_130 = foldl1 mplus [do rf <- StateT selectChars
                                                        d987_431 <- get
                                                        t485_432 <- StateT char
                                                        case t485_432 of
                                                            '*' -> return ()
                                                            _ -> gets position >>= (throwError . mkParseError "'*'" "not match pattern: " "" d987_431 ["char"])
                                                        let '*' = t485_432
                                                        return ()
                                                        return (FromL List rf),
                                                     do rf <- StateT selectChars
                                                        d989_433 <- get
                                                        t487_434 <- StateT char
                                                        case t487_434 of
                                                            '+' -> return ()
                                                            _ -> gets position >>= (throwError . mkParseError "'+'" "not match pattern: " "" d989_433 ["char"])
                                                        let '+' = t487_434
                                                        return ()
                                                        return (FromL List1 rf),
                                                     do rf <- StateT selectChars
                                                        d991_435 <- get
                                                        t489_436 <- StateT char
                                                        case t489_436 of
                                                            '?' -> return ()
                                                            _ -> gets position >>= (throwError . mkParseError "'?'" "not match pattern: " "" d991_435 ["char"])
                                                        let '?' = t489_436
                                                        return ()
                                                        return (FromL Optional rf),
                                                     do rf <- StateT selectChars
                                                        return rf]
                selectChars126_131 = foldl1 mplus [do d993_437 <- get
                                                      t491_438 <- StateT char
                                                      case t491_438 of
                                                          '[' -> return ()
                                                          _ -> gets position >>= (throwError . mkParseError "'['" "not match pattern: " "" d993_437 ["char"])
                                                      let '[' = t491_438
                                                      return ()
                                                      cs <- list1158_439 (foldl1 mplus [do d995_440 <- get
                                                                                           t493_441 <- StateT char
                                                                                           let c = t493_441
                                                                                           unless (isLower c) (gets position >>= (throwError . mkParseError "isLower c" "not match: " "" d995_440 ["char"]))
                                                                                           return c])
                                                      d996_442 <- get
                                                      t494_443 <- StateT char
                                                      case t494_443 of
                                                          ']' -> return ()
                                                          _ -> gets position >>= (throwError . mkParseError "']'" "not match pattern: " "" d996_442 ["char"])
                                                      let ']' = t494_443
                                                      return ()
                                                      return (fromTokenChars cs),
                                                   do d997_444 <- get
                                                      t495_445 <- StateT char
                                                      case t495_445 of
                                                          '[' -> return ()
                                                          _ -> gets position >>= (throwError . mkParseError "'['" "not match pattern: " "" d997_444 ["char"])
                                                      let '[' = t495_445
                                                      return ()
                                                      d998_446 <- get
                                                      t496_447 <- StateT char
                                                      let cb = t496_447
                                                      unless (cb `notElem` "\\-") (gets position >>= (throwError . mkParseError "cb `notElem` \"\\\\-\"" "not match: " "" d998_446 ["char"]))
                                                      d999_448 <- get
                                                      t497_449 <- StateT char
                                                      case t497_449 of
                                                          '-' -> return ()
                                                          _ -> gets position >>= (throwError . mkParseError "'-'" "not match pattern: " "" d999_448 ["char"])
                                                      let '-' = t497_449
                                                      return ()
                                                      d1000_450 <- get
                                                      t498_451 <- StateT char
                                                      let ce = t498_451
                                                      unless (ce `notElem` "\\-") (gets position >>= (throwError . mkParseError "ce `notElem` \"\\\\-\"" "not match: " "" d1000_450 ["char"]))
                                                      d1001_452 <- get
                                                      t499_453 <- StateT char
                                                      case t499_453 of
                                                          ']' -> return ()
                                                          _ -> gets position >>= (throwError . mkParseError "']'" "not match pattern: " "" d1001_452 ["char"])
                                                      let ']' = t499_453
                                                      return ()
                                                      return (fromTokenChars [cb .. ce]),
                                                   do d1002_454 <- get
                                                      t500_455 <- StateT char
                                                      case t500_455 of
                                                          '\'' -> return ()
                                                          _ -> gets position >>= (throwError . mkParseError "'\\''" "not match pattern: " "" d1002_454 ["char"])
                                                      let '\'' = t500_455
                                                      return ()
                                                      d1003_456 <- get
                                                      t501_457 <- StateT char
                                                      let c = t501_457
                                                      unless (c `notElem` "\\'") (gets position >>= (throwError . mkParseError "c `notElem` \"\\\\'\"" "not match: " "" d1003_456 ["char"]))
                                                      d1004_458 <- get
                                                      t502_459 <- StateT char
                                                      case t502_459 of
                                                          '\'' -> return ()
                                                          _ -> gets position >>= (throwError . mkParseError "'\\''" "not match pattern: " "" d1004_458 ["char"])
                                                      let '\'' = t502_459
                                                      return ()
                                                      return (fromTokenChars [c])]
                test127_132 = foldl1 mplus [do d1005_460 <- get
                                               t503_461 <- StateT char
                                               case t503_461 of
                                                   '[' -> return ()
                                                   _ -> gets position >>= (throwError . mkParseError "'['" "not match pattern: " "" d1005_460 ["char"])
                                               let '[' = t503_461
                                               return ()
                                               h <- StateT hsExpLam
                                               _ <- StateT spaces
                                               return ()
                                               com <- optional156_307 (StateT comForErr)
                                               d1009_462 <- get
                                               t507_463 <- StateT char
                                               case t507_463 of
                                                   ']' -> return ()
                                                   _ -> gets position >>= (throwError . mkParseError "']'" "not match pattern: " "" d1009_462 ["char"])
                                               let ']' = t507_463
                                               return ()
                                               return (h, maybe "" id com)]
                hsExpLam128_133 = foldl1 mplus [do d1010_464 <- get
                                                   t508_465 <- StateT char
                                                   case t508_465 of
                                                       '\\' -> return ()
                                                       _ -> gets position >>= (throwError . mkParseError "'\\\\'" "not match pattern: " "" d1010_464 ["char"])
                                                   let '\\' = t508_465
                                                   return ()
                                                   _ <- StateT spaces
                                                   return ()
                                                   ps <- StateT pats
                                                   _ <- StateT spaces
                                                   return ()
                                                   d1014_466 <- get
                                                   t512_467 <- StateT char
                                                   case t512_467 of
                                                       '-' -> return ()
                                                       _ -> gets position >>= (throwError . mkParseError "'-'" "not match pattern: " "" d1014_466 ["char"])
                                                   let '-' = t512_467
                                                   return ()
                                                   d1015_468 <- get
                                                   t513_469 <- StateT char
                                                   case t513_469 of
                                                       '>' -> return ()
                                                       _ -> gets position >>= (throwError . mkParseError "'>'" "not match pattern: " "" d1015_468 ["char"])
                                                   let '>' = t513_469
                                                   return ()
                                                   _ <- StateT spaces
                                                   return ()
                                                   e <- StateT hsExpTyp
                                                   return (LamE ps e),
                                                do e <- StateT hsExpTyp
                                                   return e]
                hsExpTyp129_134 = foldl1 mplus [do eo <- StateT hsExpOp
                                                   d1020_470 <- get
                                                   t518_471 <- StateT char
                                                   case t518_471 of
                                                       ':' -> return ()
                                                       _ -> gets position >>= (throwError . mkParseError "':'" "not match pattern: " "" d1020_470 ["char"])
                                                   let ':' = t518_471
                                                   return ()
                                                   d1021_472 <- get
                                                   t519_473 <- StateT char
                                                   case t519_473 of
                                                       ':' -> return ()
                                                       _ -> gets position >>= (throwError . mkParseError "':'" "not match pattern: " "" d1021_472 ["char"])
                                                   let ':' = t519_473
                                                   return ()
                                                   _ <- StateT spaces
                                                   return ()
                                                   t <- StateT hsTypeArr
                                                   return (SigE eo t),
                                                do eo <- StateT hsExpOp
                                                   return eo]
                hsExpOp130_135 = foldl1 mplus [do l <- StateT hsExp
                                                  _ <- StateT spaces
                                                  return ()
                                                  o <- StateT hsOp
                                                  _ <- StateT spaces
                                                  return ()
                                                  r <- StateT hsExpOp
                                                  return (UInfixE (l id) o r),
                                               do e <- StateT hsExp
                                                  return (e id)]
                hsOp131_136 = foldl1 mplus [do d1031_474 <- get
                                               t529_475 <- StateT char
                                               let c = t529_475
                                               unless (c `elem` "+*/-!|&.^=<>$") (gets position >>= (throwError . mkParseError "c `elem` \"+*/-!|&.^=<>$\"" "not match: " "" d1031_474 ["char"]))
                                               o <- StateT opTail
                                               return (VarE $ mkName $ c : o),
                                            do d1033_476 <- get
                                               t531_477 <- StateT char
                                               case t531_477 of
                                                   ':' -> return ()
                                                   _ -> gets position >>= (throwError . mkParseError "':'" "not match pattern: " "" d1033_476 ["char"])
                                               let ':' = t531_477
                                               return ()
                                               d1035_478 <- get
                                               do err1168_479 <- ((do d1034_480 <- get
                                                                      t532_481 <- StateT char
                                                                      case t532_481 of
                                                                          ':' -> return ()
                                                                          _ -> gets position >>= (throwError . mkParseError "':'" "not match pattern: " "" d1034_480 ["char"])
                                                                      let ':' = t532_481
                                                                      return ()) >> return False) `catchError` const (return True)
                                                  unless err1168_479 (gets position >>= (throwError . mkParseError "!':':" "not match: " "" d1035_478 ["char"]))
                                               put d1035_478
                                               o <- StateT opTail
                                               return (ConE $ mkName $ ':' : o),
                                            do d1037_482 <- get
                                               t534_483 <- StateT char
                                               case t534_483 of
                                                   '`' -> return ()
                                                   _ -> gets position >>= (throwError . mkParseError "'`'" "not match pattern: " "" d1037_482 ["char"])
                                               let '`' = t534_483
                                               return ()
                                               v <- StateT variable
                                               d1039_484 <- get
                                               t536_485 <- StateT char
                                               case t536_485 of
                                                   '`' -> return ()
                                                   _ -> gets position >>= (throwError . mkParseError "'`'" "not match pattern: " "" d1039_484 ["char"])
                                               let '`' = t536_485
                                               return ()
                                               return (VarE $ mkName v),
                                            do d1040_486 <- get
                                               t537_487 <- StateT char
                                               case t537_487 of
                                                   '`' -> return ()
                                                   _ -> gets position >>= (throwError . mkParseError "'`'" "not match pattern: " "" d1040_486 ["char"])
                                               let '`' = t537_487
                                               return ()
                                               t <- StateT typ
                                               d1042_488 <- get
                                               t539_489 <- StateT char
                                               case t539_489 of
                                                   '`' -> return ()
                                                   _ -> gets position >>= (throwError . mkParseError "'`'" "not match pattern: " "" d1042_488 ["char"])
                                               let '`' = t539_489
                                               return ()
                                               return (ConE $ mkName t)]
                opTail132_137 = foldl1 mplus [do d1043_490 <- get
                                                 t540_491 <- StateT char
                                                 let c = t540_491
                                                 unless (c `elem` ":+*/-!|&.^=<>$") (gets position >>= (throwError . mkParseError "c `elem` \":+*/-!|&.^=<>$\"" "not match: " "" d1043_490 ["char"]))
                                                 s <- StateT opTail
                                                 return (c : s),
                                              return ""]
                hsExp133_138 = foldl1 mplus [do e <- StateT hsExp1
                                                _ <- StateT spaces
                                                return ()
                                                h <- StateT hsExp
                                                return (\f -> h (f e `AppE`)),
                                             do e <- StateT hsExp1
                                                return (\f -> f e)]
                hsExp1134_139 = foldl1 mplus [do d1049_492 <- get
                                                 t546_493 <- StateT char
                                                 case t546_493 of
                                                     '(' -> return ()
                                                     _ -> gets position >>= (throwError . mkParseError "'('" "not match pattern: " "" d1049_492 ["char"])
                                                 let '(' = t546_493
                                                 return ()
                                                 l <- optional156_307 (foldl1 mplus [do e <- StateT hsExpTyp
                                                                                        return e])
                                                 _ <- StateT spaces
                                                 return ()
                                                 o <- StateT hsOp
                                                 _ <- StateT spaces
                                                 return ()
                                                 r <- optional156_307 (foldl1 mplus [do e <- StateT hsExpTyp
                                                                                        return e])
                                                 d1057_494 <- get
                                                 t554_495 <- StateT char
                                                 case t554_495 of
                                                     ')' -> return ()
                                                     _ -> gets position >>= (throwError . mkParseError "')'" "not match pattern: " "" d1057_494 ["char"])
                                                 let ')' = t554_495
                                                 return ()
                                                 return (InfixE l o r),
                                              do d1058_496 <- get
                                                 t555_497 <- StateT char
                                                 case t555_497 of
                                                     '(' -> return ()
                                                     _ -> gets position >>= (throwError . mkParseError "'('" "not match pattern: " "" d1058_496 ["char"])
                                                 let '(' = t555_497
                                                 return ()
                                                 et <- StateT hsExpTpl
                                                 d1060_498 <- get
                                                 t557_499 <- StateT char
                                                 case t557_499 of
                                                     ')' -> return ()
                                                     _ -> gets position >>= (throwError . mkParseError "')'" "not match pattern: " "" d1060_498 ["char"])
                                                 let ')' = t557_499
                                                 return ()
                                                 return (TupE et),
                                              do d1061_500 <- get
                                                 t558_501 <- StateT char
                                                 case t558_501 of
                                                     '[' -> return ()
                                                     _ -> gets position >>= (throwError . mkParseError "'['" "not match pattern: " "" d1061_500 ["char"])
                                                 let '[' = t558_501
                                                 return ()
                                                 et <- StateT hsExpTpl
                                                 d1063_502 <- get
                                                 t560_503 <- StateT char
                                                 case t560_503 of
                                                     ']' -> return ()
                                                     _ -> gets position >>= (throwError . mkParseError "']'" "not match pattern: " "" d1063_502 ["char"])
                                                 let ']' = t560_503
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
                                              do d1068_504 <- get
                                                 t565_505 <- StateT char
                                                 case t565_505 of
                                                     '\'' -> return ()
                                                     _ -> gets position >>= (throwError . mkParseError "'\\''" "not match pattern: " "" d1068_504 ["char"])
                                                 let '\'' = t565_505
                                                 return ()
                                                 c <- StateT charLit
                                                 d1070_506 <- get
                                                 t567_507 <- StateT char
                                                 case t567_507 of
                                                     '\'' -> return ()
                                                     _ -> gets position >>= (throwError . mkParseError "'\\''" "not match pattern: " "" d1070_506 ["char"])
                                                 let '\'' = t567_507
                                                 return ()
                                                 return (LitE $ charL c),
                                              do d1071_508 <- get
                                                 t568_509 <- StateT char
                                                 case t568_509 of
                                                     '"' -> return ()
                                                     _ -> gets position >>= (throwError . mkParseError "'\"'" "not match pattern: " "" d1071_508 ["char"])
                                                 let '"' = t568_509
                                                 return ()
                                                 s <- StateT stringLit
                                                 d1073_510 <- get
                                                 t570_511 <- StateT char
                                                 case t570_511 of
                                                     '"' -> return ()
                                                     _ -> gets position >>= (throwError . mkParseError "'\"'" "not match pattern: " "" d1073_510 ["char"])
                                                 let '"' = t570_511
                                                 return ()
                                                 return (LitE $ stringL s),
                                              do d1074_512 <- get
                                                 t571_513 <- StateT char
                                                 case t571_513 of
                                                     '-' -> return ()
                                                     _ -> gets position >>= (throwError . mkParseError "'-'" "not match pattern: " "" d1074_512 ["char"])
                                                 let '-' = t571_513
                                                 return ()
                                                 _ <- StateT spaces
                                                 return ()
                                                 e <- StateT hsExp1
                                                 return (AppE (VarE $ mkName "negate") e)]
                hsExpTpl135_140 = foldl1 mplus [do e <- StateT hsExpLam
                                                   _ <- StateT spaces
                                                   return ()
                                                   d1079_514 <- get
                                                   t576_515 <- StateT char
                                                   case t576_515 of
                                                       ',' -> return ()
                                                       _ -> gets position >>= (throwError . mkParseError "','" "not match pattern: " "" d1079_514 ["char"])
                                                   let ',' = t576_515
                                                   return ()
                                                   _ <- StateT spaces
                                                   return ()
                                                   et <- StateT hsExpTpl
                                                   return (e : et),
                                                do e <- StateT hsExpLam
                                                   return [e],
                                                return []]
                hsTypeArr136_141 = foldl1 mplus [do l <- StateT hsType
                                                    d1084_516 <- get
                                                    t581_517 <- StateT char
                                                    case t581_517 of
                                                        '-' -> return ()
                                                        _ -> gets position >>= (throwError . mkParseError "'-'" "not match pattern: " "" d1084_516 ["char"])
                                                    let '-' = t581_517
                                                    return ()
                                                    d1085_518 <- get
                                                    t582_519 <- StateT char
                                                    case t582_519 of
                                                        '>' -> return ()
                                                        _ -> gets position >>= (throwError . mkParseError "'>'" "not match pattern: " "" d1085_518 ["char"])
                                                    let '>' = t582_519
                                                    return ()
                                                    _ <- StateT spaces
                                                    return ()
                                                    r <- StateT hsTypeArr
                                                    return (AppT (AppT ArrowT $ l id) r),
                                                 do t <- StateT hsType
                                                    return (t id)]
                hsType137_142 = foldl1 mplus [do t <- StateT hsType1
                                                 ts <- StateT hsType
                                                 return (\f -> ts (f t `AppT`)),
                                              do t <- StateT hsType1
                                                 return ($ t)]
                hsType1138_143 = foldl1 mplus [do d1092_520 <- get
                                                  t589_521 <- StateT char
                                                  case t589_521 of
                                                      '[' -> return ()
                                                      _ -> gets position >>= (throwError . mkParseError "'['" "not match pattern: " "" d1092_520 ["char"])
                                                  let '[' = t589_521
                                                  return ()
                                                  d1093_522 <- get
                                                  t590_523 <- StateT char
                                                  case t590_523 of
                                                      ']' -> return ()
                                                      _ -> gets position >>= (throwError . mkParseError "']'" "not match pattern: " "" d1093_522 ["char"])
                                                  let ']' = t590_523
                                                  return ()
                                                  _ <- StateT spaces
                                                  return ()
                                                  return ListT,
                                               do d1095_524 <- get
                                                  t592_525 <- StateT char
                                                  case t592_525 of
                                                      '[' -> return ()
                                                      _ -> gets position >>= (throwError . mkParseError "'['" "not match pattern: " "" d1095_524 ["char"])
                                                  let '[' = t592_525
                                                  return ()
                                                  t <- StateT hsTypeArr
                                                  d1097_526 <- get
                                                  t594_527 <- StateT char
                                                  case t594_527 of
                                                      ']' -> return ()
                                                      _ -> gets position >>= (throwError . mkParseError "']'" "not match pattern: " "" d1097_526 ["char"])
                                                  let ']' = t594_527
                                                  return ()
                                                  _ <- StateT spaces
                                                  return ()
                                                  return (ListT `AppT` t),
                                               do d1099_528 <- get
                                                  t596_529 <- StateT char
                                                  case t596_529 of
                                                      '(' -> return ()
                                                      _ -> gets position >>= (throwError . mkParseError "'('" "not match pattern: " "" d1099_528 ["char"])
                                                  let '(' = t596_529
                                                  return ()
                                                  _ <- StateT spaces
                                                  return ()
                                                  tt <- StateT hsTypeTpl
                                                  d1102_530 <- get
                                                  t599_531 <- StateT char
                                                  case t599_531 of
                                                      ')' -> return ()
                                                      _ -> gets position >>= (throwError . mkParseError "')'" "not match pattern: " "" d1102_530 ["char"])
                                                  let ')' = t599_531
                                                  return ()
                                                  _ <- StateT spaces
                                                  return ()
                                                  return (foldl AppT (TupleT $ length tt) tt),
                                               do t <- StateT typToken
                                                  return (ConT $ mkName t),
                                               do d1105_532 <- get
                                                  t602_533 <- StateT char
                                                  case t602_533 of
                                                      '(' -> return ()
                                                      _ -> gets position >>= (throwError . mkParseError "'('" "not match pattern: " "" d1105_532 ["char"])
                                                  let '(' = t602_533
                                                  return ()
                                                  d1106_534 <- get
                                                  t603_535 <- StateT char
                                                  case t603_535 of
                                                      '-' -> return ()
                                                      _ -> gets position >>= (throwError . mkParseError "'-'" "not match pattern: " "" d1106_534 ["char"])
                                                  let '-' = t603_535
                                                  return ()
                                                  d1107_536 <- get
                                                  t604_537 <- StateT char
                                                  case t604_537 of
                                                      '>' -> return ()
                                                      _ -> gets position >>= (throwError . mkParseError "'>'" "not match pattern: " "" d1107_536 ["char"])
                                                  let '>' = t604_537
                                                  return ()
                                                  d1108_538 <- get
                                                  t605_539 <- StateT char
                                                  case t605_539 of
                                                      ')' -> return ()
                                                      _ -> gets position >>= (throwError . mkParseError "')'" "not match pattern: " "" d1108_538 ["char"])
                                                  let ')' = t605_539
                                                  return ()
                                                  _ <- StateT spaces
                                                  return ()
                                                  return ArrowT]
                hsTypeTpl139_144 = foldl1 mplus [do t <- StateT hsTypeArr
                                                    d1111_540 <- get
                                                    t608_541 <- StateT char
                                                    case t608_541 of
                                                        ',' -> return ()
                                                        _ -> gets position >>= (throwError . mkParseError "','" "not match pattern: " "" d1111_540 ["char"])
                                                    let ',' = t608_541
                                                    return ()
                                                    _ <- StateT spaces
                                                    return ()
                                                    tt <- StateT hsTypeTpl
                                                    return (t : tt),
                                                 do t <- StateT hsTypeArr
                                                    return [t],
                                                 return []]
                typ140_145 = foldl1 mplus [do u <- StateT upper
                                              t <- StateT tvtail
                                              return (u : t)]
                variable141_146 = foldl1 mplus [do l <- StateT lower
                                                   t <- StateT tvtail
                                                   return (l : t)]
                tvtail142_147 = foldl1 mplus [do a <- StateT alpha
                                                 t <- StateT tvtail
                                                 return (a : t),
                                              return ""]
                integer143_148 = foldl1 mplus [do dh <- StateT digit
                                                  ds <- list157_342 (foldl1 mplus [do d <- StateT digit
                                                                                      return d])
                                                  return (read $ dh : ds)]
                alpha144_149 = foldl1 mplus [do u <- StateT upper
                                                return u,
                                             do l <- StateT lower
                                                return l,
                                             do d <- StateT digit
                                                return d,
                                             do d1127_542 <- get
                                                t624_543 <- StateT char
                                                case t624_543 of
                                                    '\'' -> return ()
                                                    _ -> gets position >>= (throwError . mkParseError "'\\''" "not match pattern: " "" d1127_542 ["char"])
                                                let '\'' = t624_543
                                                return ()
                                                return '\'']
                upper145_150 = foldl1 mplus [do d1128_544 <- get
                                                t625_545 <- StateT char
                                                let u = t625_545
                                                unless (isUpper u) (gets position >>= (throwError . mkParseError "isUpper u" "not match: " "" d1128_544 ["char"]))
                                                return u]
                lower146_151 = foldl1 mplus [do d1129_546 <- get
                                                t626_547 <- StateT char
                                                let l = t626_547
                                                unless (isLower l || l == '_') (gets position >>= (throwError . mkParseError "isLower l || l == '_'" "not match: " "" d1129_546 ["char"]))
                                                return l]
                digit147_152 = foldl1 mplus [do d1130_548 <- get
                                                t627_549 <- StateT char
                                                let d = t627_549
                                                unless (isDigit d) (gets position >>= (throwError . mkParseError "isDigit d" "not match: " "" d1130_548 ["char"]))
                                                return d]
                spaces148_153 = foldl1 mplus [do _ <- StateT space
                                                 return ()
                                                 _ <- StateT spaces
                                                 return ()
                                                 return (),
                                              return ()]
                space149_154 = foldl1 mplus [do d1133_550 <- get
                                                t630_551 <- StateT char
                                                let s = t630_551
                                                unless (isSpace s) (gets position >>= (throwError . mkParseError "isSpace s" "not match: " "" d1133_550 ["char"]))
                                                return (),
                                             do d1134_552 <- get
                                                t631_553 <- StateT char
                                                case t631_553 of
                                                    '-' -> return ()
                                                    _ -> gets position >>= (throwError . mkParseError "'-'" "not match pattern: " "" d1134_552 ["char"])
                                                let '-' = t631_553
                                                return ()
                                                d1135_554 <- get
                                                t632_555 <- StateT char
                                                case t632_555 of
                                                    '-' -> return ()
                                                    _ -> gets position >>= (throwError . mkParseError "'-'" "not match pattern: " "" d1135_554 ["char"])
                                                let '-' = t632_555
                                                return ()
                                                _ <- StateT notNLString
                                                return ()
                                                _ <- StateT newLine
                                                return ()
                                                return (),
                                             do _ <- StateT comment
                                                return ()
                                                return ()]
                notNLString150_155 = foldl1 mplus [do d1140_556 <- get
                                                      do err1169_557 <- ((do _ <- StateT newLine
                                                                             return ()) >> return False) `catchError` const (return True)
                                                         unless err1169_557 (gets position >>= (throwError . mkParseError "!_:newLine" "not match: " "" d1140_556 ["newLine"]))
                                                      put d1140_556
                                                      c <- StateT char
                                                      s <- StateT notNLString
                                                      return (c : s),
                                                   return ""]
                newLine151_156 = foldl1 mplus [do d1143_558 <- get
                                                  t639_559 <- StateT char
                                                  case t639_559 of
                                                      '\n' -> return ()
                                                      _ -> gets position >>= (throwError . mkParseError "'\\n'" "not match pattern: " "" d1143_558 ["char"])
                                                  let '\n' = t639_559
                                                  return ()
                                                  return ()]
                comment152_157 = foldl1 mplus [do d1144_560 <- get
                                                  t640_561 <- StateT char
                                                  case t640_561 of
                                                      '{' -> return ()
                                                      _ -> gets position >>= (throwError . mkParseError "'{'" "not match pattern: " "" d1144_560 ["char"])
                                                  let '{' = t640_561
                                                  return ()
                                                  d1145_562 <- get
                                                  t641_563 <- StateT char
                                                  case t641_563 of
                                                      '-' -> return ()
                                                      _ -> gets position >>= (throwError . mkParseError "'-'" "not match pattern: " "" d1145_562 ["char"])
                                                  let '-' = t641_563
                                                  return ()
                                                  d1147_564 <- get
                                                  do err1170_565 <- ((do d1146_566 <- get
                                                                         t642_567 <- StateT char
                                                                         case t642_567 of
                                                                             '#' -> return ()
                                                                             _ -> gets position >>= (throwError . mkParseError "'#'" "not match pattern: " "" d1146_566 ["char"])
                                                                         let '#' = t642_567
                                                                         return ()) >> return False) `catchError` const (return True)
                                                     unless err1170_565 (gets position >>= (throwError . mkParseError "!'#':" "not match: " "" d1147_564 ["char"]))
                                                  put d1147_564
                                                  _ <- StateT comments
                                                  return ()
                                                  _ <- StateT comEnd
                                                  return ()
                                                  return ()]
                comments153_158 = foldl1 mplus [do _ <- StateT notComStr
                                                   return ()
                                                   _ <- StateT comment
                                                   return ()
                                                   _ <- StateT comments
                                                   return ()
                                                   return (),
                                                do _ <- StateT notComStr
                                                   return ()
                                                   return ()]
                notComStr154_159 = foldl1 mplus [do d1155_568 <- get
                                                    do err1171_569 <- ((do _ <- StateT comment
                                                                           return ()) >> return False) `catchError` const (return True)
                                                       unless err1171_569 (gets position >>= (throwError . mkParseError "!_:comment" "not match: " "" d1155_568 ["comment"]))
                                                    put d1155_568
                                                    d1157_570 <- get
                                                    do err1172_571 <- ((do _ <- StateT comEnd
                                                                           return ()) >> return False) `catchError` const (return True)
                                                       unless err1172_571 (gets position >>= (throwError . mkParseError "!_:comEnd" "not match: " "" d1157_570 ["comEnd"]))
                                                    put d1157_570
                                                    _ <- StateT char
                                                    return ()
                                                    _ <- StateT notComStr
                                                    return ()
                                                    return (),
                                                 return ()]
                comEnd155_160 = foldl1 mplus [do d1160_572 <- get
                                                 t653_573 <- StateT char
                                                 case t653_573 of
                                                     '-' -> return ()
                                                     _ -> gets position >>= (throwError . mkParseError "'-'" "not match pattern: " "" d1160_572 ["char"])
                                                 let '-' = t653_573
                                                 return ()
                                                 d1161_574 <- get
                                                 t654_575 <- StateT char
                                                 case t654_575 of
                                                     '}' -> return ()
                                                     _ -> gets position >>= (throwError . mkParseError "'}'" "not match pattern: " "" d1161_574 ["char"])
                                                 let '}' = t654_575
                                                 return ()
                                                 return ()]
                list157_342 :: forall m a . (MonadPlus m, Applicative m) =>
                                            m a -> m ([a])
                list1158_439 :: forall m a . (MonadPlus m, Applicative m) =>
                                             m a -> m ([a])
                list157_342 p = list1158_439 p `mplus` return []
                list1158_439 p = ((:) <$> p) <*> list157_342 p
                optional156_307 :: forall m a . (MonadPlus m, Applicative m) =>
                                                m a -> m (Maybe a)
                optional156_307 p = (Just <$> p) `mplus` return Nothing

