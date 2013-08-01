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
                                 ((PegFile, Derivs))),
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
                             ((STPeg, Derivs))),
              monadType :: (ErrorT (ParseError (Pos String) Derivs)
                                   Identity
                                   ((Type, Derivs))),
              sourceType :: (ErrorT (ParseError (Pos String) Derivs)
                                    Identity
                                    ((String, Derivs))),
              peg_ :: (ErrorT (ParseError (Pos String) Derivs)
                              Identity
                              ((Peg, Derivs))),
              definition :: (ErrorT (ParseError (Pos String) Derivs)
                                    Identity
                                    ((Definition, Derivs))),
              selection :: (ErrorT (ParseError (Pos String) Derivs)
                                   Identity
                                   ((Selection, Derivs))),
              normalSelection :: (ErrorT (ParseError (Pos String) Derivs)
                                         Identity
                                         (([Expression], Derivs))),
              plainSelection :: (ErrorT (ParseError (Pos String) Derivs)
                                        Identity
                                        (([PlainExpression], Derivs))),
              expressionHs :: (ErrorT (ParseError (Pos String) Derivs)
                                      Identity
                                      ((Expression, Derivs))),
              expressionHsSugar :: (ErrorT (ParseError (Pos String) Derivs)
                                           Identity
                                           ((Expression, Derivs))),
              plainExpressionHs :: (ErrorT (ParseError (Pos String) Derivs)
                                           Identity
                                           ((PlainExpression, Derivs))),
              plainHAReadFromLs :: (ErrorT (ParseError (Pos String) Derivs)
                                           Identity
                                           (((Lookahead, ReadFrom), Derivs))),
              plainReadFromLs :: (ErrorT (ParseError (Pos String) Derivs)
                                         Identity
                                         ((ReadFrom, Derivs))),
              expression :: (ErrorT (ParseError (Pos String) Derivs)
                                    Identity
                                    (([(Lookahead, Check)], Derivs))),
              nameLeaf_ :: (ErrorT (ParseError (Pos String) Derivs)
                                   Identity
                                   (((Lookahead, Check), Derivs))),
              nameLeaf :: (ErrorT (ParseError (Pos String) Derivs)
                                  Identity
                                  ((Check, Derivs))),
              nameLeafNoCom :: (ErrorT (ParseError (Pos String) Derivs)
                                       Identity
                                       ((Check, Derivs))),
              comForErr :: (ErrorT (ParseError (Pos String) Derivs)
                                   Identity
                                   ((String, Derivs))),
              leaf :: (ErrorT (ParseError (Pos String) Derivs)
                              Identity
                              (((ReadFrom, Maybe ((Exp, String))), Derivs))),
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
                                    ((ReadFrom, Derivs))),
              readFrom :: (ErrorT (ParseError (Pos String) Derivs)
                                  Identity
                                  ((ReadFrom, Derivs))),
              selectCharsLs :: (ErrorT (ParseError (Pos String) Derivs)
                                       Identity
                                       ((ReadFrom, Derivs))),
              selectChars :: (ErrorT (ParseError (Pos String) Derivs)
                                     Identity
                                     ((ReadFrom, Derivs))),
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
parse = parse1669_0 initialPos
          where parse1669_0 pos1668_1 s1670_2 = d662_3
                                where d662_3 = Derivs pegFile0_4 pragmas1_5 pragma2_6 pragmaStr3_7 pragmaItems4_8 pragmaEnd5_9 moduleDec6_10 moduleName7_11 moduleDecStr8_12 whr9_13 preImpPap10_14 prePeg11_15 afterPeg12_16 importPapillon13_17 varToken14_18 typToken15_19 pap16_20 peg17_21 monadType18_22 sourceType19_23 peg_20_24 definition21_25 selection22_26 normalSelection23_27 plainSelection24_28 expressionHs25_29 expressionHsSugar26_30 plainExpressionHs27_31 plainHAReadFromLs28_32 plainReadFromLs29_33 expression30_34 nameLeaf_31_35 nameLeaf32_36 nameLeafNoCom33_37 comForErr34_38 leaf35_39 patOp36_40 pat37_41 pat138_42 patList39_43 opConName40_44 charLit41_45 stringLit42_46 escapeC43_47 pats44_48 readFromLs45_49 readFrom46_50 selectCharsLs47_51 selectChars48_52 test49_53 hsExpLam50_54 hsExpTyp51_55 hsExpOp52_56 hsOp53_57 opTail54_58 hsExp55_59 hsExp156_60 hsExpTpl57_61 hsTypeArr58_62 hsType59_63 hsType160_64 hsTypeTpl61_65 typ62_66 variable63_67 tvtail64_68 integer65_69 alpha66_70 upper67_71 lower68_72 digit69_73 spaces70_74 space71_75 notNLString72_76 newLine73_77 comment74_78 comments75_79 notComStr76_80 comEnd77_81 chars1671_82 pos1668_1
                                      pegFile0_4 = runStateT pegFile78_83 d662_3
                                      pragmas1_5 = runStateT pragmas79_84 d662_3
                                      pragma2_6 = runStateT pragma80_85 d662_3
                                      pragmaStr3_7 = runStateT pragmaStr81_86 d662_3
                                      pragmaItems4_8 = runStateT pragmaItems82_87 d662_3
                                      pragmaEnd5_9 = runStateT pragmaEnd83_88 d662_3
                                      moduleDec6_10 = runStateT moduleDec84_89 d662_3
                                      moduleName7_11 = runStateT moduleName85_90 d662_3
                                      moduleDecStr8_12 = runStateT moduleDecStr86_91 d662_3
                                      whr9_13 = runStateT whr87_92 d662_3
                                      preImpPap10_14 = runStateT preImpPap88_93 d662_3
                                      prePeg11_15 = runStateT prePeg89_94 d662_3
                                      afterPeg12_16 = runStateT afterPeg90_95 d662_3
                                      importPapillon13_17 = runStateT importPapillon91_96 d662_3
                                      varToken14_18 = runStateT varToken92_97 d662_3
                                      typToken15_19 = runStateT typToken93_98 d662_3
                                      pap16_20 = runStateT pap94_99 d662_3
                                      peg17_21 = runStateT peg95_100 d662_3
                                      monadType18_22 = runStateT monadType96_101 d662_3
                                      sourceType19_23 = runStateT sourceType97_102 d662_3
                                      peg_20_24 = runStateT peg_98_103 d662_3
                                      definition21_25 = runStateT definition99_104 d662_3
                                      selection22_26 = runStateT selection100_105 d662_3
                                      normalSelection23_27 = runStateT normalSelection101_106 d662_3
                                      plainSelection24_28 = runStateT plainSelection102_107 d662_3
                                      expressionHs25_29 = runStateT expressionHs103_108 d662_3
                                      expressionHsSugar26_30 = runStateT expressionHsSugar104_109 d662_3
                                      plainExpressionHs27_31 = runStateT plainExpressionHs105_110 d662_3
                                      plainHAReadFromLs28_32 = runStateT plainHAReadFromLs106_111 d662_3
                                      plainReadFromLs29_33 = runStateT plainReadFromLs107_112 d662_3
                                      expression30_34 = runStateT expression108_113 d662_3
                                      nameLeaf_31_35 = runStateT nameLeaf_109_114 d662_3
                                      nameLeaf32_36 = runStateT nameLeaf110_115 d662_3
                                      nameLeafNoCom33_37 = runStateT nameLeafNoCom111_116 d662_3
                                      comForErr34_38 = runStateT comForErr112_117 d662_3
                                      leaf35_39 = runStateT leaf113_118 d662_3
                                      patOp36_40 = runStateT patOp114_119 d662_3
                                      pat37_41 = runStateT pat115_120 d662_3
                                      pat138_42 = runStateT pat1116_121 d662_3
                                      patList39_43 = runStateT patList117_122 d662_3
                                      opConName40_44 = runStateT opConName118_123 d662_3
                                      charLit41_45 = runStateT charLit119_124 d662_3
                                      stringLit42_46 = runStateT stringLit120_125 d662_3
                                      escapeC43_47 = runStateT escapeC121_126 d662_3
                                      pats44_48 = runStateT pats122_127 d662_3
                                      readFromLs45_49 = runStateT readFromLs123_128 d662_3
                                      readFrom46_50 = runStateT readFrom124_129 d662_3
                                      selectCharsLs47_51 = runStateT selectCharsLs125_130 d662_3
                                      selectChars48_52 = runStateT selectChars126_131 d662_3
                                      test49_53 = runStateT test127_132 d662_3
                                      hsExpLam50_54 = runStateT hsExpLam128_133 d662_3
                                      hsExpTyp51_55 = runStateT hsExpTyp129_134 d662_3
                                      hsExpOp52_56 = runStateT hsExpOp130_135 d662_3
                                      hsOp53_57 = runStateT hsOp131_136 d662_3
                                      opTail54_58 = runStateT opTail132_137 d662_3
                                      hsExp55_59 = runStateT hsExp133_138 d662_3
                                      hsExp156_60 = runStateT hsExp1134_139 d662_3
                                      hsExpTpl57_61 = runStateT hsExpTpl135_140 d662_3
                                      hsTypeArr58_62 = runStateT hsTypeArr136_141 d662_3
                                      hsType59_63 = runStateT hsType137_142 d662_3
                                      hsType160_64 = runStateT hsType1138_143 d662_3
                                      hsTypeTpl61_65 = runStateT hsTypeTpl139_144 d662_3
                                      typ62_66 = runStateT typ140_145 d662_3
                                      variable63_67 = runStateT variable141_146 d662_3
                                      tvtail64_68 = runStateT tvtail142_147 d662_3
                                      integer65_69 = runStateT integer143_148 d662_3
                                      alpha66_70 = runStateT alpha144_149 d662_3
                                      upper67_71 = runStateT upper145_150 d662_3
                                      lower68_72 = runStateT lower146_151 d662_3
                                      digit69_73 = runStateT digit147_152 d662_3
                                      spaces70_74 = runStateT spaces148_153 d662_3
                                      space71_75 = runStateT space149_154 d662_3
                                      notNLString72_76 = runStateT notNLString150_155 d662_3
                                      newLine73_77 = runStateT newLine151_156 d662_3
                                      comment74_78 = runStateT comment152_157 d662_3
                                      comments75_79 = runStateT comments153_158 d662_3
                                      notComStr76_80 = runStateT notComStr154_159 d662_3
                                      comEnd77_81 = runStateT comEnd155_160 d662_3
                                      chars1671_82 = runStateT (case getToken s1670_2 of
                                                                    Just (c1666_161,
                                                                          s'1667_162) -> do put (parse1669_0 (updatePos c1666_161 pos1668_1) s'1667_162)
                                                                                            return c1666_161
                                                                    _ -> gets position >>= (throwError . mkParseError "" "end of input" "" undefined [])) d662_3
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
                                                d670_163 <- get
                                                t167_164 <- StateT char
                                                case t167_164 of
                                                    '|' -> return ()
                                                    _ -> gets position >>= (throwError . mkParseError "'|'" "not match pattern: " "" d670_163 ["char"])
                                                let '|' = t167_164
                                                return ()
                                                d671_165 <- get
                                                t168_166 <- StateT char
                                                case t168_166 of
                                                    ']' -> return ()
                                                    _ -> gets position >>= (throwError . mkParseError "']'" "not match pattern: " "" d671_165 ["char"])
                                                let ']' = t168_166
                                                return ()
                                                d672_167 <- get
                                                t169_168 <- StateT char
                                                case t169_168 of
                                                    '\n' -> return ()
                                                    _ -> gets position >>= (throwError . mkParseError "'\\n'" "not match pattern: " "" d672_167 ["char"])
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
                                                d680_169 <- get
                                                t177_170 <- StateT char
                                                case t177_170 of
                                                    '|' -> return ()
                                                    _ -> gets position >>= (throwError . mkParseError "'|'" "not match pattern: " "" d680_169 ["char"])
                                                let '|' = t177_170
                                                return ()
                                                d681_171 <- get
                                                t178_172 <- StateT char
                                                case t178_172 of
                                                    ']' -> return ()
                                                    _ -> gets position >>= (throwError . mkParseError "']'" "not match pattern: " "" d681_171 ["char"])
                                                let ']' = t178_172
                                                return ()
                                                d682_173 <- get
                                                t179_174 <- StateT char
                                                case t179_174 of
                                                    '\n' -> return ()
                                                    _ -> gets position >>= (throwError . mkParseError "'\\n'" "not match pattern: " "" d682_173 ["char"])
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
                pragma80_85 = foldl1 mplus [do d688_175 <- get
                                               t185_176 <- StateT char
                                               case t185_176 of
                                                   '{' -> return ()
                                                   _ -> gets position >>= (throwError . mkParseError "'{'" "not match pattern: " "" d688_175 ["char"])
                                               let '{' = t185_176
                                               return ()
                                               d689_177 <- get
                                               t186_178 <- StateT char
                                               case t186_178 of
                                                   '-' -> return ()
                                                   _ -> gets position >>= (throwError . mkParseError "'-'" "not match pattern: " "" d689_177 ["char"])
                                               let '-' = t186_178
                                               return ()
                                               d690_179 <- get
                                               t187_180 <- StateT char
                                               case t187_180 of
                                                   '#' -> return ()
                                                   _ -> gets position >>= (throwError . mkParseError "'#'" "not match pattern: " "" d690_179 ["char"])
                                               let '#' = t187_180
                                               return ()
                                               _ <- StateT spaces
                                               return ()
                                               d692_181 <- get
                                               t189_182 <- StateT char
                                               case t189_182 of
                                                   'L' -> return ()
                                                   _ -> gets position >>= (throwError . mkParseError "'L'" "not match pattern: " "" d692_181 ["char"])
                                               let 'L' = t189_182
                                               return ()
                                               d693_183 <- get
                                               t190_184 <- StateT char
                                               case t190_184 of
                                                   'A' -> return ()
                                                   _ -> gets position >>= (throwError . mkParseError "'A'" "not match pattern: " "" d693_183 ["char"])
                                               let 'A' = t190_184
                                               return ()
                                               d694_185 <- get
                                               t191_186 <- StateT char
                                               case t191_186 of
                                                   'N' -> return ()
                                                   _ -> gets position >>= (throwError . mkParseError "'N'" "not match pattern: " "" d694_185 ["char"])
                                               let 'N' = t191_186
                                               return ()
                                               d695_187 <- get
                                               t192_188 <- StateT char
                                               case t192_188 of
                                                   'G' -> return ()
                                                   _ -> gets position >>= (throwError . mkParseError "'G'" "not match pattern: " "" d695_187 ["char"])
                                               let 'G' = t192_188
                                               return ()
                                               d696_189 <- get
                                               t193_190 <- StateT char
                                               case t193_190 of
                                                   'U' -> return ()
                                                   _ -> gets position >>= (throwError . mkParseError "'U'" "not match pattern: " "" d696_189 ["char"])
                                               let 'U' = t193_190
                                               return ()
                                               d697_191 <- get
                                               t194_192 <- StateT char
                                               case t194_192 of
                                                   'A' -> return ()
                                                   _ -> gets position >>= (throwError . mkParseError "'A'" "not match pattern: " "" d697_191 ["char"])
                                               let 'A' = t194_192
                                               return ()
                                               d698_193 <- get
                                               t195_194 <- StateT char
                                               case t195_194 of
                                                   'G' -> return ()
                                                   _ -> gets position >>= (throwError . mkParseError "'G'" "not match pattern: " "" d698_193 ["char"])
                                               let 'G' = t195_194
                                               return ()
                                               d699_195 <- get
                                               t196_196 <- StateT char
                                               case t196_196 of
                                                   'E' -> return ()
                                                   _ -> gets position >>= (throwError . mkParseError "'E'" "not match pattern: " "" d699_195 ["char"])
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
                                            do d704_197 <- get
                                               t201_198 <- StateT char
                                               case t201_198 of
                                                   '{' -> return ()
                                                   _ -> gets position >>= (throwError . mkParseError "'{'" "not match pattern: " "" d704_197 ["char"])
                                               let '{' = t201_198
                                               return ()
                                               d705_199 <- get
                                               t202_200 <- StateT char
                                               case t202_200 of
                                                   '-' -> return ()
                                                   _ -> gets position >>= (throwError . mkParseError "'-'" "not match pattern: " "" d705_199 ["char"])
                                               let '-' = t202_200
                                               return ()
                                               d706_201 <- get
                                               t203_202 <- StateT char
                                               case t203_202 of
                                                   '#' -> return ()
                                                   _ -> gets position >>= (throwError . mkParseError "'#'" "not match pattern: " "" d706_201 ["char"])
                                               let '#' = t203_202
                                               return ()
                                               _ <- StateT spaces
                                               return ()
                                               s <- StateT pragmaStr
                                               _ <- StateT pragmaEnd
                                               return ()
                                               return (OtherPragma s)]
                pragmaStr81_86 = foldl1 mplus [do d711_203 <- get
                                                  do err1176_204 <- ((do _ <- StateT pragmaEnd
                                                                         return ()) >> return False) `catchError` const (return True)
                                                     unless err1176_204 (gets position >>= (throwError . mkParseError "!_:pragmaEnd" "not match: " "" d711_203 ["pragmaEnd"]))
                                                  put d711_203
                                                  c <- StateT char
                                                  s <- StateT pragmaStr
                                                  return (c : s),
                                               return ""]
                pragmaItems82_87 = foldl1 mplus [do t <- StateT typToken
                                                    d715_205 <- get
                                                    t211_206 <- StateT char
                                                    case t211_206 of
                                                        ',' -> return ()
                                                        _ -> gets position >>= (throwError . mkParseError "','" "not match pattern: " "" d715_205 ["char"])
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
                                                  d720_207 <- get
                                                  t216_208 <- StateT char
                                                  case t216_208 of
                                                      '#' -> return ()
                                                      _ -> gets position >>= (throwError . mkParseError "'#'" "not match pattern: " "" d720_207 ["char"])
                                                  let '#' = t216_208
                                                  return ()
                                                  d721_209 <- get
                                                  t217_210 <- StateT char
                                                  case t217_210 of
                                                      '-' -> return ()
                                                      _ -> gets position >>= (throwError . mkParseError "'-'" "not match pattern: " "" d721_209 ["char"])
                                                  let '-' = t217_210
                                                  return ()
                                                  d722_211 <- get
                                                  t218_212 <- StateT char
                                                  case t218_212 of
                                                      '}' -> return ()
                                                      _ -> gets position >>= (throwError . mkParseError "'}'" "not match pattern: " "" d722_211 ["char"])
                                                  let '}' = t218_212
                                                  return ()
                                                  return ()]
                moduleDec84_89 = foldl1 mplus [do d723_213 <- get
                                                  t219_214 <- StateT char
                                                  case t219_214 of
                                                      'm' -> return ()
                                                      _ -> gets position >>= (throwError . mkParseError "'m'" "not match pattern: " "" d723_213 ["char"])
                                                  let 'm' = t219_214
                                                  return ()
                                                  d724_215 <- get
                                                  t220_216 <- StateT char
                                                  case t220_216 of
                                                      'o' -> return ()
                                                      _ -> gets position >>= (throwError . mkParseError "'o'" "not match pattern: " "" d724_215 ["char"])
                                                  let 'o' = t220_216
                                                  return ()
                                                  d725_217 <- get
                                                  t221_218 <- StateT char
                                                  case t221_218 of
                                                      'd' -> return ()
                                                      _ -> gets position >>= (throwError . mkParseError "'d'" "not match pattern: " "" d725_217 ["char"])
                                                  let 'd' = t221_218
                                                  return ()
                                                  d726_219 <- get
                                                  t222_220 <- StateT char
                                                  case t222_220 of
                                                      'u' -> return ()
                                                      _ -> gets position >>= (throwError . mkParseError "'u'" "not match pattern: " "" d726_219 ["char"])
                                                  let 'u' = t222_220
                                                  return ()
                                                  d727_221 <- get
                                                  t223_222 <- StateT char
                                                  case t223_222 of
                                                      'l' -> return ()
                                                      _ -> gets position >>= (throwError . mkParseError "'l'" "not match pattern: " "" d727_221 ["char"])
                                                  let 'l' = t223_222
                                                  return ()
                                                  d728_223 <- get
                                                  t224_224 <- StateT char
                                                  case t224_224 of
                                                      'e' -> return ()
                                                      _ -> gets position >>= (throwError . mkParseError "'e'" "not match pattern: " "" d728_223 ["char"])
                                                  let 'e' = t224_224
                                                  return ()
                                                  _ <- StateT spaces
                                                  return ()
                                                  n <- StateT moduleName
                                                  _ <- StateT spaces
                                                  return ()
                                                  d732_225 <- get
                                                  t228_226 <- StateT char
                                                  case t228_226 of
                                                      '(' -> return ()
                                                      _ -> gets position >>= (throwError . mkParseError "'('" "not match pattern: " "" d732_225 ["char"])
                                                  let '(' = t228_226
                                                  return ()
                                                  _ <- StateT spaces
                                                  return ()
                                                  s <- StateT moduleDecStr
                                                  _ <- StateT whr
                                                  return ()
                                                  return (Just (n, Just s)),
                                               do d736_227 <- get
                                                  t232_228 <- StateT char
                                                  case t232_228 of
                                                      'm' -> return ()
                                                      _ -> gets position >>= (throwError . mkParseError "'m'" "not match pattern: " "" d736_227 ["char"])
                                                  let 'm' = t232_228
                                                  return ()
                                                  d737_229 <- get
                                                  t233_230 <- StateT char
                                                  case t233_230 of
                                                      'o' -> return ()
                                                      _ -> gets position >>= (throwError . mkParseError "'o'" "not match pattern: " "" d737_229 ["char"])
                                                  let 'o' = t233_230
                                                  return ()
                                                  d738_231 <- get
                                                  t234_232 <- StateT char
                                                  case t234_232 of
                                                      'd' -> return ()
                                                      _ -> gets position >>= (throwError . mkParseError "'d'" "not match pattern: " "" d738_231 ["char"])
                                                  let 'd' = t234_232
                                                  return ()
                                                  d739_233 <- get
                                                  t235_234 <- StateT char
                                                  case t235_234 of
                                                      'u' -> return ()
                                                      _ -> gets position >>= (throwError . mkParseError "'u'" "not match pattern: " "" d739_233 ["char"])
                                                  let 'u' = t235_234
                                                  return ()
                                                  d740_235 <- get
                                                  t236_236 <- StateT char
                                                  case t236_236 of
                                                      'l' -> return ()
                                                      _ -> gets position >>= (throwError . mkParseError "'l'" "not match pattern: " "" d740_235 ["char"])
                                                  let 'l' = t236_236
                                                  return ()
                                                  d741_237 <- get
                                                  t237_238 <- StateT char
                                                  case t237_238 of
                                                      'e' -> return ()
                                                      _ -> gets position >>= (throwError . mkParseError "'e'" "not match pattern: " "" d741_237 ["char"])
                                                  let 'e' = t237_238
                                                  return ()
                                                  _ <- StateT spaces
                                                  return ()
                                                  n <- StateT moduleName
                                                  _ <- StateT spaces
                                                  return ()
                                                  d745_239 <- get
                                                  t241_240 <- StateT char
                                                  case t241_240 of
                                                      'w' -> return ()
                                                      _ -> gets position >>= (throwError . mkParseError "'w'" "not match pattern: " "" d745_239 ["char"])
                                                  let 'w' = t241_240
                                                  return ()
                                                  d746_241 <- get
                                                  t242_242 <- StateT char
                                                  case t242_242 of
                                                      'h' -> return ()
                                                      _ -> gets position >>= (throwError . mkParseError "'h'" "not match pattern: " "" d746_241 ["char"])
                                                  let 'h' = t242_242
                                                  return ()
                                                  d747_243 <- get
                                                  t243_244 <- StateT char
                                                  case t243_244 of
                                                      'e' -> return ()
                                                      _ -> gets position >>= (throwError . mkParseError "'e'" "not match pattern: " "" d747_243 ["char"])
                                                  let 'e' = t243_244
                                                  return ()
                                                  d748_245 <- get
                                                  t244_246 <- StateT char
                                                  case t244_246 of
                                                      'r' -> return ()
                                                      _ -> gets position >>= (throwError . mkParseError "'r'" "not match pattern: " "" d748_245 ["char"])
                                                  let 'r' = t244_246
                                                  return ()
                                                  d749_247 <- get
                                                  t245_248 <- StateT char
                                                  case t245_248 of
                                                      'e' -> return ()
                                                      _ -> gets position >>= (throwError . mkParseError "'e'" "not match pattern: " "" d749_247 ["char"])
                                                  let 'e' = t245_248
                                                  return ()
                                                  _ <- StateT spaces
                                                  return ()
                                                  return (Just (n, Nothing)),
                                               return Nothing]
                moduleName85_90 = foldl1 mplus [do t <- StateT typ
                                                   d752_249 <- get
                                                   t248_250 <- StateT char
                                                   case t248_250 of
                                                       '.' -> return ()
                                                       _ -> gets position >>= (throwError . mkParseError "'.'" "not match pattern: " "" d752_249 ["char"])
                                                   let '.' = t248_250
                                                   return ()
                                                   n <- StateT moduleName
                                                   return (t : n),
                                                do t <- StateT typ
                                                   return [t]]
                moduleDecStr86_91 = foldl1 mplus [do d756_251 <- get
                                                     do err1177_252 <- ((do _ <- StateT whr
                                                                            return ()) >> return False) `catchError` const (return True)
                                                        unless err1177_252 (gets position >>= (throwError . mkParseError "!_:whr" "not match: " "" d756_251 ["whr"]))
                                                     put d756_251
                                                     c <- StateT char
                                                     s <- StateT moduleDecStr
                                                     return (c : s),
                                                  return ""]
                whr87_92 = foldl1 mplus [do _ <- StateT spaces
                                            return ()
                                            d760_253 <- get
                                            t255_254 <- StateT char
                                            case t255_254 of
                                                ')' -> return ()
                                                _ -> gets position >>= (throwError . mkParseError "')'" "not match pattern: " "" d760_253 ["char"])
                                            let ')' = t255_254
                                            return ()
                                            _ <- StateT spaces
                                            return ()
                                            d762_255 <- get
                                            t257_256 <- StateT char
                                            case t257_256 of
                                                'w' -> return ()
                                                _ -> gets position >>= (throwError . mkParseError "'w'" "not match pattern: " "" d762_255 ["char"])
                                            let 'w' = t257_256
                                            return ()
                                            d763_257 <- get
                                            t258_258 <- StateT char
                                            case t258_258 of
                                                'h' -> return ()
                                                _ -> gets position >>= (throwError . mkParseError "'h'" "not match pattern: " "" d763_257 ["char"])
                                            let 'h' = t258_258
                                            return ()
                                            d764_259 <- get
                                            t259_260 <- StateT char
                                            case t259_260 of
                                                'e' -> return ()
                                                _ -> gets position >>= (throwError . mkParseError "'e'" "not match pattern: " "" d764_259 ["char"])
                                            let 'e' = t259_260
                                            return ()
                                            d765_261 <- get
                                            t260_262 <- StateT char
                                            case t260_262 of
                                                'r' -> return ()
                                                _ -> gets position >>= (throwError . mkParseError "'r'" "not match pattern: " "" d765_261 ["char"])
                                            let 'r' = t260_262
                                            return ()
                                            d766_263 <- get
                                            t261_264 <- StateT char
                                            case t261_264 of
                                                'e' -> return ()
                                                _ -> gets position >>= (throwError . mkParseError "'e'" "not match pattern: " "" d766_263 ["char"])
                                            let 'e' = t261_264
                                            return ()
                                            return ()]
                preImpPap88_93 = foldl1 mplus [do d768_265 <- get
                                                  do err1178_266 <- ((do _ <- StateT importPapillon
                                                                         return ()) >> return False) `catchError` const (return True)
                                                     unless err1178_266 (gets position >>= (throwError . mkParseError "!_:importPapillon" "not match: " "" d768_265 ["importPapillon"]))
                                                  put d768_265
                                                  d770_267 <- get
                                                  do err1179_268 <- ((do _ <- StateT pap
                                                                         return ()) >> return False) `catchError` const (return True)
                                                     unless err1179_268 (gets position >>= (throwError . mkParseError "!_:pap" "not match: " "" d770_267 ["pap"]))
                                                  put d770_267
                                                  c <- StateT char
                                                  pip <- StateT preImpPap
                                                  return (c : pip),
                                               return ""]
                prePeg89_94 = foldl1 mplus [do d774_269 <- get
                                               do err1180_270 <- ((do _ <- StateT pap
                                                                      return ()) >> return False) `catchError` const (return True)
                                                  unless err1180_270 (gets position >>= (throwError . mkParseError "!_:pap" "not match: " "" d774_269 ["pap"]))
                                               put d774_269
                                               c <- StateT char
                                               pp <- StateT prePeg
                                               return (c : pp),
                                            return ""]
                afterPeg90_95 = foldl1 mplus [do c <- StateT char
                                                 atp <- StateT afterPeg
                                                 return (c : atp),
                                              return ""]
                importPapillon91_96 = foldl1 mplus [do d779_271 <- get
                                                       t271_272 <- StateT varToken
                                                       case t271_272 of
                                                           "import" -> return ()
                                                           _ -> gets position >>= (throwError . mkParseError "\"import\"" "not match pattern: " "" d779_271 ["varToken"])
                                                       let "import" = t271_272
                                                       return ()
                                                       d780_273 <- get
                                                       t272_274 <- StateT typToken
                                                       case t272_274 of
                                                           "Text" -> return ()
                                                           _ -> gets position >>= (throwError . mkParseError "\"Text\"" "not match pattern: " "" d780_273 ["typToken"])
                                                       let "Text" = t272_274
                                                       return ()
                                                       d781_275 <- get
                                                       t273_276 <- StateT char
                                                       case t273_276 of
                                                           '.' -> return ()
                                                           _ -> gets position >>= (throwError . mkParseError "'.'" "not match pattern: " "" d781_275 ["char"])
                                                       let '.' = t273_276
                                                       return ()
                                                       _ <- StateT spaces
                                                       return ()
                                                       d783_277 <- get
                                                       t275_278 <- StateT typToken
                                                       case t275_278 of
                                                           "Papillon" -> return ()
                                                           _ -> gets position >>= (throwError . mkParseError "\"Papillon\"" "not match pattern: " "" d783_277 ["typToken"])
                                                       let "Papillon" = t275_278
                                                       return ()
                                                       d785_279 <- get
                                                       do err1181_280 <- ((do d784_281 <- get
                                                                              t276_282 <- StateT char
                                                                              case t276_282 of
                                                                                  '.' -> return ()
                                                                                  _ -> gets position >>= (throwError . mkParseError "'.'" "not match pattern: " "" d784_281 ["char"])
                                                                              let '.' = t276_282
                                                                              return ()) >> return False) `catchError` const (return True)
                                                          unless err1181_280 (gets position >>= (throwError . mkParseError "!'.':" "not match: " "" d785_279 ["char"]))
                                                       put d785_279
                                                       return ()]
                varToken92_97 = foldl1 mplus [do v <- StateT variable
                                                 _ <- StateT spaces
                                                 return ()
                                                 return v]
                typToken93_98 = foldl1 mplus [do t <- StateT typ
                                                 _ <- StateT spaces
                                                 return ()
                                                 return t]
                pap94_99 = foldl1 mplus [do d790_283 <- get
                                            t281_284 <- StateT char
                                            case t281_284 of
                                                '\n' -> return ()
                                                _ -> gets position >>= (throwError . mkParseError "'\\n'" "not match pattern: " "" d790_283 ["char"])
                                            let '\n' = t281_284
                                            return ()
                                            d791_285 <- get
                                            t282_286 <- StateT char
                                            case t282_286 of
                                                '[' -> return ()
                                                _ -> gets position >>= (throwError . mkParseError "'['" "not match pattern: " "" d791_285 ["char"])
                                            let '[' = t282_286
                                            return ()
                                            d792_287 <- get
                                            t283_288 <- StateT char
                                            case t283_288 of
                                                'p' -> return ()
                                                _ -> gets position >>= (throwError . mkParseError "'p'" "not match pattern: " "" d792_287 ["char"])
                                            let 'p' = t283_288
                                            return ()
                                            d793_289 <- get
                                            t284_290 <- StateT char
                                            case t284_290 of
                                                'a' -> return ()
                                                _ -> gets position >>= (throwError . mkParseError "'a'" "not match pattern: " "" d793_289 ["char"])
                                            let 'a' = t284_290
                                            return ()
                                            d794_291 <- get
                                            t285_292 <- StateT char
                                            case t285_292 of
                                                'p' -> return ()
                                                _ -> gets position >>= (throwError . mkParseError "'p'" "not match pattern: " "" d794_291 ["char"])
                                            let 'p' = t285_292
                                            return ()
                                            d795_293 <- get
                                            t286_294 <- StateT char
                                            case t286_294 of
                                                'i' -> return ()
                                                _ -> gets position >>= (throwError . mkParseError "'i'" "not match pattern: " "" d795_293 ["char"])
                                            let 'i' = t286_294
                                            return ()
                                            d796_295 <- get
                                            t287_296 <- StateT char
                                            case t287_296 of
                                                'l' -> return ()
                                                _ -> gets position >>= (throwError . mkParseError "'l'" "not match pattern: " "" d796_295 ["char"])
                                            let 'l' = t287_296
                                            return ()
                                            d797_297 <- get
                                            t288_298 <- StateT char
                                            case t288_298 of
                                                'l' -> return ()
                                                _ -> gets position >>= (throwError . mkParseError "'l'" "not match pattern: " "" d797_297 ["char"])
                                            let 'l' = t288_298
                                            return ()
                                            d798_299 <- get
                                            t289_300 <- StateT char
                                            case t289_300 of
                                                'o' -> return ()
                                                _ -> gets position >>= (throwError . mkParseError "'o'" "not match pattern: " "" d798_299 ["char"])
                                            let 'o' = t289_300
                                            return ()
                                            d799_301 <- get
                                            t290_302 <- StateT char
                                            case t290_302 of
                                                'n' -> return ()
                                                _ -> gets position >>= (throwError . mkParseError "'n'" "not match pattern: " "" d799_301 ["char"])
                                            let 'n' = t290_302
                                            return ()
                                            d800_303 <- get
                                            t291_304 <- StateT char
                                            case t291_304 of
                                                '|' -> return ()
                                                _ -> gets position >>= (throwError . mkParseError "'|'" "not match pattern: " "" d800_303 ["char"])
                                            let '|' = t291_304
                                            return ()
                                            d801_305 <- get
                                            t292_306 <- StateT char
                                            case t292_306 of
                                                '\n' -> return ()
                                                _ -> gets position >>= (throwError . mkParseError "'\\n'" "not match pattern: " "" d801_305 ["char"])
                                            let '\n' = t292_306
                                            return ()
                                            return ()]
                peg95_100 = foldl1 mplus [do mt <- optional156_307 (StateT monadType)
                                             _ <- StateT spaces
                                             return ()
                                             s <- StateT sourceType
                                             p <- StateT peg_
                                             return (mt, ConT $ mkName s, p),
                                          do mt <- optional156_307 (StateT monadType)
                                             p <- StateT peg_
                                             return (mt, ConT $ mkName "String", p)]
                monadType96_101 = foldl1 mplus [do _ <- StateT spaces
                                                   return ()
                                                   d809_308 <- get
                                                   t300_309 <- StateT varToken
                                                   case t300_309 of
                                                       "monad" -> return ()
                                                       _ -> gets position >>= (throwError . mkParseError "\"monad\"" "not match pattern: " "" d809_308 ["varToken"])
                                                   let "monad" = t300_309
                                                   return ()
                                                   d810_310 <- get
                                                   t301_311 <- StateT char
                                                   case t301_311 of
                                                       ':' -> return ()
                                                       _ -> gets position >>= (throwError . mkParseError "':'" "not match pattern: " "" d810_310 ["char"])
                                                   let ':' = t301_311
                                                   return ()
                                                   _ <- StateT spaces
                                                   return ()
                                                   t <- StateT hsTypeArr
                                                   return t]
                sourceType97_102 = foldl1 mplus [do d813_312 <- get
                                                    t304_313 <- StateT varToken
                                                    case t304_313 of
                                                        "source" -> return ()
                                                        _ -> gets position >>= (throwError . mkParseError "\"source\"" "not match pattern: " "" d813_312 ["varToken"])
                                                    let "source" = t304_313
                                                    return ()
                                                    d814_314 <- get
                                                    t305_315 <- StateT char
                                                    case t305_315 of
                                                        ':' -> return ()
                                                        _ -> gets position >>= (throwError . mkParseError "':'" "not match pattern: " "" d814_314 ["char"])
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
                                                    d822_316 <- get
                                                    t313_317 <- StateT char
                                                    case t313_317 of
                                                        ':' -> return ()
                                                        _ -> gets position >>= (throwError . mkParseError "':'" "not match pattern: " "" d822_316 ["char"])
                                                    let ':' = t313_317
                                                    return ()
                                                    d823_318 <- get
                                                    t314_319 <- StateT char
                                                    case t314_319 of
                                                        ':' -> return ()
                                                        _ -> gets position >>= (throwError . mkParseError "':'" "not match pattern: " "" d823_318 ["char"])
                                                    let ':' = t314_319
                                                    return ()
                                                    _ <- StateT spaces
                                                    return ()
                                                    t <- StateT hsTypeArr
                                                    _ <- StateT spaces
                                                    return ()
                                                    d827_320 <- get
                                                    t318_321 <- StateT char
                                                    case t318_321 of
                                                        '=' -> return ()
                                                        _ -> gets position >>= (throwError . mkParseError "'='" "not match pattern: " "" d827_320 ["char"])
                                                    let '=' = t318_321
                                                    return ()
                                                    _ <- StateT spaces
                                                    return ()
                                                    sel <- StateT selection
                                                    _ <- StateT spaces
                                                    return ()
                                                    d831_322 <- get
                                                    t322_323 <- StateT char
                                                    case t322_323 of
                                                        ';' -> return ()
                                                        _ -> gets position >>= (throwError . mkParseError "';'" "not match pattern: " "" d831_322 ["char"])
                                                    let ';' = t322_323
                                                    return ()
                                                    return (v, Just t, sel),
                                                 do v <- StateT variable
                                                    _ <- StateT spaces
                                                    return ()
                                                    d834_324 <- get
                                                    t325_325 <- StateT char
                                                    case t325_325 of
                                                        '<' -> return ()
                                                        _ -> gets position >>= (throwError . mkParseError "'<'" "not match pattern: " "" d834_324 ["char"])
                                                    let '<' = t325_325
                                                    return ()
                                                    d835_326 <- get
                                                    t326_327 <- StateT char
                                                    case t326_327 of
                                                        '-' -> return ()
                                                        _ -> gets position >>= (throwError . mkParseError "'-'" "not match pattern: " "" d835_326 ["char"])
                                                    let '-' = t326_327
                                                    return ()
                                                    _ <- StateT spaces
                                                    return ()
                                                    sel <- StateT plainSelection
                                                    _ <- StateT spaces
                                                    return ()
                                                    d839_328 <- get
                                                    t330_329 <- StateT char
                                                    case t330_329 of
                                                        ';' -> return ()
                                                        _ -> gets position >>= (throwError . mkParseError "';'" "not match pattern: " "" d839_328 ["char"])
                                                    let ';' = t330_329
                                                    return ()
                                                    return (v, Nothing, Right sel),
                                                 do v <- StateT variable
                                                    _ <- StateT spaces
                                                    return ()
                                                    d842_330 <- get
                                                    t333_331 <- StateT char
                                                    case t333_331 of
                                                        '=' -> return ()
                                                        _ -> gets position >>= (throwError . mkParseError "'='" "not match pattern: " "" d842_330 ["char"])
                                                    let '=' = t333_331
                                                    return ()
                                                    _ <- StateT spaces
                                                    return ()
                                                    sel <- StateT plainSelection
                                                    _ <- StateT spaces
                                                    return ()
                                                    d846_332 <- get
                                                    t337_333 <- StateT char
                                                    case t337_333 of
                                                        ';' -> return ()
                                                        _ -> gets position >>= (throwError . mkParseError "';'" "not match pattern: " "" d846_332 ["char"])
                                                    let ';' = t337_333
                                                    return ()
                                                    return (v, Nothing, Right sel)]
                selection100_105 = foldl1 mplus [do s <- StateT normalSelection
                                                    return (Left s),
                                                 do s <- StateT plainSelection
                                                    return (Right s)]
                normalSelection101_106 = foldl1 mplus [do ex <- StateT expressionHs
                                                          _ <- StateT spaces
                                                          return ()
                                                          d851_334 <- get
                                                          t342_335 <- StateT char
                                                          case t342_335 of
                                                              '/' -> return ()
                                                              _ -> gets position >>= (throwError . mkParseError "'/'" "not match pattern: " "" d851_334 ["char"])
                                                          let '/' = t342_335
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
                                                         d857_336 <- get
                                                         t348_337 <- StateT char
                                                         case t348_337 of
                                                             '/' -> return ()
                                                             _ -> gets position >>= (throwError . mkParseError "'/'" "not match pattern: " "" d857_336 ["char"])
                                                         let '/' = t348_337
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
                                                       d863_338 <- get
                                                       t354_339 <- StateT char
                                                       case t354_339 of
                                                           '{' -> return ()
                                                           _ -> gets position >>= (throwError . mkParseError "'{'" "not match pattern: " "" d863_338 ["char"])
                                                       let '{' = t354_339
                                                       return ()
                                                       _ <- StateT spaces
                                                       return ()
                                                       h <- StateT hsExpLam
                                                       _ <- StateT spaces
                                                       return ()
                                                       d867_340 <- get
                                                       t358_341 <- StateT char
                                                       case t358_341 of
                                                           '}' -> return ()
                                                           _ -> gets position >>= (throwError . mkParseError "'}'" "not match pattern: " "" d867_340 ["char"])
                                                       let '}' = t358_341
                                                       return ()
                                                       return (expressionQ (e, h)),
                                                    do e <- StateT expressionHsSugar
                                                       return e]
                expressionHsSugar104_109 = foldl1 mplus [do d869_342 <- get
                                                            t360_343 <- StateT char
                                                            case t360_343 of
                                                                '<' -> return ()
                                                                _ -> gets position >>= (throwError . mkParseError "'<'" "not match pattern: " "" d869_342 ["char"])
                                                            let '<' = t360_343
                                                            return ()
                                                            _ <- StateT spaces
                                                            return ()
                                                            h <- StateT hsExpLam
                                                            _ <- StateT spaces
                                                            return ()
                                                            d873_344 <- get
                                                            t364_345 <- StateT char
                                                            case t364_345 of
                                                                '>' -> return ()
                                                                _ -> gets position >>= (throwError . mkParseError "'>'" "not match pattern: " "" d873_344 ["char"])
                                                            let '>' = t364_345
                                                            return ()
                                                            return (expressionSugar h)]
                plainExpressionHs105_110 = foldl1 mplus [do rfs <- list157_346 (foldl1 mplus [do rf <- StateT plainHAReadFromLs
                                                                                                 _ <- StateT spaces
                                                                                                 return ()
                                                                                                 return rf])
                                                            return (plainExpressionQ rfs)]
                plainHAReadFromLs106_111 = foldl1 mplus [do rf <- StateT plainReadFromLs
                                                            return (Here, rf),
                                                         do d878_347 <- get
                                                            t369_348 <- StateT char
                                                            case t369_348 of
                                                                '&' -> return ()
                                                                _ -> gets position >>= (throwError . mkParseError "'&'" "not match pattern: " "" d878_347 ["char"])
                                                            let '&' = t369_348
                                                            return ()
                                                            rf <- StateT plainReadFromLs
                                                            return (Ahead, rf),
                                                         do d880_349 <- get
                                                            t371_350 <- StateT char
                                                            case t371_350 of
                                                                '!' -> return ()
                                                                _ -> gets position >>= (throwError . mkParseError "'!'" "not match pattern: " "" d880_349 ["char"])
                                                            let '!' = t371_350
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
                nameLeaf_109_114 = foldl1 mplus [do d887_351 <- get
                                                    t378_352 <- StateT char
                                                    case t378_352 of
                                                        '!' -> return ()
                                                        _ -> gets position >>= (throwError . mkParseError "'!'" "not match pattern: " "" d887_351 ["char"])
                                                    let '!' = t378_352
                                                    return ()
                                                    nl <- StateT nameLeafNoCom
                                                    _ <- StateT spaces
                                                    return ()
                                                    com <- optional156_307 (StateT comForErr)
                                                    return (NAhead $ maybe "" id com, nl),
                                                 do d891_353 <- get
                                                    t382_354 <- StateT char
                                                    case t382_354 of
                                                        '&' -> return ()
                                                        _ -> gets position >>= (throwError . mkParseError "'&'" "not match pattern: " "" d891_353 ["char"])
                                                    let '&' = t382_354
                                                    return ()
                                                    nl <- StateT nameLeaf
                                                    return (Ahead, nl),
                                                 do nl <- StateT nameLeaf
                                                    return (Here, nl)]
                nameLeaf110_115 = foldl1 mplus [do n <- StateT pat1
                                                   _ <- StateT spaces
                                                   return ()
                                                   com <- optional156_307 (StateT comForErr)
                                                   d897_355 <- get
                                                   t388_356 <- StateT char
                                                   case t388_356 of
                                                       ':' -> return ()
                                                       _ -> gets position >>= (throwError . mkParseError "':'" "not match pattern: " "" d897_355 ["char"])
                                                   let ':' = t388_356
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
                                                        d905_357 <- get
                                                        t396_358 <- StateT char
                                                        case t396_358 of
                                                            ':' -> return ()
                                                            _ -> gets position >>= (throwError . mkParseError "':'" "not match pattern: " "" d905_357 ["char"])
                                                        let ':' = t396_358
                                                        return ()
                                                        (rf, p) <- StateT leaf
                                                        return (check (n, maybe "" id com) rf p),
                                                     do n <- StateT pat1
                                                        _ <- StateT spaces
                                                        return ()
                                                        return (check (n,
                                                                       "") (FromVariable Nothing) Nothing)]
                comForErr112_117 = foldl1 mplus [do d909_359 <- get
                                                    t400_360 <- StateT char
                                                    case t400_360 of
                                                        '{' -> return ()
                                                        _ -> gets position >>= (throwError . mkParseError "'{'" "not match pattern: " "" d909_359 ["char"])
                                                    let '{' = t400_360
                                                    return ()
                                                    d910_361 <- get
                                                    t401_362 <- StateT char
                                                    case t401_362 of
                                                        '-' -> return ()
                                                        _ -> gets position >>= (throwError . mkParseError "'-'" "not match pattern: " "" d910_361 ["char"])
                                                    let '-' = t401_362
                                                    return ()
                                                    d911_363 <- get
                                                    t402_364 <- StateT char
                                                    case t402_364 of
                                                        '#' -> return ()
                                                        _ -> gets position >>= (throwError . mkParseError "'#'" "not match pattern: " "" d911_363 ["char"])
                                                    let '#' = t402_364
                                                    return ()
                                                    _ <- StateT spaces
                                                    return ()
                                                    d913_365 <- get
                                                    t404_366 <- StateT char
                                                    case t404_366 of
                                                        '"' -> return ()
                                                        _ -> gets position >>= (throwError . mkParseError "'\"'" "not match pattern: " "" d913_365 ["char"])
                                                    let '"' = t404_366
                                                    return ()
                                                    s <- StateT stringLit
                                                    d915_367 <- get
                                                    t406_368 <- StateT char
                                                    case t406_368 of
                                                        '"' -> return ()
                                                        _ -> gets position >>= (throwError . mkParseError "'\"'" "not match pattern: " "" d915_367 ["char"])
                                                    let '"' = t406_368
                                                    return ()
                                                    _ <- StateT spaces
                                                    return ()
                                                    d917_369 <- get
                                                    t408_370 <- StateT char
                                                    case t408_370 of
                                                        '#' -> return ()
                                                        _ -> gets position >>= (throwError . mkParseError "'#'" "not match pattern: " "" d917_369 ["char"])
                                                    let '#' = t408_370
                                                    return ()
                                                    d918_371 <- get
                                                    t409_372 <- StateT char
                                                    case t409_372 of
                                                        '-' -> return ()
                                                        _ -> gets position >>= (throwError . mkParseError "'-'" "not match pattern: " "" d918_371 ["char"])
                                                    let '-' = t409_372
                                                    return ()
                                                    d919_373 <- get
                                                    t410_374 <- StateT char
                                                    case t410_374 of
                                                        '}' -> return ()
                                                        _ -> gets position >>= (throwError . mkParseError "'}'" "not match pattern: " "" d919_373 ["char"])
                                                    let '}' = t410_374
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
                                                d930_375 <- get
                                                t421_376 <- StateT char
                                                case t421_376 of
                                                    '`' -> return ()
                                                    _ -> gets position >>= (throwError . mkParseError "'`'" "not match pattern: " "" d930_375 ["char"])
                                                let '`' = t421_376
                                                return ()
                                                t <- StateT typ
                                                d932_377 <- get
                                                t423_378 <- StateT char
                                                case t423_378 of
                                                    '`' -> return ()
                                                    _ -> gets position >>= (throwError . mkParseError "'`'" "not match pattern: " "" d932_377 ["char"])
                                                let '`' = t423_378
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
                                           do d939_379 <- get
                                              t430_380 <- StateT char
                                              case t430_380 of
                                                  '(' -> return ()
                                                  _ -> gets position >>= (throwError . mkParseError "'('" "not match pattern: " "" d939_379 ["char"])
                                              let '(' = t430_380
                                              return ()
                                              o <- StateT opConName
                                              d941_381 <- get
                                              t432_382 <- StateT char
                                              case t432_382 of
                                                  ')' -> return ()
                                                  _ -> gets position >>= (throwError . mkParseError "')'" "not match pattern: " "" d941_381 ["char"])
                                              let ')' = t432_382
                                              return ()
                                              _ <- StateT spaces
                                              return ()
                                              ps <- StateT pats
                                              return (ConP o ps),
                                           do p <- StateT pat1
                                              return p]
                pat1116_121 = foldl1 mplus [do t <- StateT typ
                                               return (ConP (mkName t) []),
                                            do d946_383 <- get
                                               t437_384 <- StateT variable
                                               case t437_384 of
                                                   "_" -> return ()
                                                   _ -> gets position >>= (throwError . mkParseError "\"_\"" "not match pattern: " "" d946_383 ["variable"])
                                               let "_" = t437_384
                                               return ()
                                               return WildP,
                                            do n <- StateT variable
                                               return (VarP $ mkName n),
                                            do i <- StateT integer
                                               return (LitP (IntegerL i)),
                                            do d949_385 <- get
                                               t440_386 <- StateT char
                                               case t440_386 of
                                                   '-' -> return ()
                                                   _ -> gets position >>= (throwError . mkParseError "'-'" "not match pattern: " "" d949_385 ["char"])
                                               let '-' = t440_386
                                               return ()
                                               _ <- StateT spaces
                                               return ()
                                               i <- StateT integer
                                               return (LitP (IntegerL $ negate i)),
                                            do d952_387 <- get
                                               t443_388 <- StateT char
                                               case t443_388 of
                                                   '\'' -> return ()
                                                   _ -> gets position >>= (throwError . mkParseError "'\\''" "not match pattern: " "" d952_387 ["char"])
                                               let '\'' = t443_388
                                               return ()
                                               c <- StateT charLit
                                               d954_389 <- get
                                               t445_390 <- StateT char
                                               case t445_390 of
                                                   '\'' -> return ()
                                                   _ -> gets position >>= (throwError . mkParseError "'\\''" "not match pattern: " "" d954_389 ["char"])
                                               let '\'' = t445_390
                                               return ()
                                               return (LitP $ CharL c),
                                            do d955_391 <- get
                                               t446_392 <- StateT char
                                               case t446_392 of
                                                   '"' -> return ()
                                                   _ -> gets position >>= (throwError . mkParseError "'\"'" "not match pattern: " "" d955_391 ["char"])
                                               let '"' = t446_392
                                               return ()
                                               s <- StateT stringLit
                                               d957_393 <- get
                                               t448_394 <- StateT char
                                               case t448_394 of
                                                   '"' -> return ()
                                                   _ -> gets position >>= (throwError . mkParseError "'\"'" "not match pattern: " "" d957_393 ["char"])
                                               let '"' = t448_394
                                               return ()
                                               return (LitP $ StringL s),
                                            do d958_395 <- get
                                               t449_396 <- StateT char
                                               case t449_396 of
                                                   '(' -> return ()
                                                   _ -> gets position >>= (throwError . mkParseError "'('" "not match pattern: " "" d958_395 ["char"])
                                               let '(' = t449_396
                                               return ()
                                               p <- StateT patList
                                               d960_397 <- get
                                               t451_398 <- StateT char
                                               case t451_398 of
                                                   ')' -> return ()
                                                   _ -> gets position >>= (throwError . mkParseError "')'" "not match pattern: " "" d960_397 ["char"])
                                               let ')' = t451_398
                                               return ()
                                               return (TupP p),
                                            do d961_399 <- get
                                               t452_400 <- StateT char
                                               case t452_400 of
                                                   '[' -> return ()
                                                   _ -> gets position >>= (throwError . mkParseError "'['" "not match pattern: " "" d961_399 ["char"])
                                               let '[' = t452_400
                                               return ()
                                               p <- StateT patList
                                               d963_401 <- get
                                               t454_402 <- StateT char
                                               case t454_402 of
                                                   ']' -> return ()
                                                   _ -> gets position >>= (throwError . mkParseError "']'" "not match pattern: " "" d963_401 ["char"])
                                               let ']' = t454_402
                                               return ()
                                               return (ListP p)]
                patList117_122 = foldl1 mplus [do p <- StateT patOp
                                                  _ <- StateT spaces
                                                  return ()
                                                  d966_403 <- get
                                                  t457_404 <- StateT char
                                                  case t457_404 of
                                                      ',' -> return ()
                                                      _ -> gets position >>= (throwError . mkParseError "','" "not match pattern: " "" d966_403 ["char"])
                                                  let ',' = t457_404
                                                  return ()
                                                  _ <- StateT spaces
                                                  return ()
                                                  ps <- StateT patList
                                                  return (p : ps),
                                               do p <- StateT patOp
                                                  return [p],
                                               return []]
                opConName118_123 = foldl1 mplus [do d970_405 <- get
                                                    t461_406 <- StateT char
                                                    case t461_406 of
                                                        ':' -> return ()
                                                        _ -> gets position >>= (throwError . mkParseError "':'" "not match pattern: " "" d970_405 ["char"])
                                                    let ':' = t461_406
                                                    return ()
                                                    ot <- StateT opTail
                                                    return (mkName $ ':' : ot)]
                charLit119_124 = foldl1 mplus [do d972_407 <- get
                                                  t463_408 <- StateT char
                                                  let c = t463_408
                                                  b1491_409 <- return (c `notElem` "\\'")
                                                  unless b1491_409 (gets position >>= (throwError . mkParseError "c `notElem` \"\\\\'\"" "not match: " "" d972_407 ["char"]))
                                                  return c,
                                               do d973_410 <- get
                                                  t464_411 <- StateT char
                                                  case t464_411 of
                                                      '\\' -> return ()
                                                      _ -> gets position >>= (throwError . mkParseError "'\\\\'" "not match pattern: " "" d973_410 ["char"])
                                                  let '\\' = t464_411
                                                  return ()
                                                  c <- StateT escapeC
                                                  return c]
                stringLit120_125 = foldl1 mplus [do d975_412 <- get
                                                    t466_413 <- StateT char
                                                    let c = t466_413
                                                    b1494_414 <- return (c `notElem` "\"\\")
                                                    unless b1494_414 (gets position >>= (throwError . mkParseError "c `notElem` \"\\\"\\\\\"" "not match: " "" d975_412 ["char"]))
                                                    s <- StateT stringLit
                                                    return (c : s),
                                                 do d977_415 <- get
                                                    t468_416 <- StateT char
                                                    case t468_416 of
                                                        '\\' -> return ()
                                                        _ -> gets position >>= (throwError . mkParseError "'\\\\'" "not match pattern: " "" d977_415 ["char"])
                                                    let '\\' = t468_416
                                                    return ()
                                                    c <- StateT escapeC
                                                    s <- StateT stringLit
                                                    return (c : s),
                                                 return ""]
                escapeC121_126 = foldl1 mplus [do d980_417 <- get
                                                  t471_418 <- StateT char
                                                  case t471_418 of
                                                      '"' -> return ()
                                                      _ -> gets position >>= (throwError . mkParseError "'\"'" "not match pattern: " "" d980_417 ["char"])
                                                  let '"' = t471_418
                                                  return ()
                                                  return '"',
                                               do d981_419 <- get
                                                  t472_420 <- StateT char
                                                  case t472_420 of
                                                      '\'' -> return ()
                                                      _ -> gets position >>= (throwError . mkParseError "'\\''" "not match pattern: " "" d981_419 ["char"])
                                                  let '\'' = t472_420
                                                  return ()
                                                  return '\'',
                                               do d982_421 <- get
                                                  t473_422 <- StateT char
                                                  case t473_422 of
                                                      '\\' -> return ()
                                                      _ -> gets position >>= (throwError . mkParseError "'\\\\'" "not match pattern: " "" d982_421 ["char"])
                                                  let '\\' = t473_422
                                                  return ()
                                                  return '\\',
                                               do d983_423 <- get
                                                  t474_424 <- StateT char
                                                  case t474_424 of
                                                      'n' -> return ()
                                                      _ -> gets position >>= (throwError . mkParseError "'n'" "not match pattern: " "" d983_423 ["char"])
                                                  let 'n' = t474_424
                                                  return ()
                                                  return '\n',
                                               do d984_425 <- get
                                                  t475_426 <- StateT char
                                                  case t475_426 of
                                                      't' -> return ()
                                                      _ -> gets position >>= (throwError . mkParseError "'t'" "not match pattern: " "" d984_425 ["char"])
                                                  let 't' = t475_426
                                                  return ()
                                                  return '\t']
                pats122_127 = foldl1 mplus [do p <- StateT pat
                                               _ <- StateT spaces
                                               return ()
                                               ps <- StateT pats
                                               return (p : ps),
                                            return []]
                readFromLs123_128 = foldl1 mplus [do rf <- StateT readFrom
                                                     d989_427 <- get
                                                     t480_428 <- StateT char
                                                     case t480_428 of
                                                         '*' -> return ()
                                                         _ -> gets position >>= (throwError . mkParseError "'*'" "not match pattern: " "" d989_427 ["char"])
                                                     let '*' = t480_428
                                                     return ()
                                                     return (FromL List rf),
                                                  do rf <- StateT readFrom
                                                     d991_429 <- get
                                                     t482_430 <- StateT char
                                                     case t482_430 of
                                                         '+' -> return ()
                                                         _ -> gets position >>= (throwError . mkParseError "'+'" "not match pattern: " "" d991_429 ["char"])
                                                     let '+' = t482_430
                                                     return ()
                                                     return (FromL List1 rf),
                                                  do rf <- StateT readFrom
                                                     d993_431 <- get
                                                     t484_432 <- StateT char
                                                     case t484_432 of
                                                         '?' -> return ()
                                                         _ -> gets position >>= (throwError . mkParseError "'?'" "not match pattern: " "" d993_431 ["char"])
                                                     let '?' = t484_432
                                                     return ()
                                                     return (FromL Optional rf),
                                                  do rf <- StateT readFrom
                                                     return rf]
                readFrom124_129 = foldl1 mplus [do v <- StateT variable
                                                   return (FromVariable $ Just v),
                                                do d996_433 <- get
                                                   t487_434 <- StateT char
                                                   case t487_434 of
                                                       '(' -> return ()
                                                       _ -> gets position >>= (throwError . mkParseError "'('" "not match pattern: " "" d996_433 ["char"])
                                                   let '(' = t487_434
                                                   return ()
                                                   s <- StateT selection
                                                   d998_435 <- get
                                                   t489_436 <- StateT char
                                                   case t489_436 of
                                                       ')' -> return ()
                                                       _ -> gets position >>= (throwError . mkParseError "')'" "not match pattern: " "" d998_435 ["char"])
                                                   let ')' = t489_436
                                                   return ()
                                                   return (fromSelectionQ s),
                                                do e <- StateT expressionHsSugar
                                                   return (fromSelectionQ $ Left [e])]
                selectCharsLs125_130 = foldl1 mplus [do rf <- StateT selectChars
                                                        d1001_437 <- get
                                                        t492_438 <- StateT char
                                                        case t492_438 of
                                                            '*' -> return ()
                                                            _ -> gets position >>= (throwError . mkParseError "'*'" "not match pattern: " "" d1001_437 ["char"])
                                                        let '*' = t492_438
                                                        return ()
                                                        return (FromL List rf),
                                                     do rf <- StateT selectChars
                                                        d1003_439 <- get
                                                        t494_440 <- StateT char
                                                        case t494_440 of
                                                            '+' -> return ()
                                                            _ -> gets position >>= (throwError . mkParseError "'+'" "not match pattern: " "" d1003_439 ["char"])
                                                        let '+' = t494_440
                                                        return ()
                                                        return (FromL List1 rf),
                                                     do rf <- StateT selectChars
                                                        d1005_441 <- get
                                                        t496_442 <- StateT char
                                                        case t496_442 of
                                                            '?' -> return ()
                                                            _ -> gets position >>= (throwError . mkParseError "'?'" "not match pattern: " "" d1005_441 ["char"])
                                                        let '?' = t496_442
                                                        return ()
                                                        return (FromL Optional rf),
                                                     do rf <- StateT selectChars
                                                        return rf]
                selectChars126_131 = foldl1 mplus [do d1007_443 <- get
                                                      t498_444 <- StateT char
                                                      case t498_444 of
                                                          '[' -> return ()
                                                          _ -> gets position >>= (throwError . mkParseError "'['" "not match pattern: " "" d1007_443 ["char"])
                                                      let '[' = t498_444
                                                      return ()
                                                      cs <- list1158_445 (foldl1 mplus [do d1009_446 <- get
                                                                                           t500_447 <- StateT char
                                                                                           let c = t500_447
                                                                                           b1528_448 <- return (isLower c)
                                                                                           unless b1528_448 (gets position >>= (throwError . mkParseError "isLower c" "not match: " "" d1009_446 ["char"]))
                                                                                           return c])
                                                      d1010_449 <- get
                                                      t501_450 <- StateT char
                                                      case t501_450 of
                                                          ']' -> return ()
                                                          _ -> gets position >>= (throwError . mkParseError "']'" "not match pattern: " "" d1010_449 ["char"])
                                                      let ']' = t501_450
                                                      return ()
                                                      return (fromTokenChars cs),
                                                   do d1011_451 <- get
                                                      t502_452 <- StateT char
                                                      case t502_452 of
                                                          '[' -> return ()
                                                          _ -> gets position >>= (throwError . mkParseError "'['" "not match pattern: " "" d1011_451 ["char"])
                                                      let '[' = t502_452
                                                      return ()
                                                      d1012_453 <- get
                                                      t503_454 <- StateT char
                                                      let cb = t503_454
                                                      b1531_455 <- return (cb `notElem` "\\-")
                                                      unless b1531_455 (gets position >>= (throwError . mkParseError "cb `notElem` \"\\\\-\"" "not match: " "" d1012_453 ["char"]))
                                                      d1013_456 <- get
                                                      t504_457 <- StateT char
                                                      case t504_457 of
                                                          '-' -> return ()
                                                          _ -> gets position >>= (throwError . mkParseError "'-'" "not match pattern: " "" d1013_456 ["char"])
                                                      let '-' = t504_457
                                                      return ()
                                                      d1014_458 <- get
                                                      t505_459 <- StateT char
                                                      let ce = t505_459
                                                      b1533_460 <- return (ce `notElem` "\\-")
                                                      unless b1533_460 (gets position >>= (throwError . mkParseError "ce `notElem` \"\\\\-\"" "not match: " "" d1014_458 ["char"]))
                                                      d1015_461 <- get
                                                      t506_462 <- StateT char
                                                      case t506_462 of
                                                          ']' -> return ()
                                                          _ -> gets position >>= (throwError . mkParseError "']'" "not match pattern: " "" d1015_461 ["char"])
                                                      let ']' = t506_462
                                                      return ()
                                                      return (fromTokenChars [cb .. ce]),
                                                   do d1016_463 <- get
                                                      t507_464 <- StateT char
                                                      case t507_464 of
                                                          '\'' -> return ()
                                                          _ -> gets position >>= (throwError . mkParseError "'\\''" "not match pattern: " "" d1016_463 ["char"])
                                                      let '\'' = t507_464
                                                      return ()
                                                      d1017_465 <- get
                                                      t508_466 <- StateT char
                                                      let c = t508_466
                                                      b1536_467 <- return (c `notElem` "\\'")
                                                      unless b1536_467 (gets position >>= (throwError . mkParseError "c `notElem` \"\\\\'\"" "not match: " "" d1017_465 ["char"]))
                                                      d1018_468 <- get
                                                      t509_469 <- StateT char
                                                      case t509_469 of
                                                          '\'' -> return ()
                                                          _ -> gets position >>= (throwError . mkParseError "'\\''" "not match pattern: " "" d1018_468 ["char"])
                                                      let '\'' = t509_469
                                                      return ()
                                                      return (fromTokenChars [c])]
                test127_132 = foldl1 mplus [do d1019_470 <- get
                                               t510_471 <- StateT char
                                               case t510_471 of
                                                   '[' -> return ()
                                                   _ -> gets position >>= (throwError . mkParseError "'['" "not match pattern: " "" d1019_470 ["char"])
                                               let '[' = t510_471
                                               return ()
                                               h <- StateT hsExpLam
                                               _ <- StateT spaces
                                               return ()
                                               com <- optional156_307 (StateT comForErr)
                                               d1023_472 <- get
                                               t514_473 <- StateT char
                                               case t514_473 of
                                                   ']' -> return ()
                                                   _ -> gets position >>= (throwError . mkParseError "']'" "not match pattern: " "" d1023_472 ["char"])
                                               let ']' = t514_473
                                               return ()
                                               return (h, maybe "" id com)]
                hsExpLam128_133 = foldl1 mplus [do d1024_474 <- get
                                                   t515_475 <- StateT char
                                                   case t515_475 of
                                                       '\\' -> return ()
                                                       _ -> gets position >>= (throwError . mkParseError "'\\\\'" "not match pattern: " "" d1024_474 ["char"])
                                                   let '\\' = t515_475
                                                   return ()
                                                   _ <- StateT spaces
                                                   return ()
                                                   ps <- StateT pats
                                                   _ <- StateT spaces
                                                   return ()
                                                   d1028_476 <- get
                                                   t519_477 <- StateT char
                                                   case t519_477 of
                                                       '-' -> return ()
                                                       _ -> gets position >>= (throwError . mkParseError "'-'" "not match pattern: " "" d1028_476 ["char"])
                                                   let '-' = t519_477
                                                   return ()
                                                   d1029_478 <- get
                                                   t520_479 <- StateT char
                                                   case t520_479 of
                                                       '>' -> return ()
                                                       _ -> gets position >>= (throwError . mkParseError "'>'" "not match pattern: " "" d1029_478 ["char"])
                                                   let '>' = t520_479
                                                   return ()
                                                   _ <- StateT spaces
                                                   return ()
                                                   e <- StateT hsExpTyp
                                                   return (LamE ps e),
                                                do e <- StateT hsExpTyp
                                                   return e]
                hsExpTyp129_134 = foldl1 mplus [do eo <- StateT hsExpOp
                                                   d1034_480 <- get
                                                   t525_481 <- StateT char
                                                   case t525_481 of
                                                       ':' -> return ()
                                                       _ -> gets position >>= (throwError . mkParseError "':'" "not match pattern: " "" d1034_480 ["char"])
                                                   let ':' = t525_481
                                                   return ()
                                                   d1035_482 <- get
                                                   t526_483 <- StateT char
                                                   case t526_483 of
                                                       ':' -> return ()
                                                       _ -> gets position >>= (throwError . mkParseError "':'" "not match pattern: " "" d1035_482 ["char"])
                                                   let ':' = t526_483
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
                hsOp131_136 = foldl1 mplus [do d1045_484 <- get
                                               t536_485 <- StateT char
                                               let c = t536_485
                                               b1564_486 <- return (c `elem` "+*/-!|&.^=<>$")
                                               unless b1564_486 (gets position >>= (throwError . mkParseError "c `elem` \"+*/-!|&.^=<>$\"" "not match: " "" d1045_484 ["char"]))
                                               o <- StateT opTail
                                               return (VarE $ mkName $ c : o),
                                            do d1047_487 <- get
                                               t538_488 <- StateT char
                                               case t538_488 of
                                                   ':' -> return ()
                                                   _ -> gets position >>= (throwError . mkParseError "':'" "not match pattern: " "" d1047_487 ["char"])
                                               let ':' = t538_488
                                               return ()
                                               d1049_489 <- get
                                               do err1182_490 <- ((do d1048_491 <- get
                                                                      t539_492 <- StateT char
                                                                      case t539_492 of
                                                                          ':' -> return ()
                                                                          _ -> gets position >>= (throwError . mkParseError "':'" "not match pattern: " "" d1048_491 ["char"])
                                                                      let ':' = t539_492
                                                                      return ()) >> return False) `catchError` const (return True)
                                                  unless err1182_490 (gets position >>= (throwError . mkParseError "!':':" "not match: " "" d1049_489 ["char"]))
                                               put d1049_489
                                               o <- StateT opTail
                                               return (ConE $ mkName $ ':' : o),
                                            do d1051_493 <- get
                                               t541_494 <- StateT char
                                               case t541_494 of
                                                   '`' -> return ()
                                                   _ -> gets position >>= (throwError . mkParseError "'`'" "not match pattern: " "" d1051_493 ["char"])
                                               let '`' = t541_494
                                               return ()
                                               v <- StateT variable
                                               d1053_495 <- get
                                               t543_496 <- StateT char
                                               case t543_496 of
                                                   '`' -> return ()
                                                   _ -> gets position >>= (throwError . mkParseError "'`'" "not match pattern: " "" d1053_495 ["char"])
                                               let '`' = t543_496
                                               return ()
                                               return (VarE $ mkName v),
                                            do d1054_497 <- get
                                               t544_498 <- StateT char
                                               case t544_498 of
                                                   '`' -> return ()
                                                   _ -> gets position >>= (throwError . mkParseError "'`'" "not match pattern: " "" d1054_497 ["char"])
                                               let '`' = t544_498
                                               return ()
                                               t <- StateT typ
                                               d1056_499 <- get
                                               t546_500 <- StateT char
                                               case t546_500 of
                                                   '`' -> return ()
                                                   _ -> gets position >>= (throwError . mkParseError "'`'" "not match pattern: " "" d1056_499 ["char"])
                                               let '`' = t546_500
                                               return ()
                                               return (ConE $ mkName t)]
                opTail132_137 = foldl1 mplus [do d1057_501 <- get
                                                 t547_502 <- StateT char
                                                 let c = t547_502
                                                 b1575_503 <- return (c `elem` ":+*/-!|&.^=<>$")
                                                 unless b1575_503 (gets position >>= (throwError . mkParseError "c `elem` \":+*/-!|&.^=<>$\"" "not match: " "" d1057_501 ["char"]))
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
                hsExp1134_139 = foldl1 mplus [do d1063_504 <- get
                                                 t553_505 <- StateT char
                                                 case t553_505 of
                                                     '(' -> return ()
                                                     _ -> gets position >>= (throwError . mkParseError "'('" "not match pattern: " "" d1063_504 ["char"])
                                                 let '(' = t553_505
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
                                                 d1071_506 <- get
                                                 t561_507 <- StateT char
                                                 case t561_507 of
                                                     ')' -> return ()
                                                     _ -> gets position >>= (throwError . mkParseError "')'" "not match pattern: " "" d1071_506 ["char"])
                                                 let ')' = t561_507
                                                 return ()
                                                 return (InfixE l o r),
                                              do d1072_508 <- get
                                                 t562_509 <- StateT char
                                                 case t562_509 of
                                                     '(' -> return ()
                                                     _ -> gets position >>= (throwError . mkParseError "'('" "not match pattern: " "" d1072_508 ["char"])
                                                 let '(' = t562_509
                                                 return ()
                                                 et <- StateT hsExpTpl
                                                 d1074_510 <- get
                                                 t564_511 <- StateT char
                                                 case t564_511 of
                                                     ')' -> return ()
                                                     _ -> gets position >>= (throwError . mkParseError "')'" "not match pattern: " "" d1074_510 ["char"])
                                                 let ')' = t564_511
                                                 return ()
                                                 return (TupE et),
                                              do d1075_512 <- get
                                                 t565_513 <- StateT char
                                                 case t565_513 of
                                                     '[' -> return ()
                                                     _ -> gets position >>= (throwError . mkParseError "'['" "not match pattern: " "" d1075_512 ["char"])
                                                 let '[' = t565_513
                                                 return ()
                                                 et <- StateT hsExpTpl
                                                 d1077_514 <- get
                                                 t567_515 <- StateT char
                                                 case t567_515 of
                                                     ']' -> return ()
                                                     _ -> gets position >>= (throwError . mkParseError "']'" "not match pattern: " "" d1077_514 ["char"])
                                                 let ']' = t567_515
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
                                              do d1082_516 <- get
                                                 t572_517 <- StateT char
                                                 case t572_517 of
                                                     '\'' -> return ()
                                                     _ -> gets position >>= (throwError . mkParseError "'\\''" "not match pattern: " "" d1082_516 ["char"])
                                                 let '\'' = t572_517
                                                 return ()
                                                 c <- StateT charLit
                                                 d1084_518 <- get
                                                 t574_519 <- StateT char
                                                 case t574_519 of
                                                     '\'' -> return ()
                                                     _ -> gets position >>= (throwError . mkParseError "'\\''" "not match pattern: " "" d1084_518 ["char"])
                                                 let '\'' = t574_519
                                                 return ()
                                                 return (LitE $ charL c),
                                              do d1085_520 <- get
                                                 t575_521 <- StateT char
                                                 case t575_521 of
                                                     '"' -> return ()
                                                     _ -> gets position >>= (throwError . mkParseError "'\"'" "not match pattern: " "" d1085_520 ["char"])
                                                 let '"' = t575_521
                                                 return ()
                                                 s <- StateT stringLit
                                                 d1087_522 <- get
                                                 t577_523 <- StateT char
                                                 case t577_523 of
                                                     '"' -> return ()
                                                     _ -> gets position >>= (throwError . mkParseError "'\"'" "not match pattern: " "" d1087_522 ["char"])
                                                 let '"' = t577_523
                                                 return ()
                                                 return (LitE $ stringL s),
                                              do d1088_524 <- get
                                                 t578_525 <- StateT char
                                                 case t578_525 of
                                                     '-' -> return ()
                                                     _ -> gets position >>= (throwError . mkParseError "'-'" "not match pattern: " "" d1088_524 ["char"])
                                                 let '-' = t578_525
                                                 return ()
                                                 _ <- StateT spaces
                                                 return ()
                                                 e <- StateT hsExp1
                                                 return (AppE (VarE $ mkName "negate") e)]
                hsExpTpl135_140 = foldl1 mplus [do e <- StateT hsExpLam
                                                   _ <- StateT spaces
                                                   return ()
                                                   d1093_526 <- get
                                                   t583_527 <- StateT char
                                                   case t583_527 of
                                                       ',' -> return ()
                                                       _ -> gets position >>= (throwError . mkParseError "','" "not match pattern: " "" d1093_526 ["char"])
                                                   let ',' = t583_527
                                                   return ()
                                                   _ <- StateT spaces
                                                   return ()
                                                   et <- StateT hsExpTpl
                                                   return (e : et),
                                                do e <- StateT hsExpLam
                                                   return [e],
                                                return []]
                hsTypeArr136_141 = foldl1 mplus [do l <- StateT hsType
                                                    d1098_528 <- get
                                                    t588_529 <- StateT char
                                                    case t588_529 of
                                                        '-' -> return ()
                                                        _ -> gets position >>= (throwError . mkParseError "'-'" "not match pattern: " "" d1098_528 ["char"])
                                                    let '-' = t588_529
                                                    return ()
                                                    d1099_530 <- get
                                                    t589_531 <- StateT char
                                                    case t589_531 of
                                                        '>' -> return ()
                                                        _ -> gets position >>= (throwError . mkParseError "'>'" "not match pattern: " "" d1099_530 ["char"])
                                                    let '>' = t589_531
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
                hsType1138_143 = foldl1 mplus [do d1106_532 <- get
                                                  t596_533 <- StateT char
                                                  case t596_533 of
                                                      '[' -> return ()
                                                      _ -> gets position >>= (throwError . mkParseError "'['" "not match pattern: " "" d1106_532 ["char"])
                                                  let '[' = t596_533
                                                  return ()
                                                  d1107_534 <- get
                                                  t597_535 <- StateT char
                                                  case t597_535 of
                                                      ']' -> return ()
                                                      _ -> gets position >>= (throwError . mkParseError "']'" "not match pattern: " "" d1107_534 ["char"])
                                                  let ']' = t597_535
                                                  return ()
                                                  _ <- StateT spaces
                                                  return ()
                                                  return ListT,
                                               do d1109_536 <- get
                                                  t599_537 <- StateT char
                                                  case t599_537 of
                                                      '[' -> return ()
                                                      _ -> gets position >>= (throwError . mkParseError "'['" "not match pattern: " "" d1109_536 ["char"])
                                                  let '[' = t599_537
                                                  return ()
                                                  t <- StateT hsTypeArr
                                                  d1111_538 <- get
                                                  t601_539 <- StateT char
                                                  case t601_539 of
                                                      ']' -> return ()
                                                      _ -> gets position >>= (throwError . mkParseError "']'" "not match pattern: " "" d1111_538 ["char"])
                                                  let ']' = t601_539
                                                  return ()
                                                  _ <- StateT spaces
                                                  return ()
                                                  return (ListT `AppT` t),
                                               do d1113_540 <- get
                                                  t603_541 <- StateT char
                                                  case t603_541 of
                                                      '(' -> return ()
                                                      _ -> gets position >>= (throwError . mkParseError "'('" "not match pattern: " "" d1113_540 ["char"])
                                                  let '(' = t603_541
                                                  return ()
                                                  _ <- StateT spaces
                                                  return ()
                                                  tt <- StateT hsTypeTpl
                                                  d1116_542 <- get
                                                  t606_543 <- StateT char
                                                  case t606_543 of
                                                      ')' -> return ()
                                                      _ -> gets position >>= (throwError . mkParseError "')'" "not match pattern: " "" d1116_542 ["char"])
                                                  let ')' = t606_543
                                                  return ()
                                                  _ <- StateT spaces
                                                  return ()
                                                  return (foldl AppT (TupleT $ length tt) tt),
                                               do t <- StateT typToken
                                                  return (ConT $ mkName t),
                                               do d1119_544 <- get
                                                  t609_545 <- StateT char
                                                  case t609_545 of
                                                      '(' -> return ()
                                                      _ -> gets position >>= (throwError . mkParseError "'('" "not match pattern: " "" d1119_544 ["char"])
                                                  let '(' = t609_545
                                                  return ()
                                                  d1120_546 <- get
                                                  t610_547 <- StateT char
                                                  case t610_547 of
                                                      '-' -> return ()
                                                      _ -> gets position >>= (throwError . mkParseError "'-'" "not match pattern: " "" d1120_546 ["char"])
                                                  let '-' = t610_547
                                                  return ()
                                                  d1121_548 <- get
                                                  t611_549 <- StateT char
                                                  case t611_549 of
                                                      '>' -> return ()
                                                      _ -> gets position >>= (throwError . mkParseError "'>'" "not match pattern: " "" d1121_548 ["char"])
                                                  let '>' = t611_549
                                                  return ()
                                                  d1122_550 <- get
                                                  t612_551 <- StateT char
                                                  case t612_551 of
                                                      ')' -> return ()
                                                      _ -> gets position >>= (throwError . mkParseError "')'" "not match pattern: " "" d1122_550 ["char"])
                                                  let ')' = t612_551
                                                  return ()
                                                  _ <- StateT spaces
                                                  return ()
                                                  return ArrowT]
                hsTypeTpl139_144 = foldl1 mplus [do t <- StateT hsTypeArr
                                                    d1125_552 <- get
                                                    t615_553 <- StateT char
                                                    case t615_553 of
                                                        ',' -> return ()
                                                        _ -> gets position >>= (throwError . mkParseError "','" "not match pattern: " "" d1125_552 ["char"])
                                                    let ',' = t615_553
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
                                                  ds <- list157_346 (foldl1 mplus [do d <- StateT digit
                                                                                      return d])
                                                  return (read $ dh : ds)]
                alpha144_149 = foldl1 mplus [do u <- StateT upper
                                                return u,
                                             do l <- StateT lower
                                                return l,
                                             do d <- StateT digit
                                                return d,
                                             do d1141_554 <- get
                                                t631_555 <- StateT char
                                                case t631_555 of
                                                    '\'' -> return ()
                                                    _ -> gets position >>= (throwError . mkParseError "'\\''" "not match pattern: " "" d1141_554 ["char"])
                                                let '\'' = t631_555
                                                return ()
                                                return '\'']
                upper145_150 = foldl1 mplus [do d1142_556 <- get
                                                t632_557 <- StateT char
                                                let u = t632_557
                                                b1660_558 <- return (isUpper u)
                                                unless b1660_558 (gets position >>= (throwError . mkParseError "isUpper u" "not match: " "" d1142_556 ["char"]))
                                                return u]
                lower146_151 = foldl1 mplus [do d1143_559 <- get
                                                t633_560 <- StateT char
                                                let l = t633_560
                                                b1661_561 <- return (isLower l || l == '_')
                                                unless b1661_561 (gets position >>= (throwError . mkParseError "isLower l || l == '_'" "not match: " "" d1143_559 ["char"]))
                                                return l]
                digit147_152 = foldl1 mplus [do d1144_562 <- get
                                                t634_563 <- StateT char
                                                let d = t634_563
                                                b1662_564 <- return (isDigit d)
                                                unless b1662_564 (gets position >>= (throwError . mkParseError "isDigit d" "not match: " "" d1144_562 ["char"]))
                                                return d]
                spaces148_153 = foldl1 mplus [do _ <- StateT space
                                                 return ()
                                                 _ <- StateT spaces
                                                 return ()
                                                 return (),
                                              return ()]
                space149_154 = foldl1 mplus [do d1147_565 <- get
                                                t637_566 <- StateT char
                                                let s = t637_566
                                                b1665_567 <- return (isSpace s)
                                                unless b1665_567 (gets position >>= (throwError . mkParseError "isSpace s" "not match: " "" d1147_565 ["char"]))
                                                return (),
                                             do d1148_568 <- get
                                                t638_569 <- StateT char
                                                case t638_569 of
                                                    '-' -> return ()
                                                    _ -> gets position >>= (throwError . mkParseError "'-'" "not match pattern: " "" d1148_568 ["char"])
                                                let '-' = t638_569
                                                return ()
                                                d1149_570 <- get
                                                t639_571 <- StateT char
                                                case t639_571 of
                                                    '-' -> return ()
                                                    _ -> gets position >>= (throwError . mkParseError "'-'" "not match pattern: " "" d1149_570 ["char"])
                                                let '-' = t639_571
                                                return ()
                                                _ <- StateT notNLString
                                                return ()
                                                _ <- StateT newLine
                                                return ()
                                                return (),
                                             do _ <- StateT comment
                                                return ()
                                                return ()]
                notNLString150_155 = foldl1 mplus [do d1154_572 <- get
                                                      do err1183_573 <- ((do _ <- StateT newLine
                                                                             return ()) >> return False) `catchError` const (return True)
                                                         unless err1183_573 (gets position >>= (throwError . mkParseError "!_:newLine" "not match: " "" d1154_572 ["newLine"]))
                                                      put d1154_572
                                                      c <- StateT char
                                                      s <- StateT notNLString
                                                      return (c : s),
                                                   return ""]
                newLine151_156 = foldl1 mplus [do d1157_574 <- get
                                                  t646_575 <- StateT char
                                                  case t646_575 of
                                                      '\n' -> return ()
                                                      _ -> gets position >>= (throwError . mkParseError "'\\n'" "not match pattern: " "" d1157_574 ["char"])
                                                  let '\n' = t646_575
                                                  return ()
                                                  return ()]
                comment152_157 = foldl1 mplus [do d1158_576 <- get
                                                  t647_577 <- StateT char
                                                  case t647_577 of
                                                      '{' -> return ()
                                                      _ -> gets position >>= (throwError . mkParseError "'{'" "not match pattern: " "" d1158_576 ["char"])
                                                  let '{' = t647_577
                                                  return ()
                                                  d1159_578 <- get
                                                  t648_579 <- StateT char
                                                  case t648_579 of
                                                      '-' -> return ()
                                                      _ -> gets position >>= (throwError . mkParseError "'-'" "not match pattern: " "" d1159_578 ["char"])
                                                  let '-' = t648_579
                                                  return ()
                                                  d1161_580 <- get
                                                  do err1184_581 <- ((do d1160_582 <- get
                                                                         t649_583 <- StateT char
                                                                         case t649_583 of
                                                                             '#' -> return ()
                                                                             _ -> gets position >>= (throwError . mkParseError "'#'" "not match pattern: " "" d1160_582 ["char"])
                                                                         let '#' = t649_583
                                                                         return ()) >> return False) `catchError` const (return True)
                                                     unless err1184_581 (gets position >>= (throwError . mkParseError "!'#':" "not match: " "" d1161_580 ["char"]))
                                                  put d1161_580
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
                notComStr154_159 = foldl1 mplus [do d1169_584 <- get
                                                    do err1185_585 <- ((do _ <- StateT comment
                                                                           return ()) >> return False) `catchError` const (return True)
                                                       unless err1185_585 (gets position >>= (throwError . mkParseError "!_:comment" "not match: " "" d1169_584 ["comment"]))
                                                    put d1169_584
                                                    d1171_586 <- get
                                                    do err1186_587 <- ((do _ <- StateT comEnd
                                                                           return ()) >> return False) `catchError` const (return True)
                                                       unless err1186_587 (gets position >>= (throwError . mkParseError "!_:comEnd" "not match: " "" d1171_586 ["comEnd"]))
                                                    put d1171_586
                                                    _ <- StateT char
                                                    return ()
                                                    _ <- StateT notComStr
                                                    return ()
                                                    return (),
                                                 return ()]
                comEnd155_160 = foldl1 mplus [do d1174_588 <- get
                                                 t660_589 <- StateT char
                                                 case t660_589 of
                                                     '-' -> return ()
                                                     _ -> gets position >>= (throwError . mkParseError "'-'" "not match pattern: " "" d1174_588 ["char"])
                                                 let '-' = t660_589
                                                 return ()
                                                 d1175_590 <- get
                                                 t661_591 <- StateT char
                                                 case t661_591 of
                                                     '}' -> return ()
                                                     _ -> gets position >>= (throwError . mkParseError "'}'" "not match pattern: " "" d1175_590 ["char"])
                                                 let '}' = t661_591
                                                 return ()
                                                 return ()]
                list157_346 :: forall m a . (MonadPlus m, Applicative m) =>
                                            m a -> m ([a])
                list1158_445 :: forall m a . (MonadPlus m, Applicative m) =>
                                             m a -> m ([a])
                list157_346 p = list1158_445 p `mplus` return []
                list1158_445 p = ((:) <$> p) <*> list157_346 p
                optional156_307 :: forall m a . (MonadPlus m, Applicative m) =>
                                                m a -> m (Maybe a)
                optional156_307 p = (Just <$> p) `mplus` return Nothing

