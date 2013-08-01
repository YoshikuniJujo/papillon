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

	getType,
	pprCheck,
	readings,

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
import "monads-tf" Control.Monad.Error
import Control.Applicative
import "monads-tf" Control.Monad.Identity



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
                                   ((Either (([Expression])) (([PlainExpression])), Derivs))),
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
              expressionHsSugar' :: (ErrorT (ParseError (Pos String) Derivs)
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
              hsExpLam' :: (ErrorT (ParseError (Pos String) Derivs)
                                   Identity
                                   ((Exp, Derivs))),
              hsExpTyp' :: (ErrorT (ParseError (Pos String) Derivs)
                                   Identity
                                   ((Exp, Derivs))),
              hsExpOp' :: (ErrorT (ParseError (Pos String) Derivs)
                                  Identity
                                  ((Exp, Derivs))),
              hsOp' :: (ErrorT (ParseError (Pos String) Derivs)
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
parse = parse1789_0 initialPos
          where parse1789_0 pos1788_1 s1790_2 = d707_3
                                where d707_3 = Derivs pegFile0_4 pragmas1_5 pragma2_6 pragmaStr3_7 pragmaItems4_8 pragmaEnd5_9 moduleDec6_10 moduleName7_11 moduleDecStr8_12 whr9_13 preImpPap10_14 prePeg11_15 afterPeg12_16 importPapillon13_17 varToken14_18 typToken15_19 pap16_20 peg17_21 monadType18_22 sourceType19_23 peg_20_24 definition21_25 selection22_26 normalSelection23_27 plainSelection24_28 expressionHs25_29 expressionHsSugar26_30 expressionHsSugar'27_31 plainExpressionHs28_32 plainHAReadFromLs29_33 plainReadFromLs30_34 expression31_35 nameLeaf_32_36 nameLeaf33_37 nameLeafNoCom34_38 comForErr35_39 leaf36_40 patOp37_41 pat38_42 pat139_43 patList40_44 opConName41_45 charLit42_46 stringLit43_47 escapeC44_48 pats45_49 readFromLs46_50 readFrom47_51 selectCharsLs48_52 selectChars49_53 test50_54 hsExpLam51_55 hsExpTyp52_56 hsExpOp53_57 hsOp54_58 hsExpLam'55_59 hsExpTyp'56_60 hsExpOp'57_61 hsOp'58_62 opTail59_63 hsExp60_64 hsExp161_65 hsExpTpl62_66 hsTypeArr63_67 hsType64_68 hsType165_69 hsTypeTpl66_70 typ67_71 variable68_72 tvtail69_73 integer70_74 alpha71_75 upper72_76 lower73_77 digit74_78 spaces75_79 space76_80 notNLString77_81 newLine78_82 comment79_83 comments80_84 notComStr81_85 comEnd82_86 chars1791_87 pos1788_1
                                      pegFile0_4 = runStateT pegFile83_88 d707_3
                                      pragmas1_5 = runStateT pragmas84_89 d707_3
                                      pragma2_6 = runStateT pragma85_90 d707_3
                                      pragmaStr3_7 = runStateT pragmaStr86_91 d707_3
                                      pragmaItems4_8 = runStateT pragmaItems87_92 d707_3
                                      pragmaEnd5_9 = runStateT pragmaEnd88_93 d707_3
                                      moduleDec6_10 = runStateT moduleDec89_94 d707_3
                                      moduleName7_11 = runStateT moduleName90_95 d707_3
                                      moduleDecStr8_12 = runStateT moduleDecStr91_96 d707_3
                                      whr9_13 = runStateT whr92_97 d707_3
                                      preImpPap10_14 = runStateT preImpPap93_98 d707_3
                                      prePeg11_15 = runStateT prePeg94_99 d707_3
                                      afterPeg12_16 = runStateT afterPeg95_100 d707_3
                                      importPapillon13_17 = runStateT importPapillon96_101 d707_3
                                      varToken14_18 = runStateT varToken97_102 d707_3
                                      typToken15_19 = runStateT typToken98_103 d707_3
                                      pap16_20 = runStateT pap99_104 d707_3
                                      peg17_21 = runStateT peg100_105 d707_3
                                      monadType18_22 = runStateT monadType101_106 d707_3
                                      sourceType19_23 = runStateT sourceType102_107 d707_3
                                      peg_20_24 = runStateT peg_103_108 d707_3
                                      definition21_25 = runStateT definition104_109 d707_3
                                      selection22_26 = runStateT selection105_110 d707_3
                                      normalSelection23_27 = runStateT normalSelection106_111 d707_3
                                      plainSelection24_28 = runStateT plainSelection107_112 d707_3
                                      expressionHs25_29 = runStateT expressionHs108_113 d707_3
                                      expressionHsSugar26_30 = runStateT expressionHsSugar109_114 d707_3
                                      expressionHsSugar'27_31 = runStateT expressionHsSugar'110_115 d707_3
                                      plainExpressionHs28_32 = runStateT plainExpressionHs111_116 d707_3
                                      plainHAReadFromLs29_33 = runStateT plainHAReadFromLs112_117 d707_3
                                      plainReadFromLs30_34 = runStateT plainReadFromLs113_118 d707_3
                                      expression31_35 = runStateT expression114_119 d707_3
                                      nameLeaf_32_36 = runStateT nameLeaf_115_120 d707_3
                                      nameLeaf33_37 = runStateT nameLeaf116_121 d707_3
                                      nameLeafNoCom34_38 = runStateT nameLeafNoCom117_122 d707_3
                                      comForErr35_39 = runStateT comForErr118_123 d707_3
                                      leaf36_40 = runStateT leaf119_124 d707_3
                                      patOp37_41 = runStateT patOp120_125 d707_3
                                      pat38_42 = runStateT pat121_126 d707_3
                                      pat139_43 = runStateT pat1122_127 d707_3
                                      patList40_44 = runStateT patList123_128 d707_3
                                      opConName41_45 = runStateT opConName124_129 d707_3
                                      charLit42_46 = runStateT charLit125_130 d707_3
                                      stringLit43_47 = runStateT stringLit126_131 d707_3
                                      escapeC44_48 = runStateT escapeC127_132 d707_3
                                      pats45_49 = runStateT pats128_133 d707_3
                                      readFromLs46_50 = runStateT readFromLs129_134 d707_3
                                      readFrom47_51 = runStateT readFrom130_135 d707_3
                                      selectCharsLs48_52 = runStateT selectCharsLs131_136 d707_3
                                      selectChars49_53 = runStateT selectChars132_137 d707_3
                                      test50_54 = runStateT test133_138 d707_3
                                      hsExpLam51_55 = runStateT hsExpLam134_139 d707_3
                                      hsExpTyp52_56 = runStateT hsExpTyp135_140 d707_3
                                      hsExpOp53_57 = runStateT hsExpOp136_141 d707_3
                                      hsOp54_58 = runStateT hsOp137_142 d707_3
                                      hsExpLam'55_59 = runStateT hsExpLam'138_143 d707_3
                                      hsExpTyp'56_60 = runStateT hsExpTyp'139_144 d707_3
                                      hsExpOp'57_61 = runStateT hsExpOp'140_145 d707_3
                                      hsOp'58_62 = runStateT hsOp'141_146 d707_3
                                      opTail59_63 = runStateT opTail142_147 d707_3
                                      hsExp60_64 = runStateT hsExp143_148 d707_3
                                      hsExp161_65 = runStateT hsExp1144_149 d707_3
                                      hsExpTpl62_66 = runStateT hsExpTpl145_150 d707_3
                                      hsTypeArr63_67 = runStateT hsTypeArr146_151 d707_3
                                      hsType64_68 = runStateT hsType147_152 d707_3
                                      hsType165_69 = runStateT hsType1148_153 d707_3
                                      hsTypeTpl66_70 = runStateT hsTypeTpl149_154 d707_3
                                      typ67_71 = runStateT typ150_155 d707_3
                                      variable68_72 = runStateT variable151_156 d707_3
                                      tvtail69_73 = runStateT tvtail152_157 d707_3
                                      integer70_74 = runStateT integer153_158 d707_3
                                      alpha71_75 = runStateT alpha154_159 d707_3
                                      upper72_76 = runStateT upper155_160 d707_3
                                      lower73_77 = runStateT lower156_161 d707_3
                                      digit74_78 = runStateT digit157_162 d707_3
                                      spaces75_79 = runStateT spaces158_163 d707_3
                                      space76_80 = runStateT space159_164 d707_3
                                      notNLString77_81 = runStateT notNLString160_165 d707_3
                                      newLine78_82 = runStateT newLine161_166 d707_3
                                      comment79_83 = runStateT comment162_167 d707_3
                                      comments80_84 = runStateT comments163_168 d707_3
                                      notComStr81_85 = runStateT notComStr164_169 d707_3
                                      comEnd82_86 = runStateT comEnd165_170 d707_3
                                      chars1791_87 = runStateT (case getToken s1790_2 of
                                                                    Just (c1785_171,
                                                                          s'1787_172) -> do put (parse1789_0 (updatePos c1785_171 pos1788_1) s'1787_172)
                                                                                            return c1785_171
                                                                    _ -> gets position >>= (throwError . mkParseError "" "end of input" "" undefined [])) d707_3
                pegFile83_88 = foldl1 mplus [do pr <- StateT pragmas
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
                                                d715_173 <- get
                                                t177_174 <- StateT char
                                                case t177_174 of
                                                    '|' -> return ()
                                                    _ -> gets position >>= (throwError . mkParseError "'|'" "not match pattern: " "" d715_173 ["char"])
                                                let '|' = t177_174
                                                return ()
                                                d716_175 <- get
                                                t178_176 <- StateT char
                                                case t178_176 of
                                                    ']' -> return ()
                                                    _ -> gets position >>= (throwError . mkParseError "']'" "not match pattern: " "" d716_175 ["char"])
                                                let ']' = t178_176
                                                return ()
                                                d717_177 <- get
                                                t179_178 <- StateT char
                                                case t179_178 of
                                                    '\n' -> return ()
                                                    _ -> gets position >>= (throwError . mkParseError "'\\n'" "not match pattern: " "" d717_177 ["char"])
                                                let '\n' = t179_178
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
                                                d725_179 <- get
                                                t187_180 <- StateT char
                                                case t187_180 of
                                                    '|' -> return ()
                                                    _ -> gets position >>= (throwError . mkParseError "'|'" "not match pattern: " "" d725_179 ["char"])
                                                let '|' = t187_180
                                                return ()
                                                d726_181 <- get
                                                t188_182 <- StateT char
                                                case t188_182 of
                                                    ']' -> return ()
                                                    _ -> gets position >>= (throwError . mkParseError "']'" "not match pattern: " "" d726_181 ["char"])
                                                let ']' = t188_182
                                                return ()
                                                d727_183 <- get
                                                t189_184 <- StateT char
                                                case t189_184 of
                                                    '\n' -> return ()
                                                    _ -> gets position >>= (throwError . mkParseError "'\\n'" "not match pattern: " "" d727_183 ["char"])
                                                let '\n' = t189_184
                                                return ()
                                                atp <- StateT afterPeg
                                                return (mkPegFile pr md [] pp p atp)]
                pragmas84_89 = foldl1 mplus [do _ <- StateT spaces
                                                return ()
                                                pr <- StateT pragma
                                                prs <- StateT pragmas
                                                return (pr : prs),
                                             do _ <- StateT spaces
                                                return ()
                                                return []]
                pragma85_90 = foldl1 mplus [do d733_185 <- get
                                               t195_186 <- StateT char
                                               case t195_186 of
                                                   '{' -> return ()
                                                   _ -> gets position >>= (throwError . mkParseError "'{'" "not match pattern: " "" d733_185 ["char"])
                                               let '{' = t195_186
                                               return ()
                                               d734_187 <- get
                                               t196_188 <- StateT char
                                               case t196_188 of
                                                   '-' -> return ()
                                                   _ -> gets position >>= (throwError . mkParseError "'-'" "not match pattern: " "" d734_187 ["char"])
                                               let '-' = t196_188
                                               return ()
                                               d735_189 <- get
                                               t197_190 <- StateT char
                                               case t197_190 of
                                                   '#' -> return ()
                                                   _ -> gets position >>= (throwError . mkParseError "'#'" "not match pattern: " "" d735_189 ["char"])
                                               let '#' = t197_190
                                               return ()
                                               _ <- StateT spaces
                                               return ()
                                               d737_191 <- get
                                               t199_192 <- StateT char
                                               case t199_192 of
                                                   'L' -> return ()
                                                   _ -> gets position >>= (throwError . mkParseError "'L'" "not match pattern: " "" d737_191 ["char"])
                                               let 'L' = t199_192
                                               return ()
                                               d738_193 <- get
                                               t200_194 <- StateT char
                                               case t200_194 of
                                                   'A' -> return ()
                                                   _ -> gets position >>= (throwError . mkParseError "'A'" "not match pattern: " "" d738_193 ["char"])
                                               let 'A' = t200_194
                                               return ()
                                               d739_195 <- get
                                               t201_196 <- StateT char
                                               case t201_196 of
                                                   'N' -> return ()
                                                   _ -> gets position >>= (throwError . mkParseError "'N'" "not match pattern: " "" d739_195 ["char"])
                                               let 'N' = t201_196
                                               return ()
                                               d740_197 <- get
                                               t202_198 <- StateT char
                                               case t202_198 of
                                                   'G' -> return ()
                                                   _ -> gets position >>= (throwError . mkParseError "'G'" "not match pattern: " "" d740_197 ["char"])
                                               let 'G' = t202_198
                                               return ()
                                               d741_199 <- get
                                               t203_200 <- StateT char
                                               case t203_200 of
                                                   'U' -> return ()
                                                   _ -> gets position >>= (throwError . mkParseError "'U'" "not match pattern: " "" d741_199 ["char"])
                                               let 'U' = t203_200
                                               return ()
                                               d742_201 <- get
                                               t204_202 <- StateT char
                                               case t204_202 of
                                                   'A' -> return ()
                                                   _ -> gets position >>= (throwError . mkParseError "'A'" "not match pattern: " "" d742_201 ["char"])
                                               let 'A' = t204_202
                                               return ()
                                               d743_203 <- get
                                               t205_204 <- StateT char
                                               case t205_204 of
                                                   'G' -> return ()
                                                   _ -> gets position >>= (throwError . mkParseError "'G'" "not match pattern: " "" d743_203 ["char"])
                                               let 'G' = t205_204
                                               return ()
                                               d744_205 <- get
                                               t206_206 <- StateT char
                                               case t206_206 of
                                                   'E' -> return ()
                                                   _ -> gets position >>= (throwError . mkParseError "'E'" "not match pattern: " "" d744_205 ["char"])
                                               let 'E' = t206_206
                                               return ()
                                               _ <- StateT spaces
                                               return ()
                                               s <- StateT pragmaItems
                                               _ <- StateT pragmaEnd
                                               return ()
                                               _ <- StateT spaces
                                               return ()
                                               return (LanguagePragma s),
                                            do d749_207 <- get
                                               t211_208 <- StateT char
                                               case t211_208 of
                                                   '{' -> return ()
                                                   _ -> gets position >>= (throwError . mkParseError "'{'" "not match pattern: " "" d749_207 ["char"])
                                               let '{' = t211_208
                                               return ()
                                               d750_209 <- get
                                               t212_210 <- StateT char
                                               case t212_210 of
                                                   '-' -> return ()
                                                   _ -> gets position >>= (throwError . mkParseError "'-'" "not match pattern: " "" d750_209 ["char"])
                                               let '-' = t212_210
                                               return ()
                                               d751_211 <- get
                                               t213_212 <- StateT char
                                               case t213_212 of
                                                   '#' -> return ()
                                                   _ -> gets position >>= (throwError . mkParseError "'#'" "not match pattern: " "" d751_211 ["char"])
                                               let '#' = t213_212
                                               return ()
                                               _ <- StateT spaces
                                               return ()
                                               s <- StateT pragmaStr
                                               _ <- StateT pragmaEnd
                                               return ()
                                               return (OtherPragma s)]
                pragmaStr86_91 = foldl1 mplus [do d756_213 <- get
                                                  do err1259_214 <- ((do _ <- StateT pragmaEnd
                                                                         return ()) >> return False) `catchError` const (return True)
                                                     unless err1259_214 (gets position >>= (throwError . mkParseError "!_:pragmaEnd" "not match: " "" d756_213 ["pragmaEnd"]))
                                                  put d756_213
                                                  c <- StateT char
                                                  s <- StateT pragmaStr
                                                  return (c : s),
                                               return ""]
                pragmaItems87_92 = foldl1 mplus [do t <- StateT typToken
                                                    d760_215 <- get
                                                    t221_216 <- StateT char
                                                    case t221_216 of
                                                        ',' -> return ()
                                                        _ -> gets position >>= (throwError . mkParseError "','" "not match pattern: " "" d760_215 ["char"])
                                                    let ',' = t221_216
                                                    return ()
                                                    _ <- StateT spaces
                                                    return ()
                                                    i <- StateT pragmaItems
                                                    return (t : i),
                                                 do t <- StateT typToken
                                                    return [t]]
                pragmaEnd88_93 = foldl1 mplus [do _ <- StateT spaces
                                                  return ()
                                                  d765_217 <- get
                                                  t226_218 <- StateT char
                                                  case t226_218 of
                                                      '#' -> return ()
                                                      _ -> gets position >>= (throwError . mkParseError "'#'" "not match pattern: " "" d765_217 ["char"])
                                                  let '#' = t226_218
                                                  return ()
                                                  d766_219 <- get
                                                  t227_220 <- StateT char
                                                  case t227_220 of
                                                      '-' -> return ()
                                                      _ -> gets position >>= (throwError . mkParseError "'-'" "not match pattern: " "" d766_219 ["char"])
                                                  let '-' = t227_220
                                                  return ()
                                                  d767_221 <- get
                                                  t228_222 <- StateT char
                                                  case t228_222 of
                                                      '}' -> return ()
                                                      _ -> gets position >>= (throwError . mkParseError "'}'" "not match pattern: " "" d767_221 ["char"])
                                                  let '}' = t228_222
                                                  return ()
                                                  return ()]
                moduleDec89_94 = foldl1 mplus [do d768_223 <- get
                                                  t229_224 <- StateT char
                                                  case t229_224 of
                                                      'm' -> return ()
                                                      _ -> gets position >>= (throwError . mkParseError "'m'" "not match pattern: " "" d768_223 ["char"])
                                                  let 'm' = t229_224
                                                  return ()
                                                  d769_225 <- get
                                                  t230_226 <- StateT char
                                                  case t230_226 of
                                                      'o' -> return ()
                                                      _ -> gets position >>= (throwError . mkParseError "'o'" "not match pattern: " "" d769_225 ["char"])
                                                  let 'o' = t230_226
                                                  return ()
                                                  d770_227 <- get
                                                  t231_228 <- StateT char
                                                  case t231_228 of
                                                      'd' -> return ()
                                                      _ -> gets position >>= (throwError . mkParseError "'d'" "not match pattern: " "" d770_227 ["char"])
                                                  let 'd' = t231_228
                                                  return ()
                                                  d771_229 <- get
                                                  t232_230 <- StateT char
                                                  case t232_230 of
                                                      'u' -> return ()
                                                      _ -> gets position >>= (throwError . mkParseError "'u'" "not match pattern: " "" d771_229 ["char"])
                                                  let 'u' = t232_230
                                                  return ()
                                                  d772_231 <- get
                                                  t233_232 <- StateT char
                                                  case t233_232 of
                                                      'l' -> return ()
                                                      _ -> gets position >>= (throwError . mkParseError "'l'" "not match pattern: " "" d772_231 ["char"])
                                                  let 'l' = t233_232
                                                  return ()
                                                  d773_233 <- get
                                                  t234_234 <- StateT char
                                                  case t234_234 of
                                                      'e' -> return ()
                                                      _ -> gets position >>= (throwError . mkParseError "'e'" "not match pattern: " "" d773_233 ["char"])
                                                  let 'e' = t234_234
                                                  return ()
                                                  _ <- StateT spaces
                                                  return ()
                                                  n <- StateT moduleName
                                                  _ <- StateT spaces
                                                  return ()
                                                  d777_235 <- get
                                                  t238_236 <- StateT char
                                                  case t238_236 of
                                                      '(' -> return ()
                                                      _ -> gets position >>= (throwError . mkParseError "'('" "not match pattern: " "" d777_235 ["char"])
                                                  let '(' = t238_236
                                                  return ()
                                                  _ <- StateT spaces
                                                  return ()
                                                  s <- StateT moduleDecStr
                                                  _ <- StateT whr
                                                  return ()
                                                  return (Just (n, Just s)),
                                               do d781_237 <- get
                                                  t242_238 <- StateT char
                                                  case t242_238 of
                                                      'm' -> return ()
                                                      _ -> gets position >>= (throwError . mkParseError "'m'" "not match pattern: " "" d781_237 ["char"])
                                                  let 'm' = t242_238
                                                  return ()
                                                  d782_239 <- get
                                                  t243_240 <- StateT char
                                                  case t243_240 of
                                                      'o' -> return ()
                                                      _ -> gets position >>= (throwError . mkParseError "'o'" "not match pattern: " "" d782_239 ["char"])
                                                  let 'o' = t243_240
                                                  return ()
                                                  d783_241 <- get
                                                  t244_242 <- StateT char
                                                  case t244_242 of
                                                      'd' -> return ()
                                                      _ -> gets position >>= (throwError . mkParseError "'d'" "not match pattern: " "" d783_241 ["char"])
                                                  let 'd' = t244_242
                                                  return ()
                                                  d784_243 <- get
                                                  t245_244 <- StateT char
                                                  case t245_244 of
                                                      'u' -> return ()
                                                      _ -> gets position >>= (throwError . mkParseError "'u'" "not match pattern: " "" d784_243 ["char"])
                                                  let 'u' = t245_244
                                                  return ()
                                                  d785_245 <- get
                                                  t246_246 <- StateT char
                                                  case t246_246 of
                                                      'l' -> return ()
                                                      _ -> gets position >>= (throwError . mkParseError "'l'" "not match pattern: " "" d785_245 ["char"])
                                                  let 'l' = t246_246
                                                  return ()
                                                  d786_247 <- get
                                                  t247_248 <- StateT char
                                                  case t247_248 of
                                                      'e' -> return ()
                                                      _ -> gets position >>= (throwError . mkParseError "'e'" "not match pattern: " "" d786_247 ["char"])
                                                  let 'e' = t247_248
                                                  return ()
                                                  _ <- StateT spaces
                                                  return ()
                                                  n <- StateT moduleName
                                                  _ <- StateT spaces
                                                  return ()
                                                  d790_249 <- get
                                                  t251_250 <- StateT char
                                                  case t251_250 of
                                                      'w' -> return ()
                                                      _ -> gets position >>= (throwError . mkParseError "'w'" "not match pattern: " "" d790_249 ["char"])
                                                  let 'w' = t251_250
                                                  return ()
                                                  d791_251 <- get
                                                  t252_252 <- StateT char
                                                  case t252_252 of
                                                      'h' -> return ()
                                                      _ -> gets position >>= (throwError . mkParseError "'h'" "not match pattern: " "" d791_251 ["char"])
                                                  let 'h' = t252_252
                                                  return ()
                                                  d792_253 <- get
                                                  t253_254 <- StateT char
                                                  case t253_254 of
                                                      'e' -> return ()
                                                      _ -> gets position >>= (throwError . mkParseError "'e'" "not match pattern: " "" d792_253 ["char"])
                                                  let 'e' = t253_254
                                                  return ()
                                                  d793_255 <- get
                                                  t254_256 <- StateT char
                                                  case t254_256 of
                                                      'r' -> return ()
                                                      _ -> gets position >>= (throwError . mkParseError "'r'" "not match pattern: " "" d793_255 ["char"])
                                                  let 'r' = t254_256
                                                  return ()
                                                  d794_257 <- get
                                                  t255_258 <- StateT char
                                                  case t255_258 of
                                                      'e' -> return ()
                                                      _ -> gets position >>= (throwError . mkParseError "'e'" "not match pattern: " "" d794_257 ["char"])
                                                  let 'e' = t255_258
                                                  return ()
                                                  _ <- StateT spaces
                                                  return ()
                                                  return (Just (n, Nothing)),
                                               return Nothing]
                moduleName90_95 = foldl1 mplus [do t <- StateT typ
                                                   d797_259 <- get
                                                   t258_260 <- StateT char
                                                   case t258_260 of
                                                       '.' -> return ()
                                                       _ -> gets position >>= (throwError . mkParseError "'.'" "not match pattern: " "" d797_259 ["char"])
                                                   let '.' = t258_260
                                                   return ()
                                                   n <- StateT moduleName
                                                   return (t : n),
                                                do t <- StateT typ
                                                   return [t]]
                moduleDecStr91_96 = foldl1 mplus [do d801_261 <- get
                                                     do err1260_262 <- ((do _ <- StateT whr
                                                                            return ()) >> return False) `catchError` const (return True)
                                                        unless err1260_262 (gets position >>= (throwError . mkParseError "!_:whr" "not match: " "" d801_261 ["whr"]))
                                                     put d801_261
                                                     c <- StateT char
                                                     s <- StateT moduleDecStr
                                                     return (c : s),
                                                  return ""]
                whr92_97 = foldl1 mplus [do _ <- StateT spaces
                                            return ()
                                            d805_263 <- get
                                            t265_264 <- StateT char
                                            case t265_264 of
                                                ')' -> return ()
                                                _ -> gets position >>= (throwError . mkParseError "')'" "not match pattern: " "" d805_263 ["char"])
                                            let ')' = t265_264
                                            return ()
                                            _ <- StateT spaces
                                            return ()
                                            d807_265 <- get
                                            t267_266 <- StateT char
                                            case t267_266 of
                                                'w' -> return ()
                                                _ -> gets position >>= (throwError . mkParseError "'w'" "not match pattern: " "" d807_265 ["char"])
                                            let 'w' = t267_266
                                            return ()
                                            d808_267 <- get
                                            t268_268 <- StateT char
                                            case t268_268 of
                                                'h' -> return ()
                                                _ -> gets position >>= (throwError . mkParseError "'h'" "not match pattern: " "" d808_267 ["char"])
                                            let 'h' = t268_268
                                            return ()
                                            d809_269 <- get
                                            t269_270 <- StateT char
                                            case t269_270 of
                                                'e' -> return ()
                                                _ -> gets position >>= (throwError . mkParseError "'e'" "not match pattern: " "" d809_269 ["char"])
                                            let 'e' = t269_270
                                            return ()
                                            d810_271 <- get
                                            t270_272 <- StateT char
                                            case t270_272 of
                                                'r' -> return ()
                                                _ -> gets position >>= (throwError . mkParseError "'r'" "not match pattern: " "" d810_271 ["char"])
                                            let 'r' = t270_272
                                            return ()
                                            d811_273 <- get
                                            t271_274 <- StateT char
                                            case t271_274 of
                                                'e' -> return ()
                                                _ -> gets position >>= (throwError . mkParseError "'e'" "not match pattern: " "" d811_273 ["char"])
                                            let 'e' = t271_274
                                            return ()
                                            return ()]
                preImpPap93_98 = foldl1 mplus [do d813_275 <- get
                                                  do err1261_276 <- ((do _ <- StateT importPapillon
                                                                         return ()) >> return False) `catchError` const (return True)
                                                     unless err1261_276 (gets position >>= (throwError . mkParseError "!_:importPapillon" "not match: " "" d813_275 ["importPapillon"]))
                                                  put d813_275
                                                  d815_277 <- get
                                                  do err1262_278 <- ((do _ <- StateT pap
                                                                         return ()) >> return False) `catchError` const (return True)
                                                     unless err1262_278 (gets position >>= (throwError . mkParseError "!_:pap" "not match: " "" d815_277 ["pap"]))
                                                  put d815_277
                                                  c <- StateT char
                                                  pip <- StateT preImpPap
                                                  return (c : pip),
                                               return ""]
                prePeg94_99 = foldl1 mplus [do d819_279 <- get
                                               do err1263_280 <- ((do _ <- StateT pap
                                                                      return ()) >> return False) `catchError` const (return True)
                                                  unless err1263_280 (gets position >>= (throwError . mkParseError "!_:pap" "not match: " "" d819_279 ["pap"]))
                                               put d819_279
                                               c <- StateT char
                                               pp <- StateT prePeg
                                               return (c : pp),
                                            return ""]
                afterPeg95_100 = foldl1 mplus [do c <- StateT char
                                                  atp <- StateT afterPeg
                                                  return (c : atp),
                                               return ""]
                importPapillon96_101 = foldl1 mplus [do d824_281 <- get
                                                        t281_282 <- StateT varToken
                                                        case t281_282 of
                                                            "import" -> return ()
                                                            _ -> gets position >>= (throwError . mkParseError "\"import\"" "not match pattern: " "" d824_281 ["varToken"])
                                                        let "import" = t281_282
                                                        return ()
                                                        d825_283 <- get
                                                        t282_284 <- StateT typToken
                                                        case t282_284 of
                                                            "Text" -> return ()
                                                            _ -> gets position >>= (throwError . mkParseError "\"Text\"" "not match pattern: " "" d825_283 ["typToken"])
                                                        let "Text" = t282_284
                                                        return ()
                                                        d826_285 <- get
                                                        t283_286 <- StateT char
                                                        case t283_286 of
                                                            '.' -> return ()
                                                            _ -> gets position >>= (throwError . mkParseError "'.'" "not match pattern: " "" d826_285 ["char"])
                                                        let '.' = t283_286
                                                        return ()
                                                        _ <- StateT spaces
                                                        return ()
                                                        d828_287 <- get
                                                        t285_288 <- StateT typToken
                                                        case t285_288 of
                                                            "Papillon" -> return ()
                                                            _ -> gets position >>= (throwError . mkParseError "\"Papillon\"" "not match pattern: " "" d828_287 ["typToken"])
                                                        let "Papillon" = t285_288
                                                        return ()
                                                        d830_289 <- get
                                                        do err1264_290 <- ((do d829_291 <- get
                                                                               t286_292 <- StateT char
                                                                               case t286_292 of
                                                                                   '.' -> return ()
                                                                                   _ -> gets position >>= (throwError . mkParseError "'.'" "not match pattern: " "" d829_291 ["char"])
                                                                               let '.' = t286_292
                                                                               return ()) >> return False) `catchError` const (return True)
                                                           unless err1264_290 (gets position >>= (throwError . mkParseError "!'.':" "not match: " "" d830_289 ["char"]))
                                                        put d830_289
                                                        return ()]
                varToken97_102 = foldl1 mplus [do v <- StateT variable
                                                  _ <- StateT spaces
                                                  return ()
                                                  return v]
                typToken98_103 = foldl1 mplus [do t <- StateT typ
                                                  _ <- StateT spaces
                                                  return ()
                                                  return t]
                pap99_104 = foldl1 mplus [do d835_293 <- get
                                             t291_294 <- StateT char
                                             case t291_294 of
                                                 '\n' -> return ()
                                                 _ -> gets position >>= (throwError . mkParseError "'\\n'" "not match pattern: " "" d835_293 ["char"])
                                             let '\n' = t291_294
                                             return ()
                                             d836_295 <- get
                                             t292_296 <- StateT char
                                             case t292_296 of
                                                 '[' -> return ()
                                                 _ -> gets position >>= (throwError . mkParseError "'['" "not match pattern: " "" d836_295 ["char"])
                                             let '[' = t292_296
                                             return ()
                                             d837_297 <- get
                                             t293_298 <- StateT char
                                             case t293_298 of
                                                 'p' -> return ()
                                                 _ -> gets position >>= (throwError . mkParseError "'p'" "not match pattern: " "" d837_297 ["char"])
                                             let 'p' = t293_298
                                             return ()
                                             d838_299 <- get
                                             t294_300 <- StateT char
                                             case t294_300 of
                                                 'a' -> return ()
                                                 _ -> gets position >>= (throwError . mkParseError "'a'" "not match pattern: " "" d838_299 ["char"])
                                             let 'a' = t294_300
                                             return ()
                                             d839_301 <- get
                                             t295_302 <- StateT char
                                             case t295_302 of
                                                 'p' -> return ()
                                                 _ -> gets position >>= (throwError . mkParseError "'p'" "not match pattern: " "" d839_301 ["char"])
                                             let 'p' = t295_302
                                             return ()
                                             d840_303 <- get
                                             t296_304 <- StateT char
                                             case t296_304 of
                                                 'i' -> return ()
                                                 _ -> gets position >>= (throwError . mkParseError "'i'" "not match pattern: " "" d840_303 ["char"])
                                             let 'i' = t296_304
                                             return ()
                                             d841_305 <- get
                                             t297_306 <- StateT char
                                             case t297_306 of
                                                 'l' -> return ()
                                                 _ -> gets position >>= (throwError . mkParseError "'l'" "not match pattern: " "" d841_305 ["char"])
                                             let 'l' = t297_306
                                             return ()
                                             d842_307 <- get
                                             t298_308 <- StateT char
                                             case t298_308 of
                                                 'l' -> return ()
                                                 _ -> gets position >>= (throwError . mkParseError "'l'" "not match pattern: " "" d842_307 ["char"])
                                             let 'l' = t298_308
                                             return ()
                                             d843_309 <- get
                                             t299_310 <- StateT char
                                             case t299_310 of
                                                 'o' -> return ()
                                                 _ -> gets position >>= (throwError . mkParseError "'o'" "not match pattern: " "" d843_309 ["char"])
                                             let 'o' = t299_310
                                             return ()
                                             d844_311 <- get
                                             t300_312 <- StateT char
                                             case t300_312 of
                                                 'n' -> return ()
                                                 _ -> gets position >>= (throwError . mkParseError "'n'" "not match pattern: " "" d844_311 ["char"])
                                             let 'n' = t300_312
                                             return ()
                                             d845_313 <- get
                                             t301_314 <- StateT char
                                             case t301_314 of
                                                 '|' -> return ()
                                                 _ -> gets position >>= (throwError . mkParseError "'|'" "not match pattern: " "" d845_313 ["char"])
                                             let '|' = t301_314
                                             return ()
                                             d846_315 <- get
                                             t302_316 <- StateT char
                                             case t302_316 of
                                                 '\n' -> return ()
                                                 _ -> gets position >>= (throwError . mkParseError "'\\n'" "not match pattern: " "" d846_315 ["char"])
                                             let '\n' = t302_316
                                             return ()
                                             return ()]
                peg100_105 = foldl1 mplus [do mt <- optional166_317 (StateT monadType)
                                              _ <- StateT spaces
                                              return ()
                                              s <- StateT sourceType
                                              p <- StateT peg_
                                              return (mt, ConT $ mkName s, p),
                                           do mt <- optional166_317 (StateT monadType)
                                              p <- StateT peg_
                                              return (mt, ConT $ mkName "String", p)]
                monadType101_106 = foldl1 mplus [do _ <- StateT spaces
                                                    return ()
                                                    d854_318 <- get
                                                    t310_319 <- StateT varToken
                                                    case t310_319 of
                                                        "monad" -> return ()
                                                        _ -> gets position >>= (throwError . mkParseError "\"monad\"" "not match pattern: " "" d854_318 ["varToken"])
                                                    let "monad" = t310_319
                                                    return ()
                                                    d855_320 <- get
                                                    t311_321 <- StateT char
                                                    case t311_321 of
                                                        ':' -> return ()
                                                        _ -> gets position >>= (throwError . mkParseError "':'" "not match pattern: " "" d855_320 ["char"])
                                                    let ':' = t311_321
                                                    return ()
                                                    _ <- StateT spaces
                                                    return ()
                                                    t <- StateT hsTypeArr
                                                    return t]
                sourceType102_107 = foldl1 mplus [do d858_322 <- get
                                                     t314_323 <- StateT varToken
                                                     case t314_323 of
                                                         "source" -> return ()
                                                         _ -> gets position >>= (throwError . mkParseError "\"source\"" "not match pattern: " "" d858_322 ["varToken"])
                                                     let "source" = t314_323
                                                     return ()
                                                     d859_324 <- get
                                                     t315_325 <- StateT char
                                                     case t315_325 of
                                                         ':' -> return ()
                                                         _ -> gets position >>= (throwError . mkParseError "':'" "not match pattern: " "" d859_324 ["char"])
                                                     let ':' = t315_325
                                                     return ()
                                                     _ <- StateT spaces
                                                     return ()
                                                     v <- StateT typToken
                                                     return v]
                peg_103_108 = foldl1 mplus [do _ <- StateT spaces
                                               return ()
                                               d <- StateT definition
                                               p <- StateT peg_
                                               return (d : p),
                                            return []]
                definition104_109 = foldl1 mplus [do v <- StateT variable
                                                     _ <- StateT spaces
                                                     return ()
                                                     d867_326 <- get
                                                     t323_327 <- StateT char
                                                     case t323_327 of
                                                         ':' -> return ()
                                                         _ -> gets position >>= (throwError . mkParseError "':'" "not match pattern: " "" d867_326 ["char"])
                                                     let ':' = t323_327
                                                     return ()
                                                     d868_328 <- get
                                                     t324_329 <- StateT char
                                                     case t324_329 of
                                                         ':' -> return ()
                                                         _ -> gets position >>= (throwError . mkParseError "':'" "not match pattern: " "" d868_328 ["char"])
                                                     let ':' = t324_329
                                                     return ()
                                                     _ <- StateT spaces
                                                     return ()
                                                     t <- StateT hsTypeArr
                                                     _ <- StateT spaces
                                                     return ()
                                                     d872_330 <- get
                                                     t328_331 <- StateT char
                                                     case t328_331 of
                                                         '=' -> return ()
                                                         _ -> gets position >>= (throwError . mkParseError "'='" "not match pattern: " "" d872_330 ["char"])
                                                     let '=' = t328_331
                                                     return ()
                                                     _ <- StateT spaces
                                                     return ()
                                                     sel <- StateT selection
                                                     _ <- StateT spaces
                                                     return ()
                                                     d876_332 <- get
                                                     t332_333 <- StateT char
                                                     case t332_333 of
                                                         ';' -> return ()
                                                         _ -> gets position >>= (throwError . mkParseError "';'" "not match pattern: " "" d876_332 ["char"])
                                                     let ';' = t332_333
                                                     return ()
                                                     return (v, Just t, sel),
                                                  do v <- StateT variable
                                                     _ <- StateT spaces
                                                     return ()
                                                     d879_334 <- get
                                                     t335_335 <- StateT char
                                                     case t335_335 of
                                                         '<' -> return ()
                                                         _ -> gets position >>= (throwError . mkParseError "'<'" "not match pattern: " "" d879_334 ["char"])
                                                     let '<' = t335_335
                                                     return ()
                                                     d880_336 <- get
                                                     t336_337 <- StateT char
                                                     case t336_337 of
                                                         '-' -> return ()
                                                         _ -> gets position >>= (throwError . mkParseError "'-'" "not match pattern: " "" d880_336 ["char"])
                                                     let '-' = t336_337
                                                     return ()
                                                     _ <- StateT spaces
                                                     return ()
                                                     sel <- StateT plainSelection
                                                     _ <- StateT spaces
                                                     return ()
                                                     d884_338 <- get
                                                     t340_339 <- StateT char
                                                     case t340_339 of
                                                         ';' -> return ()
                                                         _ -> gets position >>= (throwError . mkParseError "';'" "not match pattern: " "" d884_338 ["char"])
                                                     let ';' = t340_339
                                                     return ()
                                                     return (v, Nothing, Right sel),
                                                  do v <- StateT variable
                                                     _ <- StateT spaces
                                                     return ()
                                                     d887_340 <- get
                                                     t343_341 <- StateT char
                                                     case t343_341 of
                                                         '=' -> return ()
                                                         _ -> gets position >>= (throwError . mkParseError "'='" "not match pattern: " "" d887_340 ["char"])
                                                     let '=' = t343_341
                                                     return ()
                                                     _ <- StateT spaces
                                                     return ()
                                                     sel <- StateT plainSelection
                                                     _ <- StateT spaces
                                                     return ()
                                                     d891_342 <- get
                                                     t347_343 <- StateT char
                                                     case t347_343 of
                                                         ';' -> return ()
                                                         _ -> gets position >>= (throwError . mkParseError "';'" "not match pattern: " "" d891_342 ["char"])
                                                     let ';' = t347_343
                                                     return ()
                                                     return (v, Nothing, Right sel)]
                selection105_110 = foldl1 mplus [Left <$> (return (\x1786_344 -> (x1786_344)) <*> StateT normalSelection),
                                                 Right <$> (return (\x1786_344 -> (x1786_344)) <*> StateT plainSelection)]
                normalSelection106_111 = foldl1 mplus [do ex <- StateT expressionHs
                                                          _ <- StateT spaces
                                                          return ()
                                                          d896_345 <- get
                                                          t350_346 <- StateT char
                                                          case t350_346 of
                                                              '/' -> return ()
                                                              _ -> gets position >>= (throwError . mkParseError "'/'" "not match pattern: " "" d896_345 ["char"])
                                                          let '/' = t350_346
                                                          return ()
                                                          _ <- StateT spaces
                                                          return ()
                                                          sel <- StateT normalSelection
                                                          return (ex : sel),
                                                       do ex <- StateT expressionHs
                                                          return [ex]]
                plainSelection107_112 = foldl1 mplus [do ex <- StateT plainExpressionHs
                                                         _ <- StateT spaces
                                                         return ()
                                                         d902_347 <- get
                                                         t356_348 <- StateT char
                                                         case t356_348 of
                                                             '/' -> return ()
                                                             _ -> gets position >>= (throwError . mkParseError "'/'" "not match pattern: " "" d902_347 ["char"])
                                                         let '/' = t356_348
                                                         return ()
                                                         _ <- StateT spaces
                                                         return ()
                                                         sel <- StateT plainSelection
                                                         return (ex : sel),
                                                      do ex <- StateT plainExpressionHs
                                                         return [ex]]
                expressionHs108_113 = foldl1 mplus [do e <- StateT expression
                                                       _ <- StateT spaces
                                                       return ()
                                                       d908_349 <- get
                                                       t362_350 <- StateT char
                                                       case t362_350 of
                                                           '{' -> return ()
                                                           _ -> gets position >>= (throwError . mkParseError "'{'" "not match pattern: " "" d908_349 ["char"])
                                                       let '{' = t362_350
                                                       return ()
                                                       _ <- StateT spaces
                                                       return ()
                                                       h <- StateT hsExpLam
                                                       _ <- StateT spaces
                                                       return ()
                                                       d912_351 <- get
                                                       t366_352 <- StateT char
                                                       case t366_352 of
                                                           '}' -> return ()
                                                           _ -> gets position >>= (throwError . mkParseError "'}'" "not match pattern: " "" d912_351 ["char"])
                                                       let '}' = t366_352
                                                       return ()
                                                       return (Left (e, h)),
                                                    do e <- StateT expressionHsSugar
                                                       return e]
                expressionHsSugar109_114 = foldl1 mplus [do d914_353 <- get
                                                            t368_354 <- StateT char
                                                            case t368_354 of
                                                                '<' -> return ()
                                                                _ -> gets position >>= (throwError . mkParseError "'<'" "not match pattern: " "" d914_353 ["char"])
                                                            let '<' = t368_354
                                                            return ()
                                                            _ <- StateT spaces
                                                            return ()
                                                            h <- StateT hsExpLam
                                                            _ <- StateT spaces
                                                            return ()
                                                            d918_355 <- get
                                                            t372_356 <- StateT char
                                                            case t372_356 of
                                                                '>' -> return ()
                                                                _ -> gets position >>= (throwError . mkParseError "'>'" "not match pattern: " "" d918_355 ["char"])
                                                            let '>' = t372_356
                                                            return ()
                                                            return (Right h)]
                expressionHsSugar'110_115 = foldl1 mplus [do d919_357 <- get
                                                             t373_358 <- StateT char
                                                             case t373_358 of
                                                                 '<' -> return ()
                                                                 _ -> gets position >>= (throwError . mkParseError "'<'" "not match pattern: " "" d919_357 ["char"])
                                                             let '<' = t373_358
                                                             return ()
                                                             _ <- StateT spaces
                                                             return ()
                                                             h <- StateT hsExpLam'
                                                             _ <- StateT spaces
                                                             return ()
                                                             d923_359 <- get
                                                             t377_360 <- StateT char
                                                             case t377_360 of
                                                                 '>' -> return ()
                                                                 _ -> gets position >>= (throwError . mkParseError "'>'" "not match pattern: " "" d923_359 ["char"])
                                                             let '>' = t377_360
                                                             return ()
                                                             return (Right h)]
                plainExpressionHs111_116 = foldl1 mplus [do rfs <- list167_361 (foldl1 mplus [do rf <- StateT plainHAReadFromLs
                                                                                                 _ <- StateT spaces
                                                                                                 return ()
                                                                                                 return rf])
                                                            return rfs]
                plainHAReadFromLs112_117 = foldl1 mplus [do rf <- StateT plainReadFromLs
                                                            return (Here, rf),
                                                         do d928_362 <- get
                                                            t382_363 <- StateT char
                                                            case t382_363 of
                                                                '&' -> return ()
                                                                _ -> gets position >>= (throwError . mkParseError "'&'" "not match pattern: " "" d928_362 ["char"])
                                                            let '&' = t382_363
                                                            return ()
                                                            rf <- StateT plainReadFromLs
                                                            return (Ahead, rf),
                                                         do d930_364 <- get
                                                            t384_365 <- StateT char
                                                            case t384_365 of
                                                                '!' -> return ()
                                                                _ -> gets position >>= (throwError . mkParseError "'!'" "not match pattern: " "" d930_364 ["char"])
                                                            let '!' = t384_365
                                                            return ()
                                                            rf <- StateT plainReadFromLs
                                                            return (NAhead "", rf)]
                plainReadFromLs113_118 = foldl1 mplus [do rf <- StateT readFromLs
                                                          return rf,
                                                       do rf <- StateT selectCharsLs
                                                          return rf]
                expression114_119 = foldl1 mplus [do l <- StateT nameLeaf_
                                                     _ <- StateT spaces
                                                     return ()
                                                     e <- StateT expression
                                                     return (l : e),
                                                  return []]
                nameLeaf_115_120 = foldl1 mplus [do d937_366 <- get
                                                    t391_367 <- StateT char
                                                    case t391_367 of
                                                        '!' -> return ()
                                                        _ -> gets position >>= (throwError . mkParseError "'!'" "not match pattern: " "" d937_366 ["char"])
                                                    let '!' = t391_367
                                                    return ()
                                                    nl <- StateT nameLeafNoCom
                                                    _ <- StateT spaces
                                                    return ()
                                                    com <- optional166_317 (StateT comForErr)
                                                    return (NAhead $ maybe "" id com, nl),
                                                 do d941_368 <- get
                                                    t395_369 <- StateT char
                                                    case t395_369 of
                                                        '&' -> return ()
                                                        _ -> gets position >>= (throwError . mkParseError "'&'" "not match pattern: " "" d941_368 ["char"])
                                                    let '&' = t395_369
                                                    return ()
                                                    nl <- StateT nameLeaf
                                                    return (Ahead, nl),
                                                 do nl <- StateT nameLeaf
                                                    return (Here, nl)]
                nameLeaf116_121 = foldl1 mplus [do n <- StateT pat1
                                                   _ <- StateT spaces
                                                   return ()
                                                   com <- optional166_317 (StateT comForErr)
                                                   d947_370 <- get
                                                   t401_371 <- StateT char
                                                   case t401_371 of
                                                       ':' -> return ()
                                                       _ -> gets position >>= (throwError . mkParseError "':'" "not match pattern: " "" d947_370 ["char"])
                                                   let ':' = t401_371
                                                   return ()
                                                   (rf, p) <- StateT leaf
                                                   return ((n, maybe "" id com), rf, p),
                                                do n <- StateT pat1
                                                   _ <- StateT spaces
                                                   return ()
                                                   com <- optional166_317 (StateT comForErr)
                                                   return ((n, maybe "" id com),
                                                           FromVariable Nothing,
                                                           Nothing)]
                nameLeafNoCom117_122 = foldl1 mplus [do n <- StateT pat1
                                                        _ <- StateT spaces
                                                        return ()
                                                        com <- optional166_317 (StateT comForErr)
                                                        d955_372 <- get
                                                        t409_373 <- StateT char
                                                        case t409_373 of
                                                            ':' -> return ()
                                                            _ -> gets position >>= (throwError . mkParseError "':'" "not match pattern: " "" d955_372 ["char"])
                                                        let ':' = t409_373
                                                        return ()
                                                        (rf, p) <- StateT leaf
                                                        return ((n, maybe "" id com), rf, p),
                                                     do n <- StateT pat1
                                                        _ <- StateT spaces
                                                        return ()
                                                        return ((n, ""),
                                                                FromVariable Nothing,
                                                                Nothing)]
                comForErr118_123 = foldl1 mplus [do d959_374 <- get
                                                    t413_375 <- StateT char
                                                    case t413_375 of
                                                        '{' -> return ()
                                                        _ -> gets position >>= (throwError . mkParseError "'{'" "not match pattern: " "" d959_374 ["char"])
                                                    let '{' = t413_375
                                                    return ()
                                                    d960_376 <- get
                                                    t414_377 <- StateT char
                                                    case t414_377 of
                                                        '-' -> return ()
                                                        _ -> gets position >>= (throwError . mkParseError "'-'" "not match pattern: " "" d960_376 ["char"])
                                                    let '-' = t414_377
                                                    return ()
                                                    d961_378 <- get
                                                    t415_379 <- StateT char
                                                    case t415_379 of
                                                        '#' -> return ()
                                                        _ -> gets position >>= (throwError . mkParseError "'#'" "not match pattern: " "" d961_378 ["char"])
                                                    let '#' = t415_379
                                                    return ()
                                                    _ <- StateT spaces
                                                    return ()
                                                    d963_380 <- get
                                                    t417_381 <- StateT char
                                                    case t417_381 of
                                                        '"' -> return ()
                                                        _ -> gets position >>= (throwError . mkParseError "'\"'" "not match pattern: " "" d963_380 ["char"])
                                                    let '"' = t417_381
                                                    return ()
                                                    s <- StateT stringLit
                                                    d965_382 <- get
                                                    t419_383 <- StateT char
                                                    case t419_383 of
                                                        '"' -> return ()
                                                        _ -> gets position >>= (throwError . mkParseError "'\"'" "not match pattern: " "" d965_382 ["char"])
                                                    let '"' = t419_383
                                                    return ()
                                                    _ <- StateT spaces
                                                    return ()
                                                    d967_384 <- get
                                                    t421_385 <- StateT char
                                                    case t421_385 of
                                                        '#' -> return ()
                                                        _ -> gets position >>= (throwError . mkParseError "'#'" "not match pattern: " "" d967_384 ["char"])
                                                    let '#' = t421_385
                                                    return ()
                                                    d968_386 <- get
                                                    t422_387 <- StateT char
                                                    case t422_387 of
                                                        '-' -> return ()
                                                        _ -> gets position >>= (throwError . mkParseError "'-'" "not match pattern: " "" d968_386 ["char"])
                                                    let '-' = t422_387
                                                    return ()
                                                    d969_388 <- get
                                                    t423_389 <- StateT char
                                                    case t423_389 of
                                                        '}' -> return ()
                                                        _ -> gets position >>= (throwError . mkParseError "'}'" "not match pattern: " "" d969_388 ["char"])
                                                    let '}' = t423_389
                                                    return ()
                                                    _ <- StateT spaces
                                                    return ()
                                                    return s]
                leaf119_124 = foldl1 mplus [do rf <- StateT readFromLs
                                               t <- StateT test
                                               return (rf, Just t),
                                            do rf <- StateT readFromLs
                                               return (rf, Nothing),
                                            do t <- StateT test
                                               return (FromVariable Nothing, Just t)]
                patOp120_125 = foldl1 mplus [do p <- StateT pat
                                                o <- StateT opConName
                                                po <- StateT patOp
                                                return (UInfixP p o po),
                                             do p <- StateT pat
                                                _ <- StateT spaces
                                                return ()
                                                d980_390 <- get
                                                t434_391 <- StateT char
                                                case t434_391 of
                                                    '`' -> return ()
                                                    _ -> gets position >>= (throwError . mkParseError "'`'" "not match pattern: " "" d980_390 ["char"])
                                                let '`' = t434_391
                                                return ()
                                                t <- StateT typ
                                                d982_392 <- get
                                                t436_393 <- StateT char
                                                case t436_393 of
                                                    '`' -> return ()
                                                    _ -> gets position >>= (throwError . mkParseError "'`'" "not match pattern: " "" d982_392 ["char"])
                                                let '`' = t436_393
                                                return ()
                                                _ <- StateT spaces
                                                return ()
                                                po <- StateT patOp
                                                return (UInfixP p (mkName t) po),
                                             do p <- StateT pat
                                                return p]
                pat121_126 = foldl1 mplus [do t <- StateT typ
                                              _ <- StateT spaces
                                              return ()
                                              ps <- StateT pats
                                              return (ConP (mkName t) ps),
                                           do d989_394 <- get
                                              t443_395 <- StateT char
                                              case t443_395 of
                                                  '(' -> return ()
                                                  _ -> gets position >>= (throwError . mkParseError "'('" "not match pattern: " "" d989_394 ["char"])
                                              let '(' = t443_395
                                              return ()
                                              o <- StateT opConName
                                              d991_396 <- get
                                              t445_397 <- StateT char
                                              case t445_397 of
                                                  ')' -> return ()
                                                  _ -> gets position >>= (throwError . mkParseError "')'" "not match pattern: " "" d991_396 ["char"])
                                              let ')' = t445_397
                                              return ()
                                              _ <- StateT spaces
                                              return ()
                                              ps <- StateT pats
                                              return (ConP o ps),
                                           do p <- StateT pat1
                                              return p]
                pat1122_127 = foldl1 mplus [do t <- StateT typ
                                               return (ConP (mkName t) []),
                                            do d996_398 <- get
                                               t450_399 <- StateT variable
                                               case t450_399 of
                                                   "_" -> return ()
                                                   _ -> gets position >>= (throwError . mkParseError "\"_\"" "not match pattern: " "" d996_398 ["variable"])
                                               let "_" = t450_399
                                               return ()
                                               return WildP,
                                            do n <- StateT variable
                                               return (VarP $ mkName n),
                                            do i <- StateT integer
                                               return (LitP (IntegerL i)),
                                            do d999_400 <- get
                                               t453_401 <- StateT char
                                               case t453_401 of
                                                   '-' -> return ()
                                                   _ -> gets position >>= (throwError . mkParseError "'-'" "not match pattern: " "" d999_400 ["char"])
                                               let '-' = t453_401
                                               return ()
                                               _ <- StateT spaces
                                               return ()
                                               i <- StateT integer
                                               return (LitP (IntegerL $ negate i)),
                                            do d1002_402 <- get
                                               t456_403 <- StateT char
                                               case t456_403 of
                                                   '\'' -> return ()
                                                   _ -> gets position >>= (throwError . mkParseError "'\\''" "not match pattern: " "" d1002_402 ["char"])
                                               let '\'' = t456_403
                                               return ()
                                               c <- StateT charLit
                                               d1004_404 <- get
                                               t458_405 <- StateT char
                                               case t458_405 of
                                                   '\'' -> return ()
                                                   _ -> gets position >>= (throwError . mkParseError "'\\''" "not match pattern: " "" d1004_404 ["char"])
                                               let '\'' = t458_405
                                               return ()
                                               return (LitP $ CharL c),
                                            do d1005_406 <- get
                                               t459_407 <- StateT char
                                               case t459_407 of
                                                   '"' -> return ()
                                                   _ -> gets position >>= (throwError . mkParseError "'\"'" "not match pattern: " "" d1005_406 ["char"])
                                               let '"' = t459_407
                                               return ()
                                               s <- StateT stringLit
                                               d1007_408 <- get
                                               t461_409 <- StateT char
                                               case t461_409 of
                                                   '"' -> return ()
                                                   _ -> gets position >>= (throwError . mkParseError "'\"'" "not match pattern: " "" d1007_408 ["char"])
                                               let '"' = t461_409
                                               return ()
                                               return (LitP $ StringL s),
                                            do d1008_410 <- get
                                               t462_411 <- StateT char
                                               case t462_411 of
                                                   '(' -> return ()
                                                   _ -> gets position >>= (throwError . mkParseError "'('" "not match pattern: " "" d1008_410 ["char"])
                                               let '(' = t462_411
                                               return ()
                                               p <- StateT patList
                                               d1010_412 <- get
                                               t464_413 <- StateT char
                                               case t464_413 of
                                                   ')' -> return ()
                                                   _ -> gets position >>= (throwError . mkParseError "')'" "not match pattern: " "" d1010_412 ["char"])
                                               let ')' = t464_413
                                               return ()
                                               return (TupP p),
                                            do d1011_414 <- get
                                               t465_415 <- StateT char
                                               case t465_415 of
                                                   '[' -> return ()
                                                   _ -> gets position >>= (throwError . mkParseError "'['" "not match pattern: " "" d1011_414 ["char"])
                                               let '[' = t465_415
                                               return ()
                                               p <- StateT patList
                                               d1013_416 <- get
                                               t467_417 <- StateT char
                                               case t467_417 of
                                                   ']' -> return ()
                                                   _ -> gets position >>= (throwError . mkParseError "']'" "not match pattern: " "" d1013_416 ["char"])
                                               let ']' = t467_417
                                               return ()
                                               return (ListP p)]
                patList123_128 = foldl1 mplus [do p <- StateT patOp
                                                  _ <- StateT spaces
                                                  return ()
                                                  d1016_418 <- get
                                                  t470_419 <- StateT char
                                                  case t470_419 of
                                                      ',' -> return ()
                                                      _ -> gets position >>= (throwError . mkParseError "','" "not match pattern: " "" d1016_418 ["char"])
                                                  let ',' = t470_419
                                                  return ()
                                                  _ <- StateT spaces
                                                  return ()
                                                  ps <- StateT patList
                                                  return (p : ps),
                                               do p <- StateT patOp
                                                  return [p],
                                               return []]
                opConName124_129 = foldl1 mplus [do d1020_420 <- get
                                                    t474_421 <- StateT char
                                                    case t474_421 of
                                                        ':' -> return ()
                                                        _ -> gets position >>= (throwError . mkParseError "':'" "not match pattern: " "" d1020_420 ["char"])
                                                    let ':' = t474_421
                                                    return ()
                                                    ot <- StateT opTail
                                                    return (mkName $ ':' : ot)]
                charLit125_130 = foldl1 mplus [do d1022_422 <- get
                                                  t476_423 <- StateT char
                                                  let c = t476_423
                                                  b1578_424 <- return (c `notElem` "\\'")
                                                  unless b1578_424 (gets position >>= (throwError . mkParseError "c `notElem` \"\\\\'\"" "not match: " "" d1022_422 ["char"]))
                                                  return c,
                                               do d1023_425 <- get
                                                  t477_426 <- StateT char
                                                  case t477_426 of
                                                      '\\' -> return ()
                                                      _ -> gets position >>= (throwError . mkParseError "'\\\\'" "not match pattern: " "" d1023_425 ["char"])
                                                  let '\\' = t477_426
                                                  return ()
                                                  c <- StateT escapeC
                                                  return c]
                stringLit126_131 = foldl1 mplus [do d1025_427 <- get
                                                    t479_428 <- StateT char
                                                    let c = t479_428
                                                    b1581_429 <- return (c `notElem` "\"\\")
                                                    unless b1581_429 (gets position >>= (throwError . mkParseError "c `notElem` \"\\\"\\\\\"" "not match: " "" d1025_427 ["char"]))
                                                    s <- StateT stringLit
                                                    return (c : s),
                                                 do d1027_430 <- get
                                                    t481_431 <- StateT char
                                                    case t481_431 of
                                                        '\\' -> return ()
                                                        _ -> gets position >>= (throwError . mkParseError "'\\\\'" "not match pattern: " "" d1027_430 ["char"])
                                                    let '\\' = t481_431
                                                    return ()
                                                    c <- StateT escapeC
                                                    s <- StateT stringLit
                                                    return (c : s),
                                                 return ""]
                escapeC127_132 = foldl1 mplus [do d1030_432 <- get
                                                  t484_433 <- StateT char
                                                  case t484_433 of
                                                      '"' -> return ()
                                                      _ -> gets position >>= (throwError . mkParseError "'\"'" "not match pattern: " "" d1030_432 ["char"])
                                                  let '"' = t484_433
                                                  return ()
                                                  return '"',
                                               do d1031_434 <- get
                                                  t485_435 <- StateT char
                                                  case t485_435 of
                                                      '\'' -> return ()
                                                      _ -> gets position >>= (throwError . mkParseError "'\\''" "not match pattern: " "" d1031_434 ["char"])
                                                  let '\'' = t485_435
                                                  return ()
                                                  return '\'',
                                               do d1032_436 <- get
                                                  t486_437 <- StateT char
                                                  case t486_437 of
                                                      '\\' -> return ()
                                                      _ -> gets position >>= (throwError . mkParseError "'\\\\'" "not match pattern: " "" d1032_436 ["char"])
                                                  let '\\' = t486_437
                                                  return ()
                                                  return '\\',
                                               do d1033_438 <- get
                                                  t487_439 <- StateT char
                                                  case t487_439 of
                                                      'n' -> return ()
                                                      _ -> gets position >>= (throwError . mkParseError "'n'" "not match pattern: " "" d1033_438 ["char"])
                                                  let 'n' = t487_439
                                                  return ()
                                                  return '\n',
                                               do d1034_440 <- get
                                                  t488_441 <- StateT char
                                                  case t488_441 of
                                                      't' -> return ()
                                                      _ -> gets position >>= (throwError . mkParseError "'t'" "not match pattern: " "" d1034_440 ["char"])
                                                  let 't' = t488_441
                                                  return ()
                                                  return '\t']
                pats128_133 = foldl1 mplus [do p <- StateT pat
                                               _ <- StateT spaces
                                               return ()
                                               ps <- StateT pats
                                               return (p : ps),
                                            return []]
                readFromLs129_134 = foldl1 mplus [do rf <- StateT readFrom
                                                     d1039_442 <- get
                                                     t493_443 <- StateT char
                                                     case t493_443 of
                                                         '*' -> return ()
                                                         _ -> gets position >>= (throwError . mkParseError "'*'" "not match pattern: " "" d1039_442 ["char"])
                                                     let '*' = t493_443
                                                     return ()
                                                     return (FromL List rf),
                                                  do rf <- StateT readFrom
                                                     d1041_444 <- get
                                                     t495_445 <- StateT char
                                                     case t495_445 of
                                                         '+' -> return ()
                                                         _ -> gets position >>= (throwError . mkParseError "'+'" "not match pattern: " "" d1041_444 ["char"])
                                                     let '+' = t495_445
                                                     return ()
                                                     return (FromL List1 rf),
                                                  do rf <- StateT readFrom
                                                     d1043_446 <- get
                                                     t497_447 <- StateT char
                                                     case t497_447 of
                                                         '?' -> return ()
                                                         _ -> gets position >>= (throwError . mkParseError "'?'" "not match pattern: " "" d1043_446 ["char"])
                                                     let '?' = t497_447
                                                     return ()
                                                     return (FromL Optional rf),
                                                  do rf <- StateT readFrom
                                                     return rf]
                readFrom130_135 = foldl1 mplus [do v <- StateT variable
                                                   return (FromVariable $ Just v),
                                                do d1046_448 <- get
                                                   t500_449 <- StateT char
                                                   case t500_449 of
                                                       '(' -> return ()
                                                       _ -> gets position >>= (throwError . mkParseError "'('" "not match pattern: " "" d1046_448 ["char"])
                                                   let '(' = t500_449
                                                   return ()
                                                   s <- StateT selection
                                                   d1048_450 <- get
                                                   t502_451 <- StateT char
                                                   case t502_451 of
                                                       ')' -> return ()
                                                       _ -> gets position >>= (throwError . mkParseError "')'" "not match pattern: " "" d1048_450 ["char"])
                                                   let ')' = t502_451
                                                   return ()
                                                   return (FromSelection s),
                                                do e <- StateT expressionHsSugar'
                                                   return (FromSelection $ Left [e])]
                selectCharsLs131_136 = foldl1 mplus [do rf <- StateT selectChars
                                                        d1051_452 <- get
                                                        t505_453 <- StateT char
                                                        case t505_453 of
                                                            '*' -> return ()
                                                            _ -> gets position >>= (throwError . mkParseError "'*'" "not match pattern: " "" d1051_452 ["char"])
                                                        let '*' = t505_453
                                                        return ()
                                                        return (FromL List rf),
                                                     do rf <- StateT selectChars
                                                        d1053_454 <- get
                                                        t507_455 <- StateT char
                                                        case t507_455 of
                                                            '+' -> return ()
                                                            _ -> gets position >>= (throwError . mkParseError "'+'" "not match pattern: " "" d1053_454 ["char"])
                                                        let '+' = t507_455
                                                        return ()
                                                        return (FromL List1 rf),
                                                     do rf <- StateT selectChars
                                                        d1055_456 <- get
                                                        t509_457 <- StateT char
                                                        case t509_457 of
                                                            '?' -> return ()
                                                            _ -> gets position >>= (throwError . mkParseError "'?'" "not match pattern: " "" d1055_456 ["char"])
                                                        let '?' = t509_457
                                                        return ()
                                                        return (FromL Optional rf),
                                                     do rf <- StateT selectChars
                                                        return rf]
                selectChars132_137 = foldl1 mplus [do d1057_458 <- get
                                                      t511_459 <- StateT char
                                                      case t511_459 of
                                                          '[' -> return ()
                                                          _ -> gets position >>= (throwError . mkParseError "'['" "not match pattern: " "" d1057_458 ["char"])
                                                      let '[' = t511_459
                                                      return ()
                                                      cs <- list1168_460 (foldl1 mplus [do d1059_461 <- get
                                                                                           t513_462 <- StateT char
                                                                                           let c1785_171 = t513_462
                                                                                           b1615_463 <- return ((`notElem` "\\-") c1785_171)
                                                                                           unless b1615_463 (gets position >>= (throwError . mkParseError "(`notElem` \"\\\\-\") c1785_0" "not match: " "" d1059_461 ["char"]))
                                                                                           return c1785_171])
                                                      d1060_464 <- get
                                                      t514_465 <- StateT char
                                                      case t514_465 of
                                                          ']' -> return ()
                                                          _ -> gets position >>= (throwError . mkParseError "']'" "not match pattern: " "" d1060_464 ["char"])
                                                      let ']' = t514_465
                                                      return ()
                                                      return (charList cs),
                                                   do d1061_466 <- get
                                                      t515_467 <- StateT char
                                                      case t515_467 of
                                                          '[' -> return ()
                                                          _ -> gets position >>= (throwError . mkParseError "'['" "not match pattern: " "" d1061_466 ["char"])
                                                      let '[' = t515_467
                                                      return ()
                                                      d1062_468 <- get
                                                      t516_469 <- StateT char
                                                      let cb = t516_469
                                                      b1618_470 <- return (cb `notElem` "\\-")
                                                      unless b1618_470 (gets position >>= (throwError . mkParseError "cb `notElem` \"\\\\-\"" "not match: " "" d1062_468 ["char"]))
                                                      d1063_471 <- get
                                                      t517_472 <- StateT char
                                                      case t517_472 of
                                                          '-' -> return ()
                                                          _ -> gets position >>= (throwError . mkParseError "'-'" "not match pattern: " "" d1063_471 ["char"])
                                                      let '-' = t517_472
                                                      return ()
                                                      d1064_473 <- get
                                                      t518_474 <- StateT char
                                                      let ce = t518_474
                                                      b1620_475 <- return (ce `notElem` "\\-")
                                                      unless b1620_475 (gets position >>= (throwError . mkParseError "ce `notElem` \"\\\\-\"" "not match: " "" d1064_473 ["char"]))
                                                      d1065_476 <- get
                                                      t519_477 <- StateT char
                                                      case t519_477 of
                                                          ']' -> return ()
                                                          _ -> gets position >>= (throwError . mkParseError "']'" "not match pattern: " "" d1065_476 ["char"])
                                                      let ']' = t519_477
                                                      return ()
                                                      return (charList [cb .. ce]),
                                                   do d1066_478 <- get
                                                      t520_479 <- StateT char
                                                      case t520_479 of
                                                          '\'' -> return ()
                                                          _ -> gets position >>= (throwError . mkParseError "'\\''" "not match pattern: " "" d1066_478 ["char"])
                                                      let '\'' = t520_479
                                                      return ()
                                                      d1067_480 <- get
                                                      t521_481 <- StateT char
                                                      let c = t521_481
                                                      b1623_482 <- return (c `notElem` "\\'")
                                                      unless b1623_482 (gets position >>= (throwError . mkParseError "c `notElem` \"\\\\'\"" "not match: " "" d1067_480 ["char"]))
                                                      d1068_483 <- get
                                                      t522_484 <- StateT char
                                                      case t522_484 of
                                                          '\'' -> return ()
                                                          _ -> gets position >>= (throwError . mkParseError "'\\''" "not match pattern: " "" d1068_483 ["char"])
                                                      let '\'' = t522_484
                                                      return ()
                                                      return (charList [c])]
                test133_138 = foldl1 mplus [do d1069_485 <- get
                                               t523_486 <- StateT char
                                               case t523_486 of
                                                   '[' -> return ()
                                                   _ -> gets position >>= (throwError . mkParseError "'['" "not match pattern: " "" d1069_485 ["char"])
                                               let '[' = t523_486
                                               return ()
                                               h <- StateT hsExpLam
                                               _ <- StateT spaces
                                               return ()
                                               com <- optional166_317 (StateT comForErr)
                                               d1073_487 <- get
                                               t527_488 <- StateT char
                                               case t527_488 of
                                                   ']' -> return ()
                                                   _ -> gets position >>= (throwError . mkParseError "']'" "not match pattern: " "" d1073_487 ["char"])
                                               let ']' = t527_488
                                               return ()
                                               return (h, maybe "" id com)]
                hsExpLam134_139 = foldl1 mplus [do d1074_489 <- get
                                                   t528_490 <- StateT char
                                                   case t528_490 of
                                                       '\\' -> return ()
                                                       _ -> gets position >>= (throwError . mkParseError "'\\\\'" "not match pattern: " "" d1074_489 ["char"])
                                                   let '\\' = t528_490
                                                   return ()
                                                   _ <- StateT spaces
                                                   return ()
                                                   ps <- StateT pats
                                                   _ <- StateT spaces
                                                   return ()
                                                   d1078_491 <- get
                                                   t532_492 <- StateT char
                                                   case t532_492 of
                                                       '-' -> return ()
                                                       _ -> gets position >>= (throwError . mkParseError "'-'" "not match pattern: " "" d1078_491 ["char"])
                                                   let '-' = t532_492
                                                   return ()
                                                   d1079_493 <- get
                                                   t533_494 <- StateT char
                                                   case t533_494 of
                                                       '>' -> return ()
                                                       _ -> gets position >>= (throwError . mkParseError "'>'" "not match pattern: " "" d1079_493 ["char"])
                                                   let '>' = t533_494
                                                   return ()
                                                   _ <- StateT spaces
                                                   return ()
                                                   e <- StateT hsExpTyp
                                                   return (LamE ps e),
                                                do e <- StateT hsExpTyp
                                                   return e]
                hsExpTyp135_140 = foldl1 mplus [do eo <- StateT hsExpOp
                                                   d1084_495 <- get
                                                   t538_496 <- StateT char
                                                   case t538_496 of
                                                       ':' -> return ()
                                                       _ -> gets position >>= (throwError . mkParseError "':'" "not match pattern: " "" d1084_495 ["char"])
                                                   let ':' = t538_496
                                                   return ()
                                                   d1085_497 <- get
                                                   t539_498 <- StateT char
                                                   case t539_498 of
                                                       ':' -> return ()
                                                       _ -> gets position >>= (throwError . mkParseError "':'" "not match pattern: " "" d1085_497 ["char"])
                                                   let ':' = t539_498
                                                   return ()
                                                   _ <- StateT spaces
                                                   return ()
                                                   t <- StateT hsTypeArr
                                                   return (SigE eo t),
                                                do eo <- StateT hsExpOp
                                                   return eo]
                hsExpOp136_141 = foldl1 mplus [do l <- StateT hsExp
                                                  _ <- StateT spaces
                                                  return ()
                                                  o <- StateT hsOp
                                                  _ <- StateT spaces
                                                  return ()
                                                  r <- StateT hsExpOp
                                                  return (UInfixE (l id) o r),
                                               do e <- StateT hsExp
                                                  return (e id)]
                hsOp137_142 = foldl1 mplus [do d1095_499 <- get
                                               t549_500 <- StateT char
                                               let c = t549_500
                                               b1651_501 <- return (c `elem` "+*/-!|&.^=<>$")
                                               unless b1651_501 (gets position >>= (throwError . mkParseError "c `elem` \"+*/-!|&.^=<>$\"" "not match: " "" d1095_499 ["char"]))
                                               o <- StateT opTail
                                               return (VarE $ mkName $ c : o),
                                            do d1097_502 <- get
                                               t551_503 <- StateT char
                                               case t551_503 of
                                                   ':' -> return ()
                                                   _ -> gets position >>= (throwError . mkParseError "':'" "not match pattern: " "" d1097_502 ["char"])
                                               let ':' = t551_503
                                               return ()
                                               d1099_504 <- get
                                               do err1265_505 <- ((do d1098_506 <- get
                                                                      t552_507 <- StateT char
                                                                      case t552_507 of
                                                                          ':' -> return ()
                                                                          _ -> gets position >>= (throwError . mkParseError "':'" "not match pattern: " "" d1098_506 ["char"])
                                                                      let ':' = t552_507
                                                                      return ()) >> return False) `catchError` const (return True)
                                                  unless err1265_505 (gets position >>= (throwError . mkParseError "!':':" "not match: " "" d1099_504 ["char"]))
                                               put d1099_504
                                               o <- StateT opTail
                                               return (ConE $ mkName $ ':' : o),
                                            do d1101_508 <- get
                                               t554_509 <- StateT char
                                               case t554_509 of
                                                   '`' -> return ()
                                                   _ -> gets position >>= (throwError . mkParseError "'`'" "not match pattern: " "" d1101_508 ["char"])
                                               let '`' = t554_509
                                               return ()
                                               v <- StateT variable
                                               d1103_510 <- get
                                               t556_511 <- StateT char
                                               case t556_511 of
                                                   '`' -> return ()
                                                   _ -> gets position >>= (throwError . mkParseError "'`'" "not match pattern: " "" d1103_510 ["char"])
                                               let '`' = t556_511
                                               return ()
                                               return (VarE $ mkName v),
                                            do d1104_512 <- get
                                               t557_513 <- StateT char
                                               case t557_513 of
                                                   '`' -> return ()
                                                   _ -> gets position >>= (throwError . mkParseError "'`'" "not match pattern: " "" d1104_512 ["char"])
                                               let '`' = t557_513
                                               return ()
                                               t <- StateT typ
                                               d1106_514 <- get
                                               t559_515 <- StateT char
                                               case t559_515 of
                                                   '`' -> return ()
                                                   _ -> gets position >>= (throwError . mkParseError "'`'" "not match pattern: " "" d1106_514 ["char"])
                                               let '`' = t559_515
                                               return ()
                                               return (ConE $ mkName t)]
                hsExpLam'138_143 = foldl1 mplus [do d1107_516 <- get
                                                    t560_517 <- StateT char
                                                    case t560_517 of
                                                        '\\' -> return ()
                                                        _ -> gets position >>= (throwError . mkParseError "'\\\\'" "not match pattern: " "" d1107_516 ["char"])
                                                    let '\\' = t560_517
                                                    return ()
                                                    _ <- StateT spaces
                                                    return ()
                                                    ps <- StateT pats
                                                    _ <- StateT spaces
                                                    return ()
                                                    d1111_518 <- get
                                                    t564_519 <- StateT char
                                                    case t564_519 of
                                                        '-' -> return ()
                                                        _ -> gets position >>= (throwError . mkParseError "'-'" "not match pattern: " "" d1111_518 ["char"])
                                                    let '-' = t564_519
                                                    return ()
                                                    d1112_520 <- get
                                                    t565_521 <- StateT char
                                                    case t565_521 of
                                                        '>' -> return ()
                                                        _ -> gets position >>= (throwError . mkParseError "'>'" "not match pattern: " "" d1112_520 ["char"])
                                                    let '>' = t565_521
                                                    return ()
                                                    _ <- StateT spaces
                                                    return ()
                                                    e <- StateT hsExpTyp'
                                                    return (LamE ps e),
                                                 do e <- StateT hsExpTyp'
                                                    return e]
                hsExpTyp'139_144 = foldl1 mplus [do eo <- StateT hsExpOp'
                                                    d1117_522 <- get
                                                    t570_523 <- StateT char
                                                    case t570_523 of
                                                        ':' -> return ()
                                                        _ -> gets position >>= (throwError . mkParseError "':'" "not match pattern: " "" d1117_522 ["char"])
                                                    let ':' = t570_523
                                                    return ()
                                                    d1118_524 <- get
                                                    t571_525 <- StateT char
                                                    case t571_525 of
                                                        ':' -> return ()
                                                        _ -> gets position >>= (throwError . mkParseError "':'" "not match pattern: " "" d1118_524 ["char"])
                                                    let ':' = t571_525
                                                    return ()
                                                    _ <- StateT spaces
                                                    return ()
                                                    t <- StateT hsTypeArr
                                                    return (SigE eo t),
                                                 do eo <- StateT hsExpOp'
                                                    return eo]
                hsExpOp'140_145 = foldl1 mplus [do l <- StateT hsExp
                                                   _ <- StateT spaces
                                                   return ()
                                                   o <- StateT hsOp'
                                                   _ <- StateT spaces
                                                   return ()
                                                   r <- StateT hsExpOp'
                                                   return (UInfixE (l id) o r),
                                                do e <- StateT hsExp
                                                   return (e id)]
                hsOp'141_146 = foldl1 mplus [do d1128_526 <- get
                                                t581_527 <- StateT char
                                                let c = t581_527
                                                b1683_528 <- return (c `elem` "+*/-!|&.^=<$")
                                                unless b1683_528 (gets position >>= (throwError . mkParseError "c `elem` \"+*/-!|&.^=<$\"" "not match: " "" d1128_526 ["char"]))
                                                o <- StateT opTail
                                                return (VarE $ mkName $ c : o),
                                             do d1130_529 <- get
                                                t583_530 <- StateT char
                                                case t583_530 of
                                                    ':' -> return ()
                                                    _ -> gets position >>= (throwError . mkParseError "':'" "not match pattern: " "" d1130_529 ["char"])
                                                let ':' = t583_530
                                                return ()
                                                d1132_531 <- get
                                                do err1266_532 <- ((do d1131_533 <- get
                                                                       t584_534 <- StateT char
                                                                       case t584_534 of
                                                                           ':' -> return ()
                                                                           _ -> gets position >>= (throwError . mkParseError "':'" "not match pattern: " "" d1131_533 ["char"])
                                                                       let ':' = t584_534
                                                                       return ()) >> return False) `catchError` const (return True)
                                                   unless err1266_532 (gets position >>= (throwError . mkParseError "!':':" "not match: " "" d1132_531 ["char"]))
                                                put d1132_531
                                                o <- StateT opTail
                                                return (ConE $ mkName $ ':' : o),
                                             do d1134_535 <- get
                                                t586_536 <- StateT char
                                                case t586_536 of
                                                    '`' -> return ()
                                                    _ -> gets position >>= (throwError . mkParseError "'`'" "not match pattern: " "" d1134_535 ["char"])
                                                let '`' = t586_536
                                                return ()
                                                v <- StateT variable
                                                d1136_537 <- get
                                                t588_538 <- StateT char
                                                case t588_538 of
                                                    '`' -> return ()
                                                    _ -> gets position >>= (throwError . mkParseError "'`'" "not match pattern: " "" d1136_537 ["char"])
                                                let '`' = t588_538
                                                return ()
                                                return (VarE $ mkName v),
                                             do d1137_539 <- get
                                                t589_540 <- StateT char
                                                case t589_540 of
                                                    '`' -> return ()
                                                    _ -> gets position >>= (throwError . mkParseError "'`'" "not match pattern: " "" d1137_539 ["char"])
                                                let '`' = t589_540
                                                return ()
                                                t <- StateT typ
                                                d1139_541 <- get
                                                t591_542 <- StateT char
                                                case t591_542 of
                                                    '`' -> return ()
                                                    _ -> gets position >>= (throwError . mkParseError "'`'" "not match pattern: " "" d1139_541 ["char"])
                                                let '`' = t591_542
                                                return ()
                                                return (ConE $ mkName t)]
                opTail142_147 = foldl1 mplus [do d1140_543 <- get
                                                 t592_544 <- StateT char
                                                 let c = t592_544
                                                 b1694_545 <- return (c `elem` ":+*/-!|&.^=<>$")
                                                 unless b1694_545 (gets position >>= (throwError . mkParseError "c `elem` \":+*/-!|&.^=<>$\"" "not match: " "" d1140_543 ["char"]))
                                                 s <- StateT opTail
                                                 return (c : s),
                                              return ""]
                hsExp143_148 = foldl1 mplus [do e <- StateT hsExp1
                                                _ <- StateT spaces
                                                return ()
                                                h <- StateT hsExp
                                                return (\f -> h (f e `AppE`)),
                                             do e <- StateT hsExp1
                                                return (\f -> f e)]
                hsExp1144_149 = foldl1 mplus [do d1146_546 <- get
                                                 t598_547 <- StateT char
                                                 case t598_547 of
                                                     '(' -> return ()
                                                     _ -> gets position >>= (throwError . mkParseError "'('" "not match pattern: " "" d1146_546 ["char"])
                                                 let '(' = t598_547
                                                 return ()
                                                 l <- optional166_317 (foldl1 mplus [do e <- StateT hsExpTyp
                                                                                        return e])
                                                 _ <- StateT spaces
                                                 return ()
                                                 o <- StateT hsOp
                                                 _ <- StateT spaces
                                                 return ()
                                                 r <- optional166_317 (foldl1 mplus [do e <- StateT hsExpTyp
                                                                                        return e])
                                                 d1154_548 <- get
                                                 t606_549 <- StateT char
                                                 case t606_549 of
                                                     ')' -> return ()
                                                     _ -> gets position >>= (throwError . mkParseError "')'" "not match pattern: " "" d1154_548 ["char"])
                                                 let ')' = t606_549
                                                 return ()
                                                 return (InfixE l o r),
                                              do d1155_550 <- get
                                                 t607_551 <- StateT char
                                                 case t607_551 of
                                                     '(' -> return ()
                                                     _ -> gets position >>= (throwError . mkParseError "'('" "not match pattern: " "" d1155_550 ["char"])
                                                 let '(' = t607_551
                                                 return ()
                                                 et <- StateT hsExpTpl
                                                 d1157_552 <- get
                                                 t609_553 <- StateT char
                                                 case t609_553 of
                                                     ')' -> return ()
                                                     _ -> gets position >>= (throwError . mkParseError "')'" "not match pattern: " "" d1157_552 ["char"])
                                                 let ')' = t609_553
                                                 return ()
                                                 return (TupE et),
                                              do d1158_554 <- get
                                                 t610_555 <- StateT char
                                                 case t610_555 of
                                                     '[' -> return ()
                                                     _ -> gets position >>= (throwError . mkParseError "'['" "not match pattern: " "" d1158_554 ["char"])
                                                 let '[' = t610_555
                                                 return ()
                                                 et <- StateT hsExpTpl
                                                 d1160_556 <- get
                                                 t612_557 <- StateT char
                                                 case t612_557 of
                                                     ']' -> return ()
                                                     _ -> gets position >>= (throwError . mkParseError "']'" "not match pattern: " "" d1160_556 ["char"])
                                                 let ']' = t612_557
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
                                              do d1165_558 <- get
                                                 t617_559 <- StateT char
                                                 case t617_559 of
                                                     '\'' -> return ()
                                                     _ -> gets position >>= (throwError . mkParseError "'\\''" "not match pattern: " "" d1165_558 ["char"])
                                                 let '\'' = t617_559
                                                 return ()
                                                 c <- StateT charLit
                                                 d1167_560 <- get
                                                 t619_561 <- StateT char
                                                 case t619_561 of
                                                     '\'' -> return ()
                                                     _ -> gets position >>= (throwError . mkParseError "'\\''" "not match pattern: " "" d1167_560 ["char"])
                                                 let '\'' = t619_561
                                                 return ()
                                                 return (LitE $ charL c),
                                              do d1168_562 <- get
                                                 t620_563 <- StateT char
                                                 case t620_563 of
                                                     '"' -> return ()
                                                     _ -> gets position >>= (throwError . mkParseError "'\"'" "not match pattern: " "" d1168_562 ["char"])
                                                 let '"' = t620_563
                                                 return ()
                                                 s <- StateT stringLit
                                                 d1170_564 <- get
                                                 t622_565 <- StateT char
                                                 case t622_565 of
                                                     '"' -> return ()
                                                     _ -> gets position >>= (throwError . mkParseError "'\"'" "not match pattern: " "" d1170_564 ["char"])
                                                 let '"' = t622_565
                                                 return ()
                                                 return (LitE $ stringL s),
                                              do d1171_566 <- get
                                                 t623_567 <- StateT char
                                                 case t623_567 of
                                                     '-' -> return ()
                                                     _ -> gets position >>= (throwError . mkParseError "'-'" "not match pattern: " "" d1171_566 ["char"])
                                                 let '-' = t623_567
                                                 return ()
                                                 _ <- StateT spaces
                                                 return ()
                                                 e <- StateT hsExp1
                                                 return (AppE (VarE $ mkName "negate") e)]
                hsExpTpl145_150 = foldl1 mplus [do e <- StateT hsExpLam
                                                   _ <- StateT spaces
                                                   return ()
                                                   d1176_568 <- get
                                                   t628_569 <- StateT char
                                                   case t628_569 of
                                                       ',' -> return ()
                                                       _ -> gets position >>= (throwError . mkParseError "','" "not match pattern: " "" d1176_568 ["char"])
                                                   let ',' = t628_569
                                                   return ()
                                                   _ <- StateT spaces
                                                   return ()
                                                   et <- StateT hsExpTpl
                                                   return (e : et),
                                                do e <- StateT hsExpLam
                                                   return [e],
                                                return []]
                hsTypeArr146_151 = foldl1 mplus [do l <- StateT hsType
                                                    d1181_570 <- get
                                                    t633_571 <- StateT char
                                                    case t633_571 of
                                                        '-' -> return ()
                                                        _ -> gets position >>= (throwError . mkParseError "'-'" "not match pattern: " "" d1181_570 ["char"])
                                                    let '-' = t633_571
                                                    return ()
                                                    d1182_572 <- get
                                                    t634_573 <- StateT char
                                                    case t634_573 of
                                                        '>' -> return ()
                                                        _ -> gets position >>= (throwError . mkParseError "'>'" "not match pattern: " "" d1182_572 ["char"])
                                                    let '>' = t634_573
                                                    return ()
                                                    _ <- StateT spaces
                                                    return ()
                                                    r <- StateT hsTypeArr
                                                    return (AppT (AppT ArrowT $ l id) r),
                                                 do t <- StateT hsType
                                                    return (t id)]
                hsType147_152 = foldl1 mplus [do t <- StateT hsType1
                                                 ts <- StateT hsType
                                                 return (\f -> ts (f t `AppT`)),
                                              do t <- StateT hsType1
                                                 return ($ t)]
                hsType1148_153 = foldl1 mplus [do d1189_574 <- get
                                                  t641_575 <- StateT char
                                                  case t641_575 of
                                                      '[' -> return ()
                                                      _ -> gets position >>= (throwError . mkParseError "'['" "not match pattern: " "" d1189_574 ["char"])
                                                  let '[' = t641_575
                                                  return ()
                                                  d1190_576 <- get
                                                  t642_577 <- StateT char
                                                  case t642_577 of
                                                      ']' -> return ()
                                                      _ -> gets position >>= (throwError . mkParseError "']'" "not match pattern: " "" d1190_576 ["char"])
                                                  let ']' = t642_577
                                                  return ()
                                                  _ <- StateT spaces
                                                  return ()
                                                  return ListT,
                                               do d1192_578 <- get
                                                  t644_579 <- StateT char
                                                  case t644_579 of
                                                      '[' -> return ()
                                                      _ -> gets position >>= (throwError . mkParseError "'['" "not match pattern: " "" d1192_578 ["char"])
                                                  let '[' = t644_579
                                                  return ()
                                                  t <- StateT hsTypeArr
                                                  d1194_580 <- get
                                                  t646_581 <- StateT char
                                                  case t646_581 of
                                                      ']' -> return ()
                                                      _ -> gets position >>= (throwError . mkParseError "']'" "not match pattern: " "" d1194_580 ["char"])
                                                  let ']' = t646_581
                                                  return ()
                                                  _ <- StateT spaces
                                                  return ()
                                                  return (ListT `AppT` t),
                                               do d1196_582 <- get
                                                  t648_583 <- StateT char
                                                  case t648_583 of
                                                      '(' -> return ()
                                                      _ -> gets position >>= (throwError . mkParseError "'('" "not match pattern: " "" d1196_582 ["char"])
                                                  let '(' = t648_583
                                                  return ()
                                                  _ <- StateT spaces
                                                  return ()
                                                  tt <- StateT hsTypeTpl
                                                  d1199_584 <- get
                                                  t651_585 <- StateT char
                                                  case t651_585 of
                                                      ')' -> return ()
                                                      _ -> gets position >>= (throwError . mkParseError "')'" "not match pattern: " "" d1199_584 ["char"])
                                                  let ')' = t651_585
                                                  return ()
                                                  _ <- StateT spaces
                                                  return ()
                                                  return (foldl AppT (TupleT $ length tt) tt),
                                               do t <- StateT typToken
                                                  return (ConT $ mkName t),
                                               do d1202_586 <- get
                                                  t654_587 <- StateT char
                                                  case t654_587 of
                                                      '(' -> return ()
                                                      _ -> gets position >>= (throwError . mkParseError "'('" "not match pattern: " "" d1202_586 ["char"])
                                                  let '(' = t654_587
                                                  return ()
                                                  d1203_588 <- get
                                                  t655_589 <- StateT char
                                                  case t655_589 of
                                                      '-' -> return ()
                                                      _ -> gets position >>= (throwError . mkParseError "'-'" "not match pattern: " "" d1203_588 ["char"])
                                                  let '-' = t655_589
                                                  return ()
                                                  d1204_590 <- get
                                                  t656_591 <- StateT char
                                                  case t656_591 of
                                                      '>' -> return ()
                                                      _ -> gets position >>= (throwError . mkParseError "'>'" "not match pattern: " "" d1204_590 ["char"])
                                                  let '>' = t656_591
                                                  return ()
                                                  d1205_592 <- get
                                                  t657_593 <- StateT char
                                                  case t657_593 of
                                                      ')' -> return ()
                                                      _ -> gets position >>= (throwError . mkParseError "')'" "not match pattern: " "" d1205_592 ["char"])
                                                  let ')' = t657_593
                                                  return ()
                                                  _ <- StateT spaces
                                                  return ()
                                                  return ArrowT]
                hsTypeTpl149_154 = foldl1 mplus [do t <- StateT hsTypeArr
                                                    d1208_594 <- get
                                                    t660_595 <- StateT char
                                                    case t660_595 of
                                                        ',' -> return ()
                                                        _ -> gets position >>= (throwError . mkParseError "','" "not match pattern: " "" d1208_594 ["char"])
                                                    let ',' = t660_595
                                                    return ()
                                                    _ <- StateT spaces
                                                    return ()
                                                    tt <- StateT hsTypeTpl
                                                    return (t : tt),
                                                 do t <- StateT hsTypeArr
                                                    return [t],
                                                 return []]
                typ150_155 = foldl1 mplus [do u <- StateT upper
                                              t <- StateT tvtail
                                              return (u : t)]
                variable151_156 = foldl1 mplus [do l <- StateT lower
                                                   t <- StateT tvtail
                                                   return (l : t)]
                tvtail152_157 = foldl1 mplus [do a <- StateT alpha
                                                 t <- StateT tvtail
                                                 return (a : t),
                                              return ""]
                integer153_158 = foldl1 mplus [do dh <- StateT digit
                                                  ds <- list167_361 (foldl1 mplus [do d <- StateT digit
                                                                                      return d])
                                                  return (read $ dh : ds)]
                alpha154_159 = foldl1 mplus [do u <- StateT upper
                                                return u,
                                             do l <- StateT lower
                                                return l,
                                             do d <- StateT digit
                                                return d,
                                             do d1224_596 <- get
                                                t676_597 <- StateT char
                                                case t676_597 of
                                                    '\'' -> return ()
                                                    _ -> gets position >>= (throwError . mkParseError "'\\''" "not match pattern: " "" d1224_596 ["char"])
                                                let '\'' = t676_597
                                                return ()
                                                return '\'']
                upper155_160 = foldl1 mplus [do d1225_598 <- get
                                                t677_599 <- StateT char
                                                let u = t677_599
                                                b1779_600 <- return (isUpper u)
                                                unless b1779_600 (gets position >>= (throwError . mkParseError "isUpper u" "not match: " "" d1225_598 ["char"]))
                                                return u]
                lower156_161 = foldl1 mplus [do d1226_601 <- get
                                                t678_602 <- StateT char
                                                let l = t678_602
                                                b1780_603 <- return (isLower l || l == '_')
                                                unless b1780_603 (gets position >>= (throwError . mkParseError "isLower l || l == '_'" "not match: " "" d1226_601 ["char"]))
                                                return l]
                digit157_162 = foldl1 mplus [do d1227_604 <- get
                                                t679_605 <- StateT char
                                                let d = t679_605
                                                b1781_606 <- return (isDigit d)
                                                unless b1781_606 (gets position >>= (throwError . mkParseError "isDigit d" "not match: " "" d1227_604 ["char"]))
                                                return d]
                spaces158_163 = foldl1 mplus [do _ <- StateT space
                                                 return ()
                                                 _ <- StateT spaces
                                                 return ()
                                                 return (),
                                              return ()]
                space159_164 = foldl1 mplus [do d1230_607 <- get
                                                t682_608 <- StateT char
                                                let s = t682_608
                                                b1784_609 <- return (isSpace s)
                                                unless b1784_609 (gets position >>= (throwError . mkParseError "isSpace s" "not match: " "" d1230_607 ["char"]))
                                                return (),
                                             do d1231_610 <- get
                                                t683_611 <- StateT char
                                                case t683_611 of
                                                    '-' -> return ()
                                                    _ -> gets position >>= (throwError . mkParseError "'-'" "not match pattern: " "" d1231_610 ["char"])
                                                let '-' = t683_611
                                                return ()
                                                d1232_612 <- get
                                                t684_613 <- StateT char
                                                case t684_613 of
                                                    '-' -> return ()
                                                    _ -> gets position >>= (throwError . mkParseError "'-'" "not match pattern: " "" d1232_612 ["char"])
                                                let '-' = t684_613
                                                return ()
                                                _ <- StateT notNLString
                                                return ()
                                                _ <- StateT newLine
                                                return ()
                                                return (),
                                             do _ <- StateT comment
                                                return ()
                                                return ()]
                notNLString160_165 = foldl1 mplus [do d1237_614 <- get
                                                      do err1267_615 <- ((do _ <- StateT newLine
                                                                             return ()) >> return False) `catchError` const (return True)
                                                         unless err1267_615 (gets position >>= (throwError . mkParseError "!_:newLine" "not match: " "" d1237_614 ["newLine"]))
                                                      put d1237_614
                                                      c <- StateT char
                                                      s <- StateT notNLString
                                                      return (c : s),
                                                   return ""]
                newLine161_166 = foldl1 mplus [do d1240_616 <- get
                                                  t691_617 <- StateT char
                                                  case t691_617 of
                                                      '\n' -> return ()
                                                      _ -> gets position >>= (throwError . mkParseError "'\\n'" "not match pattern: " "" d1240_616 ["char"])
                                                  let '\n' = t691_617
                                                  return ()
                                                  return ()]
                comment162_167 = foldl1 mplus [do d1241_618 <- get
                                                  t692_619 <- StateT char
                                                  case t692_619 of
                                                      '{' -> return ()
                                                      _ -> gets position >>= (throwError . mkParseError "'{'" "not match pattern: " "" d1241_618 ["char"])
                                                  let '{' = t692_619
                                                  return ()
                                                  d1242_620 <- get
                                                  t693_621 <- StateT char
                                                  case t693_621 of
                                                      '-' -> return ()
                                                      _ -> gets position >>= (throwError . mkParseError "'-'" "not match pattern: " "" d1242_620 ["char"])
                                                  let '-' = t693_621
                                                  return ()
                                                  d1244_622 <- get
                                                  do err1268_623 <- ((do d1243_624 <- get
                                                                         t694_625 <- StateT char
                                                                         case t694_625 of
                                                                             '#' -> return ()
                                                                             _ -> gets position >>= (throwError . mkParseError "'#'" "not match pattern: " "" d1243_624 ["char"])
                                                                         let '#' = t694_625
                                                                         return ()) >> return False) `catchError` const (return True)
                                                     unless err1268_623 (gets position >>= (throwError . mkParseError "!'#':" "not match: " "" d1244_622 ["char"]))
                                                  put d1244_622
                                                  _ <- StateT comments
                                                  return ()
                                                  _ <- StateT comEnd
                                                  return ()
                                                  return ()]
                comments163_168 = foldl1 mplus [do _ <- StateT notComStr
                                                   return ()
                                                   _ <- StateT comment
                                                   return ()
                                                   _ <- StateT comments
                                                   return ()
                                                   return (),
                                                do _ <- StateT notComStr
                                                   return ()
                                                   return ()]
                notComStr164_169 = foldl1 mplus [do d1252_626 <- get
                                                    do err1269_627 <- ((do _ <- StateT comment
                                                                           return ()) >> return False) `catchError` const (return True)
                                                       unless err1269_627 (gets position >>= (throwError . mkParseError "!_:comment" "not match: " "" d1252_626 ["comment"]))
                                                    put d1252_626
                                                    d1254_628 <- get
                                                    do err1270_629 <- ((do _ <- StateT comEnd
                                                                           return ()) >> return False) `catchError` const (return True)
                                                       unless err1270_629 (gets position >>= (throwError . mkParseError "!_:comEnd" "not match: " "" d1254_628 ["comEnd"]))
                                                    put d1254_628
                                                    _ <- StateT char
                                                    return ()
                                                    _ <- StateT notComStr
                                                    return ()
                                                    return (),
                                                 return ()]
                comEnd165_170 = foldl1 mplus [do d1257_630 <- get
                                                 t705_631 <- StateT char
                                                 case t705_631 of
                                                     '-' -> return ()
                                                     _ -> gets position >>= (throwError . mkParseError "'-'" "not match pattern: " "" d1257_630 ["char"])
                                                 let '-' = t705_631
                                                 return ()
                                                 d1258_632 <- get
                                                 t706_633 <- StateT char
                                                 case t706_633 of
                                                     '}' -> return ()
                                                     _ -> gets position >>= (throwError . mkParseError "'}'" "not match pattern: " "" d1258_632 ["char"])
                                                 let '}' = t706_633
                                                 return ()
                                                 return ()]
                list167_361 :: forall m a . (MonadPlus m, Applicative m) =>
                                            m a -> m ([a])
                list1168_460 :: forall m a . (MonadPlus m, Applicative m) =>
                                             m a -> m ([a])
                list167_361 p = list1168_460 p `mplus` return []
                list1168_460 p = ((:) <$> p) <*> list167_361 p
                optional166_317 :: forall m a . (MonadPlus m, Applicative m) =>
                                                m a -> m (Maybe a)
                optional166_317 p = (Just <$> p) `mplus` return Nothing

