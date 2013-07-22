{-# LANGUAGE FlexibleContexts, TemplateHaskell, UndecidableInstances, PackageImports, TypeFamilies, RankNTypes #-}
module Text.Papillon.Parser (
	Peg,
	Definition,
	Selection(..),
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
                             where d = Derivs pegFile76_1 pragmas77_2 pragma78_3 pragmaStr79_4 pragmaItems80_5 pragmaEnd81_6 moduleDec82_7 moduleName83_8 moduleDecStr84_9 whr85_10 preImpPap86_11 prePeg87_12 afterPeg88_13 importPapillon89_14 varToken90_15 typToken91_16 pap92_17 peg93_18 sourceType94_19 peg_95_20 definition96_21 selection97_22 normalSelection98_23 plainSelection99_24 expressionHs100_25 plainExpressionHs101_26 expression102_27 nameLeaf_103_28 nameLeaf104_29 nameLeafNoCom105_30 comForErr106_31 leaf107_32 patOp108_33 pat109_34 pat1110_35 patList111_36 opConName112_37 charLit113_38 stringLit114_39 escapeC115_40 pats116_41 readFromLs117_42 readFrom118_43 test119_44 hsExpLam120_45 hsExpTyp121_46 hsExpOp122_47 hsOp123_48 opTail124_49 hsExp125_50 hsExp1126_51 hsExpTpl127_52 hsTypeArr128_53 hsType129_54 hsType1130_55 hsTypeTpl131_56 typ132_57 variable133_58 tvtail134_59 integer135_60 alpha136_61 upper137_62 lower138_63 digit139_64 spaces140_65 space141_66 notNLString142_67 newLine143_68 comment144_69 comments145_70 notComStr146_71 comEnd147_72 chars148_73 pos
                                   pegFile76_1 = runStateT pegFile4_74 d
                                   pragmas77_2 = runStateT pragmas5_75 d
                                   pragma78_3 = runStateT pragma6_76 d
                                   pragmaStr79_4 = runStateT pragmaStr7_77 d
                                   pragmaItems80_5 = runStateT pragmaItems8_78 d
                                   pragmaEnd81_6 = runStateT pragmaEnd9_79 d
                                   moduleDec82_7 = runStateT moduleDec10_80 d
                                   moduleName83_8 = runStateT moduleName11_81 d
                                   moduleDecStr84_9 = runStateT moduleDecStr12_82 d
                                   whr85_10 = runStateT whr13_83 d
                                   preImpPap86_11 = runStateT preImpPap14_84 d
                                   prePeg87_12 = runStateT prePeg15_85 d
                                   afterPeg88_13 = runStateT afterPeg16_86 d
                                   importPapillon89_14 = runStateT importPapillon17_87 d
                                   varToken90_15 = runStateT varToken18_88 d
                                   typToken91_16 = runStateT typToken19_89 d
                                   pap92_17 = runStateT pap20_90 d
                                   peg93_18 = runStateT peg21_91 d
                                   sourceType94_19 = runStateT sourceType22_92 d
                                   peg_95_20 = runStateT peg_23_93 d
                                   definition96_21 = runStateT definition24_94 d
                                   selection97_22 = runStateT selection25_95 d
                                   normalSelection98_23 = runStateT normalSelection26_96 d
                                   plainSelection99_24 = runStateT plainSelection27_97 d
                                   expressionHs100_25 = runStateT expressionHs28_98 d
                                   plainExpressionHs101_26 = runStateT plainExpressionHs29_99 d
                                   expression102_27 = runStateT expression30_100 d
                                   nameLeaf_103_28 = runStateT nameLeaf_31_101 d
                                   nameLeaf104_29 = runStateT nameLeaf32_102 d
                                   nameLeafNoCom105_30 = runStateT nameLeafNoCom33_103 d
                                   comForErr106_31 = runStateT comForErr34_104 d
                                   leaf107_32 = runStateT leaf35_105 d
                                   patOp108_33 = runStateT patOp36_106 d
                                   pat109_34 = runStateT pat37_107 d
                                   pat1110_35 = runStateT pat138_108 d
                                   patList111_36 = runStateT patList39_109 d
                                   opConName112_37 = runStateT opConName40_110 d
                                   charLit113_38 = runStateT charLit41_111 d
                                   stringLit114_39 = runStateT stringLit42_112 d
                                   escapeC115_40 = runStateT escapeC43_113 d
                                   pats116_41 = runStateT pats44_114 d
                                   readFromLs117_42 = runStateT readFromLs45_115 d
                                   readFrom118_43 = runStateT readFrom46_116 d
                                   test119_44 = runStateT test47_117 d
                                   hsExpLam120_45 = runStateT hsExpLam48_118 d
                                   hsExpTyp121_46 = runStateT hsExpTyp49_119 d
                                   hsExpOp122_47 = runStateT hsExpOp50_120 d
                                   hsOp123_48 = runStateT hsOp51_121 d
                                   opTail124_49 = runStateT opTail52_122 d
                                   hsExp125_50 = runStateT hsExp53_123 d
                                   hsExp1126_51 = runStateT hsExp154_124 d
                                   hsExpTpl127_52 = runStateT hsExpTpl55_125 d
                                   hsTypeArr128_53 = runStateT hsTypeArr56_126 d
                                   hsType129_54 = runStateT hsType57_127 d
                                   hsType1130_55 = runStateT hsType158_128 d
                                   hsTypeTpl131_56 = runStateT hsTypeTpl59_129 d
                                   typ132_57 = runStateT typ60_130 d
                                   variable133_58 = runStateT variable61_131 d
                                   tvtail134_59 = runStateT tvtail62_132 d
                                   integer135_60 = runStateT integer63_133 d
                                   alpha136_61 = runStateT alpha64_134 d
                                   upper137_62 = runStateT upper65_135 d
                                   lower138_63 = runStateT lower66_136 d
                                   digit139_64 = runStateT digit67_137 d
                                   spaces140_65 = runStateT spaces68_138 d
                                   space141_66 = runStateT space69_139 d
                                   notNLString142_67 = runStateT notNLString70_140 d
                                   newLine143_68 = runStateT newLine71_141 d
                                   comment144_69 = runStateT comment72_142 d
                                   comments145_70 = runStateT comments73_143 d
                                   notComStr146_71 = runStateT notComStr74_144 d
                                   comEnd147_72 = runStateT comEnd75_145 d
                                   chars148_73 = runStateT (case getToken s of
                                                                Just (c,
                                                                      s') -> do put (parse0_0 (updatePos c pos) s')
                                                                                return c
                                                                _ -> gets position >>= (throwError . mkParseError "" "end of input" "" undefined [])) d
                pegFile4_74 = foldl1 mplus [do pr <- StateT pragmas
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
                                               d166_146 <- get
                                               xx165_147 <- StateT char
                                               case xx165_147 of
                                                   '|' -> return ()
                                                   _ -> gets position >>= (throwError . mkParseError "'|'" "not match pattern: " "" d166_146 ["char"])
                                               let '|' = xx165_147
                                               return ()
                                               d168_148 <- get
                                               xx167_149 <- StateT char
                                               case xx167_149 of
                                                   ']' -> return ()
                                                   _ -> gets position >>= (throwError . mkParseError "']'" "not match pattern: " "" d168_148 ["char"])
                                               let ']' = xx167_149
                                               return ()
                                               d170_150 <- get
                                               xx169_151 <- StateT char
                                               case xx169_151 of
                                                   '\n' -> return ()
                                                   _ -> gets position >>= (throwError . mkParseError "'\\n'" "not match pattern: " "" d170_150 ["char"])
                                               let '\n' = xx169_151
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
                                               d186_152 <- get
                                               xx185_153 <- StateT char
                                               case xx185_153 of
                                                   '|' -> return ()
                                                   _ -> gets position >>= (throwError . mkParseError "'|'" "not match pattern: " "" d186_152 ["char"])
                                               let '|' = xx185_153
                                               return ()
                                               d188_154 <- get
                                               xx187_155 <- StateT char
                                               case xx187_155 of
                                                   ']' -> return ()
                                                   _ -> gets position >>= (throwError . mkParseError "']'" "not match pattern: " "" d188_154 ["char"])
                                               let ']' = xx187_155
                                               return ()
                                               d190_156 <- get
                                               xx189_157 <- StateT char
                                               case xx189_157 of
                                                   '\n' -> return ()
                                                   _ -> gets position >>= (throwError . mkParseError "'\\n'" "not match pattern: " "" d190_156 ["char"])
                                               let '\n' = xx189_157
                                               return ()
                                               atp <- StateT afterPeg
                                               return (mkPegFile pr md emp pp p atp)]
                pragmas5_75 = foldl1 mplus [do _ <- StateT spaces
                                               return ()
                                               pr <- StateT pragma
                                               prs <- StateT pragmas
                                               return (pr : prs),
                                            do _ <- StateT spaces
                                               return ()
                                               return []]
                pragma6_76 = foldl1 mplus [do d202_158 <- get
                                              xx201_159 <- StateT char
                                              case xx201_159 of
                                                  '{' -> return ()
                                                  _ -> gets position >>= (throwError . mkParseError "'{'" "not match pattern: " "" d202_158 ["char"])
                                              let '{' = xx201_159
                                              return ()
                                              d204_160 <- get
                                              xx203_161 <- StateT char
                                              case xx203_161 of
                                                  '-' -> return ()
                                                  _ -> gets position >>= (throwError . mkParseError "'-'" "not match pattern: " "" d204_160 ["char"])
                                              let '-' = xx203_161
                                              return ()
                                              d206_162 <- get
                                              xx205_163 <- StateT char
                                              case xx205_163 of
                                                  '#' -> return ()
                                                  _ -> gets position >>= (throwError . mkParseError "'#'" "not match pattern: " "" d206_162 ["char"])
                                              let '#' = xx205_163
                                              return ()
                                              _ <- StateT spaces
                                              return ()
                                              d210_164 <- get
                                              xx209_165 <- StateT char
                                              case xx209_165 of
                                                  'L' -> return ()
                                                  _ -> gets position >>= (throwError . mkParseError "'L'" "not match pattern: " "" d210_164 ["char"])
                                              let 'L' = xx209_165
                                              return ()
                                              d212_166 <- get
                                              xx211_167 <- StateT char
                                              case xx211_167 of
                                                  'A' -> return ()
                                                  _ -> gets position >>= (throwError . mkParseError "'A'" "not match pattern: " "" d212_166 ["char"])
                                              let 'A' = xx211_167
                                              return ()
                                              d214_168 <- get
                                              xx213_169 <- StateT char
                                              case xx213_169 of
                                                  'N' -> return ()
                                                  _ -> gets position >>= (throwError . mkParseError "'N'" "not match pattern: " "" d214_168 ["char"])
                                              let 'N' = xx213_169
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
                                                  'U' -> return ()
                                                  _ -> gets position >>= (throwError . mkParseError "'U'" "not match pattern: " "" d218_172 ["char"])
                                              let 'U' = xx217_173
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
                                                  'G' -> return ()
                                                  _ -> gets position >>= (throwError . mkParseError "'G'" "not match pattern: " "" d222_176 ["char"])
                                              let 'G' = xx221_177
                                              return ()
                                              d224_178 <- get
                                              xx223_179 <- StateT char
                                              case xx223_179 of
                                                  'E' -> return ()
                                                  _ -> gets position >>= (throwError . mkParseError "'E'" "not match pattern: " "" d224_178 ["char"])
                                              let 'E' = xx223_179
                                              return ()
                                              _ <- StateT spaces
                                              return ()
                                              s <- StateT pragmaItems
                                              _ <- StateT pragmaEnd
                                              return ()
                                              _ <- StateT spaces
                                              return ()
                                              return (LanguagePragma s),
                                           do d234_180 <- get
                                              xx233_181 <- StateT char
                                              case xx233_181 of
                                                  '{' -> return ()
                                                  _ -> gets position >>= (throwError . mkParseError "'{'" "not match pattern: " "" d234_180 ["char"])
                                              let '{' = xx233_181
                                              return ()
                                              d236_182 <- get
                                              xx235_183 <- StateT char
                                              case xx235_183 of
                                                  '-' -> return ()
                                                  _ -> gets position >>= (throwError . mkParseError "'-'" "not match pattern: " "" d236_182 ["char"])
                                              let '-' = xx235_183
                                              return ()
                                              d238_184 <- get
                                              xx237_185 <- StateT char
                                              case xx237_185 of
                                                  '#' -> return ()
                                                  _ -> gets position >>= (throwError . mkParseError "'#'" "not match pattern: " "" d238_184 ["char"])
                                              let '#' = xx237_185
                                              return ()
                                              _ <- StateT spaces
                                              return ()
                                              s <- StateT pragmaStr
                                              _ <- StateT pragmaEnd
                                              return ()
                                              return (OtherPragma s)]
                pragmaStr7_77 = foldl1 mplus [do ddd245_186 <- get
                                                 do err <- ((do _ <- StateT pragmaEnd
                                                                return ()) >> return False) `catchError` const (return True)
                                                    unless err (gets position >>= (throwError . mkParseError ('!' : "_:pragmaEnd") "not match: " "" ddd245_186 ["pragmaEnd"]))
                                                 put ddd245_186
                                                 c <- StateT char
                                                 s <- StateT pragmaStr
                                                 return (c : s),
                                              return ""]
                pragmaItems8_78 = foldl1 mplus [do t <- StateT typToken
                                                   d255_187 <- get
                                                   xx254_188 <- StateT char
                                                   case xx254_188 of
                                                       ',' -> return ()
                                                       _ -> gets position >>= (throwError . mkParseError "','" "not match pattern: " "" d255_187 ["char"])
                                                   let ',' = xx254_188
                                                   return ()
                                                   _ <- StateT spaces
                                                   return ()
                                                   i <- StateT pragmaItems
                                                   return (t : i),
                                                do t <- StateT typToken
                                                   return [t]]
                pragmaEnd9_79 = foldl1 mplus [do _ <- StateT spaces
                                                 return ()
                                                 d265_189 <- get
                                                 xx264_190 <- StateT char
                                                 case xx264_190 of
                                                     '#' -> return ()
                                                     _ -> gets position >>= (throwError . mkParseError "'#'" "not match pattern: " "" d265_189 ["char"])
                                                 let '#' = xx264_190
                                                 return ()
                                                 d267_191 <- get
                                                 xx266_192 <- StateT char
                                                 case xx266_192 of
                                                     '-' -> return ()
                                                     _ -> gets position >>= (throwError . mkParseError "'-'" "not match pattern: " "" d267_191 ["char"])
                                                 let '-' = xx266_192
                                                 return ()
                                                 d269_193 <- get
                                                 xx268_194 <- StateT char
                                                 case xx268_194 of
                                                     '}' -> return ()
                                                     _ -> gets position >>= (throwError . mkParseError "'}'" "not match pattern: " "" d269_193 ["char"])
                                                 let '}' = xx268_194
                                                 return ()
                                                 return ()]
                moduleDec10_80 = foldl1 mplus [do d271_195 <- get
                                                  xx270_196 <- StateT char
                                                  case xx270_196 of
                                                      'm' -> return ()
                                                      _ -> gets position >>= (throwError . mkParseError "'m'" "not match pattern: " "" d271_195 ["char"])
                                                  let 'm' = xx270_196
                                                  return ()
                                                  d273_197 <- get
                                                  xx272_198 <- StateT char
                                                  case xx272_198 of
                                                      'o' -> return ()
                                                      _ -> gets position >>= (throwError . mkParseError "'o'" "not match pattern: " "" d273_197 ["char"])
                                                  let 'o' = xx272_198
                                                  return ()
                                                  d275_199 <- get
                                                  xx274_200 <- StateT char
                                                  case xx274_200 of
                                                      'd' -> return ()
                                                      _ -> gets position >>= (throwError . mkParseError "'d'" "not match pattern: " "" d275_199 ["char"])
                                                  let 'd' = xx274_200
                                                  return ()
                                                  d277_201 <- get
                                                  xx276_202 <- StateT char
                                                  case xx276_202 of
                                                      'u' -> return ()
                                                      _ -> gets position >>= (throwError . mkParseError "'u'" "not match pattern: " "" d277_201 ["char"])
                                                  let 'u' = xx276_202
                                                  return ()
                                                  d279_203 <- get
                                                  xx278_204 <- StateT char
                                                  case xx278_204 of
                                                      'l' -> return ()
                                                      _ -> gets position >>= (throwError . mkParseError "'l'" "not match pattern: " "" d279_203 ["char"])
                                                  let 'l' = xx278_204
                                                  return ()
                                                  d281_205 <- get
                                                  xx280_206 <- StateT char
                                                  case xx280_206 of
                                                      'e' -> return ()
                                                      _ -> gets position >>= (throwError . mkParseError "'e'" "not match pattern: " "" d281_205 ["char"])
                                                  let 'e' = xx280_206
                                                  return ()
                                                  _ <- StateT spaces
                                                  return ()
                                                  n <- StateT moduleName
                                                  _ <- StateT spaces
                                                  return ()
                                                  d289_207 <- get
                                                  xx288_208 <- StateT char
                                                  case xx288_208 of
                                                      '(' -> return ()
                                                      _ -> gets position >>= (throwError . mkParseError "'('" "not match pattern: " "" d289_207 ["char"])
                                                  let '(' = xx288_208
                                                  return ()
                                                  _ <- StateT spaces
                                                  return ()
                                                  s <- StateT moduleDecStr
                                                  _ <- StateT whr
                                                  return ()
                                                  return (Just (n, Just s)),
                                               do d297_209 <- get
                                                  xx296_210 <- StateT char
                                                  case xx296_210 of
                                                      'm' -> return ()
                                                      _ -> gets position >>= (throwError . mkParseError "'m'" "not match pattern: " "" d297_209 ["char"])
                                                  let 'm' = xx296_210
                                                  return ()
                                                  d299_211 <- get
                                                  xx298_212 <- StateT char
                                                  case xx298_212 of
                                                      'o' -> return ()
                                                      _ -> gets position >>= (throwError . mkParseError "'o'" "not match pattern: " "" d299_211 ["char"])
                                                  let 'o' = xx298_212
                                                  return ()
                                                  d301_213 <- get
                                                  xx300_214 <- StateT char
                                                  case xx300_214 of
                                                      'd' -> return ()
                                                      _ -> gets position >>= (throwError . mkParseError "'d'" "not match pattern: " "" d301_213 ["char"])
                                                  let 'd' = xx300_214
                                                  return ()
                                                  d303_215 <- get
                                                  xx302_216 <- StateT char
                                                  case xx302_216 of
                                                      'u' -> return ()
                                                      _ -> gets position >>= (throwError . mkParseError "'u'" "not match pattern: " "" d303_215 ["char"])
                                                  let 'u' = xx302_216
                                                  return ()
                                                  d305_217 <- get
                                                  xx304_218 <- StateT char
                                                  case xx304_218 of
                                                      'l' -> return ()
                                                      _ -> gets position >>= (throwError . mkParseError "'l'" "not match pattern: " "" d305_217 ["char"])
                                                  let 'l' = xx304_218
                                                  return ()
                                                  d307_219 <- get
                                                  xx306_220 <- StateT char
                                                  case xx306_220 of
                                                      'e' -> return ()
                                                      _ -> gets position >>= (throwError . mkParseError "'e'" "not match pattern: " "" d307_219 ["char"])
                                                  let 'e' = xx306_220
                                                  return ()
                                                  _ <- StateT spaces
                                                  return ()
                                                  n <- StateT moduleName
                                                  _ <- StateT spaces
                                                  return ()
                                                  d315_221 <- get
                                                  xx314_222 <- StateT char
                                                  case xx314_222 of
                                                      'w' -> return ()
                                                      _ -> gets position >>= (throwError . mkParseError "'w'" "not match pattern: " "" d315_221 ["char"])
                                                  let 'w' = xx314_222
                                                  return ()
                                                  d317_223 <- get
                                                  xx316_224 <- StateT char
                                                  case xx316_224 of
                                                      'h' -> return ()
                                                      _ -> gets position >>= (throwError . mkParseError "'h'" "not match pattern: " "" d317_223 ["char"])
                                                  let 'h' = xx316_224
                                                  return ()
                                                  d319_225 <- get
                                                  xx318_226 <- StateT char
                                                  case xx318_226 of
                                                      'e' -> return ()
                                                      _ -> gets position >>= (throwError . mkParseError "'e'" "not match pattern: " "" d319_225 ["char"])
                                                  let 'e' = xx318_226
                                                  return ()
                                                  d321_227 <- get
                                                  xx320_228 <- StateT char
                                                  case xx320_228 of
                                                      'r' -> return ()
                                                      _ -> gets position >>= (throwError . mkParseError "'r'" "not match pattern: " "" d321_227 ["char"])
                                                  let 'r' = xx320_228
                                                  return ()
                                                  d323_229 <- get
                                                  xx322_230 <- StateT char
                                                  case xx322_230 of
                                                      'e' -> return ()
                                                      _ -> gets position >>= (throwError . mkParseError "'e'" "not match pattern: " "" d323_229 ["char"])
                                                  let 'e' = xx322_230
                                                  return ()
                                                  _ <- StateT spaces
                                                  return ()
                                                  return (Just (n, Nothing)),
                                               return Nothing]
                moduleName11_81 = foldl1 mplus [do t <- StateT typ
                                                   d329_231 <- get
                                                   xx328_232 <- StateT char
                                                   case xx328_232 of
                                                       '.' -> return ()
                                                       _ -> gets position >>= (throwError . mkParseError "'.'" "not match pattern: " "" d329_231 ["char"])
                                                   let '.' = xx328_232
                                                   return ()
                                                   n <- StateT moduleName
                                                   return (t : n),
                                                do t <- StateT typ
                                                   return [t]]
                moduleDecStr12_82 = foldl1 mplus [do ddd334_233 <- get
                                                     do err <- ((do _ <- StateT whr
                                                                    return ()) >> return False) `catchError` const (return True)
                                                        unless err (gets position >>= (throwError . mkParseError ('!' : "_:whr") "not match: " "" ddd334_233 ["whr"]))
                                                     put ddd334_233
                                                     c <- StateT char
                                                     s <- StateT moduleDecStr
                                                     return (c : s),
                                                  return ""]
                whr13_83 = foldl1 mplus [do _ <- StateT spaces
                                            return ()
                                            d344_234 <- get
                                            xx343_235 <- StateT char
                                            case xx343_235 of
                                                ')' -> return ()
                                                _ -> gets position >>= (throwError . mkParseError "')'" "not match pattern: " "" d344_234 ["char"])
                                            let ')' = xx343_235
                                            return ()
                                            _ <- StateT spaces
                                            return ()
                                            d348_236 <- get
                                            xx347_237 <- StateT char
                                            case xx347_237 of
                                                'w' -> return ()
                                                _ -> gets position >>= (throwError . mkParseError "'w'" "not match pattern: " "" d348_236 ["char"])
                                            let 'w' = xx347_237
                                            return ()
                                            d350_238 <- get
                                            xx349_239 <- StateT char
                                            case xx349_239 of
                                                'h' -> return ()
                                                _ -> gets position >>= (throwError . mkParseError "'h'" "not match pattern: " "" d350_238 ["char"])
                                            let 'h' = xx349_239
                                            return ()
                                            d352_240 <- get
                                            xx351_241 <- StateT char
                                            case xx351_241 of
                                                'e' -> return ()
                                                _ -> gets position >>= (throwError . mkParseError "'e'" "not match pattern: " "" d352_240 ["char"])
                                            let 'e' = xx351_241
                                            return ()
                                            d354_242 <- get
                                            xx353_243 <- StateT char
                                            case xx353_243 of
                                                'r' -> return ()
                                                _ -> gets position >>= (throwError . mkParseError "'r'" "not match pattern: " "" d354_242 ["char"])
                                            let 'r' = xx353_243
                                            return ()
                                            d356_244 <- get
                                            xx355_245 <- StateT char
                                            case xx355_245 of
                                                'e' -> return ()
                                                _ -> gets position >>= (throwError . mkParseError "'e'" "not match pattern: " "" d356_244 ["char"])
                                            let 'e' = xx355_245
                                            return ()
                                            return ()]
                preImpPap14_84 = foldl1 mplus [do ddd357_246 <- get
                                                  do err <- ((do _ <- StateT importPapillon
                                                                 return ()) >> return False) `catchError` const (return True)
                                                     unless err (gets position >>= (throwError . mkParseError ('!' : "_:importPapillon") "not match: " "" ddd357_246 ["importPapillon"]))
                                                  put ddd357_246
                                                  ddd360_247 <- get
                                                  do err <- ((do _ <- StateT pap
                                                                 return ()) >> return False) `catchError` const (return True)
                                                     unless err (gets position >>= (throwError . mkParseError ('!' : "_:pap") "not match: " "" ddd360_247 ["pap"]))
                                                  put ddd360_247
                                                  c <- StateT char
                                                  pip <- StateT preImpPap
                                                  return (cons c pip),
                                               return emp]
                prePeg15_85 = foldl1 mplus [do ddd367_248 <- get
                                               do err <- ((do _ <- StateT pap
                                                              return ()) >> return False) `catchError` const (return True)
                                                  unless err (gets position >>= (throwError . mkParseError ('!' : "_:pap") "not match: " "" ddd367_248 ["pap"]))
                                               put ddd367_248
                                               c <- StateT char
                                               pp <- StateT prePeg
                                               return (cons c pp),
                                            return emp]
                afterPeg16_86 = foldl1 mplus [do c <- StateT char
                                                 atp <- StateT afterPeg
                                                 return (cons c atp),
                                              return emp]
                importPapillon17_87 = foldl1 mplus [do d379_249 <- get
                                                       xx378_250 <- StateT varToken
                                                       case xx378_250 of
                                                           "import" -> return ()
                                                           _ -> gets position >>= (throwError . mkParseError "\"import\"" "not match pattern: " "" d379_249 ["varToken"])
                                                       let "import" = xx378_250
                                                       return ()
                                                       d381_251 <- get
                                                       xx380_252 <- StateT typToken
                                                       case xx380_252 of
                                                           "Text" -> return ()
                                                           _ -> gets position >>= (throwError . mkParseError "\"Text\"" "not match pattern: " "" d381_251 ["typToken"])
                                                       let "Text" = xx380_252
                                                       return ()
                                                       d383_253 <- get
                                                       xx382_254 <- StateT char
                                                       case xx382_254 of
                                                           '.' -> return ()
                                                           _ -> gets position >>= (throwError . mkParseError "'.'" "not match pattern: " "" d383_253 ["char"])
                                                       let '.' = xx382_254
                                                       return ()
                                                       _ <- StateT spaces
                                                       return ()
                                                       d387_255 <- get
                                                       xx386_256 <- StateT typToken
                                                       case xx386_256 of
                                                           "Papillon" -> return ()
                                                           _ -> gets position >>= (throwError . mkParseError "\"Papillon\"" "not match pattern: " "" d387_255 ["typToken"])
                                                       let "Papillon" = xx386_256
                                                       return ()
                                                       ddd388_257 <- get
                                                       do err <- ((do d390_258 <- get
                                                                      xx389_259 <- StateT char
                                                                      case xx389_259 of
                                                                          '.' -> return ()
                                                                          _ -> gets position >>= (throwError . mkParseError "'.'" "not match pattern: " "" d390_258 ["char"])
                                                                      let '.' = xx389_259
                                                                      return ()) >> return False) `catchError` const (return True)
                                                          unless err (gets position >>= (throwError . mkParseError ('!' : "'.':") "not match: " "" ddd388_257 ["char"]))
                                                       put ddd388_257
                                                       return ()]
                varToken18_88 = foldl1 mplus [do v <- StateT variable
                                                 _ <- StateT spaces
                                                 return ()
                                                 return v]
                typToken19_89 = foldl1 mplus [do t <- StateT typ
                                                 _ <- StateT spaces
                                                 return ()
                                                 return t]
                pap20_90 = foldl1 mplus [do d400_260 <- get
                                            xx399_261 <- StateT char
                                            case xx399_261 of
                                                '\n' -> return ()
                                                _ -> gets position >>= (throwError . mkParseError "'\\n'" "not match pattern: " "" d400_260 ["char"])
                                            let '\n' = xx399_261
                                            return ()
                                            d402_262 <- get
                                            xx401_263 <- StateT char
                                            case xx401_263 of
                                                '[' -> return ()
                                                _ -> gets position >>= (throwError . mkParseError "'['" "not match pattern: " "" d402_262 ["char"])
                                            let '[' = xx401_263
                                            return ()
                                            d404_264 <- get
                                            xx403_265 <- StateT char
                                            case xx403_265 of
                                                'p' -> return ()
                                                _ -> gets position >>= (throwError . mkParseError "'p'" "not match pattern: " "" d404_264 ["char"])
                                            let 'p' = xx403_265
                                            return ()
                                            d406_266 <- get
                                            xx405_267 <- StateT char
                                            case xx405_267 of
                                                'a' -> return ()
                                                _ -> gets position >>= (throwError . mkParseError "'a'" "not match pattern: " "" d406_266 ["char"])
                                            let 'a' = xx405_267
                                            return ()
                                            d408_268 <- get
                                            xx407_269 <- StateT char
                                            case xx407_269 of
                                                'p' -> return ()
                                                _ -> gets position >>= (throwError . mkParseError "'p'" "not match pattern: " "" d408_268 ["char"])
                                            let 'p' = xx407_269
                                            return ()
                                            d410_270 <- get
                                            xx409_271 <- StateT char
                                            case xx409_271 of
                                                'i' -> return ()
                                                _ -> gets position >>= (throwError . mkParseError "'i'" "not match pattern: " "" d410_270 ["char"])
                                            let 'i' = xx409_271
                                            return ()
                                            d412_272 <- get
                                            xx411_273 <- StateT char
                                            case xx411_273 of
                                                'l' -> return ()
                                                _ -> gets position >>= (throwError . mkParseError "'l'" "not match pattern: " "" d412_272 ["char"])
                                            let 'l' = xx411_273
                                            return ()
                                            d414_274 <- get
                                            xx413_275 <- StateT char
                                            case xx413_275 of
                                                'l' -> return ()
                                                _ -> gets position >>= (throwError . mkParseError "'l'" "not match pattern: " "" d414_274 ["char"])
                                            let 'l' = xx413_275
                                            return ()
                                            d416_276 <- get
                                            xx415_277 <- StateT char
                                            case xx415_277 of
                                                'o' -> return ()
                                                _ -> gets position >>= (throwError . mkParseError "'o'" "not match pattern: " "" d416_276 ["char"])
                                            let 'o' = xx415_277
                                            return ()
                                            d418_278 <- get
                                            xx417_279 <- StateT char
                                            case xx417_279 of
                                                'n' -> return ()
                                                _ -> gets position >>= (throwError . mkParseError "'n'" "not match pattern: " "" d418_278 ["char"])
                                            let 'n' = xx417_279
                                            return ()
                                            d420_280 <- get
                                            xx419_281 <- StateT char
                                            case xx419_281 of
                                                '|' -> return ()
                                                _ -> gets position >>= (throwError . mkParseError "'|'" "not match pattern: " "" d420_280 ["char"])
                                            let '|' = xx419_281
                                            return ()
                                            d422_282 <- get
                                            xx421_283 <- StateT char
                                            case xx421_283 of
                                                '\n' -> return ()
                                                _ -> gets position >>= (throwError . mkParseError "'\\n'" "not match pattern: " "" d422_282 ["char"])
                                            let '\n' = xx421_283
                                            return ()
                                            return ()]
                peg21_91 = foldl1 mplus [do _ <- StateT spaces
                                            return ()
                                            s <- StateT sourceType
                                            p <- StateT peg_
                                            return (mkTTPeg s p),
                                         do p <- StateT peg_
                                            return (mkTTPeg tString p)]
                sourceType22_92 = foldl1 mplus [do d432_284 <- get
                                                   xx431_285 <- StateT varToken
                                                   case xx431_285 of
                                                       "source" -> return ()
                                                       _ -> gets position >>= (throwError . mkParseError "\"source\"" "not match pattern: " "" d432_284 ["varToken"])
                                                   let "source" = xx431_285
                                                   return ()
                                                   d434_286 <- get
                                                   xx433_287 <- StateT char
                                                   case xx433_287 of
                                                       ':' -> return ()
                                                       _ -> gets position >>= (throwError . mkParseError "':'" "not match pattern: " "" d434_286 ["char"])
                                                   let ':' = xx433_287
                                                   return ()
                                                   _ <- StateT spaces
                                                   return ()
                                                   v <- StateT typToken
                                                   return v]
                peg_23_93 = foldl1 mplus [do _ <- StateT spaces
                                             return ()
                                             d <- StateT definition
                                             p <- StateT peg_
                                             return (cons d p),
                                          return emp]
                definition24_94 = foldl1 mplus [do v <- StateT variable
                                                   _ <- StateT spaces
                                                   return ()
                                                   d450_288 <- get
                                                   xx449_289 <- StateT char
                                                   case xx449_289 of
                                                       ':' -> return ()
                                                       _ -> gets position >>= (throwError . mkParseError "':'" "not match pattern: " "" d450_288 ["char"])
                                                   let ':' = xx449_289
                                                   return ()
                                                   d452_290 <- get
                                                   xx451_291 <- StateT char
                                                   case xx451_291 of
                                                       ':' -> return ()
                                                       _ -> gets position >>= (throwError . mkParseError "':'" "not match pattern: " "" d452_290 ["char"])
                                                   let ':' = xx451_291
                                                   return ()
                                                   _ <- StateT spaces
                                                   return ()
                                                   t <- StateT hsTypeArr
                                                   _ <- StateT spaces
                                                   return ()
                                                   d460_292 <- get
                                                   xx459_293 <- StateT char
                                                   case xx459_293 of
                                                       '=' -> return ()
                                                       _ -> gets position >>= (throwError . mkParseError "'='" "not match pattern: " "" d460_292 ["char"])
                                                   let '=' = xx459_293
                                                   return ()
                                                   _ <- StateT spaces
                                                   return ()
                                                   sel <- StateT selection
                                                   _ <- StateT spaces
                                                   return ()
                                                   d468_294 <- get
                                                   xx467_295 <- StateT char
                                                   case xx467_295 of
                                                       ';' -> return ()
                                                       _ -> gets position >>= (throwError . mkParseError "';'" "not match pattern: " "" d468_294 ["char"])
                                                   let ';' = xx467_295
                                                   return ()
                                                   return (mkDef v t sel)]
                selection25_95 = foldl1 mplus [do s <- StateT normalSelection
                                                  return s,
                                               do s <- StateT plainSelection
                                                  return s]
                normalSelection26_96 = foldl1 mplus [do ex <- StateT expressionHs
                                                        _ <- StateT spaces
                                                        return ()
                                                        d478_296 <- get
                                                        xx477_297 <- StateT char
                                                        case xx477_297 of
                                                            '/' -> return ()
                                                            _ -> gets position >>= (throwError . mkParseError "'/'" "not match pattern: " "" d478_296 ["char"])
                                                        let '/' = xx477_297
                                                        return ()
                                                        _ <- StateT spaces
                                                        return ()
                                                        sel <- StateT normalSelection
                                                        return (Selection $ ex : expressions sel),
                                                     do ex <- StateT expressionHs
                                                        return (Selection [ex])]
                plainSelection27_97 = foldl1 mplus [do ex <- StateT plainExpressionHs
                                                       _ <- StateT spaces
                                                       return ()
                                                       d490_298 <- get
                                                       xx489_299 <- StateT char
                                                       case xx489_299 of
                                                           '/' -> return ()
                                                           _ -> gets position >>= (throwError . mkParseError "'/'" "not match pattern: " "" d490_298 ["char"])
                                                       let '/' = xx489_299
                                                       return ()
                                                       _ <- StateT spaces
                                                       return ()
                                                       sel <- StateT plainSelection
                                                       return (PlainSelection $ ex : plainExpressions sel),
                                                    do ex <- StateT plainExpressionHs
                                                       return (PlainSelection [ex])]
                expressionHs28_98 = foldl1 mplus [do e <- StateT expression
                                                     _ <- StateT spaces
                                                     return ()
                                                     d502_300 <- get
                                                     xx501_301 <- StateT char
                                                     case xx501_301 of
                                                         '{' -> return ()
                                                         _ -> gets position >>= (throwError . mkParseError "'{'" "not match pattern: " "" d502_300 ["char"])
                                                     let '{' = xx501_301
                                                     return ()
                                                     _ <- StateT spaces
                                                     return ()
                                                     h <- StateT hsExpLam
                                                     _ <- StateT spaces
                                                     return ()
                                                     d510_302 <- get
                                                     xx509_303 <- StateT char
                                                     case xx509_303 of
                                                         '}' -> return ()
                                                         _ -> gets position >>= (throwError . mkParseError "'}'" "not match pattern: " "" d510_302 ["char"])
                                                     let '}' = xx509_303
                                                     return ()
                                                     return (ExpressionHs e h)]
                plainExpressionHs29_99 = foldl1 mplus [do rfs <- list1_304 (foldl1 mplus [do rf <- StateT readFromLs
                                                                                             _ <- StateT spaces
                                                                                             return ()
                                                                                             return rf])
                                                          return (PlainExpressionHs rfs)]
                expression30_100 = foldl1 mplus [do l <- StateT nameLeaf_
                                                    _ <- StateT spaces
                                                    return ()
                                                    e <- StateT expression
                                                    return (cons l e),
                                                 return emp]
                nameLeaf_31_101 = foldl1 mplus [do d524_305 <- get
                                                   xx523_306 <- StateT char
                                                   case xx523_306 of
                                                       '!' -> return ()
                                                       _ -> gets position >>= (throwError . mkParseError "'!'" "not match pattern: " "" d524_305 ["char"])
                                                   let '!' = xx523_306
                                                   return ()
                                                   nl <- StateT nameLeafNoCom
                                                   _ <- StateT spaces
                                                   return ()
                                                   com <- optional3_307 (StateT comForErr)
                                                   return (NotAfter nl $ maybe "" id com),
                                                do d532_308 <- get
                                                   xx531_309 <- StateT char
                                                   let c = xx531_309
                                                   unless (isAmp c) (gets position >>= (throwError . mkParseError "isAmp c" "not match: " "" d532_308 ["char"]))
                                                   nl <- StateT nameLeaf
                                                   return (After nl),
                                                do nl <- StateT nameLeaf
                                                   return (Here nl)]
                nameLeaf32_102 = foldl1 mplus [do n <- StateT pat1
                                                  _ <- StateT spaces
                                                  return ()
                                                  com <- optional3_307 (StateT comForErr)
                                                  d544_310 <- get
                                                  xx543_311 <- StateT char
                                                  case xx543_311 of
                                                      ':' -> return ()
                                                      _ -> gets position >>= (throwError . mkParseError "':'" "not match pattern: " "" d544_310 ["char"])
                                                  let ':' = xx543_311
                                                  return ()
                                                  (rf, p) <- StateT leaf
                                                  return (NameLeaf (n, maybe "" id com) rf p),
                                               do n <- StateT pat1
                                                  _ <- StateT spaces
                                                  return ()
                                                  com <- optional3_307 (StateT comForErr)
                                                  return (NameLeaf (n,
                                                                    maybe "" id com) FromToken Nothing)]
                nameLeafNoCom33_103 = foldl1 mplus [do n <- StateT pat1
                                                       _ <- StateT spaces
                                                       return ()
                                                       com <- optional3_307 (StateT comForErr)
                                                       d560_312 <- get
                                                       xx559_313 <- StateT char
                                                       case xx559_313 of
                                                           ':' -> return ()
                                                           _ -> gets position >>= (throwError . mkParseError "':'" "not match pattern: " "" d560_312 ["char"])
                                                       let ':' = xx559_313
                                                       return ()
                                                       (rf, p) <- StateT leaf
                                                       return (NameLeaf (n, maybe "" id com) rf p),
                                                    do n <- StateT pat1
                                                       _ <- StateT spaces
                                                       return ()
                                                       return (NameLeaf (n, "") FromToken Nothing)]
                comForErr34_104 = foldl1 mplus [do d568_314 <- get
                                                   xx567_315 <- StateT char
                                                   case xx567_315 of
                                                       '{' -> return ()
                                                       _ -> gets position >>= (throwError . mkParseError "'{'" "not match pattern: " "" d568_314 ["char"])
                                                   let '{' = xx567_315
                                                   return ()
                                                   d570_316 <- get
                                                   xx569_317 <- StateT char
                                                   case xx569_317 of
                                                       '-' -> return ()
                                                       _ -> gets position >>= (throwError . mkParseError "'-'" "not match pattern: " "" d570_316 ["char"])
                                                   let '-' = xx569_317
                                                   return ()
                                                   d572_318 <- get
                                                   xx571_319 <- StateT char
                                                   case xx571_319 of
                                                       '#' -> return ()
                                                       _ -> gets position >>= (throwError . mkParseError "'#'" "not match pattern: " "" d572_318 ["char"])
                                                   let '#' = xx571_319
                                                   return ()
                                                   _ <- StateT spaces
                                                   return ()
                                                   d576_320 <- get
                                                   xx575_321 <- StateT char
                                                   case xx575_321 of
                                                       '"' -> return ()
                                                       _ -> gets position >>= (throwError . mkParseError "'\"'" "not match pattern: " "" d576_320 ["char"])
                                                   let '"' = xx575_321
                                                   return ()
                                                   s <- StateT stringLit
                                                   d580_322 <- get
                                                   xx579_323 <- StateT char
                                                   case xx579_323 of
                                                       '"' -> return ()
                                                       _ -> gets position >>= (throwError . mkParseError "'\"'" "not match pattern: " "" d580_322 ["char"])
                                                   let '"' = xx579_323
                                                   return ()
                                                   _ <- StateT spaces
                                                   return ()
                                                   d584_324 <- get
                                                   xx583_325 <- StateT char
                                                   case xx583_325 of
                                                       '#' -> return ()
                                                       _ -> gets position >>= (throwError . mkParseError "'#'" "not match pattern: " "" d584_324 ["char"])
                                                   let '#' = xx583_325
                                                   return ()
                                                   d586_326 <- get
                                                   xx585_327 <- StateT char
                                                   case xx585_327 of
                                                       '-' -> return ()
                                                       _ -> gets position >>= (throwError . mkParseError "'-'" "not match pattern: " "" d586_326 ["char"])
                                                   let '-' = xx585_327
                                                   return ()
                                                   d588_328 <- get
                                                   xx587_329 <- StateT char
                                                   case xx587_329 of
                                                       '}' -> return ()
                                                       _ -> gets position >>= (throwError . mkParseError "'}'" "not match pattern: " "" d588_328 ["char"])
                                                   let '}' = xx587_329
                                                   return ()
                                                   _ <- StateT spaces
                                                   return ()
                                                   return s]
                leaf35_105 = foldl1 mplus [do rf <- StateT readFromLs
                                              t <- StateT test
                                              return (rf, Just t),
                                           do rf <- StateT readFromLs
                                              return (rf, Nothing),
                                           do t <- StateT test
                                              return (FromToken, Just t)]
                patOp36_106 = foldl1 mplus [do p <- StateT pat
                                               o <- StateT opConName
                                               po <- StateT patOp
                                               return (uInfixP p o po),
                                            do p <- StateT pat
                                               _ <- StateT spaces
                                               return ()
                                               d610_330 <- get
                                               xx609_331 <- StateT char
                                               let q = xx609_331
                                               unless (isBQ q) (gets position >>= (throwError . mkParseError "isBQ q" "not match: " "" d610_330 ["char"]))
                                               t <- StateT typ
                                               d614_332 <- get
                                               xx613_333 <- StateT char
                                               let q_ = xx613_333
                                               unless (isBQ q_) (gets position >>= (throwError . mkParseError "isBQ q_" "not match: " "" d614_332 ["char"]))
                                               _ <- StateT spaces
                                               return ()
                                               po <- StateT patOp
                                               return (uInfixP p (mkName t) po),
                                            do p <- StateT pat
                                               return p]
                pat37_107 = foldl1 mplus [do t <- StateT typ
                                             _ <- StateT spaces
                                             return ()
                                             ps <- StateT pats
                                             return (conToPatQ t ps),
                                          do d628_334 <- get
                                             xx627_335 <- StateT char
                                             case xx627_335 of
                                                 '(' -> return ()
                                                 _ -> gets position >>= (throwError . mkParseError "'('" "not match pattern: " "" d628_334 ["char"])
                                             let '(' = xx627_335
                                             return ()
                                             o <- StateT opConName
                                             d632_336 <- get
                                             xx631_337 <- StateT char
                                             case xx631_337 of
                                                 ')' -> return ()
                                                 _ -> gets position >>= (throwError . mkParseError "')'" "not match pattern: " "" d632_336 ["char"])
                                             let ')' = xx631_337
                                             return ()
                                             _ <- StateT spaces
                                             return ()
                                             ps <- StateT pats
                                             return (conP o ps),
                                          do p <- StateT pat1
                                             return p]
                pat138_108 = foldl1 mplus [do t <- StateT typ
                                              return (conToPatQ t emp),
                                           do d642_338 <- get
                                              xx641_339 <- StateT variable
                                              case xx641_339 of
                                                  "_" -> return ()
                                                  _ -> gets position >>= (throwError . mkParseError "\"_\"" "not match pattern: " "" d642_338 ["variable"])
                                              let "_" = xx641_339
                                              return ()
                                              return wildP,
                                           do n <- StateT variable
                                              return (strToPatQ n),
                                           do i <- StateT integer
                                              return (litP (integerL i)),
                                           do d648_340 <- get
                                              xx647_341 <- StateT char
                                              case xx647_341 of
                                                  '-' -> return ()
                                                  _ -> gets position >>= (throwError . mkParseError "'-'" "not match pattern: " "" d648_340 ["char"])
                                              let '-' = xx647_341
                                              return ()
                                              _ <- StateT spaces
                                              return ()
                                              i <- StateT integer
                                              return (litP (integerL $ negate i)),
                                           do d654_342 <- get
                                              xx653_343 <- StateT char
                                              case xx653_343 of
                                                  '\'' -> return ()
                                                  _ -> gets position >>= (throwError . mkParseError "'\\''" "not match pattern: " "" d654_342 ["char"])
                                              let '\'' = xx653_343
                                              return ()
                                              c <- StateT charLit
                                              d658_344 <- get
                                              xx657_345 <- StateT char
                                              case xx657_345 of
                                                  '\'' -> return ()
                                                  _ -> gets position >>= (throwError . mkParseError "'\\''" "not match pattern: " "" d658_344 ["char"])
                                              let '\'' = xx657_345
                                              return ()
                                              return (charP c),
                                           do d660_346 <- get
                                              xx659_347 <- StateT char
                                              case xx659_347 of
                                                  '"' -> return ()
                                                  _ -> gets position >>= (throwError . mkParseError "'\"'" "not match pattern: " "" d660_346 ["char"])
                                              let '"' = xx659_347
                                              return ()
                                              s <- StateT stringLit
                                              d664_348 <- get
                                              xx663_349 <- StateT char
                                              case xx663_349 of
                                                  '"' -> return ()
                                                  _ -> gets position >>= (throwError . mkParseError "'\"'" "not match pattern: " "" d664_348 ["char"])
                                              let '"' = xx663_349
                                              return ()
                                              return (stringP s),
                                           do d666_350 <- get
                                              xx665_351 <- StateT char
                                              case xx665_351 of
                                                  '(' -> return ()
                                                  _ -> gets position >>= (throwError . mkParseError "'('" "not match pattern: " "" d666_350 ["char"])
                                              let '(' = xx665_351
                                              return ()
                                              p <- StateT patList
                                              d670_352 <- get
                                              xx669_353 <- StateT char
                                              case xx669_353 of
                                                  ')' -> return ()
                                                  _ -> gets position >>= (throwError . mkParseError "')'" "not match pattern: " "" d670_352 ["char"])
                                              let ')' = xx669_353
                                              return ()
                                              return (tupP p),
                                           do d672_354 <- get
                                              xx671_355 <- StateT char
                                              case xx671_355 of
                                                  '[' -> return ()
                                                  _ -> gets position >>= (throwError . mkParseError "'['" "not match pattern: " "" d672_354 ["char"])
                                              let '[' = xx671_355
                                              return ()
                                              p <- StateT patList
                                              d676_356 <- get
                                              xx675_357 <- StateT char
                                              case xx675_357 of
                                                  ']' -> return ()
                                                  _ -> gets position >>= (throwError . mkParseError "']'" "not match pattern: " "" d676_356 ["char"])
                                              let ']' = xx675_357
                                              return ()
                                              return (listP p)]
                patList39_109 = foldl1 mplus [do p <- StateT patOp
                                                 _ <- StateT spaces
                                                 return ()
                                                 d682_358 <- get
                                                 xx681_359 <- StateT char
                                                 case xx681_359 of
                                                     ',' -> return ()
                                                     _ -> gets position >>= (throwError . mkParseError "','" "not match pattern: " "" d682_358 ["char"])
                                                 let ',' = xx681_359
                                                 return ()
                                                 _ <- StateT spaces
                                                 return ()
                                                 ps <- StateT patList
                                                 return (p : ps),
                                              do p <- StateT patOp
                                                 return [p],
                                              return []]
                opConName40_110 = foldl1 mplus [do d690_360 <- get
                                                   xx689_361 <- StateT char
                                                   case xx689_361 of
                                                       ':' -> return ()
                                                       _ -> gets position >>= (throwError . mkParseError "':'" "not match pattern: " "" d690_360 ["char"])
                                                   let ':' = xx689_361
                                                   return ()
                                                   ot <- StateT opTail
                                                   return (mkName $ colon : ot)]
                charLit41_111 = foldl1 mplus [do d694_362 <- get
                                                 xx693_363 <- StateT char
                                                 let c = xx693_363
                                                 unless (isAlphaNumOt c) (gets position >>= (throwError . mkParseError "isAlphaNumOt c" "not match: " "" d694_362 ["char"]))
                                                 return c,
                                              do d696_364 <- get
                                                 xx695_365 <- StateT char
                                                 case xx695_365 of
                                                     '\\' -> return ()
                                                     _ -> gets position >>= (throwError . mkParseError "'\\\\'" "not match pattern: " "" d696_364 ["char"])
                                                 let '\\' = xx695_365
                                                 return ()
                                                 c <- StateT escapeC
                                                 return c]
                stringLit42_112 = foldl1 mplus [do d700_366 <- get
                                                   xx699_367 <- StateT char
                                                   let c = xx699_367
                                                   unless (isStrLitC c) (gets position >>= (throwError . mkParseError "isStrLitC c" "not match: " "" d700_366 ["char"]))
                                                   s <- StateT stringLit
                                                   return (cons c s),
                                                do d704_368 <- get
                                                   xx703_369 <- StateT char
                                                   case xx703_369 of
                                                       '\\' -> return ()
                                                       _ -> gets position >>= (throwError . mkParseError "'\\\\'" "not match pattern: " "" d704_368 ["char"])
                                                   let '\\' = xx703_369
                                                   return ()
                                                   c <- StateT escapeC
                                                   s <- StateT stringLit
                                                   return (c : s),
                                                return emp]
                escapeC43_113 = foldl1 mplus [do d710_370 <- get
                                                 xx709_371 <- StateT char
                                                 case xx709_371 of
                                                     '"' -> return ()
                                                     _ -> gets position >>= (throwError . mkParseError "'\"'" "not match pattern: " "" d710_370 ["char"])
                                                 let '"' = xx709_371
                                                 return ()
                                                 return '"',
                                              do d712_372 <- get
                                                 xx711_373 <- StateT char
                                                 case xx711_373 of
                                                     '\'' -> return ()
                                                     _ -> gets position >>= (throwError . mkParseError "'\\''" "not match pattern: " "" d712_372 ["char"])
                                                 let '\'' = xx711_373
                                                 return ()
                                                 return '\'',
                                              do d714_374 <- get
                                                 xx713_375 <- StateT char
                                                 case xx713_375 of
                                                     '\\' -> return ()
                                                     _ -> gets position >>= (throwError . mkParseError "'\\\\'" "not match pattern: " "" d714_374 ["char"])
                                                 let '\\' = xx713_375
                                                 return ()
                                                 return '\\',
                                              do d716_376 <- get
                                                 xx715_377 <- StateT char
                                                 case xx715_377 of
                                                     'n' -> return ()
                                                     _ -> gets position >>= (throwError . mkParseError "'n'" "not match pattern: " "" d716_376 ["char"])
                                                 let 'n' = xx715_377
                                                 return ()
                                                 return '\n',
                                              do d718_378 <- get
                                                 xx717_379 <- StateT char
                                                 case xx717_379 of
                                                     't' -> return ()
                                                     _ -> gets position >>= (throwError . mkParseError "'t'" "not match pattern: " "" d718_378 ["char"])
                                                 let 't' = xx717_379
                                                 return ()
                                                 return tab]
                pats44_114 = foldl1 mplus [do p <- StateT pat
                                              _ <- StateT spaces
                                              return ()
                                              ps <- StateT pats
                                              return (cons p ps),
                                           return emp]
                readFromLs45_115 = foldl1 mplus [do rf <- StateT readFrom
                                                    d728_380 <- get
                                                    xx727_381 <- StateT char
                                                    case xx727_381 of
                                                        '*' -> return ()
                                                        _ -> gets position >>= (throwError . mkParseError "'*'" "not match pattern: " "" d728_380 ["char"])
                                                    let '*' = xx727_381
                                                    return ()
                                                    return (FromList rf),
                                                 do rf <- StateT readFrom
                                                    d732_382 <- get
                                                    xx731_383 <- StateT char
                                                    case xx731_383 of
                                                        '+' -> return ()
                                                        _ -> gets position >>= (throwError . mkParseError "'+'" "not match pattern: " "" d732_382 ["char"])
                                                    let '+' = xx731_383
                                                    return ()
                                                    return (FromList1 rf),
                                                 do rf <- StateT readFrom
                                                    d736_384 <- get
                                                    xx735_385 <- StateT char
                                                    case xx735_385 of
                                                        '?' -> return ()
                                                        _ -> gets position >>= (throwError . mkParseError "'?'" "not match pattern: " "" d736_384 ["char"])
                                                    let '?' = xx735_385
                                                    return ()
                                                    return (FromOptional rf),
                                                 do rf <- StateT readFrom
                                                    return rf]
                readFrom46_116 = foldl1 mplus [do v <- StateT variable
                                                  return (FromVariable v),
                                               do d742_386 <- get
                                                  xx741_387 <- StateT char
                                                  case xx741_387 of
                                                      '(' -> return ()
                                                      _ -> gets position >>= (throwError . mkParseError "'('" "not match pattern: " "" d742_386 ["char"])
                                                  let '(' = xx741_387
                                                  return ()
                                                  s <- StateT selection
                                                  d746_388 <- get
                                                  xx745_389 <- StateT char
                                                  case xx745_389 of
                                                      ')' -> return ()
                                                      _ -> gets position >>= (throwError . mkParseError "')'" "not match pattern: " "" d746_388 ["char"])
                                                  let ')' = xx745_389
                                                  return ()
                                                  return (FromSelection s)]
                test47_117 = foldl1 mplus [do d748_390 <- get
                                              xx747_391 <- StateT char
                                              case xx747_391 of
                                                  '[' -> return ()
                                                  _ -> gets position >>= (throwError . mkParseError "'['" "not match pattern: " "" d748_390 ["char"])
                                              let '[' = xx747_391
                                              return ()
                                              h <- StateT hsExpLam
                                              _ <- StateT spaces
                                              return ()
                                              com <- optional3_307 (StateT comForErr)
                                              d756_392 <- get
                                              xx755_393 <- StateT char
                                              case xx755_393 of
                                                  ']' -> return ()
                                                  _ -> gets position >>= (throwError . mkParseError "']'" "not match pattern: " "" d756_392 ["char"])
                                              let ']' = xx755_393
                                              return ()
                                              return (h, maybe "" id com)]
                hsExpLam48_118 = foldl1 mplus [do d758_394 <- get
                                                  xx757_395 <- StateT char
                                                  case xx757_395 of
                                                      '\\' -> return ()
                                                      _ -> gets position >>= (throwError . mkParseError "'\\\\'" "not match pattern: " "" d758_394 ["char"])
                                                  let '\\' = xx757_395
                                                  return ()
                                                  _ <- StateT spaces
                                                  return ()
                                                  ps <- StateT pats
                                                  _ <- StateT spaces
                                                  return ()
                                                  d766_396 <- get
                                                  xx765_397 <- StateT char
                                                  case xx765_397 of
                                                      '-' -> return ()
                                                      _ -> gets position >>= (throwError . mkParseError "'-'" "not match pattern: " "" d766_396 ["char"])
                                                  let '-' = xx765_397
                                                  return ()
                                                  d768_398 <- get
                                                  xx767_399 <- StateT char
                                                  let c = xx767_399
                                                  unless (isGt c) (gets position >>= (throwError . mkParseError "isGt c" "not match: " "" d768_398 ["char"]))
                                                  _ <- StateT spaces
                                                  return ()
                                                  e <- StateT hsExpTyp
                                                  return (lamE ps e),
                                               do e <- StateT hsExpTyp
                                                  return e]
                hsExpTyp49_119 = foldl1 mplus [do eo <- StateT hsExpOp
                                                  d778_400 <- get
                                                  xx777_401 <- StateT char
                                                  case xx777_401 of
                                                      ':' -> return ()
                                                      _ -> gets position >>= (throwError . mkParseError "':'" "not match pattern: " "" d778_400 ["char"])
                                                  let ':' = xx777_401
                                                  return ()
                                                  d780_402 <- get
                                                  xx779_403 <- StateT char
                                                  case xx779_403 of
                                                      ':' -> return ()
                                                      _ -> gets position >>= (throwError . mkParseError "':'" "not match pattern: " "" d780_402 ["char"])
                                                  let ':' = xx779_403
                                                  return ()
                                                  _ <- StateT spaces
                                                  return ()
                                                  t <- StateT hsTypeArr
                                                  return (sigE eo t),
                                               do eo <- StateT hsExpOp
                                                  return eo]
                hsExpOp50_120 = foldl1 mplus [do l <- StateT hsExp
                                                 _ <- StateT spaces
                                                 return ()
                                                 o <- StateT hsOp
                                                 _ <- StateT spaces
                                                 return ()
                                                 r <- StateT hsExpOp
                                                 return (uInfixE (getEx l) o r),
                                              do e <- StateT hsExp
                                                 return (getEx e)]
                hsOp51_121 = foldl1 mplus [do d800_404 <- get
                                              xx799_405 <- StateT char
                                              let c = xx799_405
                                              unless (isOpHeadChar c) (gets position >>= (throwError . mkParseError "isOpHeadChar c" "not match: " "" d800_404 ["char"]))
                                              o <- StateT opTail
                                              return (varE (mkName (cons c o))),
                                           do d804_406 <- get
                                              xx803_407 <- StateT char
                                              case xx803_407 of
                                                  ':' -> return ()
                                                  _ -> gets position >>= (throwError . mkParseError "':'" "not match pattern: " "" d804_406 ["char"])
                                              let ':' = xx803_407
                                              return ()
                                              ddd805_408 <- get
                                              do err <- ((do d807_409 <- get
                                                             xx806_410 <- StateT char
                                                             case xx806_410 of
                                                                 ':' -> return ()
                                                                 _ -> gets position >>= (throwError . mkParseError "':'" "not match pattern: " "" d807_409 ["char"])
                                                             let ':' = xx806_410
                                                             return ()) >> return False) `catchError` const (return True)
                                                 unless err (gets position >>= (throwError . mkParseError ('!' : "':':") "not match: " "" ddd805_408 ["char"]))
                                              put ddd805_408
                                              o <- StateT opTail
                                              return (conE (mkName (':' : o))),
                                           do d811_411 <- get
                                              xx810_412 <- StateT char
                                              let c = xx810_412
                                              unless (isBQ c) (gets position >>= (throwError . mkParseError "isBQ c" "not match: " "" d811_411 ["char"]))
                                              v <- StateT variable
                                              d815_413 <- get
                                              xx814_414 <- StateT char
                                              let c_ = xx814_414
                                              unless (isBQ c_) (gets position >>= (throwError . mkParseError "isBQ c_" "not match: " "" d815_413 ["char"]))
                                              return (varE (mkName v)),
                                           do d817_415 <- get
                                              xx816_416 <- StateT char
                                              let c = xx816_416
                                              unless (isBQ c) (gets position >>= (throwError . mkParseError "isBQ c" "not match: " "" d817_415 ["char"]))
                                              t <- StateT typ
                                              d821_417 <- get
                                              xx820_418 <- StateT char
                                              let c_ = xx820_418
                                              unless (isBQ c_) (gets position >>= (throwError . mkParseError "isBQ c_" "not match: " "" d821_417 ["char"]))
                                              return (conE (mkName t))]
                opTail52_122 = foldl1 mplus [do d823_419 <- get
                                                xx822_420 <- StateT char
                                                let c = xx822_420
                                                unless (isOpTailChar c) (gets position >>= (throwError . mkParseError "isOpTailChar c" "not match: " "" d823_419 ["char"]))
                                                s <- StateT opTail
                                                return (cons c s),
                                             return emp]
                hsExp53_123 = foldl1 mplus [do e <- StateT hsExp1
                                               _ <- StateT spaces
                                               return ()
                                               h <- StateT hsExp
                                               return (applyExR e h),
                                            do e <- StateT hsExp1
                                               return (toEx e)]
                hsExp154_124 = foldl1 mplus [do d835_421 <- get
                                                xx834_422 <- StateT char
                                                case xx834_422 of
                                                    '(' -> return ()
                                                    _ -> gets position >>= (throwError . mkParseError "'('" "not match pattern: " "" d835_421 ["char"])
                                                let '(' = xx834_422
                                                return ()
                                                l <- optional3_307 (foldl1 mplus [do e <- StateT hsExpTyp
                                                                                     return e])
                                                _ <- StateT spaces
                                                return ()
                                                o <- StateT hsOp
                                                _ <- StateT spaces
                                                return ()
                                                r <- optional3_307 (foldl1 mplus [do e <- StateT hsExpTyp
                                                                                     return e])
                                                d851_423 <- get
                                                xx850_424 <- StateT char
                                                case xx850_424 of
                                                    ')' -> return ()
                                                    _ -> gets position >>= (throwError . mkParseError "')'" "not match pattern: " "" d851_423 ["char"])
                                                let ')' = xx850_424
                                                return ()
                                                return (infixE l o r),
                                             do d853_425 <- get
                                                xx852_426 <- StateT char
                                                case xx852_426 of
                                                    '(' -> return ()
                                                    _ -> gets position >>= (throwError . mkParseError "'('" "not match pattern: " "" d853_425 ["char"])
                                                let '(' = xx852_426
                                                return ()
                                                et <- StateT hsExpTpl
                                                d857_427 <- get
                                                xx856_428 <- StateT char
                                                case xx856_428 of
                                                    ')' -> return ()
                                                    _ -> gets position >>= (throwError . mkParseError "')'" "not match pattern: " "" d857_427 ["char"])
                                                let ')' = xx856_428
                                                return ()
                                                return (tupE et),
                                             do d859_429 <- get
                                                xx858_430 <- StateT char
                                                case xx858_430 of
                                                    '[' -> return ()
                                                    _ -> gets position >>= (throwError . mkParseError "'['" "not match pattern: " "" d859_429 ["char"])
                                                let '[' = xx858_430
                                                return ()
                                                et <- StateT hsExpTpl
                                                d863_431 <- get
                                                xx862_432 <- StateT char
                                                case xx862_432 of
                                                    ']' -> return ()
                                                    _ -> gets position >>= (throwError . mkParseError "']'" "not match pattern: " "" d863_431 ["char"])
                                                let ']' = xx862_432
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
                                             do d873_433 <- get
                                                xx872_434 <- StateT char
                                                case xx872_434 of
                                                    '\'' -> return ()
                                                    _ -> gets position >>= (throwError . mkParseError "'\\''" "not match pattern: " "" d873_433 ["char"])
                                                let '\'' = xx872_434
                                                return ()
                                                c <- StateT charLit
                                                d877_435 <- get
                                                xx876_436 <- StateT char
                                                case xx876_436 of
                                                    '\'' -> return ()
                                                    _ -> gets position >>= (throwError . mkParseError "'\\''" "not match pattern: " "" d877_435 ["char"])
                                                let '\'' = xx876_436
                                                return ()
                                                return (litE (charL c)),
                                             do d879_437 <- get
                                                xx878_438 <- StateT char
                                                case xx878_438 of
                                                    '"' -> return ()
                                                    _ -> gets position >>= (throwError . mkParseError "'\"'" "not match pattern: " "" d879_437 ["char"])
                                                let '"' = xx878_438
                                                return ()
                                                s <- StateT stringLit
                                                d883_439 <- get
                                                xx882_440 <- StateT char
                                                case xx882_440 of
                                                    '"' -> return ()
                                                    _ -> gets position >>= (throwError . mkParseError "'\"'" "not match pattern: " "" d883_439 ["char"])
                                                let '"' = xx882_440
                                                return ()
                                                return (litE (stringL s)),
                                             do d885_441 <- get
                                                xx884_442 <- StateT char
                                                case xx884_442 of
                                                    '-' -> return ()
                                                    _ -> gets position >>= (throwError . mkParseError "'-'" "not match pattern: " "" d885_441 ["char"])
                                                let '-' = xx884_442
                                                return ()
                                                _ <- StateT spaces
                                                return ()
                                                e <- StateT hsExp1
                                                return (appE (varE $ mkName "negate") e)]
                hsExpTpl55_125 = foldl1 mplus [do e <- StateT hsExpLam
                                                  _ <- StateT spaces
                                                  return ()
                                                  d895_443 <- get
                                                  xx894_444 <- StateT char
                                                  let c = xx894_444
                                                  unless (isComma c) (gets position >>= (throwError . mkParseError "isComma c" "not match: " "" d895_443 ["char"]))
                                                  _ <- StateT spaces
                                                  return ()
                                                  et <- StateT hsExpTpl
                                                  return (cons e et),
                                               do e <- StateT hsExpLam
                                                  return (cons e emp),
                                               return emp]
                hsTypeArr56_126 = foldl1 mplus [do l <- StateT hsType
                                                   d905_445 <- get
                                                   xx904_446 <- StateT char
                                                   case xx904_446 of
                                                       '-' -> return ()
                                                       _ -> gets position >>= (throwError . mkParseError "'-'" "not match pattern: " "" d905_445 ["char"])
                                                   let '-' = xx904_446
                                                   return ()
                                                   d907_447 <- get
                                                   xx906_448 <- StateT char
                                                   let c = xx906_448
                                                   unless (isGt c) (gets position >>= (throwError . mkParseError "isGt c" "not match: " "" d907_447 ["char"]))
                                                   _ <- StateT spaces
                                                   return ()
                                                   r <- StateT hsTypeArr
                                                   return (appT (appT arrowT (getTyp l)) r),
                                                do t <- StateT hsType
                                                   return (getTyp t)]
                hsType57_127 = foldl1 mplus [do t <- StateT hsType1
                                                ts <- StateT hsType
                                                return (applyTyp (toTyp t) ts),
                                             do t <- StateT hsType1
                                                return (toTyp t)]
                hsType158_128 = foldl1 mplus [do d921_449 <- get
                                                 xx920_450 <- StateT char
                                                 case xx920_450 of
                                                     '[' -> return ()
                                                     _ -> gets position >>= (throwError . mkParseError "'['" "not match pattern: " "" d921_449 ["char"])
                                                 let '[' = xx920_450
                                                 return ()
                                                 d923_451 <- get
                                                 xx922_452 <- StateT char
                                                 case xx922_452 of
                                                     ']' -> return ()
                                                     _ -> gets position >>= (throwError . mkParseError "']'" "not match pattern: " "" d923_451 ["char"])
                                                 let ']' = xx922_452
                                                 return ()
                                                 _ <- StateT spaces
                                                 return ()
                                                 return listT,
                                              do d927_453 <- get
                                                 xx926_454 <- StateT char
                                                 case xx926_454 of
                                                     '[' -> return ()
                                                     _ -> gets position >>= (throwError . mkParseError "'['" "not match pattern: " "" d927_453 ["char"])
                                                 let '[' = xx926_454
                                                 return ()
                                                 t <- StateT hsTypeArr
                                                 d931_455 <- get
                                                 xx930_456 <- StateT char
                                                 case xx930_456 of
                                                     ']' -> return ()
                                                     _ -> gets position >>= (throwError . mkParseError "']'" "not match pattern: " "" d931_455 ["char"])
                                                 let ']' = xx930_456
                                                 return ()
                                                 _ <- StateT spaces
                                                 return ()
                                                 return (appT listT t),
                                              do d935_457 <- get
                                                 xx934_458 <- StateT char
                                                 case xx934_458 of
                                                     '(' -> return ()
                                                     _ -> gets position >>= (throwError . mkParseError "'('" "not match pattern: " "" d935_457 ["char"])
                                                 let '(' = xx934_458
                                                 return ()
                                                 _ <- StateT spaces
                                                 return ()
                                                 tt <- StateT hsTypeTpl
                                                 d941_459 <- get
                                                 xx940_460 <- StateT char
                                                 case xx940_460 of
                                                     ')' -> return ()
                                                     _ -> gets position >>= (throwError . mkParseError "')'" "not match pattern: " "" d941_459 ["char"])
                                                 let ')' = xx940_460
                                                 return ()
                                                 return (tupT tt),
                                              do t <- StateT typToken
                                                 return (conT (mkName t)),
                                              do d945_461 <- get
                                                 xx944_462 <- StateT char
                                                 case xx944_462 of
                                                     '(' -> return ()
                                                     _ -> gets position >>= (throwError . mkParseError "'('" "not match pattern: " "" d945_461 ["char"])
                                                 let '(' = xx944_462
                                                 return ()
                                                 d947_463 <- get
                                                 xx946_464 <- StateT char
                                                 case xx946_464 of
                                                     '-' -> return ()
                                                     _ -> gets position >>= (throwError . mkParseError "'-'" "not match pattern: " "" d947_463 ["char"])
                                                 let '-' = xx946_464
                                                 return ()
                                                 d949_465 <- get
                                                 xx948_466 <- StateT char
                                                 let c = xx948_466
                                                 unless (isGt c) (gets position >>= (throwError . mkParseError "isGt c" "not match: " "" d949_465 ["char"]))
                                                 d951_467 <- get
                                                 xx950_468 <- StateT char
                                                 case xx950_468 of
                                                     ')' -> return ()
                                                     _ -> gets position >>= (throwError . mkParseError "')'" "not match pattern: " "" d951_467 ["char"])
                                                 let ')' = xx950_468
                                                 return ()
                                                 _ <- StateT spaces
                                                 return ()
                                                 return arrowT]
                hsTypeTpl59_129 = foldl1 mplus [do t <- StateT hsTypeArr
                                                   d957_469 <- get
                                                   xx956_470 <- StateT char
                                                   let c = xx956_470
                                                   unless (isComma c) (gets position >>= (throwError . mkParseError "isComma c" "not match: " "" d957_469 ["char"]))
                                                   _ <- StateT spaces
                                                   return ()
                                                   tt <- StateT hsTypeTpl
                                                   return (cons t tt),
                                                do t <- StateT hsTypeArr
                                                   return (cons t emp),
                                                return emp]
                typ60_130 = foldl1 mplus [do u <- StateT upper
                                             t <- StateT tvtail
                                             return (cons u t)]
                variable61_131 = foldl1 mplus [do l <- StateT lower
                                                  t <- StateT tvtail
                                                  return (cons l t)]
                tvtail62_132 = foldl1 mplus [do a <- StateT alpha
                                                t <- StateT tvtail
                                                return (cons a t),
                                             return emp]
                integer63_133 = foldl1 mplus [do dh <- StateT digit
                                                 ds <- list1_304 (foldl1 mplus [do d <- StateT digit
                                                                                   return d])
                                                 return (read (cons dh ds))]
                alpha64_134 = foldl1 mplus [do u <- StateT upper
                                               return u,
                                            do l <- StateT lower
                                               return l,
                                            do d <- StateT digit
                                               return d,
                                            do d989_471 <- get
                                               xx988_472 <- StateT char
                                               case xx988_472 of
                                                   '\'' -> return ()
                                                   _ -> gets position >>= (throwError . mkParseError "'\\''" "not match pattern: " "" d989_471 ["char"])
                                               let '\'' = xx988_472
                                               return ()
                                               return '\'']
                upper65_135 = foldl1 mplus [do d991_473 <- get
                                               xx990_474 <- StateT char
                                               let u = xx990_474
                                               unless (isUpper u) (gets position >>= (throwError . mkParseError "isUpper u" "not match: " "" d991_473 ["char"]))
                                               return u]
                lower66_136 = foldl1 mplus [do d993_475 <- get
                                               xx992_476 <- StateT char
                                               let l = xx992_476
                                               unless (isLowerU l) (gets position >>= (throwError . mkParseError "isLowerU l" "not match: " "" d993_475 ["char"]))
                                               return l]
                digit67_137 = foldl1 mplus [do d995_477 <- get
                                               xx994_478 <- StateT char
                                               let d = xx994_478
                                               unless (isDigit d) (gets position >>= (throwError . mkParseError "isDigit d" "not match: " "" d995_477 ["char"]))
                                               return d]
                spaces68_138 = foldl1 mplus [do _ <- StateT space
                                                return ()
                                                _ <- StateT spaces
                                                return ()
                                                return (),
                                             return ()]
                space69_139 = foldl1 mplus [do d1001_479 <- get
                                               xx1000_480 <- StateT char
                                               let s = xx1000_480
                                               unless (isSpace s) (gets position >>= (throwError . mkParseError "isSpace s" "not match: " "" d1001_479 ["char"]))
                                               return (),
                                            do d1003_481 <- get
                                               xx1002_482 <- StateT char
                                               case xx1002_482 of
                                                   '-' -> return ()
                                                   _ -> gets position >>= (throwError . mkParseError "'-'" "not match pattern: " "" d1003_481 ["char"])
                                               let '-' = xx1002_482
                                               return ()
                                               d1005_483 <- get
                                               xx1004_484 <- StateT char
                                               case xx1004_484 of
                                                   '-' -> return ()
                                                   _ -> gets position >>= (throwError . mkParseError "'-'" "not match pattern: " "" d1005_483 ["char"])
                                               let '-' = xx1004_484
                                               return ()
                                               _ <- StateT notNLString
                                               return ()
                                               _ <- StateT newLine
                                               return ()
                                               return (),
                                            do _ <- StateT comment
                                               return ()
                                               return ()]
                notNLString70_140 = foldl1 mplus [do ddd1012_485 <- get
                                                     do err <- ((do _ <- StateT newLine
                                                                    return ()) >> return False) `catchError` const (return True)
                                                        unless err (gets position >>= (throwError . mkParseError ('!' : "_:newLine") "not match: " "" ddd1012_485 ["newLine"]))
                                                     put ddd1012_485
                                                     c <- StateT char
                                                     s <- StateT notNLString
                                                     return (cons c s),
                                                  return emp]
                newLine71_141 = foldl1 mplus [do d1020_486 <- get
                                                 xx1019_487 <- StateT char
                                                 case xx1019_487 of
                                                     '\n' -> return ()
                                                     _ -> gets position >>= (throwError . mkParseError "'\\n'" "not match pattern: " "" d1020_486 ["char"])
                                                 let '\n' = xx1019_487
                                                 return ()
                                                 return ()]
                comment72_142 = foldl1 mplus [do d1022_488 <- get
                                                 xx1021_489 <- StateT char
                                                 case xx1021_489 of
                                                     '{' -> return ()
                                                     _ -> gets position >>= (throwError . mkParseError "'{'" "not match pattern: " "" d1022_488 ["char"])
                                                 let '{' = xx1021_489
                                                 return ()
                                                 d1024_490 <- get
                                                 xx1023_491 <- StateT char
                                                 case xx1023_491 of
                                                     '-' -> return ()
                                                     _ -> gets position >>= (throwError . mkParseError "'-'" "not match pattern: " "" d1024_490 ["char"])
                                                 let '-' = xx1023_491
                                                 return ()
                                                 ddd1025_492 <- get
                                                 do err <- ((do d1027_493 <- get
                                                                xx1026_494 <- StateT char
                                                                case xx1026_494 of
                                                                    '#' -> return ()
                                                                    _ -> gets position >>= (throwError . mkParseError "'#'" "not match pattern: " "" d1027_493 ["char"])
                                                                let '#' = xx1026_494
                                                                return ()) >> return False) `catchError` const (return True)
                                                    unless err (gets position >>= (throwError . mkParseError ('!' : "'#':") "not match: " "" ddd1025_492 ["char"]))
                                                 put ddd1025_492
                                                 _ <- StateT comments
                                                 return ()
                                                 _ <- StateT comEnd
                                                 return ()
                                                 return ()]
                comments73_143 = foldl1 mplus [do _ <- StateT notComStr
                                                  return ()
                                                  _ <- StateT comment
                                                  return ()
                                                  _ <- StateT comments
                                                  return ()
                                                  return (),
                                               do _ <- StateT notComStr
                                                  return ()
                                                  return ()]
                notComStr74_144 = foldl1 mplus [do ddd1040_495 <- get
                                                   do err <- ((do _ <- StateT comment
                                                                  return ()) >> return False) `catchError` const (return True)
                                                      unless err (gets position >>= (throwError . mkParseError ('!' : "_:comment") "not match: " "" ddd1040_495 ["comment"]))
                                                   put ddd1040_495
                                                   ddd1043_496 <- get
                                                   do err <- ((do _ <- StateT comEnd
                                                                  return ()) >> return False) `catchError` const (return True)
                                                      unless err (gets position >>= (throwError . mkParseError ('!' : "_:comEnd") "not match: " "" ddd1043_496 ["comEnd"]))
                                                   put ddd1043_496
                                                   _ <- StateT char
                                                   return ()
                                                   _ <- StateT notComStr
                                                   return ()
                                                   return (),
                                                return ()]
                comEnd75_145 = foldl1 mplus [do d1051_497 <- get
                                                xx1050_498 <- StateT char
                                                case xx1050_498 of
                                                    '-' -> return ()
                                                    _ -> gets position >>= (throwError . mkParseError "'-'" "not match pattern: " "" d1051_497 ["char"])
                                                let '-' = xx1050_498
                                                return ()
                                                d1053_499 <- get
                                                xx1052_500 <- StateT char
                                                case xx1052_500 of
                                                    '}' -> return ()
                                                    _ -> gets position >>= (throwError . mkParseError "'}'" "not match pattern: " "" d1053_499 ["char"])
                                                let '}' = xx1052_500
                                                return ()
                                                return ()]
                list1_304 :: forall m a . (MonadPlus m, Applicative m) =>
                                          m a -> m ([a])
                list12_501 :: forall m a . (MonadPlus m, Applicative m) =>
                                           m a -> m ([a])
                list1_304 p = list12_501 p `mplus` return []
                list12_501 p = ((:) <$> p) <*> list1_304 p
                optional3_307 :: forall m a . (MonadPlus m, Applicative m) =>
                                              m a -> m (Maybe a)
                optional3_307 p = (Just <$> p) `mplus` return Nothing

