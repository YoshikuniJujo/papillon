{-# LANGUAGE FlexibleContexts, TemplateHaskell, UndecidableInstances, PackageImports, TypeFamilies, RankNTypes #-}
module Text.Papillon.Parser (
	Peg,
	Definition,
	Selection,
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
                                                     return (ExpressionHs e h),
                                                  do rfs <- list1_296 (foldl1 mplus [do rf <- StateT readFromLs
                                                                                        _ <- StateT spaces
                                                                                        return ()
                                                                                        return rf])
                                                     return (PlainExpressionHs rfs)]
                expression27_94 = foldl1 mplus [do l <- StateT nameLeaf_
                                                   _ <- StateT spaces
                                                   return ()
                                                   e <- StateT expression
                                                   return (cons l e),
                                                return emp]
                nameLeaf_28_95 = foldl1 mplus [do d502_297 <- get
                                                  xx501_298 <- StateT char
                                                  case xx501_298 of
                                                      '!' -> return ()
                                                      _ -> gets position >>= (throwError . mkParseError "'!'" "not match pattern: " "" d502_297 ["char"])
                                                  let '!' = xx501_298
                                                  return ()
                                                  nl <- StateT nameLeafNoCom
                                                  _ <- StateT spaces
                                                  return ()
                                                  com <- optional3_299 (StateT comForErr)
                                                  return (NotAfter nl $ maybe "" id com),
                                               do d510_300 <- get
                                                  xx509_301 <- StateT char
                                                  let c = xx509_301
                                                  unless (isAmp c) (gets position >>= (throwError . mkParseError "isAmp c" "not match: " "" d510_300 ["char"]))
                                                  nl <- StateT nameLeaf
                                                  return (After nl),
                                               do nl <- StateT nameLeaf
                                                  return (Here nl)]
                nameLeaf29_96 = foldl1 mplus [do n <- StateT pat1
                                                 _ <- StateT spaces
                                                 return ()
                                                 com <- optional3_299 (StateT comForErr)
                                                 d522_302 <- get
                                                 xx521_303 <- StateT char
                                                 case xx521_303 of
                                                     ':' -> return ()
                                                     _ -> gets position >>= (throwError . mkParseError "':'" "not match pattern: " "" d522_302 ["char"])
                                                 let ':' = xx521_303
                                                 return ()
                                                 (rf, p) <- StateT leaf
                                                 return (NameLeaf (n, maybe "" id com) rf p),
                                              do n <- StateT pat1
                                                 _ <- StateT spaces
                                                 return ()
                                                 com <- optional3_299 (StateT comForErr)
                                                 return (NameLeaf (n,
                                                                   maybe "" id com) FromToken Nothing)]
                nameLeafNoCom30_97 = foldl1 mplus [do n <- StateT pat1
                                                      _ <- StateT spaces
                                                      return ()
                                                      com <- optional3_299 (StateT comForErr)
                                                      d538_304 <- get
                                                      xx537_305 <- StateT char
                                                      case xx537_305 of
                                                          ':' -> return ()
                                                          _ -> gets position >>= (throwError . mkParseError "':'" "not match pattern: " "" d538_304 ["char"])
                                                      let ':' = xx537_305
                                                      return ()
                                                      (rf, p) <- StateT leaf
                                                      return (NameLeaf (n, maybe "" id com) rf p),
                                                   do n <- StateT pat1
                                                      _ <- StateT spaces
                                                      return ()
                                                      return (NameLeaf (n, "") FromToken Nothing)]
                comForErr31_98 = foldl1 mplus [do d546_306 <- get
                                                  xx545_307 <- StateT char
                                                  case xx545_307 of
                                                      '{' -> return ()
                                                      _ -> gets position >>= (throwError . mkParseError "'{'" "not match pattern: " "" d546_306 ["char"])
                                                  let '{' = xx545_307
                                                  return ()
                                                  d548_308 <- get
                                                  xx547_309 <- StateT char
                                                  case xx547_309 of
                                                      '-' -> return ()
                                                      _ -> gets position >>= (throwError . mkParseError "'-'" "not match pattern: " "" d548_308 ["char"])
                                                  let '-' = xx547_309
                                                  return ()
                                                  d550_310 <- get
                                                  xx549_311 <- StateT char
                                                  case xx549_311 of
                                                      '#' -> return ()
                                                      _ -> gets position >>= (throwError . mkParseError "'#'" "not match pattern: " "" d550_310 ["char"])
                                                  let '#' = xx549_311
                                                  return ()
                                                  _ <- StateT spaces
                                                  return ()
                                                  d554_312 <- get
                                                  xx553_313 <- StateT char
                                                  case xx553_313 of
                                                      '"' -> return ()
                                                      _ -> gets position >>= (throwError . mkParseError "'\"'" "not match pattern: " "" d554_312 ["char"])
                                                  let '"' = xx553_313
                                                  return ()
                                                  s <- StateT stringLit
                                                  d558_314 <- get
                                                  xx557_315 <- StateT char
                                                  case xx557_315 of
                                                      '"' -> return ()
                                                      _ -> gets position >>= (throwError . mkParseError "'\"'" "not match pattern: " "" d558_314 ["char"])
                                                  let '"' = xx557_315
                                                  return ()
                                                  _ <- StateT spaces
                                                  return ()
                                                  d562_316 <- get
                                                  xx561_317 <- StateT char
                                                  case xx561_317 of
                                                      '#' -> return ()
                                                      _ -> gets position >>= (throwError . mkParseError "'#'" "not match pattern: " "" d562_316 ["char"])
                                                  let '#' = xx561_317
                                                  return ()
                                                  d564_318 <- get
                                                  xx563_319 <- StateT char
                                                  case xx563_319 of
                                                      '-' -> return ()
                                                      _ -> gets position >>= (throwError . mkParseError "'-'" "not match pattern: " "" d564_318 ["char"])
                                                  let '-' = xx563_319
                                                  return ()
                                                  d566_320 <- get
                                                  xx565_321 <- StateT char
                                                  case xx565_321 of
                                                      '}' -> return ()
                                                      _ -> gets position >>= (throwError . mkParseError "'}'" "not match pattern: " "" d566_320 ["char"])
                                                  let '}' = xx565_321
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
                                               d588_322 <- get
                                               xx587_323 <- StateT char
                                               let q = xx587_323
                                               unless (isBQ q) (gets position >>= (throwError . mkParseError "isBQ q" "not match: " "" d588_322 ["char"]))
                                               t <- StateT typ
                                               d592_324 <- get
                                               xx591_325 <- StateT char
                                               let q_ = xx591_325
                                               unless (isBQ q_) (gets position >>= (throwError . mkParseError "isBQ q_" "not match: " "" d592_324 ["char"]))
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
                                          do d606_326 <- get
                                             xx605_327 <- StateT char
                                             case xx605_327 of
                                                 '(' -> return ()
                                                 _ -> gets position >>= (throwError . mkParseError "'('" "not match pattern: " "" d606_326 ["char"])
                                             let '(' = xx605_327
                                             return ()
                                             o <- StateT opConName
                                             d610_328 <- get
                                             xx609_329 <- StateT char
                                             case xx609_329 of
                                                 ')' -> return ()
                                                 _ -> gets position >>= (throwError . mkParseError "')'" "not match pattern: " "" d610_328 ["char"])
                                             let ')' = xx609_329
                                             return ()
                                             _ <- StateT spaces
                                             return ()
                                             ps <- StateT pats
                                             return (conP o ps),
                                          do p <- StateT pat1
                                             return p]
                pat135_102 = foldl1 mplus [do t <- StateT typ
                                              return (conToPatQ t emp),
                                           do d620_330 <- get
                                              xx619_331 <- StateT variable
                                              case xx619_331 of
                                                  "_" -> return ()
                                                  _ -> gets position >>= (throwError . mkParseError "\"_\"" "not match pattern: " "" d620_330 ["variable"])
                                              let "_" = xx619_331
                                              return ()
                                              return wildP,
                                           do n <- StateT variable
                                              return (strToPatQ n),
                                           do i <- StateT integer
                                              return (litP (integerL i)),
                                           do d626_332 <- get
                                              xx625_333 <- StateT char
                                              case xx625_333 of
                                                  '-' -> return ()
                                                  _ -> gets position >>= (throwError . mkParseError "'-'" "not match pattern: " "" d626_332 ["char"])
                                              let '-' = xx625_333
                                              return ()
                                              _ <- StateT spaces
                                              return ()
                                              i <- StateT integer
                                              return (litP (integerL $ negate i)),
                                           do d632_334 <- get
                                              xx631_335 <- StateT char
                                              case xx631_335 of
                                                  '\'' -> return ()
                                                  _ -> gets position >>= (throwError . mkParseError "'\\''" "not match pattern: " "" d632_334 ["char"])
                                              let '\'' = xx631_335
                                              return ()
                                              c <- StateT charLit
                                              d636_336 <- get
                                              xx635_337 <- StateT char
                                              case xx635_337 of
                                                  '\'' -> return ()
                                                  _ -> gets position >>= (throwError . mkParseError "'\\''" "not match pattern: " "" d636_336 ["char"])
                                              let '\'' = xx635_337
                                              return ()
                                              return (charP c),
                                           do d638_338 <- get
                                              xx637_339 <- StateT char
                                              case xx637_339 of
                                                  '"' -> return ()
                                                  _ -> gets position >>= (throwError . mkParseError "'\"'" "not match pattern: " "" d638_338 ["char"])
                                              let '"' = xx637_339
                                              return ()
                                              s <- StateT stringLit
                                              d642_340 <- get
                                              xx641_341 <- StateT char
                                              case xx641_341 of
                                                  '"' -> return ()
                                                  _ -> gets position >>= (throwError . mkParseError "'\"'" "not match pattern: " "" d642_340 ["char"])
                                              let '"' = xx641_341
                                              return ()
                                              return (stringP s),
                                           do d644_342 <- get
                                              xx643_343 <- StateT char
                                              case xx643_343 of
                                                  '(' -> return ()
                                                  _ -> gets position >>= (throwError . mkParseError "'('" "not match pattern: " "" d644_342 ["char"])
                                              let '(' = xx643_343
                                              return ()
                                              p <- StateT patList
                                              d648_344 <- get
                                              xx647_345 <- StateT char
                                              case xx647_345 of
                                                  ')' -> return ()
                                                  _ -> gets position >>= (throwError . mkParseError "')'" "not match pattern: " "" d648_344 ["char"])
                                              let ')' = xx647_345
                                              return ()
                                              return (tupP p),
                                           do d650_346 <- get
                                              xx649_347 <- StateT char
                                              case xx649_347 of
                                                  '[' -> return ()
                                                  _ -> gets position >>= (throwError . mkParseError "'['" "not match pattern: " "" d650_346 ["char"])
                                              let '[' = xx649_347
                                              return ()
                                              p <- StateT patList
                                              d654_348 <- get
                                              xx653_349 <- StateT char
                                              case xx653_349 of
                                                  ']' -> return ()
                                                  _ -> gets position >>= (throwError . mkParseError "']'" "not match pattern: " "" d654_348 ["char"])
                                              let ']' = xx653_349
                                              return ()
                                              return (listP p)]
                patList36_103 = foldl1 mplus [do p <- StateT patOp
                                                 _ <- StateT spaces
                                                 return ()
                                                 d660_350 <- get
                                                 xx659_351 <- StateT char
                                                 case xx659_351 of
                                                     ',' -> return ()
                                                     _ -> gets position >>= (throwError . mkParseError "','" "not match pattern: " "" d660_350 ["char"])
                                                 let ',' = xx659_351
                                                 return ()
                                                 _ <- StateT spaces
                                                 return ()
                                                 ps <- StateT patList
                                                 return (p : ps),
                                              do p <- StateT patOp
                                                 return [p],
                                              return []]
                opConName37_104 = foldl1 mplus [do d668_352 <- get
                                                   xx667_353 <- StateT char
                                                   case xx667_353 of
                                                       ':' -> return ()
                                                       _ -> gets position >>= (throwError . mkParseError "':'" "not match pattern: " "" d668_352 ["char"])
                                                   let ':' = xx667_353
                                                   return ()
                                                   ot <- StateT opTail
                                                   return (mkName $ colon : ot)]
                charLit38_105 = foldl1 mplus [do d672_354 <- get
                                                 xx671_355 <- StateT char
                                                 let c = xx671_355
                                                 unless (isAlphaNumOt c) (gets position >>= (throwError . mkParseError "isAlphaNumOt c" "not match: " "" d672_354 ["char"]))
                                                 return c,
                                              do d674_356 <- get
                                                 xx673_357 <- StateT char
                                                 case xx673_357 of
                                                     '\\' -> return ()
                                                     _ -> gets position >>= (throwError . mkParseError "'\\\\'" "not match pattern: " "" d674_356 ["char"])
                                                 let '\\' = xx673_357
                                                 return ()
                                                 c <- StateT escapeC
                                                 return c]
                stringLit39_106 = foldl1 mplus [do d678_358 <- get
                                                   xx677_359 <- StateT char
                                                   let c = xx677_359
                                                   unless (isStrLitC c) (gets position >>= (throwError . mkParseError "isStrLitC c" "not match: " "" d678_358 ["char"]))
                                                   s <- StateT stringLit
                                                   return (cons c s),
                                                do d682_360 <- get
                                                   xx681_361 <- StateT char
                                                   case xx681_361 of
                                                       '\\' -> return ()
                                                       _ -> gets position >>= (throwError . mkParseError "'\\\\'" "not match pattern: " "" d682_360 ["char"])
                                                   let '\\' = xx681_361
                                                   return ()
                                                   c <- StateT escapeC
                                                   s <- StateT stringLit
                                                   return (c : s),
                                                return emp]
                escapeC40_107 = foldl1 mplus [do d688_362 <- get
                                                 xx687_363 <- StateT char
                                                 case xx687_363 of
                                                     '"' -> return ()
                                                     _ -> gets position >>= (throwError . mkParseError "'\"'" "not match pattern: " "" d688_362 ["char"])
                                                 let '"' = xx687_363
                                                 return ()
                                                 return '"',
                                              do d690_364 <- get
                                                 xx689_365 <- StateT char
                                                 case xx689_365 of
                                                     '\'' -> return ()
                                                     _ -> gets position >>= (throwError . mkParseError "'\\''" "not match pattern: " "" d690_364 ["char"])
                                                 let '\'' = xx689_365
                                                 return ()
                                                 return '\'',
                                              do d692_366 <- get
                                                 xx691_367 <- StateT char
                                                 case xx691_367 of
                                                     '\\' -> return ()
                                                     _ -> gets position >>= (throwError . mkParseError "'\\\\'" "not match pattern: " "" d692_366 ["char"])
                                                 let '\\' = xx691_367
                                                 return ()
                                                 return '\\',
                                              do d694_368 <- get
                                                 xx693_369 <- StateT char
                                                 case xx693_369 of
                                                     'n' -> return ()
                                                     _ -> gets position >>= (throwError . mkParseError "'n'" "not match pattern: " "" d694_368 ["char"])
                                                 let 'n' = xx693_369
                                                 return ()
                                                 return '\n',
                                              do d696_370 <- get
                                                 xx695_371 <- StateT char
                                                 case xx695_371 of
                                                     't' -> return ()
                                                     _ -> gets position >>= (throwError . mkParseError "'t'" "not match pattern: " "" d696_370 ["char"])
                                                 let 't' = xx695_371
                                                 return ()
                                                 return tab]
                pats41_108 = foldl1 mplus [do p <- StateT pat
                                              _ <- StateT spaces
                                              return ()
                                              ps <- StateT pats
                                              return (cons p ps),
                                           return emp]
                readFromLs42_109 = foldl1 mplus [do rf <- StateT readFrom
                                                    d706_372 <- get
                                                    xx705_373 <- StateT char
                                                    case xx705_373 of
                                                        '*' -> return ()
                                                        _ -> gets position >>= (throwError . mkParseError "'*'" "not match pattern: " "" d706_372 ["char"])
                                                    let '*' = xx705_373
                                                    return ()
                                                    return (FromList rf),
                                                 do rf <- StateT readFrom
                                                    d710_374 <- get
                                                    xx709_375 <- StateT char
                                                    case xx709_375 of
                                                        '+' -> return ()
                                                        _ -> gets position >>= (throwError . mkParseError "'+'" "not match pattern: " "" d710_374 ["char"])
                                                    let '+' = xx709_375
                                                    return ()
                                                    return (FromList1 rf),
                                                 do rf <- StateT readFrom
                                                    d714_376 <- get
                                                    xx713_377 <- StateT char
                                                    case xx713_377 of
                                                        '?' -> return ()
                                                        _ -> gets position >>= (throwError . mkParseError "'?'" "not match pattern: " "" d714_376 ["char"])
                                                    let '?' = xx713_377
                                                    return ()
                                                    return (FromOptional rf),
                                                 do rf <- StateT readFrom
                                                    return rf]
                readFrom43_110 = foldl1 mplus [do v <- StateT variable
                                                  return (FromVariable v),
                                               do d720_378 <- get
                                                  xx719_379 <- StateT char
                                                  case xx719_379 of
                                                      '(' -> return ()
                                                      _ -> gets position >>= (throwError . mkParseError "'('" "not match pattern: " "" d720_378 ["char"])
                                                  let '(' = xx719_379
                                                  return ()
                                                  s <- StateT selection
                                                  d724_380 <- get
                                                  xx723_381 <- StateT char
                                                  case xx723_381 of
                                                      ')' -> return ()
                                                      _ -> gets position >>= (throwError . mkParseError "')'" "not match pattern: " "" d724_380 ["char"])
                                                  let ')' = xx723_381
                                                  return ()
                                                  return (FromSelection s)]
                test44_111 = foldl1 mplus [do d726_382 <- get
                                              xx725_383 <- StateT char
                                              case xx725_383 of
                                                  '[' -> return ()
                                                  _ -> gets position >>= (throwError . mkParseError "'['" "not match pattern: " "" d726_382 ["char"])
                                              let '[' = xx725_383
                                              return ()
                                              h <- StateT hsExpLam
                                              _ <- StateT spaces
                                              return ()
                                              com <- optional3_299 (StateT comForErr)
                                              d734_384 <- get
                                              xx733_385 <- StateT char
                                              case xx733_385 of
                                                  ']' -> return ()
                                                  _ -> gets position >>= (throwError . mkParseError "']'" "not match pattern: " "" d734_384 ["char"])
                                              let ']' = xx733_385
                                              return ()
                                              return (h, maybe "" id com)]
                hsExpLam45_112 = foldl1 mplus [do d736_386 <- get
                                                  xx735_387 <- StateT char
                                                  case xx735_387 of
                                                      '\\' -> return ()
                                                      _ -> gets position >>= (throwError . mkParseError "'\\\\'" "not match pattern: " "" d736_386 ["char"])
                                                  let '\\' = xx735_387
                                                  return ()
                                                  _ <- StateT spaces
                                                  return ()
                                                  ps <- StateT pats
                                                  _ <- StateT spaces
                                                  return ()
                                                  d744_388 <- get
                                                  xx743_389 <- StateT char
                                                  case xx743_389 of
                                                      '-' -> return ()
                                                      _ -> gets position >>= (throwError . mkParseError "'-'" "not match pattern: " "" d744_388 ["char"])
                                                  let '-' = xx743_389
                                                  return ()
                                                  d746_390 <- get
                                                  xx745_391 <- StateT char
                                                  let c = xx745_391
                                                  unless (isGt c) (gets position >>= (throwError . mkParseError "isGt c" "not match: " "" d746_390 ["char"]))
                                                  _ <- StateT spaces
                                                  return ()
                                                  e <- StateT hsExpTyp
                                                  return (lamE ps e),
                                               do e <- StateT hsExpTyp
                                                  return e]
                hsExpTyp46_113 = foldl1 mplus [do eo <- StateT hsExpOp
                                                  d756_392 <- get
                                                  xx755_393 <- StateT char
                                                  case xx755_393 of
                                                      ':' -> return ()
                                                      _ -> gets position >>= (throwError . mkParseError "':'" "not match pattern: " "" d756_392 ["char"])
                                                  let ':' = xx755_393
                                                  return ()
                                                  d758_394 <- get
                                                  xx757_395 <- StateT char
                                                  case xx757_395 of
                                                      ':' -> return ()
                                                      _ -> gets position >>= (throwError . mkParseError "':'" "not match pattern: " "" d758_394 ["char"])
                                                  let ':' = xx757_395
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
                hsOp48_115 = foldl1 mplus [do d778_396 <- get
                                              xx777_397 <- StateT char
                                              let c = xx777_397
                                              unless (isOpHeadChar c) (gets position >>= (throwError . mkParseError "isOpHeadChar c" "not match: " "" d778_396 ["char"]))
                                              o <- StateT opTail
                                              return (varE (mkName (cons c o))),
                                           do d782_398 <- get
                                              xx781_399 <- StateT char
                                              case xx781_399 of
                                                  ':' -> return ()
                                                  _ -> gets position >>= (throwError . mkParseError "':'" "not match pattern: " "" d782_398 ["char"])
                                              let ':' = xx781_399
                                              return ()
                                              ddd783_400 <- get
                                              do err <- ((do d785_401 <- get
                                                             xx784_402 <- StateT char
                                                             case xx784_402 of
                                                                 ':' -> return ()
                                                                 _ -> gets position >>= (throwError . mkParseError "':'" "not match pattern: " "" d785_401 ["char"])
                                                             let ':' = xx784_402
                                                             return ()) >> return False) `catchError` const (return True)
                                                 unless err (gets position >>= (throwError . mkParseError ('!' : "':':") "not match: " "" ddd783_400 ["char"]))
                                              put ddd783_400
                                              o <- StateT opTail
                                              return (conE (mkName (':' : o))),
                                           do d789_403 <- get
                                              xx788_404 <- StateT char
                                              let c = xx788_404
                                              unless (isBQ c) (gets position >>= (throwError . mkParseError "isBQ c" "not match: " "" d789_403 ["char"]))
                                              v <- StateT variable
                                              d793_405 <- get
                                              xx792_406 <- StateT char
                                              let c_ = xx792_406
                                              unless (isBQ c_) (gets position >>= (throwError . mkParseError "isBQ c_" "not match: " "" d793_405 ["char"]))
                                              return (varE (mkName v)),
                                           do d795_407 <- get
                                              xx794_408 <- StateT char
                                              let c = xx794_408
                                              unless (isBQ c) (gets position >>= (throwError . mkParseError "isBQ c" "not match: " "" d795_407 ["char"]))
                                              t <- StateT typ
                                              d799_409 <- get
                                              xx798_410 <- StateT char
                                              let c_ = xx798_410
                                              unless (isBQ c_) (gets position >>= (throwError . mkParseError "isBQ c_" "not match: " "" d799_409 ["char"]))
                                              return (conE (mkName t))]
                opTail49_116 = foldl1 mplus [do d801_411 <- get
                                                xx800_412 <- StateT char
                                                let c = xx800_412
                                                unless (isOpTailChar c) (gets position >>= (throwError . mkParseError "isOpTailChar c" "not match: " "" d801_411 ["char"]))
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
                hsExp151_118 = foldl1 mplus [do d813_413 <- get
                                                xx812_414 <- StateT char
                                                case xx812_414 of
                                                    '(' -> return ()
                                                    _ -> gets position >>= (throwError . mkParseError "'('" "not match pattern: " "" d813_413 ["char"])
                                                let '(' = xx812_414
                                                return ()
                                                l <- optional3_299 (foldl1 mplus [do e <- StateT hsExpTyp
                                                                                     return e])
                                                _ <- StateT spaces
                                                return ()
                                                o <- StateT hsOp
                                                _ <- StateT spaces
                                                return ()
                                                r <- optional3_299 (foldl1 mplus [do e <- StateT hsExpTyp
                                                                                     return e])
                                                d829_415 <- get
                                                xx828_416 <- StateT char
                                                case xx828_416 of
                                                    ')' -> return ()
                                                    _ -> gets position >>= (throwError . mkParseError "')'" "not match pattern: " "" d829_415 ["char"])
                                                let ')' = xx828_416
                                                return ()
                                                return (infixE l o r),
                                             do d831_417 <- get
                                                xx830_418 <- StateT char
                                                case xx830_418 of
                                                    '(' -> return ()
                                                    _ -> gets position >>= (throwError . mkParseError "'('" "not match pattern: " "" d831_417 ["char"])
                                                let '(' = xx830_418
                                                return ()
                                                et <- StateT hsExpTpl
                                                d835_419 <- get
                                                xx834_420 <- StateT char
                                                case xx834_420 of
                                                    ')' -> return ()
                                                    _ -> gets position >>= (throwError . mkParseError "')'" "not match pattern: " "" d835_419 ["char"])
                                                let ')' = xx834_420
                                                return ()
                                                return (tupE et),
                                             do d837_421 <- get
                                                xx836_422 <- StateT char
                                                case xx836_422 of
                                                    '[' -> return ()
                                                    _ -> gets position >>= (throwError . mkParseError "'['" "not match pattern: " "" d837_421 ["char"])
                                                let '[' = xx836_422
                                                return ()
                                                et <- StateT hsExpTpl
                                                d841_423 <- get
                                                xx840_424 <- StateT char
                                                case xx840_424 of
                                                    ']' -> return ()
                                                    _ -> gets position >>= (throwError . mkParseError "']'" "not match pattern: " "" d841_423 ["char"])
                                                let ']' = xx840_424
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
                                             do d851_425 <- get
                                                xx850_426 <- StateT char
                                                case xx850_426 of
                                                    '\'' -> return ()
                                                    _ -> gets position >>= (throwError . mkParseError "'\\''" "not match pattern: " "" d851_425 ["char"])
                                                let '\'' = xx850_426
                                                return ()
                                                c <- StateT charLit
                                                d855_427 <- get
                                                xx854_428 <- StateT char
                                                case xx854_428 of
                                                    '\'' -> return ()
                                                    _ -> gets position >>= (throwError . mkParseError "'\\''" "not match pattern: " "" d855_427 ["char"])
                                                let '\'' = xx854_428
                                                return ()
                                                return (litE (charL c)),
                                             do d857_429 <- get
                                                xx856_430 <- StateT char
                                                case xx856_430 of
                                                    '"' -> return ()
                                                    _ -> gets position >>= (throwError . mkParseError "'\"'" "not match pattern: " "" d857_429 ["char"])
                                                let '"' = xx856_430
                                                return ()
                                                s <- StateT stringLit
                                                d861_431 <- get
                                                xx860_432 <- StateT char
                                                case xx860_432 of
                                                    '"' -> return ()
                                                    _ -> gets position >>= (throwError . mkParseError "'\"'" "not match pattern: " "" d861_431 ["char"])
                                                let '"' = xx860_432
                                                return ()
                                                return (litE (stringL s)),
                                             do d863_433 <- get
                                                xx862_434 <- StateT char
                                                case xx862_434 of
                                                    '-' -> return ()
                                                    _ -> gets position >>= (throwError . mkParseError "'-'" "not match pattern: " "" d863_433 ["char"])
                                                let '-' = xx862_434
                                                return ()
                                                _ <- StateT spaces
                                                return ()
                                                e <- StateT hsExp1
                                                return (appE (varE $ mkName "negate") e)]
                hsExpTpl52_119 = foldl1 mplus [do e <- StateT hsExpLam
                                                  _ <- StateT spaces
                                                  return ()
                                                  d873_435 <- get
                                                  xx872_436 <- StateT char
                                                  let c = xx872_436
                                                  unless (isComma c) (gets position >>= (throwError . mkParseError "isComma c" "not match: " "" d873_435 ["char"]))
                                                  _ <- StateT spaces
                                                  return ()
                                                  et <- StateT hsExpTpl
                                                  return (cons e et),
                                               do e <- StateT hsExpLam
                                                  return (cons e emp),
                                               return emp]
                hsTypeArr53_120 = foldl1 mplus [do l <- StateT hsType
                                                   d883_437 <- get
                                                   xx882_438 <- StateT char
                                                   case xx882_438 of
                                                       '-' -> return ()
                                                       _ -> gets position >>= (throwError . mkParseError "'-'" "not match pattern: " "" d883_437 ["char"])
                                                   let '-' = xx882_438
                                                   return ()
                                                   d885_439 <- get
                                                   xx884_440 <- StateT char
                                                   let c = xx884_440
                                                   unless (isGt c) (gets position >>= (throwError . mkParseError "isGt c" "not match: " "" d885_439 ["char"]))
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
                hsType155_122 = foldl1 mplus [do d899_441 <- get
                                                 xx898_442 <- StateT char
                                                 case xx898_442 of
                                                     '[' -> return ()
                                                     _ -> gets position >>= (throwError . mkParseError "'['" "not match pattern: " "" d899_441 ["char"])
                                                 let '[' = xx898_442
                                                 return ()
                                                 d901_443 <- get
                                                 xx900_444 <- StateT char
                                                 case xx900_444 of
                                                     ']' -> return ()
                                                     _ -> gets position >>= (throwError . mkParseError "']'" "not match pattern: " "" d901_443 ["char"])
                                                 let ']' = xx900_444
                                                 return ()
                                                 _ <- StateT spaces
                                                 return ()
                                                 return listT,
                                              do d905_445 <- get
                                                 xx904_446 <- StateT char
                                                 case xx904_446 of
                                                     '[' -> return ()
                                                     _ -> gets position >>= (throwError . mkParseError "'['" "not match pattern: " "" d905_445 ["char"])
                                                 let '[' = xx904_446
                                                 return ()
                                                 t <- StateT hsTypeArr
                                                 d909_447 <- get
                                                 xx908_448 <- StateT char
                                                 case xx908_448 of
                                                     ']' -> return ()
                                                     _ -> gets position >>= (throwError . mkParseError "']'" "not match pattern: " "" d909_447 ["char"])
                                                 let ']' = xx908_448
                                                 return ()
                                                 _ <- StateT spaces
                                                 return ()
                                                 return (appT listT t),
                                              do d913_449 <- get
                                                 xx912_450 <- StateT char
                                                 case xx912_450 of
                                                     '(' -> return ()
                                                     _ -> gets position >>= (throwError . mkParseError "'('" "not match pattern: " "" d913_449 ["char"])
                                                 let '(' = xx912_450
                                                 return ()
                                                 _ <- StateT spaces
                                                 return ()
                                                 tt <- StateT hsTypeTpl
                                                 d919_451 <- get
                                                 xx918_452 <- StateT char
                                                 case xx918_452 of
                                                     ')' -> return ()
                                                     _ -> gets position >>= (throwError . mkParseError "')'" "not match pattern: " "" d919_451 ["char"])
                                                 let ')' = xx918_452
                                                 return ()
                                                 return (tupT tt),
                                              do t <- StateT typToken
                                                 return (conT (mkName t)),
                                              do d923_453 <- get
                                                 xx922_454 <- StateT char
                                                 case xx922_454 of
                                                     '(' -> return ()
                                                     _ -> gets position >>= (throwError . mkParseError "'('" "not match pattern: " "" d923_453 ["char"])
                                                 let '(' = xx922_454
                                                 return ()
                                                 d925_455 <- get
                                                 xx924_456 <- StateT char
                                                 case xx924_456 of
                                                     '-' -> return ()
                                                     _ -> gets position >>= (throwError . mkParseError "'-'" "not match pattern: " "" d925_455 ["char"])
                                                 let '-' = xx924_456
                                                 return ()
                                                 d927_457 <- get
                                                 xx926_458 <- StateT char
                                                 let c = xx926_458
                                                 unless (isGt c) (gets position >>= (throwError . mkParseError "isGt c" "not match: " "" d927_457 ["char"]))
                                                 d929_459 <- get
                                                 xx928_460 <- StateT char
                                                 case xx928_460 of
                                                     ')' -> return ()
                                                     _ -> gets position >>= (throwError . mkParseError "')'" "not match pattern: " "" d929_459 ["char"])
                                                 let ')' = xx928_460
                                                 return ()
                                                 _ <- StateT spaces
                                                 return ()
                                                 return arrowT]
                hsTypeTpl56_123 = foldl1 mplus [do t <- StateT hsTypeArr
                                                   d935_461 <- get
                                                   xx934_462 <- StateT char
                                                   let c = xx934_462
                                                   unless (isComma c) (gets position >>= (throwError . mkParseError "isComma c" "not match: " "" d935_461 ["char"]))
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
                                                 ds <- list1_296 (foldl1 mplus [do d <- StateT digit
                                                                                   return d])
                                                 return (read (cons dh ds))]
                alpha61_128 = foldl1 mplus [do u <- StateT upper
                                               return u,
                                            do l <- StateT lower
                                               return l,
                                            do d <- StateT digit
                                               return d,
                                            do d967_463 <- get
                                               xx966_464 <- StateT char
                                               case xx966_464 of
                                                   '\'' -> return ()
                                                   _ -> gets position >>= (throwError . mkParseError "'\\''" "not match pattern: " "" d967_463 ["char"])
                                               let '\'' = xx966_464
                                               return ()
                                               return '\'']
                upper62_129 = foldl1 mplus [do d969_465 <- get
                                               xx968_466 <- StateT char
                                               let u = xx968_466
                                               unless (isUpper u) (gets position >>= (throwError . mkParseError "isUpper u" "not match: " "" d969_465 ["char"]))
                                               return u]
                lower63_130 = foldl1 mplus [do d971_467 <- get
                                               xx970_468 <- StateT char
                                               let l = xx970_468
                                               unless (isLowerU l) (gets position >>= (throwError . mkParseError "isLowerU l" "not match: " "" d971_467 ["char"]))
                                               return l]
                digit64_131 = foldl1 mplus [do d973_469 <- get
                                               xx972_470 <- StateT char
                                               let d = xx972_470
                                               unless (isDigit d) (gets position >>= (throwError . mkParseError "isDigit d" "not match: " "" d973_469 ["char"]))
                                               return d]
                spaces65_132 = foldl1 mplus [do _ <- StateT space
                                                return ()
                                                _ <- StateT spaces
                                                return ()
                                                return (),
                                             return ()]
                space66_133 = foldl1 mplus [do d979_471 <- get
                                               xx978_472 <- StateT char
                                               let s = xx978_472
                                               unless (isSpace s) (gets position >>= (throwError . mkParseError "isSpace s" "not match: " "" d979_471 ["char"]))
                                               return (),
                                            do d981_473 <- get
                                               xx980_474 <- StateT char
                                               case xx980_474 of
                                                   '-' -> return ()
                                                   _ -> gets position >>= (throwError . mkParseError "'-'" "not match pattern: " "" d981_473 ["char"])
                                               let '-' = xx980_474
                                               return ()
                                               d983_475 <- get
                                               xx982_476 <- StateT char
                                               case xx982_476 of
                                                   '-' -> return ()
                                                   _ -> gets position >>= (throwError . mkParseError "'-'" "not match pattern: " "" d983_475 ["char"])
                                               let '-' = xx982_476
                                               return ()
                                               _ <- StateT notNLString
                                               return ()
                                               _ <- StateT newLine
                                               return ()
                                               return (),
                                            do _ <- StateT comment
                                               return ()
                                               return ()]
                notNLString67_134 = foldl1 mplus [do ddd990_477 <- get
                                                     do err <- ((do _ <- StateT newLine
                                                                    return ()) >> return False) `catchError` const (return True)
                                                        unless err (gets position >>= (throwError . mkParseError ('!' : "_:newLine") "not match: " "" ddd990_477 ["newLine"]))
                                                     put ddd990_477
                                                     c <- StateT char
                                                     s <- StateT notNLString
                                                     return (cons c s),
                                                  return emp]
                newLine68_135 = foldl1 mplus [do d998_478 <- get
                                                 xx997_479 <- StateT char
                                                 case xx997_479 of
                                                     '\n' -> return ()
                                                     _ -> gets position >>= (throwError . mkParseError "'\\n'" "not match pattern: " "" d998_478 ["char"])
                                                 let '\n' = xx997_479
                                                 return ()
                                                 return ()]
                comment69_136 = foldl1 mplus [do d1000_480 <- get
                                                 xx999_481 <- StateT char
                                                 case xx999_481 of
                                                     '{' -> return ()
                                                     _ -> gets position >>= (throwError . mkParseError "'{'" "not match pattern: " "" d1000_480 ["char"])
                                                 let '{' = xx999_481
                                                 return ()
                                                 d1002_482 <- get
                                                 xx1001_483 <- StateT char
                                                 case xx1001_483 of
                                                     '-' -> return ()
                                                     _ -> gets position >>= (throwError . mkParseError "'-'" "not match pattern: " "" d1002_482 ["char"])
                                                 let '-' = xx1001_483
                                                 return ()
                                                 ddd1003_484 <- get
                                                 do err <- ((do d1005_485 <- get
                                                                xx1004_486 <- StateT char
                                                                case xx1004_486 of
                                                                    '#' -> return ()
                                                                    _ -> gets position >>= (throwError . mkParseError "'#'" "not match pattern: " "" d1005_485 ["char"])
                                                                let '#' = xx1004_486
                                                                return ()) >> return False) `catchError` const (return True)
                                                    unless err (gets position >>= (throwError . mkParseError ('!' : "'#':") "not match: " "" ddd1003_484 ["char"]))
                                                 put ddd1003_484
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
                notComStr71_138 = foldl1 mplus [do ddd1018_487 <- get
                                                   do err <- ((do _ <- StateT comment
                                                                  return ()) >> return False) `catchError` const (return True)
                                                      unless err (gets position >>= (throwError . mkParseError ('!' : "_:comment") "not match: " "" ddd1018_487 ["comment"]))
                                                   put ddd1018_487
                                                   ddd1021_488 <- get
                                                   do err <- ((do _ <- StateT comEnd
                                                                  return ()) >> return False) `catchError` const (return True)
                                                      unless err (gets position >>= (throwError . mkParseError ('!' : "_:comEnd") "not match: " "" ddd1021_488 ["comEnd"]))
                                                   put ddd1021_488
                                                   _ <- StateT char
                                                   return ()
                                                   _ <- StateT notComStr
                                                   return ()
                                                   return (),
                                                return ()]
                comEnd72_139 = foldl1 mplus [do d1029_489 <- get
                                                xx1028_490 <- StateT char
                                                case xx1028_490 of
                                                    '-' -> return ()
                                                    _ -> gets position >>= (throwError . mkParseError "'-'" "not match pattern: " "" d1029_489 ["char"])
                                                let '-' = xx1028_490
                                                return ()
                                                d1031_491 <- get
                                                xx1030_492 <- StateT char
                                                case xx1030_492 of
                                                    '}' -> return ()
                                                    _ -> gets position >>= (throwError . mkParseError "'}'" "not match pattern: " "" d1031_491 ["char"])
                                                let '}' = xx1030_492
                                                return ()
                                                return ()]
                list1_296 :: forall m a . (MonadPlus m, Applicative m) =>
                                          m a -> m ([a])
                list12_493 :: forall m a . (MonadPlus m, Applicative m) =>
                                           m a -> m ([a])
                list1_296 p = list12_493 p `mplus` return []
                list12_493 p = ((:) <$> p) <*> list1_296 p
                optional3_299 :: forall m a . (MonadPlus m, Applicative m) =>
                                              m a -> m (Maybe a)
                optional3_299 p = (Just <$> p) `mplus` return Nothing

