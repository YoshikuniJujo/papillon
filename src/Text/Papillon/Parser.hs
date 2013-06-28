{-# LANGUAGE FlexibleContexts, TemplateHaskell, UndecidableInstances, FlexibleContexts, PackageImports, TypeFamilies, RankNTypes, FlexibleInstances #-}
module  Text.Papillon.Parser (
	Peg,
	Definition,
	Selection,
	ExpressionHs,
	NameLeaf(..),
	NameLeaf_(..),
	ReadFrom(..),
	parse,
	initialPos,
	showNameLeaf,
	nameFromRF,
	ParseError(..),
	Derivs(peg, pegFile, derivsChars),
	Pos(..),
	ListPos(..)
)  where
import "monads-tf" Control.Monad.State
import "monads-tf" Control.Monad.Error
import Control.Monad.Trans.Error (Error (..))

import Control.Applicative



import Data.Char
import Language.Haskell.TH
import Text.Papillon.SyntaxTree

data Derivs
    = Derivs {pegFile :: (Either (ParseError (Pos String))
                                 ((PegFile, Derivs))),
              pragma :: (Either (ParseError (Pos String))
                                ((Maybe String, Derivs))),
              pragmaStr :: (Either (ParseError (Pos String)) ((String, Derivs))),
              pragmaItems :: (Either (ParseError (Pos String))
                                     (([String], Derivs))),
              delPragmas :: (Either (ParseError (Pos String)) (((), Derivs))),
              pragmaEnd :: (Either (ParseError (Pos String)) (((), Derivs))),
              moduleDec :: (Either (ParseError (Pos String))
                                   ((Maybe String, Derivs))),
              moduleDecStr :: (Either (ParseError (Pos String))
                                      ((String, Derivs))),
              whr :: (Either (ParseError (Pos String)) (((), Derivs))),
              preImpPap :: (Either (ParseError (Pos String)) ((String, Derivs))),
              prePeg :: (Either (ParseError (Pos String)) ((String, Derivs))),
              afterPeg :: (Either (ParseError (Pos String)) ((String, Derivs))),
              importPapillon :: (Either (ParseError (Pos String))
                                        (((), Derivs))),
              varToken :: (Either (ParseError (Pos String)) ((String, Derivs))),
              typToken :: (Either (ParseError (Pos String)) ((String, Derivs))),
              pap :: (Either (ParseError (Pos String)) (((), Derivs))),
              peg :: (Either (ParseError (Pos String)) ((TTPeg, Derivs))),
              sourceType :: (Either (ParseError (Pos String))
                                    ((String, Derivs))),
              peg_ :: (Either (ParseError (Pos String)) ((Peg, Derivs))),
              definition :: (Either (ParseError (Pos String))
                                    ((Definition, Derivs))),
              selection :: (Either (ParseError (Pos String))
                                   ((Selection, Derivs))),
              expressionHs :: (Either (ParseError (Pos String))
                                      ((ExpressionHs, Derivs))),
              expression :: (Either (ParseError (Pos String))
                                    ((Expression, Derivs))),
              nameLeaf_ :: (Either (ParseError (Pos String))
                                   ((NameLeaf_, Derivs))),
              nameLeaf :: (Either (ParseError (Pos String))
                                  ((NameLeaf, Derivs))),
              nameLeafNoCom :: (Either (ParseError (Pos String))
                                       ((NameLeaf, Derivs))),
              comForErr :: (Either (ParseError (Pos String)) ((String, Derivs))),
              leaf :: (Either (ParseError (Pos String))
                              (((ReadFrom, Maybe ((ExpQ, String))), Derivs))),
              patOp :: (Either (ParseError (Pos String)) ((PatQ, Derivs))),
              pat :: (Either (ParseError (Pos String)) ((PatQ, Derivs))),
              pat1 :: (Either (ParseError (Pos String)) ((PatQ, Derivs))),
              patList :: (Either (ParseError (Pos String)) (([PatQ], Derivs))),
              opConName :: (Either (ParseError (Pos String)) ((Name, Derivs))),
              charLit :: (Either (ParseError (Pos String)) ((Char, Derivs))),
              stringLit :: (Either (ParseError (Pos String)) ((String, Derivs))),
              escapeC :: (Either (ParseError (Pos String)) ((Char, Derivs))),
              pats :: (Either (ParseError (Pos String)) ((PatQs, Derivs))),
              readFromLs :: (Either (ParseError (Pos String))
                                    ((ReadFrom, Derivs))),
              readFrom :: (Either (ParseError (Pos String))
                                  ((ReadFrom, Derivs))),
              test :: (Either (ParseError (Pos String))
                              (((ExR, String), Derivs))),
              hsExpLam :: (Either (ParseError (Pos String)) ((ExR, Derivs))),
              hsExpTyp :: (Either (ParseError (Pos String)) ((ExR, Derivs))),
              hsExpOp :: (Either (ParseError (Pos String)) ((ExR, Derivs))),
              hsOp :: (Either (ParseError (Pos String)) ((ExR, Derivs))),
              opTail :: (Either (ParseError (Pos String)) ((String, Derivs))),
              hsExp :: (Either (ParseError (Pos String)) ((Ex, Derivs))),
              hsExp1 :: (Either (ParseError (Pos String)) ((ExR, Derivs))),
              hsExpTpl :: (Either (ParseError (Pos String)) ((ExRL, Derivs))),
              hsTypeArr :: (Either (ParseError (Pos String)) ((TypeQ, Derivs))),
              hsType :: (Either (ParseError (Pos String)) ((Typ, Derivs))),
              hsType1 :: (Either (ParseError (Pos String)) ((TypeQ, Derivs))),
              hsTypeTpl :: (Either (ParseError (Pos String)) ((TypeQL, Derivs))),
              typ :: (Either (ParseError (Pos String)) ((String, Derivs))),
              variable :: (Either (ParseError (Pos String)) ((String, Derivs))),
              tvtail :: (Either (ParseError (Pos String)) ((String, Derivs))),
              integer :: (Either (ParseError (Pos String)) ((Integer, Derivs))),
              alpha :: (Either (ParseError (Pos String)) ((Char, Derivs))),
              upper :: (Either (ParseError (Pos String)) ((Char, Derivs))),
              lower :: (Either (ParseError (Pos String)) ((Char, Derivs))),
              digit :: (Either (ParseError (Pos String)) ((Char, Derivs))),
              spaces :: (Either (ParseError (Pos String)) (((), Derivs))),
              space :: (Either (ParseError (Pos String)) (((), Derivs))),
              notNLString :: (Either (ParseError (Pos String))
                                     ((String, Derivs))),
              newLine :: (Either (ParseError (Pos String)) (((), Derivs))),
              comment :: (Either (ParseError (Pos String)) (((), Derivs))),
              comments :: (Either (ParseError (Pos String)) (((), Derivs))),
              notComStr :: (Either (ParseError (Pos String)) (((), Derivs))),
              comEnd :: (Either (ParseError (Pos String)) (((), Derivs))),
              derivsChars :: (Either (ParseError (Pos String))
                                     ((Token String, Derivs))),
              derivsPosition :: (Pos String)}
data ParseError pos
    = ParseError String String String Derivs ([String]) pos
instance Error (ParseError pos)
    where strMsg msg = ParseError "" msg "" undefined undefined undefined
parse :: String -> Derivs
parse = parse0_0 initialPos
          where parse0_0 pos s = d
                             where d = Derivs pegFile69_1 pragma70_2 pragmaStr71_3 pragmaItems72_4 delPragmas73_5 pragmaEnd74_6 moduleDec75_7 moduleDecStr76_8 whr77_9 preImpPap78_10 prePeg79_11 afterPeg80_12 importPapillon81_13 varToken82_14 typToken83_15 pap84_16 peg85_17 sourceType86_18 peg_87_19 definition88_20 selection89_21 expressionHs90_22 expression91_23 nameLeaf_92_24 nameLeaf93_25 nameLeafNoCom94_26 comForErr95_27 leaf96_28 patOp97_29 pat98_30 pat199_31 patList100_32 opConName101_33 charLit102_34 stringLit103_35 escapeC104_36 pats105_37 readFromLs106_38 readFrom107_39 test108_40 hsExpLam109_41 hsExpTyp110_42 hsExpOp111_43 hsOp112_44 opTail113_45 hsExp114_46 hsExp1115_47 hsExpTpl116_48 hsTypeArr117_49 hsType118_50 hsType1119_51 hsTypeTpl120_52 typ121_53 variable122_54 tvtail123_55 integer124_56 alpha125_57 upper126_58 lower127_59 digit128_60 spaces129_61 space130_62 notNLString131_63 newLine132_64 comment133_65 comments134_66 notComStr135_67 comEnd136_68 chars137_69 pos
                                   pegFile69_1 = runStateT pegFile1_70 d
                                   pragma70_2 = runStateT pragma2_71 d
                                   pragmaStr71_3 = runStateT pragmaStr3_72 d
                                   pragmaItems72_4 = runStateT pragmaItems4_73 d
                                   delPragmas73_5 = runStateT delPragmas5_74 d
                                   pragmaEnd74_6 = runStateT pragmaEnd6_75 d
                                   moduleDec75_7 = runStateT moduleDec7_76 d
                                   moduleDecStr76_8 = runStateT moduleDecStr8_77 d
                                   whr77_9 = runStateT whr9_78 d
                                   preImpPap78_10 = runStateT preImpPap10_79 d
                                   prePeg79_11 = runStateT prePeg11_80 d
                                   afterPeg80_12 = runStateT afterPeg12_81 d
                                   importPapillon81_13 = runStateT importPapillon13_82 d
                                   varToken82_14 = runStateT varToken14_83 d
                                   typToken83_15 = runStateT typToken15_84 d
                                   pap84_16 = runStateT pap16_85 d
                                   peg85_17 = runStateT peg17_86 d
                                   sourceType86_18 = runStateT sourceType18_87 d
                                   peg_87_19 = runStateT peg_19_88 d
                                   definition88_20 = runStateT definition20_89 d
                                   selection89_21 = runStateT selection21_90 d
                                   expressionHs90_22 = runStateT expressionHs22_91 d
                                   expression91_23 = runStateT expression23_92 d
                                   nameLeaf_92_24 = runStateT nameLeaf_24_93 d
                                   nameLeaf93_25 = runStateT nameLeaf25_94 d
                                   nameLeafNoCom94_26 = runStateT nameLeafNoCom26_95 d
                                   comForErr95_27 = runStateT comForErr27_96 d
                                   leaf96_28 = runStateT leaf28_97 d
                                   patOp97_29 = runStateT patOp29_98 d
                                   pat98_30 = runStateT pat30_99 d
                                   pat199_31 = runStateT pat131_100 d
                                   patList100_32 = runStateT patList32_101 d
                                   opConName101_33 = runStateT opConName33_102 d
                                   charLit102_34 = runStateT charLit34_103 d
                                   stringLit103_35 = runStateT stringLit35_104 d
                                   escapeC104_36 = runStateT escapeC36_105 d
                                   pats105_37 = runStateT pats37_106 d
                                   readFromLs106_38 = runStateT readFromLs38_107 d
                                   readFrom107_39 = runStateT readFrom39_108 d
                                   test108_40 = runStateT test40_109 d
                                   hsExpLam109_41 = runStateT hsExpLam41_110 d
                                   hsExpTyp110_42 = runStateT hsExpTyp42_111 d
                                   hsExpOp111_43 = runStateT hsExpOp43_112 d
                                   hsOp112_44 = runStateT hsOp44_113 d
                                   opTail113_45 = runStateT opTail45_114 d
                                   hsExp114_46 = runStateT hsExp46_115 d
                                   hsExp1115_47 = runStateT hsExp147_116 d
                                   hsExpTpl116_48 = runStateT hsExpTpl48_117 d
                                   hsTypeArr117_49 = runStateT hsTypeArr49_118 d
                                   hsType118_50 = runStateT hsType50_119 d
                                   hsType1119_51 = runStateT hsType151_120 d
                                   hsTypeTpl120_52 = runStateT hsTypeTpl52_121 d
                                   typ121_53 = runStateT typ53_122 d
                                   variable122_54 = runStateT variable54_123 d
                                   tvtail123_55 = runStateT tvtail55_124 d
                                   integer124_56 = runStateT integer56_125 d
                                   alpha125_57 = runStateT alpha57_126 d
                                   upper126_58 = runStateT upper58_127 d
                                   lower127_59 = runStateT lower59_128 d
                                   digit128_60 = runStateT digit60_129 d
                                   spaces129_61 = runStateT spaces61_130 d
                                   space130_62 = runStateT space62_131 d
                                   notNLString131_63 = runStateT notNLString63_132 d
                                   newLine132_64 = runStateT newLine64_133 d
                                   comment133_65 = runStateT comment65_134 d
                                   comments134_66 = runStateT comments66_135 d
                                   notComStr135_67 = runStateT notComStr67_136 d
                                   comEnd136_68 = runStateT comEnd68_137 d
                                   chars137_69 = runStateT (case getToken s of
                                                                Just (c,
                                                                      s') -> do put (parse0_0 (updatePos c pos) s')
                                                                                return c
                                                                _ -> gets derivsPosition >>= (throwError . ParseError "" "end of input" "" undefined [])) d
                pegFile1_70 = foldl1 mplus [do pr <- StateT pragma
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
                                               d155_138 <- get
                                               xx154_139 <- StateT derivsChars
                                               case xx154_139 of
                                                   '|' -> return ()
                                                   _ -> gets derivsPosition >>= (throwError . ParseError "'|'" "not match pattern: " "" d155_138 ["dvChars"])
                                               let '|' = xx154_139
                                               return ()
                                               d157_140 <- get
                                               xx156_141 <- StateT derivsChars
                                               case xx156_141 of
                                                   ']' -> return ()
                                                   _ -> gets derivsPosition >>= (throwError . ParseError "']'" "not match pattern: " "" d157_140 ["dvChars"])
                                               let ']' = xx156_141
                                               return ()
                                               d159_142 <- get
                                               xx158_143 <- StateT derivsChars
                                               case xx158_143 of
                                                   '\n' -> return ()
                                                   _ -> gets derivsPosition >>= (throwError . ParseError "'\\n'" "not match pattern: " "" d159_142 ["dvChars"])
                                               let '\n' = xx158_143
                                               return ()
                                               atp <- StateT afterPeg
                                               return (mkPegFile pr md pip pp p atp),
                                            do pr <- StateT pragma
                                               md <- StateT moduleDec
                                               pp <- StateT prePeg
                                               _ <- StateT pap
                                               return ()
                                               p <- StateT peg
                                               _ <- StateT spaces
                                               return ()
                                               d175_144 <- get
                                               xx174_145 <- StateT derivsChars
                                               case xx174_145 of
                                                   '|' -> return ()
                                                   _ -> gets derivsPosition >>= (throwError . ParseError "'|'" "not match pattern: " "" d175_144 ["dvChars"])
                                               let '|' = xx174_145
                                               return ()
                                               d177_146 <- get
                                               xx176_147 <- StateT derivsChars
                                               case xx176_147 of
                                                   ']' -> return ()
                                                   _ -> gets derivsPosition >>= (throwError . ParseError "']'" "not match pattern: " "" d177_146 ["dvChars"])
                                               let ']' = xx176_147
                                               return ()
                                               d179_148 <- get
                                               xx178_149 <- StateT derivsChars
                                               case xx178_149 of
                                                   '\n' -> return ()
                                                   _ -> gets derivsPosition >>= (throwError . ParseError "'\\n'" "not match pattern: " "" d179_148 ["dvChars"])
                                               let '\n' = xx178_149
                                               return ()
                                               atp <- StateT afterPeg
                                               return (mkPegFile pr md emp pp p atp)]
                pragma2_71 = foldl1 mplus [do _ <- StateT spaces
                                              return ()
                                              d185_150 <- get
                                              xx184_151 <- StateT derivsChars
                                              case xx184_151 of
                                                  '{' -> return ()
                                                  _ -> gets derivsPosition >>= (throwError . ParseError "'{'" "not match pattern: " "" d185_150 ["dvChars"])
                                              let '{' = xx184_151
                                              return ()
                                              d187_152 <- get
                                              xx186_153 <- StateT derivsChars
                                              case xx186_153 of
                                                  '-' -> return ()
                                                  _ -> gets derivsPosition >>= (throwError . ParseError "'-'" "not match pattern: " "" d187_152 ["dvChars"])
                                              let '-' = xx186_153
                                              return ()
                                              d189_154 <- get
                                              xx188_155 <- StateT derivsChars
                                              case xx188_155 of
                                                  '#' -> return ()
                                                  _ -> gets derivsPosition >>= (throwError . ParseError "'#'" "not match pattern: " "" d189_154 ["dvChars"])
                                              let '#' = xx188_155
                                              return ()
                                              _ <- StateT spaces
                                              return ()
                                              d193_156 <- get
                                              xx192_157 <- StateT derivsChars
                                              case xx192_157 of
                                                  'L' -> return ()
                                                  _ -> gets derivsPosition >>= (throwError . ParseError "'L'" "not match pattern: " "" d193_156 ["dvChars"])
                                              let 'L' = xx192_157
                                              return ()
                                              d195_158 <- get
                                              xx194_159 <- StateT derivsChars
                                              case xx194_159 of
                                                  'A' -> return ()
                                                  _ -> gets derivsPosition >>= (throwError . ParseError "'A'" "not match pattern: " "" d195_158 ["dvChars"])
                                              let 'A' = xx194_159
                                              return ()
                                              d197_160 <- get
                                              xx196_161 <- StateT derivsChars
                                              case xx196_161 of
                                                  'N' -> return ()
                                                  _ -> gets derivsPosition >>= (throwError . ParseError "'N'" "not match pattern: " "" d197_160 ["dvChars"])
                                              let 'N' = xx196_161
                                              return ()
                                              d199_162 <- get
                                              xx198_163 <- StateT derivsChars
                                              case xx198_163 of
                                                  'G' -> return ()
                                                  _ -> gets derivsPosition >>= (throwError . ParseError "'G'" "not match pattern: " "" d199_162 ["dvChars"])
                                              let 'G' = xx198_163
                                              return ()
                                              d201_164 <- get
                                              xx200_165 <- StateT derivsChars
                                              case xx200_165 of
                                                  'U' -> return ()
                                                  _ -> gets derivsPosition >>= (throwError . ParseError "'U'" "not match pattern: " "" d201_164 ["dvChars"])
                                              let 'U' = xx200_165
                                              return ()
                                              d203_166 <- get
                                              xx202_167 <- StateT derivsChars
                                              case xx202_167 of
                                                  'A' -> return ()
                                                  _ -> gets derivsPosition >>= (throwError . ParseError "'A'" "not match pattern: " "" d203_166 ["dvChars"])
                                              let 'A' = xx202_167
                                              return ()
                                              d205_168 <- get
                                              xx204_169 <- StateT derivsChars
                                              case xx204_169 of
                                                  'G' -> return ()
                                                  _ -> gets derivsPosition >>= (throwError . ParseError "'G'" "not match pattern: " "" d205_168 ["dvChars"])
                                              let 'G' = xx204_169
                                              return ()
                                              d207_170 <- get
                                              xx206_171 <- StateT derivsChars
                                              case xx206_171 of
                                                  'E' -> return ()
                                                  _ -> gets derivsPosition >>= (throwError . ParseError "'E'" "not match pattern: " "" d207_170 ["dvChars"])
                                              let 'E' = xx206_171
                                              return ()
                                              _ <- StateT spaces
                                              return ()
                                              s <- StateT pragmaItems
                                              _ <- StateT pragmaEnd
                                              return ()
                                              _ <- StateT spaces
                                              return ()
                                              return (just $ " LANGUAGE " ++ concatMap (++ ", ") s),
                                           do _ <- StateT spaces
                                              return ()
                                              return nothing]
                pragmaStr3_72 = foldl1 mplus [do _ <- StateT delPragmas
                                                 return ()
                                                 _ <- StateT spaces
                                                 return ()
                                                 d223_172 <- get
                                                 xx222_173 <- StateT derivsChars
                                                 case xx222_173 of
                                                     ',' -> return ()
                                                     _ -> gets derivsPosition >>= (throwError . ParseError "','" "not match pattern: " "" d223_172 ["dvChars"])
                                                 let ',' = xx222_173
                                                 return ()
                                                 _ <- StateT spaces
                                                 return ()
                                                 s <- StateT pragmaStr
                                                 return (' ' : s),
                                              do ddd228_174 <- get
                                                 do err <- ((do _ <- StateT pragmaEnd
                                                                return ()) >> return False) `catchError` const (return True)
                                                    unless err (gets derivsPosition >>= (throwError . ParseError ('!' : "_:pragmaEnd") "not match: " "" ddd228_174 ["pragmaEnd"]))
                                                 put ddd228_174
                                                 ddd231_175 <- get
                                                 do err <- ((do _ <- StateT delPragmas
                                                                return ()) >> return False) `catchError` const (return True)
                                                    unless err (gets derivsPosition >>= (throwError . ParseError ('!' : "_:delPragmas") "not match: " "" ddd231_175 ["delPragmas"]))
                                                 put ddd231_175
                                                 c <- StateT derivsChars
                                                 s <- StateT pragmaStr
                                                 return (c : s),
                                              return emp]
                pragmaItems4_73 = foldl1 mplus [do _ <- StateT delPragmas
                                                   return ()
                                                   _ <- StateT spaces
                                                   return ()
                                                   d243_176 <- get
                                                   xx242_177 <- StateT derivsChars
                                                   case xx242_177 of
                                                       ',' -> return ()
                                                       _ -> gets derivsPosition >>= (throwError . ParseError "','" "not match pattern: " "" d243_176 ["dvChars"])
                                                   let ',' = xx242_177
                                                   return ()
                                                   _ <- StateT spaces
                                                   return ()
                                                   i <- StateT pragmaItems
                                                   return i,
                                                do t <- StateT typToken
                                                   d251_178 <- get
                                                   xx250_179 <- StateT derivsChars
                                                   case xx250_179 of
                                                       ',' -> return ()
                                                       _ -> gets derivsPosition >>= (throwError . ParseError "','" "not match pattern: " "" d251_178 ["dvChars"])
                                                   let ',' = xx250_179
                                                   return ()
                                                   _ <- StateT spaces
                                                   return ()
                                                   i <- StateT pragmaItems
                                                   return (t : i),
                                                do _ <- StateT delPragmas
                                                   return ()
                                                   _ <- StateT spaces
                                                   return ()
                                                   return [],
                                                do t <- StateT typToken
                                                   return [t]]
                delPragmas5_74 = foldl1 mplus [do d263_180 <- get
                                                  xx262_181 <- StateT derivsChars
                                                  case xx262_181 of
                                                      'Q' -> return ()
                                                      _ -> gets derivsPosition >>= (throwError . ParseError "'Q'" "not match pattern: " "" d263_180 ["dvChars"])
                                                  let 'Q' = xx262_181
                                                  return ()
                                                  d265_182 <- get
                                                  xx264_183 <- StateT derivsChars
                                                  case xx264_183 of
                                                      'u' -> return ()
                                                      _ -> gets derivsPosition >>= (throwError . ParseError "'u'" "not match pattern: " "" d265_182 ["dvChars"])
                                                  let 'u' = xx264_183
                                                  return ()
                                                  d267_184 <- get
                                                  xx266_185 <- StateT derivsChars
                                                  case xx266_185 of
                                                      'a' -> return ()
                                                      _ -> gets derivsPosition >>= (throwError . ParseError "'a'" "not match pattern: " "" d267_184 ["dvChars"])
                                                  let 'a' = xx266_185
                                                  return ()
                                                  d269_186 <- get
                                                  xx268_187 <- StateT derivsChars
                                                  case xx268_187 of
                                                      's' -> return ()
                                                      _ -> gets derivsPosition >>= (throwError . ParseError "'s'" "not match pattern: " "" d269_186 ["dvChars"])
                                                  let 's' = xx268_187
                                                  return ()
                                                  d271_188 <- get
                                                  xx270_189 <- StateT derivsChars
                                                  case xx270_189 of
                                                      'i' -> return ()
                                                      _ -> gets derivsPosition >>= (throwError . ParseError "'i'" "not match pattern: " "" d271_188 ["dvChars"])
                                                  let 'i' = xx270_189
                                                  return ()
                                                  d273_190 <- get
                                                  xx272_191 <- StateT derivsChars
                                                  case xx272_191 of
                                                      'Q' -> return ()
                                                      _ -> gets derivsPosition >>= (throwError . ParseError "'Q'" "not match pattern: " "" d273_190 ["dvChars"])
                                                  let 'Q' = xx272_191
                                                  return ()
                                                  d275_192 <- get
                                                  xx274_193 <- StateT derivsChars
                                                  case xx274_193 of
                                                      'u' -> return ()
                                                      _ -> gets derivsPosition >>= (throwError . ParseError "'u'" "not match pattern: " "" d275_192 ["dvChars"])
                                                  let 'u' = xx274_193
                                                  return ()
                                                  d277_194 <- get
                                                  xx276_195 <- StateT derivsChars
                                                  case xx276_195 of
                                                      'o' -> return ()
                                                      _ -> gets derivsPosition >>= (throwError . ParseError "'o'" "not match pattern: " "" d277_194 ["dvChars"])
                                                  let 'o' = xx276_195
                                                  return ()
                                                  d279_196 <- get
                                                  xx278_197 <- StateT derivsChars
                                                  case xx278_197 of
                                                      't' -> return ()
                                                      _ -> gets derivsPosition >>= (throwError . ParseError "'t'" "not match pattern: " "" d279_196 ["dvChars"])
                                                  let 't' = xx278_197
                                                  return ()
                                                  d281_198 <- get
                                                  xx280_199 <- StateT derivsChars
                                                  case xx280_199 of
                                                      'e' -> return ()
                                                      _ -> gets derivsPosition >>= (throwError . ParseError "'e'" "not match pattern: " "" d281_198 ["dvChars"])
                                                  let 'e' = xx280_199
                                                  return ()
                                                  d283_200 <- get
                                                  xx282_201 <- StateT derivsChars
                                                  case xx282_201 of
                                                      's' -> return ()
                                                      _ -> gets derivsPosition >>= (throwError . ParseError "'s'" "not match pattern: " "" d283_200 ["dvChars"])
                                                  let 's' = xx282_201
                                                  return ()
                                                  return (),
                                               do d285_202 <- get
                                                  xx284_203 <- StateT derivsChars
                                                  case xx284_203 of
                                                      'T' -> return ()
                                                      _ -> gets derivsPosition >>= (throwError . ParseError "'T'" "not match pattern: " "" d285_202 ["dvChars"])
                                                  let 'T' = xx284_203
                                                  return ()
                                                  d287_204 <- get
                                                  xx286_205 <- StateT derivsChars
                                                  case xx286_205 of
                                                      'y' -> return ()
                                                      _ -> gets derivsPosition >>= (throwError . ParseError "'y'" "not match pattern: " "" d287_204 ["dvChars"])
                                                  let 'y' = xx286_205
                                                  return ()
                                                  d289_206 <- get
                                                  xx288_207 <- StateT derivsChars
                                                  case xx288_207 of
                                                      'p' -> return ()
                                                      _ -> gets derivsPosition >>= (throwError . ParseError "'p'" "not match pattern: " "" d289_206 ["dvChars"])
                                                  let 'p' = xx288_207
                                                  return ()
                                                  d291_208 <- get
                                                  xx290_209 <- StateT derivsChars
                                                  case xx290_209 of
                                                      'e' -> return ()
                                                      _ -> gets derivsPosition >>= (throwError . ParseError "'e'" "not match pattern: " "" d291_208 ["dvChars"])
                                                  let 'e' = xx290_209
                                                  return ()
                                                  d293_210 <- get
                                                  xx292_211 <- StateT derivsChars
                                                  case xx292_211 of
                                                      'F' -> return ()
                                                      _ -> gets derivsPosition >>= (throwError . ParseError "'F'" "not match pattern: " "" d293_210 ["dvChars"])
                                                  let 'F' = xx292_211
                                                  return ()
                                                  d295_212 <- get
                                                  xx294_213 <- StateT derivsChars
                                                  case xx294_213 of
                                                      'a' -> return ()
                                                      _ -> gets derivsPosition >>= (throwError . ParseError "'a'" "not match pattern: " "" d295_212 ["dvChars"])
                                                  let 'a' = xx294_213
                                                  return ()
                                                  d297_214 <- get
                                                  xx296_215 <- StateT derivsChars
                                                  case xx296_215 of
                                                      'm' -> return ()
                                                      _ -> gets derivsPosition >>= (throwError . ParseError "'m'" "not match pattern: " "" d297_214 ["dvChars"])
                                                  let 'm' = xx296_215
                                                  return ()
                                                  d299_216 <- get
                                                  xx298_217 <- StateT derivsChars
                                                  case xx298_217 of
                                                      'i' -> return ()
                                                      _ -> gets derivsPosition >>= (throwError . ParseError "'i'" "not match pattern: " "" d299_216 ["dvChars"])
                                                  let 'i' = xx298_217
                                                  return ()
                                                  d301_218 <- get
                                                  xx300_219 <- StateT derivsChars
                                                  case xx300_219 of
                                                      'l' -> return ()
                                                      _ -> gets derivsPosition >>= (throwError . ParseError "'l'" "not match pattern: " "" d301_218 ["dvChars"])
                                                  let 'l' = xx300_219
                                                  return ()
                                                  d303_220 <- get
                                                  xx302_221 <- StateT derivsChars
                                                  case xx302_221 of
                                                      'i' -> return ()
                                                      _ -> gets derivsPosition >>= (throwError . ParseError "'i'" "not match pattern: " "" d303_220 ["dvChars"])
                                                  let 'i' = xx302_221
                                                  return ()
                                                  d305_222 <- get
                                                  xx304_223 <- StateT derivsChars
                                                  case xx304_223 of
                                                      'e' -> return ()
                                                      _ -> gets derivsPosition >>= (throwError . ParseError "'e'" "not match pattern: " "" d305_222 ["dvChars"])
                                                  let 'e' = xx304_223
                                                  return ()
                                                  d307_224 <- get
                                                  xx306_225 <- StateT derivsChars
                                                  case xx306_225 of
                                                      's' -> return ()
                                                      _ -> gets derivsPosition >>= (throwError . ParseError "'s'" "not match pattern: " "" d307_224 ["dvChars"])
                                                  let 's' = xx306_225
                                                  return ()
                                                  return ()]
                pragmaEnd6_75 = foldl1 mplus [do _ <- StateT delPragmas
                                                 return ()
                                                 _ <- StateT spaces
                                                 return ()
                                                 d313_226 <- get
                                                 xx312_227 <- StateT derivsChars
                                                 case xx312_227 of
                                                     '#' -> return ()
                                                     _ -> gets derivsPosition >>= (throwError . ParseError "'#'" "not match pattern: " "" d313_226 ["dvChars"])
                                                 let '#' = xx312_227
                                                 return ()
                                                 d315_228 <- get
                                                 xx314_229 <- StateT derivsChars
                                                 case xx314_229 of
                                                     '-' -> return ()
                                                     _ -> gets derivsPosition >>= (throwError . ParseError "'-'" "not match pattern: " "" d315_228 ["dvChars"])
                                                 let '-' = xx314_229
                                                 return ()
                                                 d317_230 <- get
                                                 xx316_231 <- StateT derivsChars
                                                 case xx316_231 of
                                                     '}' -> return ()
                                                     _ -> gets derivsPosition >>= (throwError . ParseError "'}'" "not match pattern: " "" d317_230 ["dvChars"])
                                                 let '}' = xx316_231
                                                 return ()
                                                 return (),
                                              do d319_232 <- get
                                                 xx318_233 <- StateT derivsChars
                                                 case xx318_233 of
                                                     '#' -> return ()
                                                     _ -> gets derivsPosition >>= (throwError . ParseError "'#'" "not match pattern: " "" d319_232 ["dvChars"])
                                                 let '#' = xx318_233
                                                 return ()
                                                 d321_234 <- get
                                                 xx320_235 <- StateT derivsChars
                                                 case xx320_235 of
                                                     '-' -> return ()
                                                     _ -> gets derivsPosition >>= (throwError . ParseError "'-'" "not match pattern: " "" d321_234 ["dvChars"])
                                                 let '-' = xx320_235
                                                 return ()
                                                 d323_236 <- get
                                                 xx322_237 <- StateT derivsChars
                                                 case xx322_237 of
                                                     '}' -> return ()
                                                     _ -> gets derivsPosition >>= (throwError . ParseError "'}'" "not match pattern: " "" d323_236 ["dvChars"])
                                                 let '}' = xx322_237
                                                 return ()
                                                 return ()]
                moduleDec7_76 = foldl1 mplus [do d325_238 <- get
                                                 xx324_239 <- StateT derivsChars
                                                 case xx324_239 of
                                                     'm' -> return ()
                                                     _ -> gets derivsPosition >>= (throwError . ParseError "'m'" "not match pattern: " "" d325_238 ["dvChars"])
                                                 let 'm' = xx324_239
                                                 return ()
                                                 d327_240 <- get
                                                 xx326_241 <- StateT derivsChars
                                                 case xx326_241 of
                                                     'o' -> return ()
                                                     _ -> gets derivsPosition >>= (throwError . ParseError "'o'" "not match pattern: " "" d327_240 ["dvChars"])
                                                 let 'o' = xx326_241
                                                 return ()
                                                 d329_242 <- get
                                                 xx328_243 <- StateT derivsChars
                                                 case xx328_243 of
                                                     'd' -> return ()
                                                     _ -> gets derivsPosition >>= (throwError . ParseError "'d'" "not match pattern: " "" d329_242 ["dvChars"])
                                                 let 'd' = xx328_243
                                                 return ()
                                                 d331_244 <- get
                                                 xx330_245 <- StateT derivsChars
                                                 case xx330_245 of
                                                     'u' -> return ()
                                                     _ -> gets derivsPosition >>= (throwError . ParseError "'u'" "not match pattern: " "" d331_244 ["dvChars"])
                                                 let 'u' = xx330_245
                                                 return ()
                                                 d333_246 <- get
                                                 xx332_247 <- StateT derivsChars
                                                 case xx332_247 of
                                                     'l' -> return ()
                                                     _ -> gets derivsPosition >>= (throwError . ParseError "'l'" "not match pattern: " "" d333_246 ["dvChars"])
                                                 let 'l' = xx332_247
                                                 return ()
                                                 d335_248 <- get
                                                 xx334_249 <- StateT derivsChars
                                                 case xx334_249 of
                                                     'e' -> return ()
                                                     _ -> gets derivsPosition >>= (throwError . ParseError "'e'" "not match pattern: " "" d335_248 ["dvChars"])
                                                 let 'e' = xx334_249
                                                 return ()
                                                 s <- StateT moduleDecStr
                                                 _ <- StateT whr
                                                 return ()
                                                 return (just s),
                                              return nothing]
                moduleDecStr8_77 = foldl1 mplus [do ddd340_250 <- get
                                                    do err <- ((do _ <- StateT whr
                                                                   return ()) >> return False) `catchError` const (return True)
                                                       unless err (gets derivsPosition >>= (throwError . ParseError ('!' : "_:whr") "not match: " "" ddd340_250 ["whr"]))
                                                    put ddd340_250
                                                    c <- StateT derivsChars
                                                    s <- StateT moduleDecStr
                                                    return (cons c s),
                                                 return emp]
                whr9_78 = foldl1 mplus [do d348_251 <- get
                                           xx347_252 <- StateT derivsChars
                                           case xx347_252 of
                                               'w' -> return ()
                                               _ -> gets derivsPosition >>= (throwError . ParseError "'w'" "not match pattern: " "" d348_251 ["dvChars"])
                                           let 'w' = xx347_252
                                           return ()
                                           d350_253 <- get
                                           xx349_254 <- StateT derivsChars
                                           case xx349_254 of
                                               'h' -> return ()
                                               _ -> gets derivsPosition >>= (throwError . ParseError "'h'" "not match pattern: " "" d350_253 ["dvChars"])
                                           let 'h' = xx349_254
                                           return ()
                                           d352_255 <- get
                                           xx351_256 <- StateT derivsChars
                                           case xx351_256 of
                                               'e' -> return ()
                                               _ -> gets derivsPosition >>= (throwError . ParseError "'e'" "not match pattern: " "" d352_255 ["dvChars"])
                                           let 'e' = xx351_256
                                           return ()
                                           d354_257 <- get
                                           xx353_258 <- StateT derivsChars
                                           case xx353_258 of
                                               'r' -> return ()
                                               _ -> gets derivsPosition >>= (throwError . ParseError "'r'" "not match pattern: " "" d354_257 ["dvChars"])
                                           let 'r' = xx353_258
                                           return ()
                                           d356_259 <- get
                                           xx355_260 <- StateT derivsChars
                                           case xx355_260 of
                                               'e' -> return ()
                                               _ -> gets derivsPosition >>= (throwError . ParseError "'e'" "not match pattern: " "" d356_259 ["dvChars"])
                                           let 'e' = xx355_260
                                           return ()
                                           return ()]
                preImpPap10_79 = foldl1 mplus [do ddd357_261 <- get
                                                  do err <- ((do _ <- StateT importPapillon
                                                                 return ()) >> return False) `catchError` const (return True)
                                                     unless err (gets derivsPosition >>= (throwError . ParseError ('!' : "_:importPapillon") "not match: " "" ddd357_261 ["importPapillon"]))
                                                  put ddd357_261
                                                  ddd360_262 <- get
                                                  do err <- ((do _ <- StateT pap
                                                                 return ()) >> return False) `catchError` const (return True)
                                                     unless err (gets derivsPosition >>= (throwError . ParseError ('!' : "_:pap") "not match: " "" ddd360_262 ["pap"]))
                                                  put ddd360_262
                                                  c <- StateT derivsChars
                                                  pip <- StateT preImpPap
                                                  return (cons c pip),
                                               return emp]
                prePeg11_80 = foldl1 mplus [do ddd367_263 <- get
                                               do err <- ((do _ <- StateT pap
                                                              return ()) >> return False) `catchError` const (return True)
                                                  unless err (gets derivsPosition >>= (throwError . ParseError ('!' : "_:pap") "not match: " "" ddd367_263 ["pap"]))
                                               put ddd367_263
                                               c <- StateT derivsChars
                                               pp <- StateT prePeg
                                               return (cons c pp),
                                            return emp]
                afterPeg12_81 = foldl1 mplus [do c <- StateT derivsChars
                                                 atp <- StateT afterPeg
                                                 return (cons c atp),
                                              return emp]
                importPapillon13_82 = foldl1 mplus [do d379_264 <- get
                                                       xx378_265 <- StateT varToken
                                                       case xx378_265 of
                                                           "import" -> return ()
                                                           _ -> gets derivsPosition >>= (throwError . ParseError "\"import\"" "not match pattern: " "" d379_264 ["varToken"])
                                                       let "import" = xx378_265
                                                       return ()
                                                       d381_266 <- get
                                                       xx380_267 <- StateT typToken
                                                       case xx380_267 of
                                                           "Text" -> return ()
                                                           _ -> gets derivsPosition >>= (throwError . ParseError "\"Text\"" "not match pattern: " "" d381_266 ["typToken"])
                                                       let "Text" = xx380_267
                                                       return ()
                                                       d383_268 <- get
                                                       xx382_269 <- StateT derivsChars
                                                       case xx382_269 of
                                                           '.' -> return ()
                                                           _ -> gets derivsPosition >>= (throwError . ParseError "'.'" "not match pattern: " "" d383_268 ["dvChars"])
                                                       let '.' = xx382_269
                                                       return ()
                                                       _ <- StateT spaces
                                                       return ()
                                                       d387_270 <- get
                                                       xx386_271 <- StateT typToken
                                                       case xx386_271 of
                                                           "Papillon" -> return ()
                                                           _ -> gets derivsPosition >>= (throwError . ParseError "\"Papillon\"" "not match pattern: " "" d387_270 ["typToken"])
                                                       let "Papillon" = xx386_271
                                                       return ()
                                                       ddd388_272 <- get
                                                       do err <- ((do d390_273 <- get
                                                                      xx389_274 <- StateT derivsChars
                                                                      case xx389_274 of
                                                                          '.' -> return ()
                                                                          _ -> gets derivsPosition >>= (throwError . ParseError "'.'" "not match pattern: " "" d390_273 ["dvChars"])
                                                                      let '.' = xx389_274
                                                                      return ()) >> return False) `catchError` const (return True)
                                                          unless err (gets derivsPosition >>= (throwError . ParseError ('!' : "'.':") "not match: " "" ddd388_272 ["dvChars"]))
                                                       put ddd388_272
                                                       return ()]
                varToken14_83 = foldl1 mplus [do v <- StateT variable
                                                 _ <- StateT spaces
                                                 return ()
                                                 return v]
                typToken15_84 = foldl1 mplus [do t <- StateT typ
                                                 _ <- StateT spaces
                                                 return ()
                                                 return t]
                pap16_85 = foldl1 mplus [do d400_275 <- get
                                            xx399_276 <- StateT derivsChars
                                            case xx399_276 of
                                                '\n' -> return ()
                                                _ -> gets derivsPosition >>= (throwError . ParseError "'\\n'" "not match pattern: " "" d400_275 ["dvChars"])
                                            let '\n' = xx399_276
                                            return ()
                                            d402_277 <- get
                                            xx401_278 <- StateT derivsChars
                                            case xx401_278 of
                                                '[' -> return ()
                                                _ -> gets derivsPosition >>= (throwError . ParseError "'['" "not match pattern: " "" d402_277 ["dvChars"])
                                            let '[' = xx401_278
                                            return ()
                                            d404_279 <- get
                                            xx403_280 <- StateT derivsChars
                                            case xx403_280 of
                                                'p' -> return ()
                                                _ -> gets derivsPosition >>= (throwError . ParseError "'p'" "not match pattern: " "" d404_279 ["dvChars"])
                                            let 'p' = xx403_280
                                            return ()
                                            d406_281 <- get
                                            xx405_282 <- StateT derivsChars
                                            case xx405_282 of
                                                'a' -> return ()
                                                _ -> gets derivsPosition >>= (throwError . ParseError "'a'" "not match pattern: " "" d406_281 ["dvChars"])
                                            let 'a' = xx405_282
                                            return ()
                                            d408_283 <- get
                                            xx407_284 <- StateT derivsChars
                                            case xx407_284 of
                                                'p' -> return ()
                                                _ -> gets derivsPosition >>= (throwError . ParseError "'p'" "not match pattern: " "" d408_283 ["dvChars"])
                                            let 'p' = xx407_284
                                            return ()
                                            d410_285 <- get
                                            xx409_286 <- StateT derivsChars
                                            case xx409_286 of
                                                'i' -> return ()
                                                _ -> gets derivsPosition >>= (throwError . ParseError "'i'" "not match pattern: " "" d410_285 ["dvChars"])
                                            let 'i' = xx409_286
                                            return ()
                                            d412_287 <- get
                                            xx411_288 <- StateT derivsChars
                                            case xx411_288 of
                                                'l' -> return ()
                                                _ -> gets derivsPosition >>= (throwError . ParseError "'l'" "not match pattern: " "" d412_287 ["dvChars"])
                                            let 'l' = xx411_288
                                            return ()
                                            d414_289 <- get
                                            xx413_290 <- StateT derivsChars
                                            case xx413_290 of
                                                'l' -> return ()
                                                _ -> gets derivsPosition >>= (throwError . ParseError "'l'" "not match pattern: " "" d414_289 ["dvChars"])
                                            let 'l' = xx413_290
                                            return ()
                                            d416_291 <- get
                                            xx415_292 <- StateT derivsChars
                                            case xx415_292 of
                                                'o' -> return ()
                                                _ -> gets derivsPosition >>= (throwError . ParseError "'o'" "not match pattern: " "" d416_291 ["dvChars"])
                                            let 'o' = xx415_292
                                            return ()
                                            d418_293 <- get
                                            xx417_294 <- StateT derivsChars
                                            case xx417_294 of
                                                'n' -> return ()
                                                _ -> gets derivsPosition >>= (throwError . ParseError "'n'" "not match pattern: " "" d418_293 ["dvChars"])
                                            let 'n' = xx417_294
                                            return ()
                                            d420_295 <- get
                                            xx419_296 <- StateT derivsChars
                                            case xx419_296 of
                                                '|' -> return ()
                                                _ -> gets derivsPosition >>= (throwError . ParseError "'|'" "not match pattern: " "" d420_295 ["dvChars"])
                                            let '|' = xx419_296
                                            return ()
                                            d422_297 <- get
                                            xx421_298 <- StateT derivsChars
                                            case xx421_298 of
                                                '\n' -> return ()
                                                _ -> gets derivsPosition >>= (throwError . ParseError "'\\n'" "not match pattern: " "" d422_297 ["dvChars"])
                                            let '\n' = xx421_298
                                            return ()
                                            return ()]
                peg17_86 = foldl1 mplus [do _ <- StateT spaces
                                            return ()
                                            s <- StateT sourceType
                                            p <- StateT peg_
                                            return (mkTTPeg s p),
                                         do p <- StateT peg_
                                            return (mkTTPeg tString p)]
                sourceType18_87 = foldl1 mplus [do d432_299 <- get
                                                   xx431_300 <- StateT varToken
                                                   case xx431_300 of
                                                       "source" -> return ()
                                                       _ -> gets derivsPosition >>= (throwError . ParseError "\"source\"" "not match pattern: " "" d432_299 ["varToken"])
                                                   let "source" = xx431_300
                                                   return ()
                                                   d434_301 <- get
                                                   xx433_302 <- StateT derivsChars
                                                   case xx433_302 of
                                                       ':' -> return ()
                                                       _ -> gets derivsPosition >>= (throwError . ParseError "':'" "not match pattern: " "" d434_301 ["dvChars"])
                                                   let ':' = xx433_302
                                                   return ()
                                                   _ <- StateT spaces
                                                   return ()
                                                   v <- StateT typToken
                                                   return v]
                peg_19_88 = foldl1 mplus [do _ <- StateT spaces
                                             return ()
                                             d <- StateT definition
                                             p <- StateT peg_
                                             return (cons d p),
                                          return emp]
                definition20_89 = foldl1 mplus [do v <- StateT variable
                                                   _ <- StateT spaces
                                                   return ()
                                                   d450_303 <- get
                                                   xx449_304 <- StateT derivsChars
                                                   case xx449_304 of
                                                       ':' -> return ()
                                                       _ -> gets derivsPosition >>= (throwError . ParseError "':'" "not match pattern: " "" d450_303 ["dvChars"])
                                                   let ':' = xx449_304
                                                   return ()
                                                   d452_305 <- get
                                                   xx451_306 <- StateT derivsChars
                                                   case xx451_306 of
                                                       ':' -> return ()
                                                       _ -> gets derivsPosition >>= (throwError . ParseError "':'" "not match pattern: " "" d452_305 ["dvChars"])
                                                   let ':' = xx451_306
                                                   return ()
                                                   _ <- StateT spaces
                                                   return ()
                                                   t <- StateT hsTypeArr
                                                   _ <- StateT spaces
                                                   return ()
                                                   d460_307 <- get
                                                   xx459_308 <- StateT derivsChars
                                                   case xx459_308 of
                                                       '=' -> return ()
                                                       _ -> gets derivsPosition >>= (throwError . ParseError "'='" "not match pattern: " "" d460_307 ["dvChars"])
                                                   let '=' = xx459_308
                                                   return ()
                                                   _ <- StateT spaces
                                                   return ()
                                                   sel <- StateT selection
                                                   _ <- StateT spaces
                                                   return ()
                                                   d468_309 <- get
                                                   xx467_310 <- StateT derivsChars
                                                   case xx467_310 of
                                                       ';' -> return ()
                                                       _ -> gets derivsPosition >>= (throwError . ParseError "';'" "not match pattern: " "" d468_309 ["dvChars"])
                                                   let ';' = xx467_310
                                                   return ()
                                                   return (mkDef v t sel)]
                selection21_90 = foldl1 mplus [do ex <- StateT expressionHs
                                                  _ <- StateT spaces
                                                  return ()
                                                  d474_311 <- get
                                                  xx473_312 <- StateT derivsChars
                                                  case xx473_312 of
                                                      '/' -> return ()
                                                      _ -> gets derivsPosition >>= (throwError . ParseError "'/'" "not match pattern: " "" d474_311 ["dvChars"])
                                                  let '/' = xx473_312
                                                  return ()
                                                  _ <- StateT spaces
                                                  return ()
                                                  sel <- StateT selection
                                                  return (cons ex sel),
                                               do ex <- StateT expressionHs
                                                  return (cons ex emp)]
                expressionHs22_91 = foldl1 mplus [do e <- StateT expression
                                                     _ <- StateT spaces
                                                     return ()
                                                     d486_313 <- get
                                                     xx485_314 <- StateT derivsChars
                                                     case xx485_314 of
                                                         '{' -> return ()
                                                         _ -> gets derivsPosition >>= (throwError . ParseError "'{'" "not match pattern: " "" d486_313 ["dvChars"])
                                                     let '{' = xx485_314
                                                     return ()
                                                     _ <- StateT spaces
                                                     return ()
                                                     h <- StateT hsExpLam
                                                     _ <- StateT spaces
                                                     return ()
                                                     d494_315 <- get
                                                     xx493_316 <- StateT derivsChars
                                                     case xx493_316 of
                                                         '}' -> return ()
                                                         _ -> gets derivsPosition >>= (throwError . ParseError "'}'" "not match pattern: " "" d494_315 ["dvChars"])
                                                     let '}' = xx493_316
                                                     return ()
                                                     return (mkExpressionHs e h)]
                expression23_92 = foldl1 mplus [do l <- StateT nameLeaf_
                                                   _ <- StateT spaces
                                                   return ()
                                                   e <- StateT expression
                                                   return (cons l e),
                                                return emp]
                nameLeaf_24_93 = foldl1 mplus [do d502_317 <- get
                                                  xx501_318 <- StateT derivsChars
                                                  case xx501_318 of
                                                      '!' -> return ()
                                                      _ -> gets derivsPosition >>= (throwError . ParseError "'!'" "not match pattern: " "" d502_317 ["dvChars"])
                                                  let '!' = xx501_318
                                                  return ()
                                                  nl <- StateT nameLeafNoCom
                                                  _ <- StateT spaces
                                                  return ()
                                                  com <- papOptional (StateT comForErr)
                                                  return (NotAfter nl $ maybe "" id com),
                                               do d510_319 <- get
                                                  xx509_320 <- StateT derivsChars
                                                  let c = xx509_320
                                                  unless (isAmp c) (gets derivsPosition >>= (throwError . ParseError "isAmp c" "not match: " "" d510_319 ["dvChars"]))
                                                  nl <- StateT nameLeaf
                                                  return (After nl),
                                               do nl <- StateT nameLeaf
                                                  return (Here nl)]
                nameLeaf25_94 = foldl1 mplus [do n <- StateT pat1
                                                 _ <- StateT spaces
                                                 return ()
                                                 com <- papOptional (StateT comForErr)
                                                 d522_321 <- get
                                                 xx521_322 <- StateT derivsChars
                                                 case xx521_322 of
                                                     ':' -> return ()
                                                     _ -> gets derivsPosition >>= (throwError . ParseError "':'" "not match pattern: " "" d522_321 ["dvChars"])
                                                 let ':' = xx521_322
                                                 return ()
                                                 (rf, p) <- StateT leaf
                                                 return (NameLeaf (n, maybe "" id com) rf p),
                                              do n <- StateT pat1
                                                 _ <- StateT spaces
                                                 return ()
                                                 com <- papOptional (StateT comForErr)
                                                 return (NameLeaf (n,
                                                                   maybe "" id com) FromToken Nothing)]
                nameLeafNoCom26_95 = foldl1 mplus [do n <- StateT pat1
                                                      _ <- StateT spaces
                                                      return ()
                                                      com <- papOptional (StateT comForErr)
                                                      d538_323 <- get
                                                      xx537_324 <- StateT derivsChars
                                                      case xx537_324 of
                                                          ':' -> return ()
                                                          _ -> gets derivsPosition >>= (throwError . ParseError "':'" "not match pattern: " "" d538_323 ["dvChars"])
                                                      let ':' = xx537_324
                                                      return ()
                                                      (rf, p) <- StateT leaf
                                                      return (NameLeaf (n, maybe "" id com) rf p),
                                                   do n <- StateT pat1
                                                      _ <- StateT spaces
                                                      return ()
                                                      return (NameLeaf (n, "") FromToken Nothing)]
                comForErr27_96 = foldl1 mplus [do d546_325 <- get
                                                  xx545_326 <- StateT derivsChars
                                                  case xx545_326 of
                                                      '{' -> return ()
                                                      _ -> gets derivsPosition >>= (throwError . ParseError "'{'" "not match pattern: " "" d546_325 ["dvChars"])
                                                  let '{' = xx545_326
                                                  return ()
                                                  d548_327 <- get
                                                  xx547_328 <- StateT derivsChars
                                                  case xx547_328 of
                                                      '-' -> return ()
                                                      _ -> gets derivsPosition >>= (throwError . ParseError "'-'" "not match pattern: " "" d548_327 ["dvChars"])
                                                  let '-' = xx547_328
                                                  return ()
                                                  d550_329 <- get
                                                  xx549_330 <- StateT derivsChars
                                                  case xx549_330 of
                                                      '#' -> return ()
                                                      _ -> gets derivsPosition >>= (throwError . ParseError "'#'" "not match pattern: " "" d550_329 ["dvChars"])
                                                  let '#' = xx549_330
                                                  return ()
                                                  _ <- StateT spaces
                                                  return ()
                                                  d554_331 <- get
                                                  xx553_332 <- StateT derivsChars
                                                  case xx553_332 of
                                                      '"' -> return ()
                                                      _ -> gets derivsPosition >>= (throwError . ParseError "'\"'" "not match pattern: " "" d554_331 ["dvChars"])
                                                  let '"' = xx553_332
                                                  return ()
                                                  s <- StateT stringLit
                                                  d558_333 <- get
                                                  xx557_334 <- StateT derivsChars
                                                  case xx557_334 of
                                                      '"' -> return ()
                                                      _ -> gets derivsPosition >>= (throwError . ParseError "'\"'" "not match pattern: " "" d558_333 ["dvChars"])
                                                  let '"' = xx557_334
                                                  return ()
                                                  _ <- StateT spaces
                                                  return ()
                                                  d562_335 <- get
                                                  xx561_336 <- StateT derivsChars
                                                  case xx561_336 of
                                                      '#' -> return ()
                                                      _ -> gets derivsPosition >>= (throwError . ParseError "'#'" "not match pattern: " "" d562_335 ["dvChars"])
                                                  let '#' = xx561_336
                                                  return ()
                                                  d564_337 <- get
                                                  xx563_338 <- StateT derivsChars
                                                  case xx563_338 of
                                                      '-' -> return ()
                                                      _ -> gets derivsPosition >>= (throwError . ParseError "'-'" "not match pattern: " "" d564_337 ["dvChars"])
                                                  let '-' = xx563_338
                                                  return ()
                                                  d566_339 <- get
                                                  xx565_340 <- StateT derivsChars
                                                  case xx565_340 of
                                                      '}' -> return ()
                                                      _ -> gets derivsPosition >>= (throwError . ParseError "'}'" "not match pattern: " "" d566_339 ["dvChars"])
                                                  let '}' = xx565_340
                                                  return ()
                                                  _ <- StateT spaces
                                                  return ()
                                                  return s]
                leaf28_97 = foldl1 mplus [do rf <- StateT readFromLs
                                             t <- StateT test
                                             return (rf, Just t),
                                          do rf <- StateT readFromLs
                                             return (rf, Nothing),
                                          do t <- StateT test
                                             return (FromToken, Just t)]
                patOp29_98 = foldl1 mplus [do p <- StateT pat
                                              o <- StateT opConName
                                              po <- StateT patOp
                                              return (uInfixP p o po),
                                           do p <- StateT pat
                                              _ <- StateT spaces
                                              return ()
                                              d588_341 <- get
                                              xx587_342 <- StateT derivsChars
                                              let q = xx587_342
                                              unless (isBQ q) (gets derivsPosition >>= (throwError . ParseError "isBQ q" "not match: " "" d588_341 ["dvChars"]))
                                              t <- StateT typ
                                              d592_343 <- get
                                              xx591_344 <- StateT derivsChars
                                              let q_ = xx591_344
                                              unless (isBQ q_) (gets derivsPosition >>= (throwError . ParseError "isBQ q_" "not match: " "" d592_343 ["dvChars"]))
                                              _ <- StateT spaces
                                              return ()
                                              po <- StateT patOp
                                              return (uInfixP p (mkName t) po),
                                           do p <- StateT pat
                                              return p]
                pat30_99 = foldl1 mplus [do t <- StateT typ
                                            _ <- StateT spaces
                                            return ()
                                            ps <- StateT pats
                                            return (conToPatQ t ps),
                                         do d606_345 <- get
                                            xx605_346 <- StateT derivsChars
                                            case xx605_346 of
                                                '(' -> return ()
                                                _ -> gets derivsPosition >>= (throwError . ParseError "'('" "not match pattern: " "" d606_345 ["dvChars"])
                                            let '(' = xx605_346
                                            return ()
                                            o <- StateT opConName
                                            d610_347 <- get
                                            xx609_348 <- StateT derivsChars
                                            case xx609_348 of
                                                ')' -> return ()
                                                _ -> gets derivsPosition >>= (throwError . ParseError "')'" "not match pattern: " "" d610_347 ["dvChars"])
                                            let ')' = xx609_348
                                            return ()
                                            _ <- StateT spaces
                                            return ()
                                            ps <- StateT pats
                                            return (conP o ps),
                                         do p <- StateT pat1
                                            return p]
                pat131_100 = foldl1 mplus [do t <- StateT typ
                                              return (conToPatQ t emp),
                                           do d620_349 <- get
                                              xx619_350 <- StateT variable
                                              case xx619_350 of
                                                  "_" -> return ()
                                                  _ -> gets derivsPosition >>= (throwError . ParseError "\"_\"" "not match pattern: " "" d620_349 ["variable"])
                                              let "_" = xx619_350
                                              return ()
                                              return wildP,
                                           do n <- StateT variable
                                              return (strToPatQ n),
                                           do i <- StateT integer
                                              return (litP (integerL i)),
                                           do d626_351 <- get
                                              xx625_352 <- StateT derivsChars
                                              case xx625_352 of
                                                  '-' -> return ()
                                                  _ -> gets derivsPosition >>= (throwError . ParseError "'-'" "not match pattern: " "" d626_351 ["dvChars"])
                                              let '-' = xx625_352
                                              return ()
                                              _ <- StateT spaces
                                              return ()
                                              i <- StateT integer
                                              return (litP (integerL $ negate i)),
                                           do d632_353 <- get
                                              xx631_354 <- StateT derivsChars
                                              case xx631_354 of
                                                  '\'' -> return ()
                                                  _ -> gets derivsPosition >>= (throwError . ParseError "'\\''" "not match pattern: " "" d632_353 ["dvChars"])
                                              let '\'' = xx631_354
                                              return ()
                                              c <- StateT charLit
                                              d636_355 <- get
                                              xx635_356 <- StateT derivsChars
                                              case xx635_356 of
                                                  '\'' -> return ()
                                                  _ -> gets derivsPosition >>= (throwError . ParseError "'\\''" "not match pattern: " "" d636_355 ["dvChars"])
                                              let '\'' = xx635_356
                                              return ()
                                              return (charP c),
                                           do d638_357 <- get
                                              xx637_358 <- StateT derivsChars
                                              case xx637_358 of
                                                  '"' -> return ()
                                                  _ -> gets derivsPosition >>= (throwError . ParseError "'\"'" "not match pattern: " "" d638_357 ["dvChars"])
                                              let '"' = xx637_358
                                              return ()
                                              s <- StateT stringLit
                                              d642_359 <- get
                                              xx641_360 <- StateT derivsChars
                                              case xx641_360 of
                                                  '"' -> return ()
                                                  _ -> gets derivsPosition >>= (throwError . ParseError "'\"'" "not match pattern: " "" d642_359 ["dvChars"])
                                              let '"' = xx641_360
                                              return ()
                                              return (stringP s),
                                           do d644_361 <- get
                                              xx643_362 <- StateT derivsChars
                                              case xx643_362 of
                                                  '(' -> return ()
                                                  _ -> gets derivsPosition >>= (throwError . ParseError "'('" "not match pattern: " "" d644_361 ["dvChars"])
                                              let '(' = xx643_362
                                              return ()
                                              p <- StateT patList
                                              d648_363 <- get
                                              xx647_364 <- StateT derivsChars
                                              case xx647_364 of
                                                  ')' -> return ()
                                                  _ -> gets derivsPosition >>= (throwError . ParseError "')'" "not match pattern: " "" d648_363 ["dvChars"])
                                              let ')' = xx647_364
                                              return ()
                                              return (tupP p),
                                           do d650_365 <- get
                                              xx649_366 <- StateT derivsChars
                                              case xx649_366 of
                                                  '[' -> return ()
                                                  _ -> gets derivsPosition >>= (throwError . ParseError "'['" "not match pattern: " "" d650_365 ["dvChars"])
                                              let '[' = xx649_366
                                              return ()
                                              p <- StateT patList
                                              d654_367 <- get
                                              xx653_368 <- StateT derivsChars
                                              case xx653_368 of
                                                  ']' -> return ()
                                                  _ -> gets derivsPosition >>= (throwError . ParseError "']'" "not match pattern: " "" d654_367 ["dvChars"])
                                              let ']' = xx653_368
                                              return ()
                                              return (listP p)]
                patList32_101 = foldl1 mplus [do p <- StateT patOp
                                                 _ <- StateT spaces
                                                 return ()
                                                 d660_369 <- get
                                                 xx659_370 <- StateT derivsChars
                                                 case xx659_370 of
                                                     ',' -> return ()
                                                     _ -> gets derivsPosition >>= (throwError . ParseError "','" "not match pattern: " "" d660_369 ["dvChars"])
                                                 let ',' = xx659_370
                                                 return ()
                                                 _ <- StateT spaces
                                                 return ()
                                                 ps <- StateT patList
                                                 return (p : ps),
                                              do p <- StateT patOp
                                                 return [p],
                                              return []]
                opConName33_102 = foldl1 mplus [do d668_371 <- get
                                                   xx667_372 <- StateT derivsChars
                                                   case xx667_372 of
                                                       ':' -> return ()
                                                       _ -> gets derivsPosition >>= (throwError . ParseError "':'" "not match pattern: " "" d668_371 ["dvChars"])
                                                   let ':' = xx667_372
                                                   return ()
                                                   ot <- StateT opTail
                                                   return (mkName $ colon : ot)]
                charLit34_103 = foldl1 mplus [do d672_373 <- get
                                                 xx671_374 <- StateT derivsChars
                                                 let c = xx671_374
                                                 unless (isAlphaNumOt c) (gets derivsPosition >>= (throwError . ParseError "isAlphaNumOt c" "not match: " "" d672_373 ["dvChars"]))
                                                 return c,
                                              do d674_375 <- get
                                                 xx673_376 <- StateT derivsChars
                                                 case xx673_376 of
                                                     '\\' -> return ()
                                                     _ -> gets derivsPosition >>= (throwError . ParseError "'\\\\'" "not match pattern: " "" d674_375 ["dvChars"])
                                                 let '\\' = xx673_376
                                                 return ()
                                                 c <- StateT escapeC
                                                 return c]
                stringLit35_104 = foldl1 mplus [do d678_377 <- get
                                                   xx677_378 <- StateT derivsChars
                                                   let c = xx677_378
                                                   unless (isStrLitC c) (gets derivsPosition >>= (throwError . ParseError "isStrLitC c" "not match: " "" d678_377 ["dvChars"]))
                                                   s <- StateT stringLit
                                                   return (cons c s),
                                                do d682_379 <- get
                                                   xx681_380 <- StateT derivsChars
                                                   case xx681_380 of
                                                       '\\' -> return ()
                                                       _ -> gets derivsPosition >>= (throwError . ParseError "'\\\\'" "not match pattern: " "" d682_379 ["dvChars"])
                                                   let '\\' = xx681_380
                                                   return ()
                                                   c <- StateT escapeC
                                                   s <- StateT stringLit
                                                   return (c : s),
                                                return emp]
                escapeC36_105 = foldl1 mplus [do d688_381 <- get
                                                 xx687_382 <- StateT derivsChars
                                                 case xx687_382 of
                                                     '"' -> return ()
                                                     _ -> gets derivsPosition >>= (throwError . ParseError "'\"'" "not match pattern: " "" d688_381 ["dvChars"])
                                                 let '"' = xx687_382
                                                 return ()
                                                 return '"',
                                              do d690_383 <- get
                                                 xx689_384 <- StateT derivsChars
                                                 case xx689_384 of
                                                     '\'' -> return ()
                                                     _ -> gets derivsPosition >>= (throwError . ParseError "'\\''" "not match pattern: " "" d690_383 ["dvChars"])
                                                 let '\'' = xx689_384
                                                 return ()
                                                 return '\'',
                                              do d692_385 <- get
                                                 xx691_386 <- StateT derivsChars
                                                 case xx691_386 of
                                                     '\\' -> return ()
                                                     _ -> gets derivsPosition >>= (throwError . ParseError "'\\\\'" "not match pattern: " "" d692_385 ["dvChars"])
                                                 let '\\' = xx691_386
                                                 return ()
                                                 return '\\',
                                              do d694_387 <- get
                                                 xx693_388 <- StateT derivsChars
                                                 case xx693_388 of
                                                     'n' -> return ()
                                                     _ -> gets derivsPosition >>= (throwError . ParseError "'n'" "not match pattern: " "" d694_387 ["dvChars"])
                                                 let 'n' = xx693_388
                                                 return ()
                                                 return '\n',
                                              do d696_389 <- get
                                                 xx695_390 <- StateT derivsChars
                                                 case xx695_390 of
                                                     't' -> return ()
                                                     _ -> gets derivsPosition >>= (throwError . ParseError "'t'" "not match pattern: " "" d696_389 ["dvChars"])
                                                 let 't' = xx695_390
                                                 return ()
                                                 return tab]
                pats37_106 = foldl1 mplus [do p <- StateT pat
                                              _ <- StateT spaces
                                              return ()
                                              ps <- StateT pats
                                              return (cons p ps),
                                           return emp]
                readFromLs38_107 = foldl1 mplus [do rf <- StateT readFrom
                                                    d706_391 <- get
                                                    xx705_392 <- StateT derivsChars
                                                    case xx705_392 of
                                                        '*' -> return ()
                                                        _ -> gets derivsPosition >>= (throwError . ParseError "'*'" "not match pattern: " "" d706_391 ["dvChars"])
                                                    let '*' = xx705_392
                                                    return ()
                                                    return (FromList rf),
                                                 do rf <- StateT readFrom
                                                    d710_393 <- get
                                                    xx709_394 <- StateT derivsChars
                                                    case xx709_394 of
                                                        '+' -> return ()
                                                        _ -> gets derivsPosition >>= (throwError . ParseError "'+'" "not match pattern: " "" d710_393 ["dvChars"])
                                                    let '+' = xx709_394
                                                    return ()
                                                    return (FromList1 rf),
                                                 do rf <- StateT readFrom
                                                    d714_395 <- get
                                                    xx713_396 <- StateT derivsChars
                                                    case xx713_396 of
                                                        '?' -> return ()
                                                        _ -> gets derivsPosition >>= (throwError . ParseError "'?'" "not match pattern: " "" d714_395 ["dvChars"])
                                                    let '?' = xx713_396
                                                    return ()
                                                    return (FromOptional rf),
                                                 do rf <- StateT readFrom
                                                    return rf]
                readFrom39_108 = foldl1 mplus [do v <- StateT variable
                                                  return (FromVariable v),
                                               do d720_397 <- get
                                                  xx719_398 <- StateT derivsChars
                                                  case xx719_398 of
                                                      '(' -> return ()
                                                      _ -> gets derivsPosition >>= (throwError . ParseError "'('" "not match pattern: " "" d720_397 ["dvChars"])
                                                  let '(' = xx719_398
                                                  return ()
                                                  s <- StateT selection
                                                  d724_399 <- get
                                                  xx723_400 <- StateT derivsChars
                                                  case xx723_400 of
                                                      ')' -> return ()
                                                      _ -> gets derivsPosition >>= (throwError . ParseError "')'" "not match pattern: " "" d724_399 ["dvChars"])
                                                  let ')' = xx723_400
                                                  return ()
                                                  return (FromSelection s)]
                test40_109 = foldl1 mplus [do d726_401 <- get
                                              xx725_402 <- StateT derivsChars
                                              case xx725_402 of
                                                  '[' -> return ()
                                                  _ -> gets derivsPosition >>= (throwError . ParseError "'['" "not match pattern: " "" d726_401 ["dvChars"])
                                              let '[' = xx725_402
                                              return ()
                                              h <- StateT hsExpLam
                                              _ <- StateT spaces
                                              return ()
                                              com <- papOptional (StateT comForErr)
                                              d734_403 <- get
                                              xx733_404 <- StateT derivsChars
                                              case xx733_404 of
                                                  ']' -> return ()
                                                  _ -> gets derivsPosition >>= (throwError . ParseError "']'" "not match pattern: " "" d734_403 ["dvChars"])
                                              let ']' = xx733_404
                                              return ()
                                              return (h, maybe "" id com)]
                hsExpLam41_110 = foldl1 mplus [do d736_405 <- get
                                                  xx735_406 <- StateT derivsChars
                                                  case xx735_406 of
                                                      '\\' -> return ()
                                                      _ -> gets derivsPosition >>= (throwError . ParseError "'\\\\'" "not match pattern: " "" d736_405 ["dvChars"])
                                                  let '\\' = xx735_406
                                                  return ()
                                                  _ <- StateT spaces
                                                  return ()
                                                  ps <- StateT pats
                                                  _ <- StateT spaces
                                                  return ()
                                                  d744_407 <- get
                                                  xx743_408 <- StateT derivsChars
                                                  case xx743_408 of
                                                      '-' -> return ()
                                                      _ -> gets derivsPosition >>= (throwError . ParseError "'-'" "not match pattern: " "" d744_407 ["dvChars"])
                                                  let '-' = xx743_408
                                                  return ()
                                                  d746_409 <- get
                                                  xx745_410 <- StateT derivsChars
                                                  let c = xx745_410
                                                  unless (isGt c) (gets derivsPosition >>= (throwError . ParseError "isGt c" "not match: " "" d746_409 ["dvChars"]))
                                                  _ <- StateT spaces
                                                  return ()
                                                  e <- StateT hsExpTyp
                                                  return (lamE ps e),
                                               do e <- StateT hsExpTyp
                                                  return e]
                hsExpTyp42_111 = foldl1 mplus [do eo <- StateT hsExpOp
                                                  d756_411 <- get
                                                  xx755_412 <- StateT derivsChars
                                                  case xx755_412 of
                                                      ':' -> return ()
                                                      _ -> gets derivsPosition >>= (throwError . ParseError "':'" "not match pattern: " "" d756_411 ["dvChars"])
                                                  let ':' = xx755_412
                                                  return ()
                                                  d758_413 <- get
                                                  xx757_414 <- StateT derivsChars
                                                  case xx757_414 of
                                                      ':' -> return ()
                                                      _ -> gets derivsPosition >>= (throwError . ParseError "':'" "not match pattern: " "" d758_413 ["dvChars"])
                                                  let ':' = xx757_414
                                                  return ()
                                                  _ <- StateT spaces
                                                  return ()
                                                  t <- StateT hsTypeArr
                                                  return (sigE eo t),
                                               do eo <- StateT hsExpOp
                                                  return eo]
                hsExpOp43_112 = foldl1 mplus [do l <- StateT hsExp
                                                 _ <- StateT spaces
                                                 return ()
                                                 o <- StateT hsOp
                                                 _ <- StateT spaces
                                                 return ()
                                                 r <- StateT hsExpOp
                                                 return (uInfixE (getEx l) o r),
                                              do e <- StateT hsExp
                                                 return (getEx e)]
                hsOp44_113 = foldl1 mplus [do d778_415 <- get
                                              xx777_416 <- StateT derivsChars
                                              let c = xx777_416
                                              unless (isOpHeadChar c) (gets derivsPosition >>= (throwError . ParseError "isOpHeadChar c" "not match: " "" d778_415 ["dvChars"]))
                                              o <- StateT opTail
                                              return (varE (mkName (cons c o))),
                                           do d782_417 <- get
                                              xx781_418 <- StateT derivsChars
                                              case xx781_418 of
                                                  ':' -> return ()
                                                  _ -> gets derivsPosition >>= (throwError . ParseError "':'" "not match pattern: " "" d782_417 ["dvChars"])
                                              let ':' = xx781_418
                                              return ()
                                              ddd783_419 <- get
                                              do err <- ((do d785_420 <- get
                                                             xx784_421 <- StateT derivsChars
                                                             case xx784_421 of
                                                                 ':' -> return ()
                                                                 _ -> gets derivsPosition >>= (throwError . ParseError "':'" "not match pattern: " "" d785_420 ["dvChars"])
                                                             let ':' = xx784_421
                                                             return ()) >> return False) `catchError` const (return True)
                                                 unless err (gets derivsPosition >>= (throwError . ParseError ('!' : "':':") "not match: " "" ddd783_419 ["dvChars"]))
                                              put ddd783_419
                                              o <- StateT opTail
                                              return (conE (mkName (':' : o))),
                                           do d789_422 <- get
                                              xx788_423 <- StateT derivsChars
                                              let c = xx788_423
                                              unless (isBQ c) (gets derivsPosition >>= (throwError . ParseError "isBQ c" "not match: " "" d789_422 ["dvChars"]))
                                              v <- StateT variable
                                              d793_424 <- get
                                              xx792_425 <- StateT derivsChars
                                              let c_ = xx792_425
                                              unless (isBQ c_) (gets derivsPosition >>= (throwError . ParseError "isBQ c_" "not match: " "" d793_424 ["dvChars"]))
                                              return (varE (mkName v)),
                                           do d795_426 <- get
                                              xx794_427 <- StateT derivsChars
                                              let c = xx794_427
                                              unless (isBQ c) (gets derivsPosition >>= (throwError . ParseError "isBQ c" "not match: " "" d795_426 ["dvChars"]))
                                              t <- StateT typ
                                              d799_428 <- get
                                              xx798_429 <- StateT derivsChars
                                              let c_ = xx798_429
                                              unless (isBQ c_) (gets derivsPosition >>= (throwError . ParseError "isBQ c_" "not match: " "" d799_428 ["dvChars"]))
                                              return (conE (mkName t))]
                opTail45_114 = foldl1 mplus [do d801_430 <- get
                                                xx800_431 <- StateT derivsChars
                                                let c = xx800_431
                                                unless (isOpTailChar c) (gets derivsPosition >>= (throwError . ParseError "isOpTailChar c" "not match: " "" d801_430 ["dvChars"]))
                                                s <- StateT opTail
                                                return (cons c s),
                                             return emp]
                hsExp46_115 = foldl1 mplus [do e <- StateT hsExp1
                                               _ <- StateT spaces
                                               return ()
                                               h <- StateT hsExp
                                               return (applyExR e h),
                                            do e <- StateT hsExp1
                                               return (toEx e)]
                hsExp147_116 = foldl1 mplus [do d813_432 <- get
                                                xx812_433 <- StateT derivsChars
                                                case xx812_433 of
                                                    '(' -> return ()
                                                    _ -> gets derivsPosition >>= (throwError . ParseError "'('" "not match pattern: " "" d813_432 ["dvChars"])
                                                let '(' = xx812_433
                                                return ()
                                                l <- papOptional (foldl1 mplus [do e <- StateT hsExpTyp
                                                                                   return e])
                                                _ <- StateT spaces
                                                return ()
                                                o <- StateT hsOp
                                                _ <- StateT spaces
                                                return ()
                                                r <- papOptional (foldl1 mplus [do e <- StateT hsExpTyp
                                                                                   return e])
                                                d829_434 <- get
                                                xx828_435 <- StateT derivsChars
                                                case xx828_435 of
                                                    ')' -> return ()
                                                    _ -> gets derivsPosition >>= (throwError . ParseError "')'" "not match pattern: " "" d829_434 ["dvChars"])
                                                let ')' = xx828_435
                                                return ()
                                                return (infixE l o r),
                                             do d831_436 <- get
                                                xx830_437 <- StateT derivsChars
                                                case xx830_437 of
                                                    '(' -> return ()
                                                    _ -> gets derivsPosition >>= (throwError . ParseError "'('" "not match pattern: " "" d831_436 ["dvChars"])
                                                let '(' = xx830_437
                                                return ()
                                                et <- StateT hsExpTpl
                                                d835_438 <- get
                                                xx834_439 <- StateT derivsChars
                                                case xx834_439 of
                                                    ')' -> return ()
                                                    _ -> gets derivsPosition >>= (throwError . ParseError "')'" "not match pattern: " "" d835_438 ["dvChars"])
                                                let ')' = xx834_439
                                                return ()
                                                return (tupE et),
                                             do d837_440 <- get
                                                xx836_441 <- StateT derivsChars
                                                case xx836_441 of
                                                    '[' -> return ()
                                                    _ -> gets derivsPosition >>= (throwError . ParseError "'['" "not match pattern: " "" d837_440 ["dvChars"])
                                                let '[' = xx836_441
                                                return ()
                                                et <- StateT hsExpTpl
                                                d841_442 <- get
                                                xx840_443 <- StateT derivsChars
                                                case xx840_443 of
                                                    ']' -> return ()
                                                    _ -> gets derivsPosition >>= (throwError . ParseError "']'" "not match pattern: " "" d841_442 ["dvChars"])
                                                let ']' = xx840_443
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
                                             do d851_444 <- get
                                                xx850_445 <- StateT derivsChars
                                                case xx850_445 of
                                                    '\'' -> return ()
                                                    _ -> gets derivsPosition >>= (throwError . ParseError "'\\''" "not match pattern: " "" d851_444 ["dvChars"])
                                                let '\'' = xx850_445
                                                return ()
                                                c <- StateT charLit
                                                d855_446 <- get
                                                xx854_447 <- StateT derivsChars
                                                case xx854_447 of
                                                    '\'' -> return ()
                                                    _ -> gets derivsPosition >>= (throwError . ParseError "'\\''" "not match pattern: " "" d855_446 ["dvChars"])
                                                let '\'' = xx854_447
                                                return ()
                                                return (litE (charL c)),
                                             do d857_448 <- get
                                                xx856_449 <- StateT derivsChars
                                                case xx856_449 of
                                                    '"' -> return ()
                                                    _ -> gets derivsPosition >>= (throwError . ParseError "'\"'" "not match pattern: " "" d857_448 ["dvChars"])
                                                let '"' = xx856_449
                                                return ()
                                                s <- StateT stringLit
                                                d861_450 <- get
                                                xx860_451 <- StateT derivsChars
                                                case xx860_451 of
                                                    '"' -> return ()
                                                    _ -> gets derivsPosition >>= (throwError . ParseError "'\"'" "not match pattern: " "" d861_450 ["dvChars"])
                                                let '"' = xx860_451
                                                return ()
                                                return (litE (stringL s)),
                                             do d863_452 <- get
                                                xx862_453 <- StateT derivsChars
                                                case xx862_453 of
                                                    '-' -> return ()
                                                    _ -> gets derivsPosition >>= (throwError . ParseError "'-'" "not match pattern: " "" d863_452 ["dvChars"])
                                                let '-' = xx862_453
                                                return ()
                                                _ <- StateT spaces
                                                return ()
                                                e <- StateT hsExp1
                                                return (appE (varE $ mkName "negate") e)]
                hsExpTpl48_117 = foldl1 mplus [do e <- StateT hsExpLam
                                                  _ <- StateT spaces
                                                  return ()
                                                  d873_454 <- get
                                                  xx872_455 <- StateT derivsChars
                                                  let c = xx872_455
                                                  unless (isComma c) (gets derivsPosition >>= (throwError . ParseError "isComma c" "not match: " "" d873_454 ["dvChars"]))
                                                  _ <- StateT spaces
                                                  return ()
                                                  et <- StateT hsExpTpl
                                                  return (cons e et),
                                               do e <- StateT hsExpLam
                                                  return (cons e emp),
                                               return emp]
                hsTypeArr49_118 = foldl1 mplus [do l <- StateT hsType
                                                   d883_456 <- get
                                                   xx882_457 <- StateT derivsChars
                                                   case xx882_457 of
                                                       '-' -> return ()
                                                       _ -> gets derivsPosition >>= (throwError . ParseError "'-'" "not match pattern: " "" d883_456 ["dvChars"])
                                                   let '-' = xx882_457
                                                   return ()
                                                   d885_458 <- get
                                                   xx884_459 <- StateT derivsChars
                                                   let c = xx884_459
                                                   unless (isGt c) (gets derivsPosition >>= (throwError . ParseError "isGt c" "not match: " "" d885_458 ["dvChars"]))
                                                   _ <- StateT spaces
                                                   return ()
                                                   r <- StateT hsTypeArr
                                                   return (appT (appT arrowT (getTyp l)) r),
                                                do t <- StateT hsType
                                                   return (getTyp t)]
                hsType50_119 = foldl1 mplus [do t <- StateT hsType1
                                                ts <- StateT hsType
                                                return (applyTyp (toTyp t) ts),
                                             do t <- StateT hsType1
                                                return (toTyp t)]
                hsType151_120 = foldl1 mplus [do d899_460 <- get
                                                 xx898_461 <- StateT derivsChars
                                                 case xx898_461 of
                                                     '[' -> return ()
                                                     _ -> gets derivsPosition >>= (throwError . ParseError "'['" "not match pattern: " "" d899_460 ["dvChars"])
                                                 let '[' = xx898_461
                                                 return ()
                                                 d901_462 <- get
                                                 xx900_463 <- StateT derivsChars
                                                 case xx900_463 of
                                                     ']' -> return ()
                                                     _ -> gets derivsPosition >>= (throwError . ParseError "']'" "not match pattern: " "" d901_462 ["dvChars"])
                                                 let ']' = xx900_463
                                                 return ()
                                                 _ <- StateT spaces
                                                 return ()
                                                 return listT,
                                              do d905_464 <- get
                                                 xx904_465 <- StateT derivsChars
                                                 case xx904_465 of
                                                     '[' -> return ()
                                                     _ -> gets derivsPosition >>= (throwError . ParseError "'['" "not match pattern: " "" d905_464 ["dvChars"])
                                                 let '[' = xx904_465
                                                 return ()
                                                 t <- StateT hsTypeArr
                                                 d909_466 <- get
                                                 xx908_467 <- StateT derivsChars
                                                 case xx908_467 of
                                                     ']' -> return ()
                                                     _ -> gets derivsPosition >>= (throwError . ParseError "']'" "not match pattern: " "" d909_466 ["dvChars"])
                                                 let ']' = xx908_467
                                                 return ()
                                                 _ <- StateT spaces
                                                 return ()
                                                 return (appT listT t),
                                              do d913_468 <- get
                                                 xx912_469 <- StateT derivsChars
                                                 case xx912_469 of
                                                     '(' -> return ()
                                                     _ -> gets derivsPosition >>= (throwError . ParseError "'('" "not match pattern: " "" d913_468 ["dvChars"])
                                                 let '(' = xx912_469
                                                 return ()
                                                 _ <- StateT spaces
                                                 return ()
                                                 tt <- StateT hsTypeTpl
                                                 d919_470 <- get
                                                 xx918_471 <- StateT derivsChars
                                                 case xx918_471 of
                                                     ')' -> return ()
                                                     _ -> gets derivsPosition >>= (throwError . ParseError "')'" "not match pattern: " "" d919_470 ["dvChars"])
                                                 let ')' = xx918_471
                                                 return ()
                                                 return (tupT tt),
                                              do t <- StateT typToken
                                                 return (conT (mkName t)),
                                              do d923_472 <- get
                                                 xx922_473 <- StateT derivsChars
                                                 case xx922_473 of
                                                     '(' -> return ()
                                                     _ -> gets derivsPosition >>= (throwError . ParseError "'('" "not match pattern: " "" d923_472 ["dvChars"])
                                                 let '(' = xx922_473
                                                 return ()
                                                 d925_474 <- get
                                                 xx924_475 <- StateT derivsChars
                                                 case xx924_475 of
                                                     '-' -> return ()
                                                     _ -> gets derivsPosition >>= (throwError . ParseError "'-'" "not match pattern: " "" d925_474 ["dvChars"])
                                                 let '-' = xx924_475
                                                 return ()
                                                 d927_476 <- get
                                                 xx926_477 <- StateT derivsChars
                                                 let c = xx926_477
                                                 unless (isGt c) (gets derivsPosition >>= (throwError . ParseError "isGt c" "not match: " "" d927_476 ["dvChars"]))
                                                 d929_478 <- get
                                                 xx928_479 <- StateT derivsChars
                                                 case xx928_479 of
                                                     ')' -> return ()
                                                     _ -> gets derivsPosition >>= (throwError . ParseError "')'" "not match pattern: " "" d929_478 ["dvChars"])
                                                 let ')' = xx928_479
                                                 return ()
                                                 _ <- StateT spaces
                                                 return ()
                                                 return arrowT]
                hsTypeTpl52_121 = foldl1 mplus [do t <- StateT hsTypeArr
                                                   d935_480 <- get
                                                   xx934_481 <- StateT derivsChars
                                                   let c = xx934_481
                                                   unless (isComma c) (gets derivsPosition >>= (throwError . ParseError "isComma c" "not match: " "" d935_480 ["dvChars"]))
                                                   _ <- StateT spaces
                                                   return ()
                                                   tt <- StateT hsTypeTpl
                                                   return (cons t tt),
                                                do t <- StateT hsTypeArr
                                                   return (cons t emp),
                                                return emp]
                typ53_122 = foldl1 mplus [do u <- StateT upper
                                             t <- StateT tvtail
                                             return (cons u t)]
                variable54_123 = foldl1 mplus [do l <- StateT lower
                                                  t <- StateT tvtail
                                                  return (cons l t)]
                tvtail55_124 = foldl1 mplus [do a <- StateT alpha
                                                t <- StateT tvtail
                                                return (cons a t),
                                             return emp]
                integer56_125 = foldl1 mplus [do dh <- StateT digit
                                                 ds <- list (foldl1 mplus [do d <- StateT digit
                                                                              return d])
                                                 return (read (cons dh ds))]
                alpha57_126 = foldl1 mplus [do u <- StateT upper
                                               return u,
                                            do l <- StateT lower
                                               return l,
                                            do d <- StateT digit
                                               return d,
                                            do d967_482 <- get
                                               xx966_483 <- StateT derivsChars
                                               case xx966_483 of
                                                   '\'' -> return ()
                                                   _ -> gets derivsPosition >>= (throwError . ParseError "'\\''" "not match pattern: " "" d967_482 ["dvChars"])
                                               let '\'' = xx966_483
                                               return ()
                                               return '\'']
                upper58_127 = foldl1 mplus [do d969_484 <- get
                                               xx968_485 <- StateT derivsChars
                                               let u = xx968_485
                                               unless (isUpper u) (gets derivsPosition >>= (throwError . ParseError "isUpper u" "not match: " "" d969_484 ["dvChars"]))
                                               return u]
                lower59_128 = foldl1 mplus [do d971_486 <- get
                                               xx970_487 <- StateT derivsChars
                                               let l = xx970_487
                                               unless (isLowerU l) (gets derivsPosition >>= (throwError . ParseError "isLowerU l" "not match: " "" d971_486 ["dvChars"]))
                                               return l]
                digit60_129 = foldl1 mplus [do d973_488 <- get
                                               xx972_489 <- StateT derivsChars
                                               let d = xx972_489
                                               unless (isDigit d) (gets derivsPosition >>= (throwError . ParseError "isDigit d" "not match: " "" d973_488 ["dvChars"]))
                                               return d]
                spaces61_130 = foldl1 mplus [do _ <- StateT space
                                                return ()
                                                _ <- StateT spaces
                                                return ()
                                                return (),
                                             return ()]
                space62_131 = foldl1 mplus [do d979_490 <- get
                                               xx978_491 <- StateT derivsChars
                                               let s = xx978_491
                                               unless (isSpace s) (gets derivsPosition >>= (throwError . ParseError "isSpace s" "not match: " "" d979_490 ["dvChars"]))
                                               return (),
                                            do d981_492 <- get
                                               xx980_493 <- StateT derivsChars
                                               case xx980_493 of
                                                   '-' -> return ()
                                                   _ -> gets derivsPosition >>= (throwError . ParseError "'-'" "not match pattern: " "" d981_492 ["dvChars"])
                                               let '-' = xx980_493
                                               return ()
                                               d983_494 <- get
                                               xx982_495 <- StateT derivsChars
                                               case xx982_495 of
                                                   '-' -> return ()
                                                   _ -> gets derivsPosition >>= (throwError . ParseError "'-'" "not match pattern: " "" d983_494 ["dvChars"])
                                               let '-' = xx982_495
                                               return ()
                                               _ <- StateT notNLString
                                               return ()
                                               _ <- StateT newLine
                                               return ()
                                               return (),
                                            do _ <- StateT comment
                                               return ()
                                               return ()]
                notNLString63_132 = foldl1 mplus [do ddd990_496 <- get
                                                     do err <- ((do _ <- StateT newLine
                                                                    return ()) >> return False) `catchError` const (return True)
                                                        unless err (gets derivsPosition >>= (throwError . ParseError ('!' : "_:newLine") "not match: " "" ddd990_496 ["newLine"]))
                                                     put ddd990_496
                                                     c <- StateT derivsChars
                                                     s <- StateT notNLString
                                                     return (cons c s),
                                                  return emp]
                newLine64_133 = foldl1 mplus [do d998_497 <- get
                                                 xx997_498 <- StateT derivsChars
                                                 case xx997_498 of
                                                     '\n' -> return ()
                                                     _ -> gets derivsPosition >>= (throwError . ParseError "'\\n'" "not match pattern: " "" d998_497 ["dvChars"])
                                                 let '\n' = xx997_498
                                                 return ()
                                                 return ()]
                comment65_134 = foldl1 mplus [do d1000_499 <- get
                                                 xx999_500 <- StateT derivsChars
                                                 case xx999_500 of
                                                     '{' -> return ()
                                                     _ -> gets derivsPosition >>= (throwError . ParseError "'{'" "not match pattern: " "" d1000_499 ["dvChars"])
                                                 let '{' = xx999_500
                                                 return ()
                                                 d1002_501 <- get
                                                 xx1001_502 <- StateT derivsChars
                                                 case xx1001_502 of
                                                     '-' -> return ()
                                                     _ -> gets derivsPosition >>= (throwError . ParseError "'-'" "not match pattern: " "" d1002_501 ["dvChars"])
                                                 let '-' = xx1001_502
                                                 return ()
                                                 ddd1003_503 <- get
                                                 do err <- ((do d1005_504 <- get
                                                                xx1004_505 <- StateT derivsChars
                                                                case xx1004_505 of
                                                                    '#' -> return ()
                                                                    _ -> gets derivsPosition >>= (throwError . ParseError "'#'" "not match pattern: " "" d1005_504 ["dvChars"])
                                                                let '#' = xx1004_505
                                                                return ()) >> return False) `catchError` const (return True)
                                                    unless err (gets derivsPosition >>= (throwError . ParseError ('!' : "'#':") "not match: " "" ddd1003_503 ["dvChars"]))
                                                 put ddd1003_503
                                                 _ <- StateT comments
                                                 return ()
                                                 _ <- StateT comEnd
                                                 return ()
                                                 return ()]
                comments66_135 = foldl1 mplus [do _ <- StateT notComStr
                                                  return ()
                                                  _ <- StateT comment
                                                  return ()
                                                  _ <- StateT comments
                                                  return ()
                                                  return (),
                                               do _ <- StateT notComStr
                                                  return ()
                                                  return ()]
                notComStr67_136 = foldl1 mplus [do ddd1018_506 <- get
                                                   do err <- ((do _ <- StateT comment
                                                                  return ()) >> return False) `catchError` const (return True)
                                                      unless err (gets derivsPosition >>= (throwError . ParseError ('!' : "_:comment") "not match: " "" ddd1018_506 ["comment"]))
                                                   put ddd1018_506
                                                   ddd1021_507 <- get
                                                   do err <- ((do _ <- StateT comEnd
                                                                  return ()) >> return False) `catchError` const (return True)
                                                      unless err (gets derivsPosition >>= (throwError . ParseError ('!' : "_:comEnd") "not match: " "" ddd1021_507 ["comEnd"]))
                                                   put ddd1021_507
                                                   _ <- StateT derivsChars
                                                   return ()
                                                   _ <- StateT notComStr
                                                   return ()
                                                   return (),
                                                return ()]
                comEnd68_137 = foldl1 mplus [do d1029_508 <- get
                                                xx1028_509 <- StateT derivsChars
                                                case xx1028_509 of
                                                    '-' -> return ()
                                                    _ -> gets derivsPosition >>= (throwError . ParseError "'-'" "not match pattern: " "" d1029_508 ["dvChars"])
                                                let '-' = xx1028_509
                                                return ()
                                                d1031_510 <- get
                                                xx1030_511 <- StateT derivsChars
                                                case xx1030_511 of
                                                    '}' -> return ()
                                                    _ -> gets derivsPosition >>= (throwError . ParseError "'}'" "not match pattern: " "" d1031_510 ["dvChars"])
                                                let '}' = xx1030_511
                                                return ()
                                                return ()]

class Source sl
    where type Token sl
          data Pos sl
          getToken :: sl -> Maybe ((Token sl, sl))
          initialPos :: Pos sl
          updatePos :: Token sl -> Pos sl -> Pos sl
class SourceList c
    where data ListPos c
          listToken :: [c] -> Maybe ((c, [c]))
          listInitialPos :: ListPos c
          listUpdatePos :: c -> ListPos c -> ListPos c
          listShowPos :: ListPos c -> String
instance SourceList c => Source ([c])
    where type Token ([c]) = c
          newtype Pos ([c]) = ListPos (ListPos c)
          getToken = listToken
          initialPos = ListPos listInitialPos
          updatePos c (ListPos p) = ListPos (listUpdatePos c p)
instance Show (ListPos a) => Show (Pos ([a]))
    where show (ListPos x) = "(" ++ (("ListPos (" ++ (show x ++ ")")) ++ ")")
instance SourceList Char
    where newtype ListPos Char = CharPos ((Int, Int)) deriving (Show)
          listToken (c : s) = Just (c, s)
          listToken _ = Nothing
          listInitialPos = CharPos (1, 1)
          listUpdatePos '\n' (CharPos (y, _)) = CharPos (y + 1, 0)
          listUpdatePos _ (CharPos (y, x)) = CharPos (y, x + 1)
          listShowPos (CharPos pos) = show pos
list :: forall m a . (MonadPlus m, Applicative m) => m a -> m ([a])
list1 :: forall m a . (MonadPlus m, Applicative m) =>
                      m a -> m ([a])
list p = list1 p `mplus` return []
list1 p = ((:) <$> p) <*> list p
papOptional :: forall m a . (MonadPlus m, Applicative m) =>
                            m a -> m (Maybe a)
papOptional p = (Just <$> p) `mplus` return Nothing