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
	Derivs(peg, pegFile, dvChars),
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
              dvChars :: (Either (ParseError (Pos String))
                                 ((Token String, Derivs))),
              dvPos :: (Pos String)}
data ParseError pos
    = ParseError String String String Derivs ([String]) pos
instance Error (ParseError pos)
    where strMsg msg = ParseError "" msg "" undefined undefined undefined
parse :: String -> Derivs
parse = parseGen initialPos
          where parseGen pos s = d
                             where d = Derivs localpegFile0_0 localpragma1_1 localpragmaStr2_2 localpragmaItems3_3 localdelPragmas4_4 localpragmaEnd5_5 localmoduleDec6_6 localmoduleDecStr7_7 localwhr8_8 localpreImpPap9_9 localprePeg10_10 localafterPeg11_11 localimportPapillon12_12 localvarToken13_13 localtypToken14_14 localpap15_15 localpeg16_16 localsourceType17_17 localpeg_18_18 localdefinition19_19 localselection20_20 localexpressionHs21_21 localexpression22_22 localnameLeaf_23_23 localnameLeaf24_24 localnameLeafNoCom25_25 localcomForErr26_26 localleaf27_27 localpatOp28_28 localpat29_29 localpat130_30 localpatList31_31 localopConName32_32 localcharLit33_33 localstringLit34_34 localescapeC35_35 localpats36_36 localreadFromLs37_37 localreadFrom38_38 localtest39_39 localhsExpLam40_40 localhsExpTyp41_41 localhsExpOp42_42 localhsOp43_43 localopTail44_44 localhsExp45_45 localhsExp146_46 localhsExpTpl47_47 localhsTypeArr48_48 localhsType49_49 localhsType150_50 localhsTypeTpl51_51 localtyp52_52 localvariable53_53 localtvtail54_54 localinteger55_55 localalpha56_56 localupper57_57 locallower58_58 localdigit59_59 localspaces60_60 localspace61_61 localnotNLString62_62 localnewLine63_63 localcomment64_64 localcomments65_65 localnotComStr66_66 localcomEnd67_67 char pos
                                   localpegFile0_0 = runStateT pegFileP d
                                   localpragma1_1 = runStateT pragmaP d
                                   localpragmaStr2_2 = runStateT pragmaStrP d
                                   localpragmaItems3_3 = runStateT pragmaItemsP d
                                   localdelPragmas4_4 = runStateT delPragmasP d
                                   localpragmaEnd5_5 = runStateT pragmaEndP d
                                   localmoduleDec6_6 = runStateT moduleDecP d
                                   localmoduleDecStr7_7 = runStateT moduleDecStrP d
                                   localwhr8_8 = runStateT whrP d
                                   localpreImpPap9_9 = runStateT preImpPapP d
                                   localprePeg10_10 = runStateT prePegP d
                                   localafterPeg11_11 = runStateT afterPegP d
                                   localimportPapillon12_12 = runStateT importPapillonP d
                                   localvarToken13_13 = runStateT varTokenP d
                                   localtypToken14_14 = runStateT typTokenP d
                                   localpap15_15 = runStateT papP d
                                   localpeg16_16 = runStateT pegP d
                                   localsourceType17_17 = runStateT sourceTypeP d
                                   localpeg_18_18 = runStateT peg_P d
                                   localdefinition19_19 = runStateT definitionP d
                                   localselection20_20 = runStateT selectionP d
                                   localexpressionHs21_21 = runStateT expressionHsP d
                                   localexpression22_22 = runStateT expressionP d
                                   localnameLeaf_23_23 = runStateT nameLeaf_P d
                                   localnameLeaf24_24 = runStateT nameLeafP d
                                   localnameLeafNoCom25_25 = runStateT nameLeafNoComP d
                                   localcomForErr26_26 = runStateT comForErrP d
                                   localleaf27_27 = runStateT leafP d
                                   localpatOp28_28 = runStateT patOpP d
                                   localpat29_29 = runStateT patP d
                                   localpat130_30 = runStateT pat1P d
                                   localpatList31_31 = runStateT patListP d
                                   localopConName32_32 = runStateT opConNameP d
                                   localcharLit33_33 = runStateT charLitP d
                                   localstringLit34_34 = runStateT stringLitP d
                                   localescapeC35_35 = runStateT escapeCP d
                                   localpats36_36 = runStateT patsP d
                                   localreadFromLs37_37 = runStateT readFromLsP d
                                   localreadFrom38_38 = runStateT readFromP d
                                   localtest39_39 = runStateT testP d
                                   localhsExpLam40_40 = runStateT hsExpLamP d
                                   localhsExpTyp41_41 = runStateT hsExpTypP d
                                   localhsExpOp42_42 = runStateT hsExpOpP d
                                   localhsOp43_43 = runStateT hsOpP d
                                   localopTail44_44 = runStateT opTailP d
                                   localhsExp45_45 = runStateT hsExpP d
                                   localhsExp146_46 = runStateT hsExp1P d
                                   localhsExpTpl47_47 = runStateT hsExpTplP d
                                   localhsTypeArr48_48 = runStateT hsTypeArrP d
                                   localhsType49_49 = runStateT hsTypeP d
                                   localhsType150_50 = runStateT hsType1P d
                                   localhsTypeTpl51_51 = runStateT hsTypeTplP d
                                   localtyp52_52 = runStateT typP d
                                   localvariable53_53 = runStateT variableP d
                                   localtvtail54_54 = runStateT tvtailP d
                                   localinteger55_55 = runStateT integerP d
                                   localalpha56_56 = runStateT alphaP d
                                   localupper57_57 = runStateT upperP d
                                   locallower58_58 = runStateT lowerP d
                                   localdigit59_59 = runStateT digitP d
                                   localspaces60_60 = runStateT spacesP d
                                   localspace61_61 = runStateT spaceP d
                                   localnotNLString62_62 = runStateT notNLStringP d
                                   localnewLine63_63 = runStateT newLineP d
                                   localcomment64_64 = runStateT commentP d
                                   localcomments65_65 = runStateT commentsP d
                                   localnotComStr66_66 = runStateT notComStrP d
                                   localcomEnd67_67 = runStateT comEndP d
                                   char = runStateT (case getToken s of
                                                         Just (c,
                                                               s') -> do put (parseGen (updatePos c pos) s')
                                                                         return c
                                                         _ -> gets dvPos >>= (throwError . ParseError "" "end of input" "" undefined [])) d
                pegFileP = foldl1 mplus [do pr <- StateT pragma
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
                                            d85_68 <- get
                                            xx84_69 <- StateT dvChars
                                            case xx84_69 of
                                                '|' -> return ()
                                                _ -> gets dvPos >>= (throwError . ParseError "'|'" "not match pattern: " "" d85_68 ["dvChars"])
                                            let '|' = xx84_69
                                            return ()
                                            d87_70 <- get
                                            xx86_71 <- StateT dvChars
                                            case xx86_71 of
                                                ']' -> return ()
                                                _ -> gets dvPos >>= (throwError . ParseError "']'" "not match pattern: " "" d87_70 ["dvChars"])
                                            let ']' = xx86_71
                                            return ()
                                            d89_72 <- get
                                            xx88_73 <- StateT dvChars
                                            case xx88_73 of
                                                '\n' -> return ()
                                                _ -> gets dvPos >>= (throwError . ParseError "'\\n'" "not match pattern: " "" d89_72 ["dvChars"])
                                            let '\n' = xx88_73
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
                                            d105_74 <- get
                                            xx104_75 <- StateT dvChars
                                            case xx104_75 of
                                                '|' -> return ()
                                                _ -> gets dvPos >>= (throwError . ParseError "'|'" "not match pattern: " "" d105_74 ["dvChars"])
                                            let '|' = xx104_75
                                            return ()
                                            d107_76 <- get
                                            xx106_77 <- StateT dvChars
                                            case xx106_77 of
                                                ']' -> return ()
                                                _ -> gets dvPos >>= (throwError . ParseError "']'" "not match pattern: " "" d107_76 ["dvChars"])
                                            let ']' = xx106_77
                                            return ()
                                            d109_78 <- get
                                            xx108_79 <- StateT dvChars
                                            case xx108_79 of
                                                '\n' -> return ()
                                                _ -> gets dvPos >>= (throwError . ParseError "'\\n'" "not match pattern: " "" d109_78 ["dvChars"])
                                            let '\n' = xx108_79
                                            return ()
                                            atp <- StateT afterPeg
                                            return (mkPegFile pr md emp pp p atp)]
                pragmaP = foldl1 mplus [do _ <- StateT spaces
                                           return ()
                                           d115_80 <- get
                                           xx114_81 <- StateT dvChars
                                           case xx114_81 of
                                               '{' -> return ()
                                               _ -> gets dvPos >>= (throwError . ParseError "'{'" "not match pattern: " "" d115_80 ["dvChars"])
                                           let '{' = xx114_81
                                           return ()
                                           d117_82 <- get
                                           xx116_83 <- StateT dvChars
                                           case xx116_83 of
                                               '-' -> return ()
                                               _ -> gets dvPos >>= (throwError . ParseError "'-'" "not match pattern: " "" d117_82 ["dvChars"])
                                           let '-' = xx116_83
                                           return ()
                                           d119_84 <- get
                                           xx118_85 <- StateT dvChars
                                           case xx118_85 of
                                               '#' -> return ()
                                               _ -> gets dvPos >>= (throwError . ParseError "'#'" "not match pattern: " "" d119_84 ["dvChars"])
                                           let '#' = xx118_85
                                           return ()
                                           _ <- StateT spaces
                                           return ()
                                           d123_86 <- get
                                           xx122_87 <- StateT dvChars
                                           case xx122_87 of
                                               'L' -> return ()
                                               _ -> gets dvPos >>= (throwError . ParseError "'L'" "not match pattern: " "" d123_86 ["dvChars"])
                                           let 'L' = xx122_87
                                           return ()
                                           d125_88 <- get
                                           xx124_89 <- StateT dvChars
                                           case xx124_89 of
                                               'A' -> return ()
                                               _ -> gets dvPos >>= (throwError . ParseError "'A'" "not match pattern: " "" d125_88 ["dvChars"])
                                           let 'A' = xx124_89
                                           return ()
                                           d127_90 <- get
                                           xx126_91 <- StateT dvChars
                                           case xx126_91 of
                                               'N' -> return ()
                                               _ -> gets dvPos >>= (throwError . ParseError "'N'" "not match pattern: " "" d127_90 ["dvChars"])
                                           let 'N' = xx126_91
                                           return ()
                                           d129_92 <- get
                                           xx128_93 <- StateT dvChars
                                           case xx128_93 of
                                               'G' -> return ()
                                               _ -> gets dvPos >>= (throwError . ParseError "'G'" "not match pattern: " "" d129_92 ["dvChars"])
                                           let 'G' = xx128_93
                                           return ()
                                           d131_94 <- get
                                           xx130_95 <- StateT dvChars
                                           case xx130_95 of
                                               'U' -> return ()
                                               _ -> gets dvPos >>= (throwError . ParseError "'U'" "not match pattern: " "" d131_94 ["dvChars"])
                                           let 'U' = xx130_95
                                           return ()
                                           d133_96 <- get
                                           xx132_97 <- StateT dvChars
                                           case xx132_97 of
                                               'A' -> return ()
                                               _ -> gets dvPos >>= (throwError . ParseError "'A'" "not match pattern: " "" d133_96 ["dvChars"])
                                           let 'A' = xx132_97
                                           return ()
                                           d135_98 <- get
                                           xx134_99 <- StateT dvChars
                                           case xx134_99 of
                                               'G' -> return ()
                                               _ -> gets dvPos >>= (throwError . ParseError "'G'" "not match pattern: " "" d135_98 ["dvChars"])
                                           let 'G' = xx134_99
                                           return ()
                                           d137_100 <- get
                                           xx136_101 <- StateT dvChars
                                           case xx136_101 of
                                               'E' -> return ()
                                               _ -> gets dvPos >>= (throwError . ParseError "'E'" "not match pattern: " "" d137_100 ["dvChars"])
                                           let 'E' = xx136_101
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
                pragmaStrP = foldl1 mplus [do _ <- StateT delPragmas
                                              return ()
                                              _ <- StateT spaces
                                              return ()
                                              d153_102 <- get
                                              xx152_103 <- StateT dvChars
                                              case xx152_103 of
                                                  ',' -> return ()
                                                  _ -> gets dvPos >>= (throwError . ParseError "','" "not match pattern: " "" d153_102 ["dvChars"])
                                              let ',' = xx152_103
                                              return ()
                                              _ <- StateT spaces
                                              return ()
                                              s <- StateT pragmaStr
                                              return (' ' : s),
                                           do ddd158_104 <- get
                                              do err <- ((do _ <- StateT pragmaEnd
                                                             return ()) >> return False) `catchError` const (return True)
                                                 unless err (gets dvPos >>= (throwError . ParseError ('!' : "_:pragmaEnd") "not match: " "" ddd158_104 ["pragmaEnd"]))
                                              put ddd158_104
                                              ddd161_105 <- get
                                              do err <- ((do _ <- StateT delPragmas
                                                             return ()) >> return False) `catchError` const (return True)
                                                 unless err (gets dvPos >>= (throwError . ParseError ('!' : "_:delPragmas") "not match: " "" ddd161_105 ["delPragmas"]))
                                              put ddd161_105
                                              c <- StateT dvChars
                                              s <- StateT pragmaStr
                                              return (c : s),
                                           return emp]
                pragmaItemsP = foldl1 mplus [do _ <- StateT delPragmas
                                                return ()
                                                _ <- StateT spaces
                                                return ()
                                                d173_106 <- get
                                                xx172_107 <- StateT dvChars
                                                case xx172_107 of
                                                    ',' -> return ()
                                                    _ -> gets dvPos >>= (throwError . ParseError "','" "not match pattern: " "" d173_106 ["dvChars"])
                                                let ',' = xx172_107
                                                return ()
                                                _ <- StateT spaces
                                                return ()
                                                i <- StateT pragmaItems
                                                return i,
                                             do t <- StateT typToken
                                                d181_108 <- get
                                                xx180_109 <- StateT dvChars
                                                case xx180_109 of
                                                    ',' -> return ()
                                                    _ -> gets dvPos >>= (throwError . ParseError "','" "not match pattern: " "" d181_108 ["dvChars"])
                                                let ',' = xx180_109
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
                delPragmasP = foldl1 mplus [do d193_110 <- get
                                               xx192_111 <- StateT dvChars
                                               case xx192_111 of
                                                   'Q' -> return ()
                                                   _ -> gets dvPos >>= (throwError . ParseError "'Q'" "not match pattern: " "" d193_110 ["dvChars"])
                                               let 'Q' = xx192_111
                                               return ()
                                               d195_112 <- get
                                               xx194_113 <- StateT dvChars
                                               case xx194_113 of
                                                   'u' -> return ()
                                                   _ -> gets dvPos >>= (throwError . ParseError "'u'" "not match pattern: " "" d195_112 ["dvChars"])
                                               let 'u' = xx194_113
                                               return ()
                                               d197_114 <- get
                                               xx196_115 <- StateT dvChars
                                               case xx196_115 of
                                                   'a' -> return ()
                                                   _ -> gets dvPos >>= (throwError . ParseError "'a'" "not match pattern: " "" d197_114 ["dvChars"])
                                               let 'a' = xx196_115
                                               return ()
                                               d199_116 <- get
                                               xx198_117 <- StateT dvChars
                                               case xx198_117 of
                                                   's' -> return ()
                                                   _ -> gets dvPos >>= (throwError . ParseError "'s'" "not match pattern: " "" d199_116 ["dvChars"])
                                               let 's' = xx198_117
                                               return ()
                                               d201_118 <- get
                                               xx200_119 <- StateT dvChars
                                               case xx200_119 of
                                                   'i' -> return ()
                                                   _ -> gets dvPos >>= (throwError . ParseError "'i'" "not match pattern: " "" d201_118 ["dvChars"])
                                               let 'i' = xx200_119
                                               return ()
                                               d203_120 <- get
                                               xx202_121 <- StateT dvChars
                                               case xx202_121 of
                                                   'Q' -> return ()
                                                   _ -> gets dvPos >>= (throwError . ParseError "'Q'" "not match pattern: " "" d203_120 ["dvChars"])
                                               let 'Q' = xx202_121
                                               return ()
                                               d205_122 <- get
                                               xx204_123 <- StateT dvChars
                                               case xx204_123 of
                                                   'u' -> return ()
                                                   _ -> gets dvPos >>= (throwError . ParseError "'u'" "not match pattern: " "" d205_122 ["dvChars"])
                                               let 'u' = xx204_123
                                               return ()
                                               d207_124 <- get
                                               xx206_125 <- StateT dvChars
                                               case xx206_125 of
                                                   'o' -> return ()
                                                   _ -> gets dvPos >>= (throwError . ParseError "'o'" "not match pattern: " "" d207_124 ["dvChars"])
                                               let 'o' = xx206_125
                                               return ()
                                               d209_126 <- get
                                               xx208_127 <- StateT dvChars
                                               case xx208_127 of
                                                   't' -> return ()
                                                   _ -> gets dvPos >>= (throwError . ParseError "'t'" "not match pattern: " "" d209_126 ["dvChars"])
                                               let 't' = xx208_127
                                               return ()
                                               d211_128 <- get
                                               xx210_129 <- StateT dvChars
                                               case xx210_129 of
                                                   'e' -> return ()
                                                   _ -> gets dvPos >>= (throwError . ParseError "'e'" "not match pattern: " "" d211_128 ["dvChars"])
                                               let 'e' = xx210_129
                                               return ()
                                               d213_130 <- get
                                               xx212_131 <- StateT dvChars
                                               case xx212_131 of
                                                   's' -> return ()
                                                   _ -> gets dvPos >>= (throwError . ParseError "'s'" "not match pattern: " "" d213_130 ["dvChars"])
                                               let 's' = xx212_131
                                               return ()
                                               return (),
                                            do d215_132 <- get
                                               xx214_133 <- StateT dvChars
                                               case xx214_133 of
                                                   'T' -> return ()
                                                   _ -> gets dvPos >>= (throwError . ParseError "'T'" "not match pattern: " "" d215_132 ["dvChars"])
                                               let 'T' = xx214_133
                                               return ()
                                               d217_134 <- get
                                               xx216_135 <- StateT dvChars
                                               case xx216_135 of
                                                   'y' -> return ()
                                                   _ -> gets dvPos >>= (throwError . ParseError "'y'" "not match pattern: " "" d217_134 ["dvChars"])
                                               let 'y' = xx216_135
                                               return ()
                                               d219_136 <- get
                                               xx218_137 <- StateT dvChars
                                               case xx218_137 of
                                                   'p' -> return ()
                                                   _ -> gets dvPos >>= (throwError . ParseError "'p'" "not match pattern: " "" d219_136 ["dvChars"])
                                               let 'p' = xx218_137
                                               return ()
                                               d221_138 <- get
                                               xx220_139 <- StateT dvChars
                                               case xx220_139 of
                                                   'e' -> return ()
                                                   _ -> gets dvPos >>= (throwError . ParseError "'e'" "not match pattern: " "" d221_138 ["dvChars"])
                                               let 'e' = xx220_139
                                               return ()
                                               d223_140 <- get
                                               xx222_141 <- StateT dvChars
                                               case xx222_141 of
                                                   'F' -> return ()
                                                   _ -> gets dvPos >>= (throwError . ParseError "'F'" "not match pattern: " "" d223_140 ["dvChars"])
                                               let 'F' = xx222_141
                                               return ()
                                               d225_142 <- get
                                               xx224_143 <- StateT dvChars
                                               case xx224_143 of
                                                   'a' -> return ()
                                                   _ -> gets dvPos >>= (throwError . ParseError "'a'" "not match pattern: " "" d225_142 ["dvChars"])
                                               let 'a' = xx224_143
                                               return ()
                                               d227_144 <- get
                                               xx226_145 <- StateT dvChars
                                               case xx226_145 of
                                                   'm' -> return ()
                                                   _ -> gets dvPos >>= (throwError . ParseError "'m'" "not match pattern: " "" d227_144 ["dvChars"])
                                               let 'm' = xx226_145
                                               return ()
                                               d229_146 <- get
                                               xx228_147 <- StateT dvChars
                                               case xx228_147 of
                                                   'i' -> return ()
                                                   _ -> gets dvPos >>= (throwError . ParseError "'i'" "not match pattern: " "" d229_146 ["dvChars"])
                                               let 'i' = xx228_147
                                               return ()
                                               d231_148 <- get
                                               xx230_149 <- StateT dvChars
                                               case xx230_149 of
                                                   'l' -> return ()
                                                   _ -> gets dvPos >>= (throwError . ParseError "'l'" "not match pattern: " "" d231_148 ["dvChars"])
                                               let 'l' = xx230_149
                                               return ()
                                               d233_150 <- get
                                               xx232_151 <- StateT dvChars
                                               case xx232_151 of
                                                   'i' -> return ()
                                                   _ -> gets dvPos >>= (throwError . ParseError "'i'" "not match pattern: " "" d233_150 ["dvChars"])
                                               let 'i' = xx232_151
                                               return ()
                                               d235_152 <- get
                                               xx234_153 <- StateT dvChars
                                               case xx234_153 of
                                                   'e' -> return ()
                                                   _ -> gets dvPos >>= (throwError . ParseError "'e'" "not match pattern: " "" d235_152 ["dvChars"])
                                               let 'e' = xx234_153
                                               return ()
                                               d237_154 <- get
                                               xx236_155 <- StateT dvChars
                                               case xx236_155 of
                                                   's' -> return ()
                                                   _ -> gets dvPos >>= (throwError . ParseError "'s'" "not match pattern: " "" d237_154 ["dvChars"])
                                               let 's' = xx236_155
                                               return ()
                                               return ()]
                pragmaEndP = foldl1 mplus [do _ <- StateT delPragmas
                                              return ()
                                              _ <- StateT spaces
                                              return ()
                                              d243_156 <- get
                                              xx242_157 <- StateT dvChars
                                              case xx242_157 of
                                                  '#' -> return ()
                                                  _ -> gets dvPos >>= (throwError . ParseError "'#'" "not match pattern: " "" d243_156 ["dvChars"])
                                              let '#' = xx242_157
                                              return ()
                                              d245_158 <- get
                                              xx244_159 <- StateT dvChars
                                              case xx244_159 of
                                                  '-' -> return ()
                                                  _ -> gets dvPos >>= (throwError . ParseError "'-'" "not match pattern: " "" d245_158 ["dvChars"])
                                              let '-' = xx244_159
                                              return ()
                                              d247_160 <- get
                                              xx246_161 <- StateT dvChars
                                              case xx246_161 of
                                                  '}' -> return ()
                                                  _ -> gets dvPos >>= (throwError . ParseError "'}'" "not match pattern: " "" d247_160 ["dvChars"])
                                              let '}' = xx246_161
                                              return ()
                                              return (),
                                           do d249_162 <- get
                                              xx248_163 <- StateT dvChars
                                              case xx248_163 of
                                                  '#' -> return ()
                                                  _ -> gets dvPos >>= (throwError . ParseError "'#'" "not match pattern: " "" d249_162 ["dvChars"])
                                              let '#' = xx248_163
                                              return ()
                                              d251_164 <- get
                                              xx250_165 <- StateT dvChars
                                              case xx250_165 of
                                                  '-' -> return ()
                                                  _ -> gets dvPos >>= (throwError . ParseError "'-'" "not match pattern: " "" d251_164 ["dvChars"])
                                              let '-' = xx250_165
                                              return ()
                                              d253_166 <- get
                                              xx252_167 <- StateT dvChars
                                              case xx252_167 of
                                                  '}' -> return ()
                                                  _ -> gets dvPos >>= (throwError . ParseError "'}'" "not match pattern: " "" d253_166 ["dvChars"])
                                              let '}' = xx252_167
                                              return ()
                                              return ()]
                moduleDecP = foldl1 mplus [do d255_168 <- get
                                              xx254_169 <- StateT dvChars
                                              case xx254_169 of
                                                  'm' -> return ()
                                                  _ -> gets dvPos >>= (throwError . ParseError "'m'" "not match pattern: " "" d255_168 ["dvChars"])
                                              let 'm' = xx254_169
                                              return ()
                                              d257_170 <- get
                                              xx256_171 <- StateT dvChars
                                              case xx256_171 of
                                                  'o' -> return ()
                                                  _ -> gets dvPos >>= (throwError . ParseError "'o'" "not match pattern: " "" d257_170 ["dvChars"])
                                              let 'o' = xx256_171
                                              return ()
                                              d259_172 <- get
                                              xx258_173 <- StateT dvChars
                                              case xx258_173 of
                                                  'd' -> return ()
                                                  _ -> gets dvPos >>= (throwError . ParseError "'d'" "not match pattern: " "" d259_172 ["dvChars"])
                                              let 'd' = xx258_173
                                              return ()
                                              d261_174 <- get
                                              xx260_175 <- StateT dvChars
                                              case xx260_175 of
                                                  'u' -> return ()
                                                  _ -> gets dvPos >>= (throwError . ParseError "'u'" "not match pattern: " "" d261_174 ["dvChars"])
                                              let 'u' = xx260_175
                                              return ()
                                              d263_176 <- get
                                              xx262_177 <- StateT dvChars
                                              case xx262_177 of
                                                  'l' -> return ()
                                                  _ -> gets dvPos >>= (throwError . ParseError "'l'" "not match pattern: " "" d263_176 ["dvChars"])
                                              let 'l' = xx262_177
                                              return ()
                                              d265_178 <- get
                                              xx264_179 <- StateT dvChars
                                              case xx264_179 of
                                                  'e' -> return ()
                                                  _ -> gets dvPos >>= (throwError . ParseError "'e'" "not match pattern: " "" d265_178 ["dvChars"])
                                              let 'e' = xx264_179
                                              return ()
                                              s <- StateT moduleDecStr
                                              _ <- StateT whr
                                              return ()
                                              return (just s),
                                           return nothing]
                moduleDecStrP = foldl1 mplus [do ddd270_180 <- get
                                                 do err <- ((do _ <- StateT whr
                                                                return ()) >> return False) `catchError` const (return True)
                                                    unless err (gets dvPos >>= (throwError . ParseError ('!' : "_:whr") "not match: " "" ddd270_180 ["whr"]))
                                                 put ddd270_180
                                                 c <- StateT dvChars
                                                 s <- StateT moduleDecStr
                                                 return (cons c s),
                                              return emp]
                whrP = foldl1 mplus [do d278_181 <- get
                                        xx277_182 <- StateT dvChars
                                        case xx277_182 of
                                            'w' -> return ()
                                            _ -> gets dvPos >>= (throwError . ParseError "'w'" "not match pattern: " "" d278_181 ["dvChars"])
                                        let 'w' = xx277_182
                                        return ()
                                        d280_183 <- get
                                        xx279_184 <- StateT dvChars
                                        case xx279_184 of
                                            'h' -> return ()
                                            _ -> gets dvPos >>= (throwError . ParseError "'h'" "not match pattern: " "" d280_183 ["dvChars"])
                                        let 'h' = xx279_184
                                        return ()
                                        d282_185 <- get
                                        xx281_186 <- StateT dvChars
                                        case xx281_186 of
                                            'e' -> return ()
                                            _ -> gets dvPos >>= (throwError . ParseError "'e'" "not match pattern: " "" d282_185 ["dvChars"])
                                        let 'e' = xx281_186
                                        return ()
                                        d284_187 <- get
                                        xx283_188 <- StateT dvChars
                                        case xx283_188 of
                                            'r' -> return ()
                                            _ -> gets dvPos >>= (throwError . ParseError "'r'" "not match pattern: " "" d284_187 ["dvChars"])
                                        let 'r' = xx283_188
                                        return ()
                                        d286_189 <- get
                                        xx285_190 <- StateT dvChars
                                        case xx285_190 of
                                            'e' -> return ()
                                            _ -> gets dvPos >>= (throwError . ParseError "'e'" "not match pattern: " "" d286_189 ["dvChars"])
                                        let 'e' = xx285_190
                                        return ()
                                        return ()]
                preImpPapP = foldl1 mplus [do ddd287_191 <- get
                                              do err <- ((do _ <- StateT importPapillon
                                                             return ()) >> return False) `catchError` const (return True)
                                                 unless err (gets dvPos >>= (throwError . ParseError ('!' : "_:importPapillon") "not match: " "" ddd287_191 ["importPapillon"]))
                                              put ddd287_191
                                              ddd290_192 <- get
                                              do err <- ((do _ <- StateT pap
                                                             return ()) >> return False) `catchError` const (return True)
                                                 unless err (gets dvPos >>= (throwError . ParseError ('!' : "_:pap") "not match: " "" ddd290_192 ["pap"]))
                                              put ddd290_192
                                              c <- StateT dvChars
                                              pip <- StateT preImpPap
                                              return (cons c pip),
                                           return emp]
                prePegP = foldl1 mplus [do ddd297_193 <- get
                                           do err <- ((do _ <- StateT pap
                                                          return ()) >> return False) `catchError` const (return True)
                                              unless err (gets dvPos >>= (throwError . ParseError ('!' : "_:pap") "not match: " "" ddd297_193 ["pap"]))
                                           put ddd297_193
                                           c <- StateT dvChars
                                           pp <- StateT prePeg
                                           return (cons c pp),
                                        return emp]
                afterPegP = foldl1 mplus [do c <- StateT dvChars
                                             atp <- StateT afterPeg
                                             return (cons c atp),
                                          return emp]
                importPapillonP = foldl1 mplus [do d309_194 <- get
                                                   xx308_195 <- StateT varToken
                                                   case xx308_195 of
                                                       "import" -> return ()
                                                       _ -> gets dvPos >>= (throwError . ParseError "\"import\"" "not match pattern: " "" d309_194 ["varToken"])
                                                   let "import" = xx308_195
                                                   return ()
                                                   d311_196 <- get
                                                   xx310_197 <- StateT typToken
                                                   case xx310_197 of
                                                       "Text" -> return ()
                                                       _ -> gets dvPos >>= (throwError . ParseError "\"Text\"" "not match pattern: " "" d311_196 ["typToken"])
                                                   let "Text" = xx310_197
                                                   return ()
                                                   d313_198 <- get
                                                   xx312_199 <- StateT dvChars
                                                   case xx312_199 of
                                                       '.' -> return ()
                                                       _ -> gets dvPos >>= (throwError . ParseError "'.'" "not match pattern: " "" d313_198 ["dvChars"])
                                                   let '.' = xx312_199
                                                   return ()
                                                   _ <- StateT spaces
                                                   return ()
                                                   d317_200 <- get
                                                   xx316_201 <- StateT typToken
                                                   case xx316_201 of
                                                       "Papillon" -> return ()
                                                       _ -> gets dvPos >>= (throwError . ParseError "\"Papillon\"" "not match pattern: " "" d317_200 ["typToken"])
                                                   let "Papillon" = xx316_201
                                                   return ()
                                                   ddd318_202 <- get
                                                   do err <- ((do d320_203 <- get
                                                                  xx319_204 <- StateT dvChars
                                                                  case xx319_204 of
                                                                      '.' -> return ()
                                                                      _ -> gets dvPos >>= (throwError . ParseError "'.'" "not match pattern: " "" d320_203 ["dvChars"])
                                                                  let '.' = xx319_204
                                                                  return ()) >> return False) `catchError` const (return True)
                                                      unless err (gets dvPos >>= (throwError . ParseError ('!' : "'.':") "not match: " "" ddd318_202 ["dvChars"]))
                                                   put ddd318_202
                                                   return ()]
                varTokenP = foldl1 mplus [do v <- StateT variable
                                             _ <- StateT spaces
                                             return ()
                                             return v]
                typTokenP = foldl1 mplus [do t <- StateT typ
                                             _ <- StateT spaces
                                             return ()
                                             return t]
                papP = foldl1 mplus [do d330_205 <- get
                                        xx329_206 <- StateT dvChars
                                        case xx329_206 of
                                            '\n' -> return ()
                                            _ -> gets dvPos >>= (throwError . ParseError "'\\n'" "not match pattern: " "" d330_205 ["dvChars"])
                                        let '\n' = xx329_206
                                        return ()
                                        d332_207 <- get
                                        xx331_208 <- StateT dvChars
                                        case xx331_208 of
                                            '[' -> return ()
                                            _ -> gets dvPos >>= (throwError . ParseError "'['" "not match pattern: " "" d332_207 ["dvChars"])
                                        let '[' = xx331_208
                                        return ()
                                        d334_209 <- get
                                        xx333_210 <- StateT dvChars
                                        case xx333_210 of
                                            'p' -> return ()
                                            _ -> gets dvPos >>= (throwError . ParseError "'p'" "not match pattern: " "" d334_209 ["dvChars"])
                                        let 'p' = xx333_210
                                        return ()
                                        d336_211 <- get
                                        xx335_212 <- StateT dvChars
                                        case xx335_212 of
                                            'a' -> return ()
                                            _ -> gets dvPos >>= (throwError . ParseError "'a'" "not match pattern: " "" d336_211 ["dvChars"])
                                        let 'a' = xx335_212
                                        return ()
                                        d338_213 <- get
                                        xx337_214 <- StateT dvChars
                                        case xx337_214 of
                                            'p' -> return ()
                                            _ -> gets dvPos >>= (throwError . ParseError "'p'" "not match pattern: " "" d338_213 ["dvChars"])
                                        let 'p' = xx337_214
                                        return ()
                                        d340_215 <- get
                                        xx339_216 <- StateT dvChars
                                        case xx339_216 of
                                            'i' -> return ()
                                            _ -> gets dvPos >>= (throwError . ParseError "'i'" "not match pattern: " "" d340_215 ["dvChars"])
                                        let 'i' = xx339_216
                                        return ()
                                        d342_217 <- get
                                        xx341_218 <- StateT dvChars
                                        case xx341_218 of
                                            'l' -> return ()
                                            _ -> gets dvPos >>= (throwError . ParseError "'l'" "not match pattern: " "" d342_217 ["dvChars"])
                                        let 'l' = xx341_218
                                        return ()
                                        d344_219 <- get
                                        xx343_220 <- StateT dvChars
                                        case xx343_220 of
                                            'l' -> return ()
                                            _ -> gets dvPos >>= (throwError . ParseError "'l'" "not match pattern: " "" d344_219 ["dvChars"])
                                        let 'l' = xx343_220
                                        return ()
                                        d346_221 <- get
                                        xx345_222 <- StateT dvChars
                                        case xx345_222 of
                                            'o' -> return ()
                                            _ -> gets dvPos >>= (throwError . ParseError "'o'" "not match pattern: " "" d346_221 ["dvChars"])
                                        let 'o' = xx345_222
                                        return ()
                                        d348_223 <- get
                                        xx347_224 <- StateT dvChars
                                        case xx347_224 of
                                            'n' -> return ()
                                            _ -> gets dvPos >>= (throwError . ParseError "'n'" "not match pattern: " "" d348_223 ["dvChars"])
                                        let 'n' = xx347_224
                                        return ()
                                        d350_225 <- get
                                        xx349_226 <- StateT dvChars
                                        case xx349_226 of
                                            '|' -> return ()
                                            _ -> gets dvPos >>= (throwError . ParseError "'|'" "not match pattern: " "" d350_225 ["dvChars"])
                                        let '|' = xx349_226
                                        return ()
                                        d352_227 <- get
                                        xx351_228 <- StateT dvChars
                                        case xx351_228 of
                                            '\n' -> return ()
                                            _ -> gets dvPos >>= (throwError . ParseError "'\\n'" "not match pattern: " "" d352_227 ["dvChars"])
                                        let '\n' = xx351_228
                                        return ()
                                        return ()]
                pegP = foldl1 mplus [do _ <- StateT spaces
                                        return ()
                                        s <- StateT sourceType
                                        p <- StateT peg_
                                        return (mkTTPeg s p),
                                     do p <- StateT peg_
                                        return (mkTTPeg tString p)]
                sourceTypeP = foldl1 mplus [do d362_229 <- get
                                               xx361_230 <- StateT varToken
                                               case xx361_230 of
                                                   "source" -> return ()
                                                   _ -> gets dvPos >>= (throwError . ParseError "\"source\"" "not match pattern: " "" d362_229 ["varToken"])
                                               let "source" = xx361_230
                                               return ()
                                               d364_231 <- get
                                               xx363_232 <- StateT dvChars
                                               case xx363_232 of
                                                   ':' -> return ()
                                                   _ -> gets dvPos >>= (throwError . ParseError "':'" "not match pattern: " "" d364_231 ["dvChars"])
                                               let ':' = xx363_232
                                               return ()
                                               _ <- StateT spaces
                                               return ()
                                               v <- StateT typToken
                                               return v]
                peg_P = foldl1 mplus [do _ <- StateT spaces
                                         return ()
                                         d <- StateT definition
                                         p <- StateT peg_
                                         return (cons d p),
                                      return emp]
                definitionP = foldl1 mplus [do v <- StateT variable
                                               _ <- StateT spaces
                                               return ()
                                               d380_233 <- get
                                               xx379_234 <- StateT dvChars
                                               case xx379_234 of
                                                   ':' -> return ()
                                                   _ -> gets dvPos >>= (throwError . ParseError "':'" "not match pattern: " "" d380_233 ["dvChars"])
                                               let ':' = xx379_234
                                               return ()
                                               d382_235 <- get
                                               xx381_236 <- StateT dvChars
                                               case xx381_236 of
                                                   ':' -> return ()
                                                   _ -> gets dvPos >>= (throwError . ParseError "':'" "not match pattern: " "" d382_235 ["dvChars"])
                                               let ':' = xx381_236
                                               return ()
                                               _ <- StateT spaces
                                               return ()
                                               t <- StateT hsTypeArr
                                               _ <- StateT spaces
                                               return ()
                                               d390_237 <- get
                                               xx389_238 <- StateT dvChars
                                               case xx389_238 of
                                                   '=' -> return ()
                                                   _ -> gets dvPos >>= (throwError . ParseError "'='" "not match pattern: " "" d390_237 ["dvChars"])
                                               let '=' = xx389_238
                                               return ()
                                               _ <- StateT spaces
                                               return ()
                                               sel <- StateT selection
                                               _ <- StateT spaces
                                               return ()
                                               d398_239 <- get
                                               xx397_240 <- StateT dvChars
                                               case xx397_240 of
                                                   ';' -> return ()
                                                   _ -> gets dvPos >>= (throwError . ParseError "';'" "not match pattern: " "" d398_239 ["dvChars"])
                                               let ';' = xx397_240
                                               return ()
                                               return (mkDef v t sel)]
                selectionP = foldl1 mplus [do ex <- StateT expressionHs
                                              _ <- StateT spaces
                                              return ()
                                              d404_241 <- get
                                              xx403_242 <- StateT dvChars
                                              case xx403_242 of
                                                  '/' -> return ()
                                                  _ -> gets dvPos >>= (throwError . ParseError "'/'" "not match pattern: " "" d404_241 ["dvChars"])
                                              let '/' = xx403_242
                                              return ()
                                              _ <- StateT spaces
                                              return ()
                                              sel <- StateT selection
                                              return (cons ex sel),
                                           do ex <- StateT expressionHs
                                              return (cons ex emp)]
                expressionHsP = foldl1 mplus [do e <- StateT expression
                                                 _ <- StateT spaces
                                                 return ()
                                                 d416_243 <- get
                                                 xx415_244 <- StateT dvChars
                                                 case xx415_244 of
                                                     '{' -> return ()
                                                     _ -> gets dvPos >>= (throwError . ParseError "'{'" "not match pattern: " "" d416_243 ["dvChars"])
                                                 let '{' = xx415_244
                                                 return ()
                                                 _ <- StateT spaces
                                                 return ()
                                                 h <- StateT hsExpLam
                                                 _ <- StateT spaces
                                                 return ()
                                                 d424_245 <- get
                                                 xx423_246 <- StateT dvChars
                                                 case xx423_246 of
                                                     '}' -> return ()
                                                     _ -> gets dvPos >>= (throwError . ParseError "'}'" "not match pattern: " "" d424_245 ["dvChars"])
                                                 let '}' = xx423_246
                                                 return ()
                                                 return (mkExpressionHs e h)]
                expressionP = foldl1 mplus [do l <- StateT nameLeaf_
                                               _ <- StateT spaces
                                               return ()
                                               e <- StateT expression
                                               return (cons l e),
                                            return emp]
                nameLeaf_P = foldl1 mplus [do d432_247 <- get
                                              xx431_248 <- StateT dvChars
                                              case xx431_248 of
                                                  '!' -> return ()
                                                  _ -> gets dvPos >>= (throwError . ParseError "'!'" "not match pattern: " "" d432_247 ["dvChars"])
                                              let '!' = xx431_248
                                              return ()
                                              nl <- StateT nameLeafNoCom
                                              _ <- StateT spaces
                                              return ()
                                              com <- papOptional (StateT comForErr)
                                              return (NotAfter nl $ maybe "" id com),
                                           do d440_249 <- get
                                              xx439_250 <- StateT dvChars
                                              let c = xx439_250
                                              unless (isAmp c) (gets dvPos >>= (throwError . ParseError "isAmp c" "not match: " "" d440_249 ["dvChars"]))
                                              nl <- StateT nameLeaf
                                              return (After nl),
                                           do nl <- StateT nameLeaf
                                              return (Here nl)]
                nameLeafP = foldl1 mplus [do n <- StateT pat1
                                             _ <- StateT spaces
                                             return ()
                                             com <- papOptional (StateT comForErr)
                                             d452_251 <- get
                                             xx451_252 <- StateT dvChars
                                             case xx451_252 of
                                                 ':' -> return ()
                                                 _ -> gets dvPos >>= (throwError . ParseError "':'" "not match pattern: " "" d452_251 ["dvChars"])
                                             let ':' = xx451_252
                                             return ()
                                             (rf, p) <- StateT leaf
                                             return (NameLeaf (n, maybe "" id com) rf p),
                                          do n <- StateT pat1
                                             _ <- StateT spaces
                                             return ()
                                             com <- papOptional (StateT comForErr)
                                             return (NameLeaf (n,
                                                               maybe "" id com) FromToken Nothing)]
                nameLeafNoComP = foldl1 mplus [do n <- StateT pat1
                                                  _ <- StateT spaces
                                                  return ()
                                                  com <- papOptional (StateT comForErr)
                                                  d468_253 <- get
                                                  xx467_254 <- StateT dvChars
                                                  case xx467_254 of
                                                      ':' -> return ()
                                                      _ -> gets dvPos >>= (throwError . ParseError "':'" "not match pattern: " "" d468_253 ["dvChars"])
                                                  let ':' = xx467_254
                                                  return ()
                                                  (rf, p) <- StateT leaf
                                                  return (NameLeaf (n, maybe "" id com) rf p),
                                               do n <- StateT pat1
                                                  _ <- StateT spaces
                                                  return ()
                                                  return (NameLeaf (n, "") FromToken Nothing)]
                comForErrP = foldl1 mplus [do d476_255 <- get
                                              xx475_256 <- StateT dvChars
                                              case xx475_256 of
                                                  '{' -> return ()
                                                  _ -> gets dvPos >>= (throwError . ParseError "'{'" "not match pattern: " "" d476_255 ["dvChars"])
                                              let '{' = xx475_256
                                              return ()
                                              d478_257 <- get
                                              xx477_258 <- StateT dvChars
                                              case xx477_258 of
                                                  '-' -> return ()
                                                  _ -> gets dvPos >>= (throwError . ParseError "'-'" "not match pattern: " "" d478_257 ["dvChars"])
                                              let '-' = xx477_258
                                              return ()
                                              d480_259 <- get
                                              xx479_260 <- StateT dvChars
                                              case xx479_260 of
                                                  '#' -> return ()
                                                  _ -> gets dvPos >>= (throwError . ParseError "'#'" "not match pattern: " "" d480_259 ["dvChars"])
                                              let '#' = xx479_260
                                              return ()
                                              _ <- StateT spaces
                                              return ()
                                              d484_261 <- get
                                              xx483_262 <- StateT dvChars
                                              case xx483_262 of
                                                  '"' -> return ()
                                                  _ -> gets dvPos >>= (throwError . ParseError "'\"'" "not match pattern: " "" d484_261 ["dvChars"])
                                              let '"' = xx483_262
                                              return ()
                                              s <- StateT stringLit
                                              d488_263 <- get
                                              xx487_264 <- StateT dvChars
                                              case xx487_264 of
                                                  '"' -> return ()
                                                  _ -> gets dvPos >>= (throwError . ParseError "'\"'" "not match pattern: " "" d488_263 ["dvChars"])
                                              let '"' = xx487_264
                                              return ()
                                              _ <- StateT spaces
                                              return ()
                                              d492_265 <- get
                                              xx491_266 <- StateT dvChars
                                              case xx491_266 of
                                                  '#' -> return ()
                                                  _ -> gets dvPos >>= (throwError . ParseError "'#'" "not match pattern: " "" d492_265 ["dvChars"])
                                              let '#' = xx491_266
                                              return ()
                                              d494_267 <- get
                                              xx493_268 <- StateT dvChars
                                              case xx493_268 of
                                                  '-' -> return ()
                                                  _ -> gets dvPos >>= (throwError . ParseError "'-'" "not match pattern: " "" d494_267 ["dvChars"])
                                              let '-' = xx493_268
                                              return ()
                                              d496_269 <- get
                                              xx495_270 <- StateT dvChars
                                              case xx495_270 of
                                                  '}' -> return ()
                                                  _ -> gets dvPos >>= (throwError . ParseError "'}'" "not match pattern: " "" d496_269 ["dvChars"])
                                              let '}' = xx495_270
                                              return ()
                                              _ <- StateT spaces
                                              return ()
                                              return s]
                leafP = foldl1 mplus [do rf <- StateT readFromLs
                                         t <- StateT test
                                         return (rf, Just t),
                                      do rf <- StateT readFromLs
                                         return (rf, Nothing),
                                      do t <- StateT test
                                         return (FromToken, Just t)]
                patOpP = foldl1 mplus [do p <- StateT pat
                                          o <- StateT opConName
                                          po <- StateT patOp
                                          return (uInfixP p o po),
                                       do p <- StateT pat
                                          _ <- StateT spaces
                                          return ()
                                          d518_271 <- get
                                          xx517_272 <- StateT dvChars
                                          let q = xx517_272
                                          unless (isBQ q) (gets dvPos >>= (throwError . ParseError "isBQ q" "not match: " "" d518_271 ["dvChars"]))
                                          t <- StateT typ
                                          d522_273 <- get
                                          xx521_274 <- StateT dvChars
                                          let q_ = xx521_274
                                          unless (isBQ q_) (gets dvPos >>= (throwError . ParseError "isBQ q_" "not match: " "" d522_273 ["dvChars"]))
                                          _ <- StateT spaces
                                          return ()
                                          po <- StateT patOp
                                          return (uInfixP p (mkName t) po),
                                       do p <- StateT pat
                                          return p]
                patP = foldl1 mplus [do t <- StateT typ
                                        _ <- StateT spaces
                                        return ()
                                        ps <- StateT pats
                                        return (conToPatQ t ps),
                                     do d536_275 <- get
                                        xx535_276 <- StateT dvChars
                                        case xx535_276 of
                                            '(' -> return ()
                                            _ -> gets dvPos >>= (throwError . ParseError "'('" "not match pattern: " "" d536_275 ["dvChars"])
                                        let '(' = xx535_276
                                        return ()
                                        o <- StateT opConName
                                        d540_277 <- get
                                        xx539_278 <- StateT dvChars
                                        case xx539_278 of
                                            ')' -> return ()
                                            _ -> gets dvPos >>= (throwError . ParseError "')'" "not match pattern: " "" d540_277 ["dvChars"])
                                        let ')' = xx539_278
                                        return ()
                                        _ <- StateT spaces
                                        return ()
                                        ps <- StateT pats
                                        return (conP o ps),
                                     do p <- StateT pat1
                                        return p]
                pat1P = foldl1 mplus [do t <- StateT typ
                                         return (conToPatQ t emp),
                                      do d550_279 <- get
                                         xx549_280 <- StateT variable
                                         case xx549_280 of
                                             "_" -> return ()
                                             _ -> gets dvPos >>= (throwError . ParseError "\"_\"" "not match pattern: " "" d550_279 ["variable"])
                                         let "_" = xx549_280
                                         return ()
                                         return wildP,
                                      do n <- StateT variable
                                         return (strToPatQ n),
                                      do i <- StateT integer
                                         return (litP (integerL i)),
                                      do d556_281 <- get
                                         xx555_282 <- StateT dvChars
                                         case xx555_282 of
                                             '-' -> return ()
                                             _ -> gets dvPos >>= (throwError . ParseError "'-'" "not match pattern: " "" d556_281 ["dvChars"])
                                         let '-' = xx555_282
                                         return ()
                                         _ <- StateT spaces
                                         return ()
                                         i <- StateT integer
                                         return (litP (integerL $ negate i)),
                                      do d562_283 <- get
                                         xx561_284 <- StateT dvChars
                                         case xx561_284 of
                                             '\'' -> return ()
                                             _ -> gets dvPos >>= (throwError . ParseError "'\\''" "not match pattern: " "" d562_283 ["dvChars"])
                                         let '\'' = xx561_284
                                         return ()
                                         c <- StateT charLit
                                         d566_285 <- get
                                         xx565_286 <- StateT dvChars
                                         case xx565_286 of
                                             '\'' -> return ()
                                             _ -> gets dvPos >>= (throwError . ParseError "'\\''" "not match pattern: " "" d566_285 ["dvChars"])
                                         let '\'' = xx565_286
                                         return ()
                                         return (charP c),
                                      do d568_287 <- get
                                         xx567_288 <- StateT dvChars
                                         case xx567_288 of
                                             '"' -> return ()
                                             _ -> gets dvPos >>= (throwError . ParseError "'\"'" "not match pattern: " "" d568_287 ["dvChars"])
                                         let '"' = xx567_288
                                         return ()
                                         s <- StateT stringLit
                                         d572_289 <- get
                                         xx571_290 <- StateT dvChars
                                         case xx571_290 of
                                             '"' -> return ()
                                             _ -> gets dvPos >>= (throwError . ParseError "'\"'" "not match pattern: " "" d572_289 ["dvChars"])
                                         let '"' = xx571_290
                                         return ()
                                         return (stringP s),
                                      do d574_291 <- get
                                         xx573_292 <- StateT dvChars
                                         case xx573_292 of
                                             '(' -> return ()
                                             _ -> gets dvPos >>= (throwError . ParseError "'('" "not match pattern: " "" d574_291 ["dvChars"])
                                         let '(' = xx573_292
                                         return ()
                                         p <- StateT patList
                                         d578_293 <- get
                                         xx577_294 <- StateT dvChars
                                         case xx577_294 of
                                             ')' -> return ()
                                             _ -> gets dvPos >>= (throwError . ParseError "')'" "not match pattern: " "" d578_293 ["dvChars"])
                                         let ')' = xx577_294
                                         return ()
                                         return (tupP p),
                                      do d580_295 <- get
                                         xx579_296 <- StateT dvChars
                                         case xx579_296 of
                                             '[' -> return ()
                                             _ -> gets dvPos >>= (throwError . ParseError "'['" "not match pattern: " "" d580_295 ["dvChars"])
                                         let '[' = xx579_296
                                         return ()
                                         p <- StateT patList
                                         d584_297 <- get
                                         xx583_298 <- StateT dvChars
                                         case xx583_298 of
                                             ']' -> return ()
                                             _ -> gets dvPos >>= (throwError . ParseError "']'" "not match pattern: " "" d584_297 ["dvChars"])
                                         let ']' = xx583_298
                                         return ()
                                         return (listP p)]
                patListP = foldl1 mplus [do p <- StateT patOp
                                            _ <- StateT spaces
                                            return ()
                                            d590_299 <- get
                                            xx589_300 <- StateT dvChars
                                            case xx589_300 of
                                                ',' -> return ()
                                                _ -> gets dvPos >>= (throwError . ParseError "','" "not match pattern: " "" d590_299 ["dvChars"])
                                            let ',' = xx589_300
                                            return ()
                                            _ <- StateT spaces
                                            return ()
                                            ps <- StateT patList
                                            return (p : ps),
                                         do p <- StateT patOp
                                            return [p],
                                         return []]
                opConNameP = foldl1 mplus [do d598_301 <- get
                                              xx597_302 <- StateT dvChars
                                              case xx597_302 of
                                                  ':' -> return ()
                                                  _ -> gets dvPos >>= (throwError . ParseError "':'" "not match pattern: " "" d598_301 ["dvChars"])
                                              let ':' = xx597_302
                                              return ()
                                              ot <- StateT opTail
                                              return (mkName $ colon : ot)]
                charLitP = foldl1 mplus [do d602_303 <- get
                                            xx601_304 <- StateT dvChars
                                            let c = xx601_304
                                            unless (isAlphaNumOt c) (gets dvPos >>= (throwError . ParseError "isAlphaNumOt c" "not match: " "" d602_303 ["dvChars"]))
                                            return c,
                                         do d604_305 <- get
                                            xx603_306 <- StateT dvChars
                                            case xx603_306 of
                                                '\\' -> return ()
                                                _ -> gets dvPos >>= (throwError . ParseError "'\\\\'" "not match pattern: " "" d604_305 ["dvChars"])
                                            let '\\' = xx603_306
                                            return ()
                                            c <- StateT escapeC
                                            return c]
                stringLitP = foldl1 mplus [do d608_307 <- get
                                              xx607_308 <- StateT dvChars
                                              let c = xx607_308
                                              unless (isStrLitC c) (gets dvPos >>= (throwError . ParseError "isStrLitC c" "not match: " "" d608_307 ["dvChars"]))
                                              s <- StateT stringLit
                                              return (cons c s),
                                           do d612_309 <- get
                                              xx611_310 <- StateT dvChars
                                              case xx611_310 of
                                                  '\\' -> return ()
                                                  _ -> gets dvPos >>= (throwError . ParseError "'\\\\'" "not match pattern: " "" d612_309 ["dvChars"])
                                              let '\\' = xx611_310
                                              return ()
                                              c <- StateT escapeC
                                              s <- StateT stringLit
                                              return (c : s),
                                           return emp]
                escapeCP = foldl1 mplus [do d618_311 <- get
                                            xx617_312 <- StateT dvChars
                                            case xx617_312 of
                                                '"' -> return ()
                                                _ -> gets dvPos >>= (throwError . ParseError "'\"'" "not match pattern: " "" d618_311 ["dvChars"])
                                            let '"' = xx617_312
                                            return ()
                                            return '"',
                                         do d620_313 <- get
                                            xx619_314 <- StateT dvChars
                                            case xx619_314 of
                                                '\'' -> return ()
                                                _ -> gets dvPos >>= (throwError . ParseError "'\\''" "not match pattern: " "" d620_313 ["dvChars"])
                                            let '\'' = xx619_314
                                            return ()
                                            return '\'',
                                         do d622_315 <- get
                                            xx621_316 <- StateT dvChars
                                            case xx621_316 of
                                                '\\' -> return ()
                                                _ -> gets dvPos >>= (throwError . ParseError "'\\\\'" "not match pattern: " "" d622_315 ["dvChars"])
                                            let '\\' = xx621_316
                                            return ()
                                            return '\\',
                                         do d624_317 <- get
                                            xx623_318 <- StateT dvChars
                                            case xx623_318 of
                                                'n' -> return ()
                                                _ -> gets dvPos >>= (throwError . ParseError "'n'" "not match pattern: " "" d624_317 ["dvChars"])
                                            let 'n' = xx623_318
                                            return ()
                                            return '\n',
                                         do d626_319 <- get
                                            xx625_320 <- StateT dvChars
                                            case xx625_320 of
                                                't' -> return ()
                                                _ -> gets dvPos >>= (throwError . ParseError "'t'" "not match pattern: " "" d626_319 ["dvChars"])
                                            let 't' = xx625_320
                                            return ()
                                            return tab]
                patsP = foldl1 mplus [do p <- StateT pat
                                         _ <- StateT spaces
                                         return ()
                                         ps <- StateT pats
                                         return (cons p ps),
                                      return emp]
                readFromLsP = foldl1 mplus [do rf <- StateT readFrom
                                               d636_321 <- get
                                               xx635_322 <- StateT dvChars
                                               case xx635_322 of
                                                   '*' -> return ()
                                                   _ -> gets dvPos >>= (throwError . ParseError "'*'" "not match pattern: " "" d636_321 ["dvChars"])
                                               let '*' = xx635_322
                                               return ()
                                               return (FromList rf),
                                            do rf <- StateT readFrom
                                               d640_323 <- get
                                               xx639_324 <- StateT dvChars
                                               case xx639_324 of
                                                   '+' -> return ()
                                                   _ -> gets dvPos >>= (throwError . ParseError "'+'" "not match pattern: " "" d640_323 ["dvChars"])
                                               let '+' = xx639_324
                                               return ()
                                               return (FromList1 rf),
                                            do rf <- StateT readFrom
                                               d644_325 <- get
                                               xx643_326 <- StateT dvChars
                                               case xx643_326 of
                                                   '?' -> return ()
                                                   _ -> gets dvPos >>= (throwError . ParseError "'?'" "not match pattern: " "" d644_325 ["dvChars"])
                                               let '?' = xx643_326
                                               return ()
                                               return (FromOptional rf),
                                            do rf <- StateT readFrom
                                               return rf]
                readFromP = foldl1 mplus [do v <- StateT variable
                                             return (FromVariable v),
                                          do d650_327 <- get
                                             xx649_328 <- StateT dvChars
                                             case xx649_328 of
                                                 '(' -> return ()
                                                 _ -> gets dvPos >>= (throwError . ParseError "'('" "not match pattern: " "" d650_327 ["dvChars"])
                                             let '(' = xx649_328
                                             return ()
                                             s <- StateT selection
                                             d654_329 <- get
                                             xx653_330 <- StateT dvChars
                                             case xx653_330 of
                                                 ')' -> return ()
                                                 _ -> gets dvPos >>= (throwError . ParseError "')'" "not match pattern: " "" d654_329 ["dvChars"])
                                             let ')' = xx653_330
                                             return ()
                                             return (FromSelection s)]
                testP = foldl1 mplus [do d656_331 <- get
                                         xx655_332 <- StateT dvChars
                                         case xx655_332 of
                                             '[' -> return ()
                                             _ -> gets dvPos >>= (throwError . ParseError "'['" "not match pattern: " "" d656_331 ["dvChars"])
                                         let '[' = xx655_332
                                         return ()
                                         h <- StateT hsExpLam
                                         _ <- StateT spaces
                                         return ()
                                         com <- papOptional (StateT comForErr)
                                         d664_333 <- get
                                         xx663_334 <- StateT dvChars
                                         case xx663_334 of
                                             ']' -> return ()
                                             _ -> gets dvPos >>= (throwError . ParseError "']'" "not match pattern: " "" d664_333 ["dvChars"])
                                         let ']' = xx663_334
                                         return ()
                                         return (h, maybe "" id com)]
                hsExpLamP = foldl1 mplus [do d666_335 <- get
                                             xx665_336 <- StateT dvChars
                                             case xx665_336 of
                                                 '\\' -> return ()
                                                 _ -> gets dvPos >>= (throwError . ParseError "'\\\\'" "not match pattern: " "" d666_335 ["dvChars"])
                                             let '\\' = xx665_336
                                             return ()
                                             _ <- StateT spaces
                                             return ()
                                             ps <- StateT pats
                                             _ <- StateT spaces
                                             return ()
                                             d674_337 <- get
                                             xx673_338 <- StateT dvChars
                                             case xx673_338 of
                                                 '-' -> return ()
                                                 _ -> gets dvPos >>= (throwError . ParseError "'-'" "not match pattern: " "" d674_337 ["dvChars"])
                                             let '-' = xx673_338
                                             return ()
                                             d676_339 <- get
                                             xx675_340 <- StateT dvChars
                                             let c = xx675_340
                                             unless (isGt c) (gets dvPos >>= (throwError . ParseError "isGt c" "not match: " "" d676_339 ["dvChars"]))
                                             _ <- StateT spaces
                                             return ()
                                             e <- StateT hsExpTyp
                                             return (lamE ps e),
                                          do e <- StateT hsExpTyp
                                             return e]
                hsExpTypP = foldl1 mplus [do eo <- StateT hsExpOp
                                             d686_341 <- get
                                             xx685_342 <- StateT dvChars
                                             case xx685_342 of
                                                 ':' -> return ()
                                                 _ -> gets dvPos >>= (throwError . ParseError "':'" "not match pattern: " "" d686_341 ["dvChars"])
                                             let ':' = xx685_342
                                             return ()
                                             d688_343 <- get
                                             xx687_344 <- StateT dvChars
                                             case xx687_344 of
                                                 ':' -> return ()
                                                 _ -> gets dvPos >>= (throwError . ParseError "':'" "not match pattern: " "" d688_343 ["dvChars"])
                                             let ':' = xx687_344
                                             return ()
                                             _ <- StateT spaces
                                             return ()
                                             t <- StateT hsTypeArr
                                             return (sigE eo t),
                                          do eo <- StateT hsExpOp
                                             return eo]
                hsExpOpP = foldl1 mplus [do l <- StateT hsExp
                                            _ <- StateT spaces
                                            return ()
                                            o <- StateT hsOp
                                            _ <- StateT spaces
                                            return ()
                                            r <- StateT hsExpOp
                                            return (uInfixE (getEx l) o r),
                                         do e <- StateT hsExp
                                            return (getEx e)]
                hsOpP = foldl1 mplus [do d708_345 <- get
                                         xx707_346 <- StateT dvChars
                                         let c = xx707_346
                                         unless (isOpHeadChar c) (gets dvPos >>= (throwError . ParseError "isOpHeadChar c" "not match: " "" d708_345 ["dvChars"]))
                                         o <- StateT opTail
                                         return (varE (mkName (cons c o))),
                                      do d712_347 <- get
                                         xx711_348 <- StateT dvChars
                                         case xx711_348 of
                                             ':' -> return ()
                                             _ -> gets dvPos >>= (throwError . ParseError "':'" "not match pattern: " "" d712_347 ["dvChars"])
                                         let ':' = xx711_348
                                         return ()
                                         ddd713_349 <- get
                                         do err <- ((do d715_350 <- get
                                                        xx714_351 <- StateT dvChars
                                                        case xx714_351 of
                                                            ':' -> return ()
                                                            _ -> gets dvPos >>= (throwError . ParseError "':'" "not match pattern: " "" d715_350 ["dvChars"])
                                                        let ':' = xx714_351
                                                        return ()) >> return False) `catchError` const (return True)
                                            unless err (gets dvPos >>= (throwError . ParseError ('!' : "':':") "not match: " "" ddd713_349 ["dvChars"]))
                                         put ddd713_349
                                         o <- StateT opTail
                                         return (conE (mkName (':' : o))),
                                      do d719_352 <- get
                                         xx718_353 <- StateT dvChars
                                         let c = xx718_353
                                         unless (isBQ c) (gets dvPos >>= (throwError . ParseError "isBQ c" "not match: " "" d719_352 ["dvChars"]))
                                         v <- StateT variable
                                         d723_354 <- get
                                         xx722_355 <- StateT dvChars
                                         let c_ = xx722_355
                                         unless (isBQ c_) (gets dvPos >>= (throwError . ParseError "isBQ c_" "not match: " "" d723_354 ["dvChars"]))
                                         return (varE (mkName v)),
                                      do d725_356 <- get
                                         xx724_357 <- StateT dvChars
                                         let c = xx724_357
                                         unless (isBQ c) (gets dvPos >>= (throwError . ParseError "isBQ c" "not match: " "" d725_356 ["dvChars"]))
                                         t <- StateT typ
                                         d729_358 <- get
                                         xx728_359 <- StateT dvChars
                                         let c_ = xx728_359
                                         unless (isBQ c_) (gets dvPos >>= (throwError . ParseError "isBQ c_" "not match: " "" d729_358 ["dvChars"]))
                                         return (conE (mkName t))]
                opTailP = foldl1 mplus [do d731_360 <- get
                                           xx730_361 <- StateT dvChars
                                           let c = xx730_361
                                           unless (isOpTailChar c) (gets dvPos >>= (throwError . ParseError "isOpTailChar c" "not match: " "" d731_360 ["dvChars"]))
                                           s <- StateT opTail
                                           return (cons c s),
                                        return emp]
                hsExpP = foldl1 mplus [do e <- StateT hsExp1
                                          _ <- StateT spaces
                                          return ()
                                          h <- StateT hsExp
                                          return (applyExR e h),
                                       do e <- StateT hsExp1
                                          return (toEx e)]
                hsExp1P = foldl1 mplus [do d743_362 <- get
                                           xx742_363 <- StateT dvChars
                                           case xx742_363 of
                                               '(' -> return ()
                                               _ -> gets dvPos >>= (throwError . ParseError "'('" "not match pattern: " "" d743_362 ["dvChars"])
                                           let '(' = xx742_363
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
                                           d759_364 <- get
                                           xx758_365 <- StateT dvChars
                                           case xx758_365 of
                                               ')' -> return ()
                                               _ -> gets dvPos >>= (throwError . ParseError "')'" "not match pattern: " "" d759_364 ["dvChars"])
                                           let ')' = xx758_365
                                           return ()
                                           return (infixE l o r),
                                        do d761_366 <- get
                                           xx760_367 <- StateT dvChars
                                           case xx760_367 of
                                               '(' -> return ()
                                               _ -> gets dvPos >>= (throwError . ParseError "'('" "not match pattern: " "" d761_366 ["dvChars"])
                                           let '(' = xx760_367
                                           return ()
                                           et <- StateT hsExpTpl
                                           d765_368 <- get
                                           xx764_369 <- StateT dvChars
                                           case xx764_369 of
                                               ')' -> return ()
                                               _ -> gets dvPos >>= (throwError . ParseError "')'" "not match pattern: " "" d765_368 ["dvChars"])
                                           let ')' = xx764_369
                                           return ()
                                           return (tupE et),
                                        do d767_370 <- get
                                           xx766_371 <- StateT dvChars
                                           case xx766_371 of
                                               '[' -> return ()
                                               _ -> gets dvPos >>= (throwError . ParseError "'['" "not match pattern: " "" d767_370 ["dvChars"])
                                           let '[' = xx766_371
                                           return ()
                                           et <- StateT hsExpTpl
                                           d771_372 <- get
                                           xx770_373 <- StateT dvChars
                                           case xx770_373 of
                                               ']' -> return ()
                                               _ -> gets dvPos >>= (throwError . ParseError "']'" "not match pattern: " "" d771_372 ["dvChars"])
                                           let ']' = xx770_373
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
                                        do d781_374 <- get
                                           xx780_375 <- StateT dvChars
                                           case xx780_375 of
                                               '\'' -> return ()
                                               _ -> gets dvPos >>= (throwError . ParseError "'\\''" "not match pattern: " "" d781_374 ["dvChars"])
                                           let '\'' = xx780_375
                                           return ()
                                           c <- StateT charLit
                                           d785_376 <- get
                                           xx784_377 <- StateT dvChars
                                           case xx784_377 of
                                               '\'' -> return ()
                                               _ -> gets dvPos >>= (throwError . ParseError "'\\''" "not match pattern: " "" d785_376 ["dvChars"])
                                           let '\'' = xx784_377
                                           return ()
                                           return (litE (charL c)),
                                        do d787_378 <- get
                                           xx786_379 <- StateT dvChars
                                           case xx786_379 of
                                               '"' -> return ()
                                               _ -> gets dvPos >>= (throwError . ParseError "'\"'" "not match pattern: " "" d787_378 ["dvChars"])
                                           let '"' = xx786_379
                                           return ()
                                           s <- StateT stringLit
                                           d791_380 <- get
                                           xx790_381 <- StateT dvChars
                                           case xx790_381 of
                                               '"' -> return ()
                                               _ -> gets dvPos >>= (throwError . ParseError "'\"'" "not match pattern: " "" d791_380 ["dvChars"])
                                           let '"' = xx790_381
                                           return ()
                                           return (litE (stringL s)),
                                        do d793_382 <- get
                                           xx792_383 <- StateT dvChars
                                           case xx792_383 of
                                               '-' -> return ()
                                               _ -> gets dvPos >>= (throwError . ParseError "'-'" "not match pattern: " "" d793_382 ["dvChars"])
                                           let '-' = xx792_383
                                           return ()
                                           _ <- StateT spaces
                                           return ()
                                           e <- StateT hsExp1
                                           return (appE (varE $ mkName "negate") e)]
                hsExpTplP = foldl1 mplus [do e <- StateT hsExpLam
                                             _ <- StateT spaces
                                             return ()
                                             d803_384 <- get
                                             xx802_385 <- StateT dvChars
                                             let c = xx802_385
                                             unless (isComma c) (gets dvPos >>= (throwError . ParseError "isComma c" "not match: " "" d803_384 ["dvChars"]))
                                             _ <- StateT spaces
                                             return ()
                                             et <- StateT hsExpTpl
                                             return (cons e et),
                                          do e <- StateT hsExpLam
                                             return (cons e emp),
                                          return emp]
                hsTypeArrP = foldl1 mplus [do l <- StateT hsType
                                              d813_386 <- get
                                              xx812_387 <- StateT dvChars
                                              case xx812_387 of
                                                  '-' -> return ()
                                                  _ -> gets dvPos >>= (throwError . ParseError "'-'" "not match pattern: " "" d813_386 ["dvChars"])
                                              let '-' = xx812_387
                                              return ()
                                              d815_388 <- get
                                              xx814_389 <- StateT dvChars
                                              let c = xx814_389
                                              unless (isGt c) (gets dvPos >>= (throwError . ParseError "isGt c" "not match: " "" d815_388 ["dvChars"]))
                                              _ <- StateT spaces
                                              return ()
                                              r <- StateT hsTypeArr
                                              return (appT (appT arrowT (getTyp l)) r),
                                           do t <- StateT hsType
                                              return (getTyp t)]
                hsTypeP = foldl1 mplus [do t <- StateT hsType1
                                           ts <- StateT hsType
                                           return (applyTyp (toTyp t) ts),
                                        do t <- StateT hsType1
                                           return (toTyp t)]
                hsType1P = foldl1 mplus [do d829_390 <- get
                                            xx828_391 <- StateT dvChars
                                            case xx828_391 of
                                                '[' -> return ()
                                                _ -> gets dvPos >>= (throwError . ParseError "'['" "not match pattern: " "" d829_390 ["dvChars"])
                                            let '[' = xx828_391
                                            return ()
                                            d831_392 <- get
                                            xx830_393 <- StateT dvChars
                                            case xx830_393 of
                                                ']' -> return ()
                                                _ -> gets dvPos >>= (throwError . ParseError "']'" "not match pattern: " "" d831_392 ["dvChars"])
                                            let ']' = xx830_393
                                            return ()
                                            _ <- StateT spaces
                                            return ()
                                            return listT,
                                         do d835_394 <- get
                                            xx834_395 <- StateT dvChars
                                            case xx834_395 of
                                                '[' -> return ()
                                                _ -> gets dvPos >>= (throwError . ParseError "'['" "not match pattern: " "" d835_394 ["dvChars"])
                                            let '[' = xx834_395
                                            return ()
                                            t <- StateT hsTypeArr
                                            d839_396 <- get
                                            xx838_397 <- StateT dvChars
                                            case xx838_397 of
                                                ']' -> return ()
                                                _ -> gets dvPos >>= (throwError . ParseError "']'" "not match pattern: " "" d839_396 ["dvChars"])
                                            let ']' = xx838_397
                                            return ()
                                            _ <- StateT spaces
                                            return ()
                                            return (appT listT t),
                                         do d843_398 <- get
                                            xx842_399 <- StateT dvChars
                                            case xx842_399 of
                                                '(' -> return ()
                                                _ -> gets dvPos >>= (throwError . ParseError "'('" "not match pattern: " "" d843_398 ["dvChars"])
                                            let '(' = xx842_399
                                            return ()
                                            _ <- StateT spaces
                                            return ()
                                            tt <- StateT hsTypeTpl
                                            d849_400 <- get
                                            xx848_401 <- StateT dvChars
                                            case xx848_401 of
                                                ')' -> return ()
                                                _ -> gets dvPos >>= (throwError . ParseError "')'" "not match pattern: " "" d849_400 ["dvChars"])
                                            let ')' = xx848_401
                                            return ()
                                            return (tupT tt),
                                         do t <- StateT typToken
                                            return (conT (mkName t)),
                                         do d853_402 <- get
                                            xx852_403 <- StateT dvChars
                                            case xx852_403 of
                                                '(' -> return ()
                                                _ -> gets dvPos >>= (throwError . ParseError "'('" "not match pattern: " "" d853_402 ["dvChars"])
                                            let '(' = xx852_403
                                            return ()
                                            d855_404 <- get
                                            xx854_405 <- StateT dvChars
                                            case xx854_405 of
                                                '-' -> return ()
                                                _ -> gets dvPos >>= (throwError . ParseError "'-'" "not match pattern: " "" d855_404 ["dvChars"])
                                            let '-' = xx854_405
                                            return ()
                                            d857_406 <- get
                                            xx856_407 <- StateT dvChars
                                            let c = xx856_407
                                            unless (isGt c) (gets dvPos >>= (throwError . ParseError "isGt c" "not match: " "" d857_406 ["dvChars"]))
                                            d859_408 <- get
                                            xx858_409 <- StateT dvChars
                                            case xx858_409 of
                                                ')' -> return ()
                                                _ -> gets dvPos >>= (throwError . ParseError "')'" "not match pattern: " "" d859_408 ["dvChars"])
                                            let ')' = xx858_409
                                            return ()
                                            _ <- StateT spaces
                                            return ()
                                            return arrowT]
                hsTypeTplP = foldl1 mplus [do t <- StateT hsTypeArr
                                              d865_410 <- get
                                              xx864_411 <- StateT dvChars
                                              let c = xx864_411
                                              unless (isComma c) (gets dvPos >>= (throwError . ParseError "isComma c" "not match: " "" d865_410 ["dvChars"]))
                                              _ <- StateT spaces
                                              return ()
                                              tt <- StateT hsTypeTpl
                                              return (cons t tt),
                                           do t <- StateT hsTypeArr
                                              return (cons t emp),
                                           return emp]
                typP = foldl1 mplus [do u <- StateT upper
                                        t <- StateT tvtail
                                        return (cons u t)]
                variableP = foldl1 mplus [do l <- StateT lower
                                             t <- StateT tvtail
                                             return (cons l t)]
                tvtailP = foldl1 mplus [do a <- StateT alpha
                                           t <- StateT tvtail
                                           return (cons a t),
                                        return emp]
                integerP = foldl1 mplus [do dh <- StateT digit
                                            ds <- list (foldl1 mplus [do d <- StateT digit
                                                                         return d])
                                            return (read (cons dh ds))]
                alphaP = foldl1 mplus [do u <- StateT upper
                                          return u,
                                       do l <- StateT lower
                                          return l,
                                       do d <- StateT digit
                                          return d,
                                       do d897_412 <- get
                                          xx896_413 <- StateT dvChars
                                          case xx896_413 of
                                              '\'' -> return ()
                                              _ -> gets dvPos >>= (throwError . ParseError "'\\''" "not match pattern: " "" d897_412 ["dvChars"])
                                          let '\'' = xx896_413
                                          return ()
                                          return '\'']
                upperP = foldl1 mplus [do d899_414 <- get
                                          xx898_415 <- StateT dvChars
                                          let u = xx898_415
                                          unless (isUpper u) (gets dvPos >>= (throwError . ParseError "isUpper u" "not match: " "" d899_414 ["dvChars"]))
                                          return u]
                lowerP = foldl1 mplus [do d901_416 <- get
                                          xx900_417 <- StateT dvChars
                                          let l = xx900_417
                                          unless (isLowerU l) (gets dvPos >>= (throwError . ParseError "isLowerU l" "not match: " "" d901_416 ["dvChars"]))
                                          return l]
                digitP = foldl1 mplus [do d903_418 <- get
                                          xx902_419 <- StateT dvChars
                                          let d = xx902_419
                                          unless (isDigit d) (gets dvPos >>= (throwError . ParseError "isDigit d" "not match: " "" d903_418 ["dvChars"]))
                                          return d]
                spacesP = foldl1 mplus [do _ <- StateT space
                                           return ()
                                           _ <- StateT spaces
                                           return ()
                                           return (),
                                        return ()]
                spaceP = foldl1 mplus [do d909_420 <- get
                                          xx908_421 <- StateT dvChars
                                          let s = xx908_421
                                          unless (isSpace s) (gets dvPos >>= (throwError . ParseError "isSpace s" "not match: " "" d909_420 ["dvChars"]))
                                          return (),
                                       do d911_422 <- get
                                          xx910_423 <- StateT dvChars
                                          case xx910_423 of
                                              '-' -> return ()
                                              _ -> gets dvPos >>= (throwError . ParseError "'-'" "not match pattern: " "" d911_422 ["dvChars"])
                                          let '-' = xx910_423
                                          return ()
                                          d913_424 <- get
                                          xx912_425 <- StateT dvChars
                                          case xx912_425 of
                                              '-' -> return ()
                                              _ -> gets dvPos >>= (throwError . ParseError "'-'" "not match pattern: " "" d913_424 ["dvChars"])
                                          let '-' = xx912_425
                                          return ()
                                          _ <- StateT notNLString
                                          return ()
                                          _ <- StateT newLine
                                          return ()
                                          return (),
                                       do _ <- StateT comment
                                          return ()
                                          return ()]
                notNLStringP = foldl1 mplus [do ddd920_426 <- get
                                                do err <- ((do _ <- StateT newLine
                                                               return ()) >> return False) `catchError` const (return True)
                                                   unless err (gets dvPos >>= (throwError . ParseError ('!' : "_:newLine") "not match: " "" ddd920_426 ["newLine"]))
                                                put ddd920_426
                                                c <- StateT dvChars
                                                s <- StateT notNLString
                                                return (cons c s),
                                             return emp]
                newLineP = foldl1 mplus [do d928_427 <- get
                                            xx927_428 <- StateT dvChars
                                            case xx927_428 of
                                                '\n' -> return ()
                                                _ -> gets dvPos >>= (throwError . ParseError "'\\n'" "not match pattern: " "" d928_427 ["dvChars"])
                                            let '\n' = xx927_428
                                            return ()
                                            return ()]
                commentP = foldl1 mplus [do d930_429 <- get
                                            xx929_430 <- StateT dvChars
                                            case xx929_430 of
                                                '{' -> return ()
                                                _ -> gets dvPos >>= (throwError . ParseError "'{'" "not match pattern: " "" d930_429 ["dvChars"])
                                            let '{' = xx929_430
                                            return ()
                                            d932_431 <- get
                                            xx931_432 <- StateT dvChars
                                            case xx931_432 of
                                                '-' -> return ()
                                                _ -> gets dvPos >>= (throwError . ParseError "'-'" "not match pattern: " "" d932_431 ["dvChars"])
                                            let '-' = xx931_432
                                            return ()
                                            ddd933_433 <- get
                                            do err <- ((do d935_434 <- get
                                                           xx934_435 <- StateT dvChars
                                                           case xx934_435 of
                                                               '#' -> return ()
                                                               _ -> gets dvPos >>= (throwError . ParseError "'#'" "not match pattern: " "" d935_434 ["dvChars"])
                                                           let '#' = xx934_435
                                                           return ()) >> return False) `catchError` const (return True)
                                               unless err (gets dvPos >>= (throwError . ParseError ('!' : "'#':") "not match: " "" ddd933_433 ["dvChars"]))
                                            put ddd933_433
                                            _ <- StateT comments
                                            return ()
                                            _ <- StateT comEnd
                                            return ()
                                            return ()]
                commentsP = foldl1 mplus [do _ <- StateT notComStr
                                             return ()
                                             _ <- StateT comment
                                             return ()
                                             _ <- StateT comments
                                             return ()
                                             return (),
                                          do _ <- StateT notComStr
                                             return ()
                                             return ()]
                notComStrP = foldl1 mplus [do ddd948_436 <- get
                                              do err <- ((do _ <- StateT comment
                                                             return ()) >> return False) `catchError` const (return True)
                                                 unless err (gets dvPos >>= (throwError . ParseError ('!' : "_:comment") "not match: " "" ddd948_436 ["comment"]))
                                              put ddd948_436
                                              ddd951_437 <- get
                                              do err <- ((do _ <- StateT comEnd
                                                             return ()) >> return False) `catchError` const (return True)
                                                 unless err (gets dvPos >>= (throwError . ParseError ('!' : "_:comEnd") "not match: " "" ddd951_437 ["comEnd"]))
                                              put ddd951_437
                                              _ <- StateT dvChars
                                              return ()
                                              _ <- StateT notComStr
                                              return ()
                                              return (),
                                           return ()]
                comEndP = foldl1 mplus [do d959_438 <- get
                                           xx958_439 <- StateT dvChars
                                           case xx958_439 of
                                               '-' -> return ()
                                               _ -> gets dvPos >>= (throwError . ParseError "'-'" "not match pattern: " "" d959_438 ["dvChars"])
                                           let '-' = xx958_439
                                           return ()
                                           d961_440 <- get
                                           xx960_441 <- StateT dvChars
                                           case xx960_441 of
                                               '}' -> return ()
                                               _ -> gets dvPos >>= (throwError . ParseError "'}'" "not match pattern: " "" d961_440 ["dvChars"])
                                           let '}' = xx960_441
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