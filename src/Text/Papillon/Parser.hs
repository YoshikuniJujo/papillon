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
    = Derivs {pegFile :: (Result PegFile),
              pragma :: (Result (Maybe String)),
              pragmaStr :: (Result String),
              pragmaItems :: (Result ([String])),
              delPragmas :: (Result ()),
              pragmaEnd :: (Result ()),
              moduleDec :: (Result (Maybe String)),
              moduleDecStr :: (Result String),
              whr :: (Result ()),
              preImpPap :: (Result String),
              prePeg :: (Result String),
              afterPeg :: (Result String),
              importPapillon :: (Result ()),
              varToken :: (Result String),
              typToken :: (Result String),
              pap :: (Result ()),
              peg :: (Result TTPeg),
              sourceType :: (Result String),
              peg_ :: (Result Peg),
              definition :: (Result Definition),
              selection :: (Result Selection),
              expressionHs :: (Result ExpressionHs),
              expression :: (Result Expression),
              nameLeaf_ :: (Result NameLeaf_),
              nameLeaf :: (Result NameLeaf),
              nameLeafNoCom :: (Result NameLeaf),
              comForErr :: (Result String),
              leaf :: (Result ((ReadFrom, (ExpQ, String)))),
              patOp :: (Result PatQ),
              pat :: (Result PatQ),
              pat1 :: (Result PatQ),
              patList :: (Result ([PatQ])),
              opConName :: (Result Name),
              charLit :: (Result Char),
              stringLit :: (Result String),
              escapeC :: (Result Char),
              pats :: (Result PatQs),
              readFromLs :: (Result ReadFrom),
              readFrom :: (Result ReadFrom),
              test :: (Result ((ExR, String))),
              hsExpLam :: (Result ExR),
              hsExpTyp :: (Result ExR),
              hsExpOp :: (Result ExR),
              hsOp :: (Result ExR),
              opTail :: (Result String),
              hsExp :: (Result Ex),
              hsExp1 :: (Result ExR),
              hsExpTpl :: (Result ExRL),
              hsTypeArr :: (Result TypeQ),
              hsType :: (Result Typ),
              hsType1 :: (Result TypeQ),
              hsTypeTpl :: (Result TypeQL),
              typ :: (Result String),
              variable :: (Result String),
              tvtail :: (Result String),
              integer :: (Result Integer),
              alpha :: (Result Char),
              upper :: (Result Char),
              lower :: (Result Char),
              digit :: (Result Char),
              spaces :: (Result ()),
              space :: (Result ()),
              notNLString :: (Result String),
              newLine :: (Result ()),
              comment :: (Result ()),
              comments :: (Result ()),
              notComStr :: (Result ()),
              comEnd :: (Result ()),
              dvChars :: (Result (Token String)),
              dvPos :: (Pos String)}
type Result v = Either (ParseError (Pos String)) ((v, Derivs))
type PackratM = StateT Derivs (Either (ParseError (Pos String)))
data ParseError pos
    = ParseError String String String Derivs ([String]) pos
instance Error (ParseError pos)
    where strMsg msg = ParseError "" msg "" undefined undefined undefined
dv_pragmaM :: PackratM (Maybe String)
dv_pragmaStrM :: PackratM String
dv_pragmaItemsM :: PackratM ([String])
dv_delPragmasM :: PackratM ()
dv_pragmaEndM :: PackratM ()
dv_moduleDecM :: PackratM (Maybe String)
dv_moduleDecStrM :: PackratM String
dv_whrM :: PackratM ()
dv_preImpPapM :: PackratM String
dv_prePegM :: PackratM String
dv_afterPegM :: PackratM String
dv_importPapillonM :: PackratM ()
dv_varTokenM :: PackratM String
dv_typTokenM :: PackratM String
dv_papM :: PackratM ()
dv_pegM :: PackratM TTPeg
dv_sourceTypeM :: PackratM String
dv_peg_M :: PackratM Peg
dv_definitionM :: PackratM Definition
dv_selectionM :: PackratM Selection
dv_expressionHsM :: PackratM ExpressionHs
dv_expressionM :: PackratM Expression
dv_nameLeaf_M :: PackratM NameLeaf_
dv_nameLeafM :: PackratM NameLeaf
dv_nameLeafNoComM :: PackratM NameLeaf
dv_comForErrM :: PackratM String
dv_leafM :: PackratM ((ReadFrom, (ExpQ, String)))
dv_patOpM :: PackratM PatQ
dv_patM :: PackratM PatQ
dv_pat1M :: PackratM PatQ
dv_patListM :: PackratM ([PatQ])
dv_opConNameM :: PackratM Name
dv_charLitM :: PackratM Char
dv_stringLitM :: PackratM String
dv_escapeCM :: PackratM Char
dv_patsM :: PackratM PatQs
dv_readFromLsM :: PackratM ReadFrom
dv_readFromM :: PackratM ReadFrom
dv_testM :: PackratM ((ExR, String))
dv_hsExpLamM :: PackratM ExR
dv_hsExpTypM :: PackratM ExR
dv_hsExpOpM :: PackratM ExR
dv_hsOpM :: PackratM ExR
dv_opTailM :: PackratM String
dv_hsExpM :: PackratM Ex
dv_hsExp1M :: PackratM ExR
dv_hsExpTplM :: PackratM ExRL
dv_hsTypeArrM :: PackratM TypeQ
dv_hsTypeM :: PackratM Typ
dv_hsType1M :: PackratM TypeQ
dv_hsTypeTplM :: PackratM TypeQL
dv_typM :: PackratM String
dv_variableM :: PackratM String
dv_tvtailM :: PackratM String
dv_integerM :: PackratM Integer
dv_alphaM :: PackratM Char
dv_upperM :: PackratM Char
dv_lowerM :: PackratM Char
dv_digitM :: PackratM Char
dv_spacesM :: PackratM ()
dv_spaceM :: PackratM ()
dv_notNLStringM :: PackratM String
dv_newLineM :: PackratM ()
dv_commentM :: PackratM ()
dv_commentsM :: PackratM ()
dv_notComStrM :: PackratM ()
dv_comEndM :: PackratM ()
dv_pragmaM = StateT pragma
dv_pragmaStrM = StateT pragmaStr
dv_pragmaItemsM = StateT pragmaItems
dv_delPragmasM = StateT delPragmas
dv_pragmaEndM = StateT pragmaEnd
dv_moduleDecM = StateT moduleDec
dv_moduleDecStrM = StateT moduleDecStr
dv_whrM = StateT whr
dv_preImpPapM = StateT preImpPap
dv_prePegM = StateT prePeg
dv_afterPegM = StateT afterPeg
dv_importPapillonM = StateT importPapillon
dv_varTokenM = StateT varToken
dv_typTokenM = StateT typToken
dv_papM = StateT pap
dv_pegM = StateT peg
dv_sourceTypeM = StateT sourceType
dv_peg_M = StateT peg_
dv_definitionM = StateT definition
dv_selectionM = StateT selection
dv_expressionHsM = StateT expressionHs
dv_expressionM = StateT expression
dv_nameLeaf_M = StateT nameLeaf_
dv_nameLeafM = StateT nameLeaf
dv_nameLeafNoComM = StateT nameLeafNoCom
dv_comForErrM = StateT comForErr
dv_leafM = StateT leaf
dv_patOpM = StateT patOp
dv_patM = StateT pat
dv_pat1M = StateT pat1
dv_patListM = StateT patList
dv_opConNameM = StateT opConName
dv_charLitM = StateT charLit
dv_stringLitM = StateT stringLit
dv_escapeCM = StateT escapeC
dv_patsM = StateT pats
dv_readFromLsM = StateT readFromLs
dv_readFromM = StateT readFrom
dv_testM = StateT test
dv_hsExpLamM = StateT hsExpLam
dv_hsExpTypM = StateT hsExpTyp
dv_hsExpOpM = StateT hsExpOp
dv_hsOpM = StateT hsOp
dv_opTailM = StateT opTail
dv_hsExpM = StateT hsExp
dv_hsExp1M = StateT hsExp1
dv_hsExpTplM = StateT hsExpTpl
dv_hsTypeArrM = StateT hsTypeArr
dv_hsTypeM = StateT hsType
dv_hsType1M = StateT hsType1
dv_hsTypeTplM = StateT hsTypeTpl
dv_typM = StateT typ
dv_variableM = StateT variable
dv_tvtailM = StateT tvtail
dv_integerM = StateT integer
dv_alphaM = StateT alpha
dv_upperM = StateT upper
dv_lowerM = StateT lower
dv_digitM = StateT digit
dv_spacesM = StateT spaces
dv_spaceM = StateT space
dv_notNLStringM = StateT notNLString
dv_newLineM = StateT newLine
dv_commentM = StateT comment
dv_commentsM = StateT comments
dv_notComStrM = StateT notComStr
dv_comEndM = StateT comEnd
dvCharsM :: PackratM (Token String)
dvCharsM = StateT dvChars
parse :: Pos String -> String -> Derivs
parse pos s = d
          where d = Derivs localpegFile0_0 localpragma1_1 localpragmaStr2_2 localpragmaItems3_3 localdelPragmas4_4 localpragmaEnd5_5 localmoduleDec6_6 localmoduleDecStr7_7 localwhr8_8 localpreImpPap9_9 localprePeg10_10 localafterPeg11_11 localimportPapillon12_12 localvarToken13_13 localtypToken14_14 localpap15_15 localpeg16_16 localsourceType17_17 localpeg_18_18 localdefinition19_19 localselection20_20 localexpressionHs21_21 localexpression22_22 localnameLeaf_23_23 localnameLeaf24_24 localnameLeafNoCom25_25 localcomForErr26_26 localleaf27_27 localpatOp28_28 localpat29_29 localpat130_30 localpatList31_31 localopConName32_32 localcharLit33_33 localstringLit34_34 localescapeC35_35 localpats36_36 localreadFromLs37_37 localreadFrom38_38 localtest39_39 localhsExpLam40_40 localhsExpTyp41_41 localhsExpOp42_42 localhsOp43_43 localopTail44_44 localhsExp45_45 localhsExp146_46 localhsExpTpl47_47 localhsTypeArr48_48 localhsType49_49 localhsType150_50 localhsTypeTpl51_51 localtyp52_52 localvariable53_53 localtvtail54_54 localinteger55_55 localalpha56_56 localupper57_57 locallower58_58 localdigit59_59 localspaces60_60 localspace61_61 localnotNLString62_62 localnewLine63_63 localcomment64_64 localcomments65_65 localnotComStr66_66 localcomEnd67_67 char pos
                localpegFile0_0 = runStateT p_pegFile d
                localpragma1_1 = runStateT p_pragma d
                localpragmaStr2_2 = runStateT p_pragmaStr d
                localpragmaItems3_3 = runStateT p_pragmaItems d
                localdelPragmas4_4 = runStateT p_delPragmas d
                localpragmaEnd5_5 = runStateT p_pragmaEnd d
                localmoduleDec6_6 = runStateT p_moduleDec d
                localmoduleDecStr7_7 = runStateT p_moduleDecStr d
                localwhr8_8 = runStateT p_whr d
                localpreImpPap9_9 = runStateT p_preImpPap d
                localprePeg10_10 = runStateT p_prePeg d
                localafterPeg11_11 = runStateT p_afterPeg d
                localimportPapillon12_12 = runStateT p_importPapillon d
                localvarToken13_13 = runStateT p_varToken d
                localtypToken14_14 = runStateT p_typToken d
                localpap15_15 = runStateT p_pap d
                localpeg16_16 = runStateT p_peg d
                localsourceType17_17 = runStateT p_sourceType d
                localpeg_18_18 = runStateT p_peg_ d
                localdefinition19_19 = runStateT p_definition d
                localselection20_20 = runStateT p_selection d
                localexpressionHs21_21 = runStateT p_expressionHs d
                localexpression22_22 = runStateT p_expression d
                localnameLeaf_23_23 = runStateT p_nameLeaf_ d
                localnameLeaf24_24 = runStateT p_nameLeaf d
                localnameLeafNoCom25_25 = runStateT p_nameLeafNoCom d
                localcomForErr26_26 = runStateT p_comForErr d
                localleaf27_27 = runStateT p_leaf d
                localpatOp28_28 = runStateT p_patOp d
                localpat29_29 = runStateT p_pat d
                localpat130_30 = runStateT p_pat1 d
                localpatList31_31 = runStateT p_patList d
                localopConName32_32 = runStateT p_opConName d
                localcharLit33_33 = runStateT p_charLit d
                localstringLit34_34 = runStateT p_stringLit d
                localescapeC35_35 = runStateT p_escapeC d
                localpats36_36 = runStateT p_pats d
                localreadFromLs37_37 = runStateT p_readFromLs d
                localreadFrom38_38 = runStateT p_readFrom d
                localtest39_39 = runStateT p_test d
                localhsExpLam40_40 = runStateT p_hsExpLam d
                localhsExpTyp41_41 = runStateT p_hsExpTyp d
                localhsExpOp42_42 = runStateT p_hsExpOp d
                localhsOp43_43 = runStateT p_hsOp d
                localopTail44_44 = runStateT p_opTail d
                localhsExp45_45 = runStateT p_hsExp d
                localhsExp146_46 = runStateT p_hsExp1 d
                localhsExpTpl47_47 = runStateT p_hsExpTpl d
                localhsTypeArr48_48 = runStateT p_hsTypeArr d
                localhsType49_49 = runStateT p_hsType d
                localhsType150_50 = runStateT p_hsType1 d
                localhsTypeTpl51_51 = runStateT p_hsTypeTpl d
                localtyp52_52 = runStateT p_typ d
                localvariable53_53 = runStateT p_variable d
                localtvtail54_54 = runStateT p_tvtail d
                localinteger55_55 = runStateT p_integer d
                localalpha56_56 = runStateT p_alpha d
                localupper57_57 = runStateT p_upper d
                locallower58_58 = runStateT p_lower d
                localdigit59_59 = runStateT p_digit d
                localspaces60_60 = runStateT p_spaces d
                localspace61_61 = runStateT p_space d
                localnotNLString62_62 = runStateT p_notNLString d
                localnewLine63_63 = runStateT p_newLine d
                localcomment64_64 = runStateT p_comment d
                localcomments65_65 = runStateT p_comments d
                localnotComStr66_66 = runStateT p_notComStr d
                localcomEnd67_67 = runStateT p_comEnd d
                char = runStateT (case getToken s of
                                      Just (c, s') -> do put (parse (updatePos c pos) s')
                                                         return c
                                      _ -> gets dvPos >>= (throwError . ParseError "" "end of input" "" undefined [])) d
p_pegFile :: PackratM PegFile
p_pragma :: PackratM (Maybe String)
p_pragmaStr :: PackratM String
p_pragmaItems :: PackratM ([String])
p_delPragmas :: PackratM ()
p_pragmaEnd :: PackratM ()
p_moduleDec :: PackratM (Maybe String)
p_moduleDecStr :: PackratM String
p_whr :: PackratM ()
p_preImpPap :: PackratM String
p_prePeg :: PackratM String
p_afterPeg :: PackratM String
p_importPapillon :: PackratM ()
p_varToken :: PackratM String
p_typToken :: PackratM String
p_pap :: PackratM ()
p_peg :: PackratM TTPeg
p_sourceType :: PackratM String
p_peg_ :: PackratM Peg
p_definition :: PackratM Definition
p_selection :: PackratM Selection
p_expressionHs :: PackratM ExpressionHs
p_expression :: PackratM Expression
p_nameLeaf_ :: PackratM NameLeaf_
p_nameLeaf :: PackratM NameLeaf
p_nameLeafNoCom :: PackratM NameLeaf
p_comForErr :: PackratM String
p_leaf :: PackratM ((ReadFrom, (ExpQ, String)))
p_patOp :: PackratM PatQ
p_pat :: PackratM PatQ
p_pat1 :: PackratM PatQ
p_patList :: PackratM ([PatQ])
p_opConName :: PackratM Name
p_charLit :: PackratM Char
p_stringLit :: PackratM String
p_escapeC :: PackratM Char
p_pats :: PackratM PatQs
p_readFromLs :: PackratM ReadFrom
p_readFrom :: PackratM ReadFrom
p_test :: PackratM ((ExR, String))
p_hsExpLam :: PackratM ExR
p_hsExpTyp :: PackratM ExR
p_hsExpOp :: PackratM ExR
p_hsOp :: PackratM ExR
p_opTail :: PackratM String
p_hsExp :: PackratM Ex
p_hsExp1 :: PackratM ExR
p_hsExpTpl :: PackratM ExRL
p_hsTypeArr :: PackratM TypeQ
p_hsType :: PackratM Typ
p_hsType1 :: PackratM TypeQ
p_hsTypeTpl :: PackratM TypeQL
p_typ :: PackratM String
p_variable :: PackratM String
p_tvtail :: PackratM String
p_integer :: PackratM Integer
p_alpha :: PackratM Char
p_upper :: PackratM Char
p_lower :: PackratM Char
p_digit :: PackratM Char
p_spaces :: PackratM ()
p_space :: PackratM ()
p_notNLString :: PackratM String
p_newLine :: PackratM ()
p_comment :: PackratM ()
p_comments :: PackratM ()
p_notComStr :: PackratM ()
p_comEnd :: PackratM ()
p_pegFile = foldl1 mplus [do d69_68 <- get
                             xx68_69 <- dv_pragmaM
                             let pr = xx68_69
                             unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d69_68 ["pragma"]))
                             d71_70 <- get
                             xx70_71 <- dv_moduleDecM
                             let md = xx70_71
                             unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d71_70 ["moduleDec"]))
                             d73_72 <- get
                             xx72_73 <- dv_preImpPapM
                             let pip = xx72_73
                             unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d73_72 ["preImpPap"]))
                             d75_74 <- get
                             _ <- dv_importPapillonM
                             unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d75_74 ["importPapillon"]))
                             d77_75 <- get
                             xx76_76 <- dv_prePegM
                             let pp = xx76_76
                             unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d77_75 ["prePeg"]))
                             d79_77 <- get
                             _ <- dv_papM
                             unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d79_77 ["pap"]))
                             d81_78 <- get
                             xx80_79 <- dv_pegM
                             let p = xx80_79
                             unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d81_78 ["peg"]))
                             d83_80 <- get
                             _ <- dv_spacesM
                             unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d83_80 ["spaces"]))
                             d85_81 <- get
                             xx84_82 <- dvCharsM
                             case xx84_82 of
                                 '|' -> return ()
                                 _ -> gets dvPos >>= (throwError . ParseError "'|'" "not match pattern: " "" d85_81 ["dvChars"])
                             let '|' = xx84_82
                             return ()
                             unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d85_81 ["dvChars"]))
                             d87_83 <- get
                             xx86_84 <- dvCharsM
                             case xx86_84 of
                                 ']' -> return ()
                                 _ -> gets dvPos >>= (throwError . ParseError "']'" "not match pattern: " "" d87_83 ["dvChars"])
                             let ']' = xx86_84
                             return ()
                             unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d87_83 ["dvChars"]))
                             d89_85 <- get
                             xx88_86 <- dvCharsM
                             case xx88_86 of
                                 '\n' -> return ()
                                 _ -> gets dvPos >>= (throwError . ParseError "'\\n'" "not match pattern: " "" d89_85 ["dvChars"])
                             let '\n' = xx88_86
                             return ()
                             unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d89_85 ["dvChars"]))
                             d91_87 <- get
                             xx90_88 <- dv_afterPegM
                             let atp = xx90_88
                             unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d91_87 ["afterPeg"]))
                             return (mkPegFile pr md pip pp p atp),
                          do d93_89 <- get
                             xx92_90 <- dv_pragmaM
                             let pr = xx92_90
                             unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d93_89 ["pragma"]))
                             d95_91 <- get
                             xx94_92 <- dv_moduleDecM
                             let md = xx94_92
                             unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d95_91 ["moduleDec"]))
                             d97_93 <- get
                             xx96_94 <- dv_prePegM
                             let pp = xx96_94
                             unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d97_93 ["prePeg"]))
                             d99_95 <- get
                             _ <- dv_papM
                             unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d99_95 ["pap"]))
                             d101_96 <- get
                             xx100_97 <- dv_pegM
                             let p = xx100_97
                             unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d101_96 ["peg"]))
                             d103_98 <- get
                             _ <- dv_spacesM
                             unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d103_98 ["spaces"]))
                             d105_99 <- get
                             xx104_100 <- dvCharsM
                             case xx104_100 of
                                 '|' -> return ()
                                 _ -> gets dvPos >>= (throwError . ParseError "'|'" "not match pattern: " "" d105_99 ["dvChars"])
                             let '|' = xx104_100
                             return ()
                             unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d105_99 ["dvChars"]))
                             d107_101 <- get
                             xx106_102 <- dvCharsM
                             case xx106_102 of
                                 ']' -> return ()
                                 _ -> gets dvPos >>= (throwError . ParseError "']'" "not match pattern: " "" d107_101 ["dvChars"])
                             let ']' = xx106_102
                             return ()
                             unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d107_101 ["dvChars"]))
                             d109_103 <- get
                             xx108_104 <- dvCharsM
                             case xx108_104 of
                                 '\n' -> return ()
                                 _ -> gets dvPos >>= (throwError . ParseError "'\\n'" "not match pattern: " "" d109_103 ["dvChars"])
                             let '\n' = xx108_104
                             return ()
                             unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d109_103 ["dvChars"]))
                             d111_105 <- get
                             xx110_106 <- dv_afterPegM
                             let atp = xx110_106
                             unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d111_105 ["afterPeg"]))
                             return (mkPegFile pr md emp pp p atp)]
p_pragma = foldl1 mplus [do d113_107 <- get
                            _ <- dv_spacesM
                            unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d113_107 ["spaces"]))
                            d115_108 <- get
                            xx114_109 <- dvCharsM
                            case xx114_109 of
                                '{' -> return ()
                                _ -> gets dvPos >>= (throwError . ParseError "'{'" "not match pattern: " "" d115_108 ["dvChars"])
                            let '{' = xx114_109
                            return ()
                            unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d115_108 ["dvChars"]))
                            d117_110 <- get
                            xx116_111 <- dvCharsM
                            case xx116_111 of
                                '-' -> return ()
                                _ -> gets dvPos >>= (throwError . ParseError "'-'" "not match pattern: " "" d117_110 ["dvChars"])
                            let '-' = xx116_111
                            return ()
                            unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d117_110 ["dvChars"]))
                            d119_112 <- get
                            xx118_113 <- dvCharsM
                            case xx118_113 of
                                '#' -> return ()
                                _ -> gets dvPos >>= (throwError . ParseError "'#'" "not match pattern: " "" d119_112 ["dvChars"])
                            let '#' = xx118_113
                            return ()
                            unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d119_112 ["dvChars"]))
                            d121_114 <- get
                            _ <- dv_spacesM
                            unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d121_114 ["spaces"]))
                            d123_115 <- get
                            xx122_116 <- dvCharsM
                            case xx122_116 of
                                'L' -> return ()
                                _ -> gets dvPos >>= (throwError . ParseError "'L'" "not match pattern: " "" d123_115 ["dvChars"])
                            let 'L' = xx122_116
                            return ()
                            unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d123_115 ["dvChars"]))
                            d125_117 <- get
                            xx124_118 <- dvCharsM
                            case xx124_118 of
                                'A' -> return ()
                                _ -> gets dvPos >>= (throwError . ParseError "'A'" "not match pattern: " "" d125_117 ["dvChars"])
                            let 'A' = xx124_118
                            return ()
                            unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d125_117 ["dvChars"]))
                            d127_119 <- get
                            xx126_120 <- dvCharsM
                            case xx126_120 of
                                'N' -> return ()
                                _ -> gets dvPos >>= (throwError . ParseError "'N'" "not match pattern: " "" d127_119 ["dvChars"])
                            let 'N' = xx126_120
                            return ()
                            unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d127_119 ["dvChars"]))
                            d129_121 <- get
                            xx128_122 <- dvCharsM
                            case xx128_122 of
                                'G' -> return ()
                                _ -> gets dvPos >>= (throwError . ParseError "'G'" "not match pattern: " "" d129_121 ["dvChars"])
                            let 'G' = xx128_122
                            return ()
                            unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d129_121 ["dvChars"]))
                            d131_123 <- get
                            xx130_124 <- dvCharsM
                            case xx130_124 of
                                'U' -> return ()
                                _ -> gets dvPos >>= (throwError . ParseError "'U'" "not match pattern: " "" d131_123 ["dvChars"])
                            let 'U' = xx130_124
                            return ()
                            unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d131_123 ["dvChars"]))
                            d133_125 <- get
                            xx132_126 <- dvCharsM
                            case xx132_126 of
                                'A' -> return ()
                                _ -> gets dvPos >>= (throwError . ParseError "'A'" "not match pattern: " "" d133_125 ["dvChars"])
                            let 'A' = xx132_126
                            return ()
                            unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d133_125 ["dvChars"]))
                            d135_127 <- get
                            xx134_128 <- dvCharsM
                            case xx134_128 of
                                'G' -> return ()
                                _ -> gets dvPos >>= (throwError . ParseError "'G'" "not match pattern: " "" d135_127 ["dvChars"])
                            let 'G' = xx134_128
                            return ()
                            unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d135_127 ["dvChars"]))
                            d137_129 <- get
                            xx136_130 <- dvCharsM
                            case xx136_130 of
                                'E' -> return ()
                                _ -> gets dvPos >>= (throwError . ParseError "'E'" "not match pattern: " "" d137_129 ["dvChars"])
                            let 'E' = xx136_130
                            return ()
                            unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d137_129 ["dvChars"]))
                            d139_131 <- get
                            _ <- dv_spacesM
                            unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d139_131 ["spaces"]))
                            d141_132 <- get
                            xx140_133 <- dv_pragmaItemsM
                            let s = xx140_133
                            unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d141_132 ["pragmaItems"]))
                            d143_134 <- get
                            _ <- dv_pragmaEndM
                            unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d143_134 ["pragmaEnd"]))
                            d145_135 <- get
                            _ <- dv_spacesM
                            unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d145_135 ["spaces"]))
                            return (just $ " LANGUAGE " ++ concatMap (++ ", ") s),
                         do d147_136 <- get
                            _ <- dv_spacesM
                            unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d147_136 ["spaces"]))
                            return nothing]
p_pragmaStr = foldl1 mplus [do d149_137 <- get
                               _ <- dv_delPragmasM
                               unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d149_137 ["delPragmas"]))
                               d151_138 <- get
                               _ <- dv_spacesM
                               unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d151_138 ["spaces"]))
                               d153_139 <- get
                               xx152_140 <- dvCharsM
                               case xx152_140 of
                                   ',' -> return ()
                                   _ -> gets dvPos >>= (throwError . ParseError "','" "not match pattern: " "" d153_139 ["dvChars"])
                               let ',' = xx152_140
                               return ()
                               unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d153_139 ["dvChars"]))
                               d155_141 <- get
                               _ <- dv_spacesM
                               unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d155_141 ["spaces"]))
                               d157_142 <- get
                               xx156_143 <- dv_pragmaStrM
                               let s = xx156_143
                               unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d157_142 ["pragmaStr"]))
                               return (' ' : s),
                            do ddd158_144 <- get
                               do err <- ((do d160_145 <- get
                                              _ <- dv_pragmaEndM
                                              unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d160_145 ["pragmaEnd"]))) >> return False) `catchError` const (return True)
                                  unless err (gets dvPos >>= (throwError . ParseError ('!' : "_:pragmaEnd[True]") "not match: " "" ddd158_144 ["pragmaEnd"]))
                               put ddd158_144
                               ddd161_146 <- get
                               do err <- ((do d163_147 <- get
                                              _ <- dv_delPragmasM
                                              unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d163_147 ["delPragmas"]))) >> return False) `catchError` const (return True)
                                  unless err (gets dvPos >>= (throwError . ParseError ('!' : "_:delPragmas[True]") "not match: " "" ddd161_146 ["delPragmas"]))
                               put ddd161_146
                               d165_148 <- get
                               xx164_149 <- dvCharsM
                               let c = xx164_149
                               unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d165_148 ["dvChars"]))
                               d167_150 <- get
                               xx166_151 <- dv_pragmaStrM
                               let s = xx166_151
                               unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d167_150 ["pragmaStr"]))
                               return (c : s),
                            return emp]
p_pragmaItems = foldl1 mplus [do d169_152 <- get
                                 _ <- dv_delPragmasM
                                 unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d169_152 ["delPragmas"]))
                                 d171_153 <- get
                                 _ <- dv_spacesM
                                 unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d171_153 ["spaces"]))
                                 d173_154 <- get
                                 xx172_155 <- dvCharsM
                                 case xx172_155 of
                                     ',' -> return ()
                                     _ -> gets dvPos >>= (throwError . ParseError "','" "not match pattern: " "" d173_154 ["dvChars"])
                                 let ',' = xx172_155
                                 return ()
                                 unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d173_154 ["dvChars"]))
                                 d175_156 <- get
                                 _ <- dv_spacesM
                                 unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d175_156 ["spaces"]))
                                 d177_157 <- get
                                 xx176_158 <- dv_pragmaItemsM
                                 let i = xx176_158
                                 unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d177_157 ["pragmaItems"]))
                                 return i,
                              do d179_159 <- get
                                 xx178_160 <- dv_typTokenM
                                 let t = xx178_160
                                 unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d179_159 ["typToken"]))
                                 d181_161 <- get
                                 xx180_162 <- dvCharsM
                                 case xx180_162 of
                                     ',' -> return ()
                                     _ -> gets dvPos >>= (throwError . ParseError "','" "not match pattern: " "" d181_161 ["dvChars"])
                                 let ',' = xx180_162
                                 return ()
                                 unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d181_161 ["dvChars"]))
                                 d183_163 <- get
                                 _ <- dv_spacesM
                                 unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d183_163 ["spaces"]))
                                 d185_164 <- get
                                 xx184_165 <- dv_pragmaItemsM
                                 let i = xx184_165
                                 unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d185_164 ["pragmaItems"]))
                                 return (t : i),
                              do d187_166 <- get
                                 _ <- dv_delPragmasM
                                 unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d187_166 ["delPragmas"]))
                                 d189_167 <- get
                                 _ <- dv_spacesM
                                 unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d189_167 ["spaces"]))
                                 return [],
                              do d191_168 <- get
                                 xx190_169 <- dv_typTokenM
                                 let t = xx190_169
                                 unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d191_168 ["typToken"]))
                                 return [t]]
p_delPragmas = foldl1 mplus [do d193_170 <- get
                                xx192_171 <- dvCharsM
                                case xx192_171 of
                                    'Q' -> return ()
                                    _ -> gets dvPos >>= (throwError . ParseError "'Q'" "not match pattern: " "" d193_170 ["dvChars"])
                                let 'Q' = xx192_171
                                return ()
                                unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d193_170 ["dvChars"]))
                                d195_172 <- get
                                xx194_173 <- dvCharsM
                                case xx194_173 of
                                    'u' -> return ()
                                    _ -> gets dvPos >>= (throwError . ParseError "'u'" "not match pattern: " "" d195_172 ["dvChars"])
                                let 'u' = xx194_173
                                return ()
                                unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d195_172 ["dvChars"]))
                                d197_174 <- get
                                xx196_175 <- dvCharsM
                                case xx196_175 of
                                    'a' -> return ()
                                    _ -> gets dvPos >>= (throwError . ParseError "'a'" "not match pattern: " "" d197_174 ["dvChars"])
                                let 'a' = xx196_175
                                return ()
                                unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d197_174 ["dvChars"]))
                                d199_176 <- get
                                xx198_177 <- dvCharsM
                                case xx198_177 of
                                    's' -> return ()
                                    _ -> gets dvPos >>= (throwError . ParseError "'s'" "not match pattern: " "" d199_176 ["dvChars"])
                                let 's' = xx198_177
                                return ()
                                unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d199_176 ["dvChars"]))
                                d201_178 <- get
                                xx200_179 <- dvCharsM
                                case xx200_179 of
                                    'i' -> return ()
                                    _ -> gets dvPos >>= (throwError . ParseError "'i'" "not match pattern: " "" d201_178 ["dvChars"])
                                let 'i' = xx200_179
                                return ()
                                unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d201_178 ["dvChars"]))
                                d203_180 <- get
                                xx202_181 <- dvCharsM
                                case xx202_181 of
                                    'Q' -> return ()
                                    _ -> gets dvPos >>= (throwError . ParseError "'Q'" "not match pattern: " "" d203_180 ["dvChars"])
                                let 'Q' = xx202_181
                                return ()
                                unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d203_180 ["dvChars"]))
                                d205_182 <- get
                                xx204_183 <- dvCharsM
                                case xx204_183 of
                                    'u' -> return ()
                                    _ -> gets dvPos >>= (throwError . ParseError "'u'" "not match pattern: " "" d205_182 ["dvChars"])
                                let 'u' = xx204_183
                                return ()
                                unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d205_182 ["dvChars"]))
                                d207_184 <- get
                                xx206_185 <- dvCharsM
                                case xx206_185 of
                                    'o' -> return ()
                                    _ -> gets dvPos >>= (throwError . ParseError "'o'" "not match pattern: " "" d207_184 ["dvChars"])
                                let 'o' = xx206_185
                                return ()
                                unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d207_184 ["dvChars"]))
                                d209_186 <- get
                                xx208_187 <- dvCharsM
                                case xx208_187 of
                                    't' -> return ()
                                    _ -> gets dvPos >>= (throwError . ParseError "'t'" "not match pattern: " "" d209_186 ["dvChars"])
                                let 't' = xx208_187
                                return ()
                                unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d209_186 ["dvChars"]))
                                d211_188 <- get
                                xx210_189 <- dvCharsM
                                case xx210_189 of
                                    'e' -> return ()
                                    _ -> gets dvPos >>= (throwError . ParseError "'e'" "not match pattern: " "" d211_188 ["dvChars"])
                                let 'e' = xx210_189
                                return ()
                                unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d211_188 ["dvChars"]))
                                d213_190 <- get
                                xx212_191 <- dvCharsM
                                case xx212_191 of
                                    's' -> return ()
                                    _ -> gets dvPos >>= (throwError . ParseError "'s'" "not match pattern: " "" d213_190 ["dvChars"])
                                let 's' = xx212_191
                                return ()
                                unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d213_190 ["dvChars"]))
                                return (),
                             do d215_192 <- get
                                xx214_193 <- dvCharsM
                                case xx214_193 of
                                    'T' -> return ()
                                    _ -> gets dvPos >>= (throwError . ParseError "'T'" "not match pattern: " "" d215_192 ["dvChars"])
                                let 'T' = xx214_193
                                return ()
                                unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d215_192 ["dvChars"]))
                                d217_194 <- get
                                xx216_195 <- dvCharsM
                                case xx216_195 of
                                    'y' -> return ()
                                    _ -> gets dvPos >>= (throwError . ParseError "'y'" "not match pattern: " "" d217_194 ["dvChars"])
                                let 'y' = xx216_195
                                return ()
                                unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d217_194 ["dvChars"]))
                                d219_196 <- get
                                xx218_197 <- dvCharsM
                                case xx218_197 of
                                    'p' -> return ()
                                    _ -> gets dvPos >>= (throwError . ParseError "'p'" "not match pattern: " "" d219_196 ["dvChars"])
                                let 'p' = xx218_197
                                return ()
                                unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d219_196 ["dvChars"]))
                                d221_198 <- get
                                xx220_199 <- dvCharsM
                                case xx220_199 of
                                    'e' -> return ()
                                    _ -> gets dvPos >>= (throwError . ParseError "'e'" "not match pattern: " "" d221_198 ["dvChars"])
                                let 'e' = xx220_199
                                return ()
                                unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d221_198 ["dvChars"]))
                                d223_200 <- get
                                xx222_201 <- dvCharsM
                                case xx222_201 of
                                    'F' -> return ()
                                    _ -> gets dvPos >>= (throwError . ParseError "'F'" "not match pattern: " "" d223_200 ["dvChars"])
                                let 'F' = xx222_201
                                return ()
                                unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d223_200 ["dvChars"]))
                                d225_202 <- get
                                xx224_203 <- dvCharsM
                                case xx224_203 of
                                    'a' -> return ()
                                    _ -> gets dvPos >>= (throwError . ParseError "'a'" "not match pattern: " "" d225_202 ["dvChars"])
                                let 'a' = xx224_203
                                return ()
                                unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d225_202 ["dvChars"]))
                                d227_204 <- get
                                xx226_205 <- dvCharsM
                                case xx226_205 of
                                    'm' -> return ()
                                    _ -> gets dvPos >>= (throwError . ParseError "'m'" "not match pattern: " "" d227_204 ["dvChars"])
                                let 'm' = xx226_205
                                return ()
                                unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d227_204 ["dvChars"]))
                                d229_206 <- get
                                xx228_207 <- dvCharsM
                                case xx228_207 of
                                    'i' -> return ()
                                    _ -> gets dvPos >>= (throwError . ParseError "'i'" "not match pattern: " "" d229_206 ["dvChars"])
                                let 'i' = xx228_207
                                return ()
                                unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d229_206 ["dvChars"]))
                                d231_208 <- get
                                xx230_209 <- dvCharsM
                                case xx230_209 of
                                    'l' -> return ()
                                    _ -> gets dvPos >>= (throwError . ParseError "'l'" "not match pattern: " "" d231_208 ["dvChars"])
                                let 'l' = xx230_209
                                return ()
                                unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d231_208 ["dvChars"]))
                                d233_210 <- get
                                xx232_211 <- dvCharsM
                                case xx232_211 of
                                    'i' -> return ()
                                    _ -> gets dvPos >>= (throwError . ParseError "'i'" "not match pattern: " "" d233_210 ["dvChars"])
                                let 'i' = xx232_211
                                return ()
                                unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d233_210 ["dvChars"]))
                                d235_212 <- get
                                xx234_213 <- dvCharsM
                                case xx234_213 of
                                    'e' -> return ()
                                    _ -> gets dvPos >>= (throwError . ParseError "'e'" "not match pattern: " "" d235_212 ["dvChars"])
                                let 'e' = xx234_213
                                return ()
                                unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d235_212 ["dvChars"]))
                                d237_214 <- get
                                xx236_215 <- dvCharsM
                                case xx236_215 of
                                    's' -> return ()
                                    _ -> gets dvPos >>= (throwError . ParseError "'s'" "not match pattern: " "" d237_214 ["dvChars"])
                                let 's' = xx236_215
                                return ()
                                unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d237_214 ["dvChars"]))
                                return ()]
p_pragmaEnd = foldl1 mplus [do d239_216 <- get
                               _ <- dv_delPragmasM
                               unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d239_216 ["delPragmas"]))
                               d241_217 <- get
                               _ <- dv_spacesM
                               unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d241_217 ["spaces"]))
                               d243_218 <- get
                               xx242_219 <- dvCharsM
                               case xx242_219 of
                                   '#' -> return ()
                                   _ -> gets dvPos >>= (throwError . ParseError "'#'" "not match pattern: " "" d243_218 ["dvChars"])
                               let '#' = xx242_219
                               return ()
                               unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d243_218 ["dvChars"]))
                               d245_220 <- get
                               xx244_221 <- dvCharsM
                               case xx244_221 of
                                   '-' -> return ()
                                   _ -> gets dvPos >>= (throwError . ParseError "'-'" "not match pattern: " "" d245_220 ["dvChars"])
                               let '-' = xx244_221
                               return ()
                               unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d245_220 ["dvChars"]))
                               d247_222 <- get
                               xx246_223 <- dvCharsM
                               case xx246_223 of
                                   '}' -> return ()
                                   _ -> gets dvPos >>= (throwError . ParseError "'}'" "not match pattern: " "" d247_222 ["dvChars"])
                               let '}' = xx246_223
                               return ()
                               unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d247_222 ["dvChars"]))
                               return (),
                            do d249_224 <- get
                               xx248_225 <- dvCharsM
                               case xx248_225 of
                                   '#' -> return ()
                                   _ -> gets dvPos >>= (throwError . ParseError "'#'" "not match pattern: " "" d249_224 ["dvChars"])
                               let '#' = xx248_225
                               return ()
                               unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d249_224 ["dvChars"]))
                               d251_226 <- get
                               xx250_227 <- dvCharsM
                               case xx250_227 of
                                   '-' -> return ()
                                   _ -> gets dvPos >>= (throwError . ParseError "'-'" "not match pattern: " "" d251_226 ["dvChars"])
                               let '-' = xx250_227
                               return ()
                               unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d251_226 ["dvChars"]))
                               d253_228 <- get
                               xx252_229 <- dvCharsM
                               case xx252_229 of
                                   '}' -> return ()
                                   _ -> gets dvPos >>= (throwError . ParseError "'}'" "not match pattern: " "" d253_228 ["dvChars"])
                               let '}' = xx252_229
                               return ()
                               unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d253_228 ["dvChars"]))
                               return ()]
p_moduleDec = foldl1 mplus [do d255_230 <- get
                               xx254_231 <- dvCharsM
                               case xx254_231 of
                                   'm' -> return ()
                                   _ -> gets dvPos >>= (throwError . ParseError "'m'" "not match pattern: " "" d255_230 ["dvChars"])
                               let 'm' = xx254_231
                               return ()
                               unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d255_230 ["dvChars"]))
                               d257_232 <- get
                               xx256_233 <- dvCharsM
                               case xx256_233 of
                                   'o' -> return ()
                                   _ -> gets dvPos >>= (throwError . ParseError "'o'" "not match pattern: " "" d257_232 ["dvChars"])
                               let 'o' = xx256_233
                               return ()
                               unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d257_232 ["dvChars"]))
                               d259_234 <- get
                               xx258_235 <- dvCharsM
                               case xx258_235 of
                                   'd' -> return ()
                                   _ -> gets dvPos >>= (throwError . ParseError "'d'" "not match pattern: " "" d259_234 ["dvChars"])
                               let 'd' = xx258_235
                               return ()
                               unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d259_234 ["dvChars"]))
                               d261_236 <- get
                               xx260_237 <- dvCharsM
                               case xx260_237 of
                                   'u' -> return ()
                                   _ -> gets dvPos >>= (throwError . ParseError "'u'" "not match pattern: " "" d261_236 ["dvChars"])
                               let 'u' = xx260_237
                               return ()
                               unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d261_236 ["dvChars"]))
                               d263_238 <- get
                               xx262_239 <- dvCharsM
                               case xx262_239 of
                                   'l' -> return ()
                                   _ -> gets dvPos >>= (throwError . ParseError "'l'" "not match pattern: " "" d263_238 ["dvChars"])
                               let 'l' = xx262_239
                               return ()
                               unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d263_238 ["dvChars"]))
                               d265_240 <- get
                               xx264_241 <- dvCharsM
                               case xx264_241 of
                                   'e' -> return ()
                                   _ -> gets dvPos >>= (throwError . ParseError "'e'" "not match pattern: " "" d265_240 ["dvChars"])
                               let 'e' = xx264_241
                               return ()
                               unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d265_240 ["dvChars"]))
                               d267_242 <- get
                               xx266_243 <- dv_moduleDecStrM
                               let s = xx266_243
                               unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d267_242 ["moduleDecStr"]))
                               d269_244 <- get
                               _ <- dv_whrM
                               unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d269_244 ["whr"]))
                               return (just s),
                            return nothing]
p_moduleDecStr = foldl1 mplus [do ddd270_245 <- get
                                  do err <- ((do d272_246 <- get
                                                 _ <- dv_whrM
                                                 unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d272_246 ["whr"]))) >> return False) `catchError` const (return True)
                                     unless err (gets dvPos >>= (throwError . ParseError ('!' : "_:whr[True]") "not match: " "" ddd270_245 ["whr"]))
                                  put ddd270_245
                                  d274_247 <- get
                                  xx273_248 <- dvCharsM
                                  let c = xx273_248
                                  unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d274_247 ["dvChars"]))
                                  d276_249 <- get
                                  xx275_250 <- dv_moduleDecStrM
                                  let s = xx275_250
                                  unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d276_249 ["moduleDecStr"]))
                                  return (cons c s),
                               return emp]
p_whr = foldl1 mplus [do d278_251 <- get
                         xx277_252 <- dvCharsM
                         case xx277_252 of
                             'w' -> return ()
                             _ -> gets dvPos >>= (throwError . ParseError "'w'" "not match pattern: " "" d278_251 ["dvChars"])
                         let 'w' = xx277_252
                         return ()
                         unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d278_251 ["dvChars"]))
                         d280_253 <- get
                         xx279_254 <- dvCharsM
                         case xx279_254 of
                             'h' -> return ()
                             _ -> gets dvPos >>= (throwError . ParseError "'h'" "not match pattern: " "" d280_253 ["dvChars"])
                         let 'h' = xx279_254
                         return ()
                         unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d280_253 ["dvChars"]))
                         d282_255 <- get
                         xx281_256 <- dvCharsM
                         case xx281_256 of
                             'e' -> return ()
                             _ -> gets dvPos >>= (throwError . ParseError "'e'" "not match pattern: " "" d282_255 ["dvChars"])
                         let 'e' = xx281_256
                         return ()
                         unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d282_255 ["dvChars"]))
                         d284_257 <- get
                         xx283_258 <- dvCharsM
                         case xx283_258 of
                             'r' -> return ()
                             _ -> gets dvPos >>= (throwError . ParseError "'r'" "not match pattern: " "" d284_257 ["dvChars"])
                         let 'r' = xx283_258
                         return ()
                         unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d284_257 ["dvChars"]))
                         d286_259 <- get
                         xx285_260 <- dvCharsM
                         case xx285_260 of
                             'e' -> return ()
                             _ -> gets dvPos >>= (throwError . ParseError "'e'" "not match pattern: " "" d286_259 ["dvChars"])
                         let 'e' = xx285_260
                         return ()
                         unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d286_259 ["dvChars"]))
                         return ()]
p_preImpPap = foldl1 mplus [do ddd287_261 <- get
                               do err <- ((do d289_262 <- get
                                              _ <- dv_importPapillonM
                                              unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d289_262 ["importPapillon"]))) >> return False) `catchError` const (return True)
                                  unless err (gets dvPos >>= (throwError . ParseError ('!' : "_:importPapillon[True]") "not match: " "" ddd287_261 ["importPapillon"]))
                               put ddd287_261
                               ddd290_263 <- get
                               do err <- ((do d292_264 <- get
                                              _ <- dv_papM
                                              unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d292_264 ["pap"]))) >> return False) `catchError` const (return True)
                                  unless err (gets dvPos >>= (throwError . ParseError ('!' : "_:pap[True]") "not match: " "" ddd290_263 ["pap"]))
                               put ddd290_263
                               d294_265 <- get
                               xx293_266 <- dvCharsM
                               let c = xx293_266
                               unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d294_265 ["dvChars"]))
                               d296_267 <- get
                               xx295_268 <- dv_preImpPapM
                               let pip = xx295_268
                               unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d296_267 ["preImpPap"]))
                               return (cons c pip),
                            return emp]
p_prePeg = foldl1 mplus [do ddd297_269 <- get
                            do err <- ((do d299_270 <- get
                                           _ <- dv_papM
                                           unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d299_270 ["pap"]))) >> return False) `catchError` const (return True)
                               unless err (gets dvPos >>= (throwError . ParseError ('!' : "_:pap[True]") "not match: " "" ddd297_269 ["pap"]))
                            put ddd297_269
                            d301_271 <- get
                            xx300_272 <- dvCharsM
                            let c = xx300_272
                            unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d301_271 ["dvChars"]))
                            d303_273 <- get
                            xx302_274 <- dv_prePegM
                            let pp = xx302_274
                            unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d303_273 ["prePeg"]))
                            return (cons c pp),
                         return emp]
p_afterPeg = foldl1 mplus [do d305_275 <- get
                              xx304_276 <- dvCharsM
                              let c = xx304_276
                              unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d305_275 ["dvChars"]))
                              d307_277 <- get
                              xx306_278 <- dv_afterPegM
                              let atp = xx306_278
                              unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d307_277 ["afterPeg"]))
                              return (cons c atp),
                           return emp]
p_importPapillon = foldl1 mplus [do d309_279 <- get
                                    xx308_280 <- dv_varTokenM
                                    case xx308_280 of
                                        "import" -> return ()
                                        _ -> gets dvPos >>= (throwError . ParseError "\"import\"" "not match pattern: " "" d309_279 ["varToken"])
                                    let "import" = xx308_280
                                    return ()
                                    unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d309_279 ["varToken"]))
                                    d311_281 <- get
                                    xx310_282 <- dv_typTokenM
                                    case xx310_282 of
                                        "Text" -> return ()
                                        _ -> gets dvPos >>= (throwError . ParseError "\"Text\"" "not match pattern: " "" d311_281 ["typToken"])
                                    let "Text" = xx310_282
                                    return ()
                                    unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d311_281 ["typToken"]))
                                    d313_283 <- get
                                    xx312_284 <- dvCharsM
                                    case xx312_284 of
                                        '.' -> return ()
                                        _ -> gets dvPos >>= (throwError . ParseError "'.'" "not match pattern: " "" d313_283 ["dvChars"])
                                    let '.' = xx312_284
                                    return ()
                                    unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d313_283 ["dvChars"]))
                                    d315_285 <- get
                                    _ <- dv_spacesM
                                    unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d315_285 ["spaces"]))
                                    d317_286 <- get
                                    xx316_287 <- dv_typTokenM
                                    case xx316_287 of
                                        "Papillon" -> return ()
                                        _ -> gets dvPos >>= (throwError . ParseError "\"Papillon\"" "not match pattern: " "" d317_286 ["typToken"])
                                    let "Papillon" = xx316_287
                                    return ()
                                    unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d317_286 ["typToken"]))
                                    ddd318_288 <- get
                                    do err <- ((do d320_289 <- get
                                                   xx319_290 <- dvCharsM
                                                   case xx319_290 of
                                                       '.' -> return ()
                                                       _ -> gets dvPos >>= (throwError . ParseError "'.'" "not match pattern: " "" d320_289 ["dvChars"])
                                                   let '.' = xx319_290
                                                   return ()
                                                   unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d320_289 ["dvChars"]))) >> return False) `catchError` const (return True)
                                       unless err (gets dvPos >>= (throwError . ParseError ('!' : "'.':[True]") "not match: " "" ddd318_288 ["dvChars"]))
                                    put ddd318_288
                                    return ()]
p_varToken = foldl1 mplus [do d322_291 <- get
                              xx321_292 <- dv_variableM
                              let v = xx321_292
                              unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d322_291 ["variable"]))
                              d324_293 <- get
                              _ <- dv_spacesM
                              unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d324_293 ["spaces"]))
                              return v]
p_typToken = foldl1 mplus [do d326_294 <- get
                              xx325_295 <- dv_typM
                              let t = xx325_295
                              unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d326_294 ["typ"]))
                              d328_296 <- get
                              _ <- dv_spacesM
                              unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d328_296 ["spaces"]))
                              return t]
p_pap = foldl1 mplus [do d330_297 <- get
                         xx329_298 <- dvCharsM
                         case xx329_298 of
                             '\n' -> return ()
                             _ -> gets dvPos >>= (throwError . ParseError "'\\n'" "not match pattern: " "" d330_297 ["dvChars"])
                         let '\n' = xx329_298
                         return ()
                         unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d330_297 ["dvChars"]))
                         d332_299 <- get
                         xx331_300 <- dvCharsM
                         case xx331_300 of
                             '[' -> return ()
                             _ -> gets dvPos >>= (throwError . ParseError "'['" "not match pattern: " "" d332_299 ["dvChars"])
                         let '[' = xx331_300
                         return ()
                         unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d332_299 ["dvChars"]))
                         d334_301 <- get
                         xx333_302 <- dvCharsM
                         case xx333_302 of
                             'p' -> return ()
                             _ -> gets dvPos >>= (throwError . ParseError "'p'" "not match pattern: " "" d334_301 ["dvChars"])
                         let 'p' = xx333_302
                         return ()
                         unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d334_301 ["dvChars"]))
                         d336_303 <- get
                         xx335_304 <- dvCharsM
                         case xx335_304 of
                             'a' -> return ()
                             _ -> gets dvPos >>= (throwError . ParseError "'a'" "not match pattern: " "" d336_303 ["dvChars"])
                         let 'a' = xx335_304
                         return ()
                         unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d336_303 ["dvChars"]))
                         d338_305 <- get
                         xx337_306 <- dvCharsM
                         case xx337_306 of
                             'p' -> return ()
                             _ -> gets dvPos >>= (throwError . ParseError "'p'" "not match pattern: " "" d338_305 ["dvChars"])
                         let 'p' = xx337_306
                         return ()
                         unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d338_305 ["dvChars"]))
                         d340_307 <- get
                         xx339_308 <- dvCharsM
                         case xx339_308 of
                             'i' -> return ()
                             _ -> gets dvPos >>= (throwError . ParseError "'i'" "not match pattern: " "" d340_307 ["dvChars"])
                         let 'i' = xx339_308
                         return ()
                         unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d340_307 ["dvChars"]))
                         d342_309 <- get
                         xx341_310 <- dvCharsM
                         case xx341_310 of
                             'l' -> return ()
                             _ -> gets dvPos >>= (throwError . ParseError "'l'" "not match pattern: " "" d342_309 ["dvChars"])
                         let 'l' = xx341_310
                         return ()
                         unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d342_309 ["dvChars"]))
                         d344_311 <- get
                         xx343_312 <- dvCharsM
                         case xx343_312 of
                             'l' -> return ()
                             _ -> gets dvPos >>= (throwError . ParseError "'l'" "not match pattern: " "" d344_311 ["dvChars"])
                         let 'l' = xx343_312
                         return ()
                         unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d344_311 ["dvChars"]))
                         d346_313 <- get
                         xx345_314 <- dvCharsM
                         case xx345_314 of
                             'o' -> return ()
                             _ -> gets dvPos >>= (throwError . ParseError "'o'" "not match pattern: " "" d346_313 ["dvChars"])
                         let 'o' = xx345_314
                         return ()
                         unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d346_313 ["dvChars"]))
                         d348_315 <- get
                         xx347_316 <- dvCharsM
                         case xx347_316 of
                             'n' -> return ()
                             _ -> gets dvPos >>= (throwError . ParseError "'n'" "not match pattern: " "" d348_315 ["dvChars"])
                         let 'n' = xx347_316
                         return ()
                         unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d348_315 ["dvChars"]))
                         d350_317 <- get
                         xx349_318 <- dvCharsM
                         case xx349_318 of
                             '|' -> return ()
                             _ -> gets dvPos >>= (throwError . ParseError "'|'" "not match pattern: " "" d350_317 ["dvChars"])
                         let '|' = xx349_318
                         return ()
                         unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d350_317 ["dvChars"]))
                         d352_319 <- get
                         xx351_320 <- dvCharsM
                         case xx351_320 of
                             '\n' -> return ()
                             _ -> gets dvPos >>= (throwError . ParseError "'\\n'" "not match pattern: " "" d352_319 ["dvChars"])
                         let '\n' = xx351_320
                         return ()
                         unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d352_319 ["dvChars"]))
                         return ()]
p_peg = foldl1 mplus [do d354_321 <- get
                         _ <- dv_spacesM
                         unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d354_321 ["spaces"]))
                         d356_322 <- get
                         xx355_323 <- dv_sourceTypeM
                         let s = xx355_323
                         unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d356_322 ["sourceType"]))
                         d358_324 <- get
                         xx357_325 <- dv_peg_M
                         let p = xx357_325
                         unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d358_324 ["peg_"]))
                         return (mkTTPeg s p),
                      do d360_326 <- get
                         xx359_327 <- dv_peg_M
                         let p = xx359_327
                         unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d360_326 ["peg_"]))
                         return (mkTTPeg tString p)]
p_sourceType = foldl1 mplus [do d362_328 <- get
                                xx361_329 <- dv_varTokenM
                                case xx361_329 of
                                    "source" -> return ()
                                    _ -> gets dvPos >>= (throwError . ParseError "\"source\"" "not match pattern: " "" d362_328 ["varToken"])
                                let "source" = xx361_329
                                return ()
                                unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d362_328 ["varToken"]))
                                d364_330 <- get
                                xx363_331 <- dvCharsM
                                case xx363_331 of
                                    ':' -> return ()
                                    _ -> gets dvPos >>= (throwError . ParseError "':'" "not match pattern: " "" d364_330 ["dvChars"])
                                let ':' = xx363_331
                                return ()
                                unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d364_330 ["dvChars"]))
                                d366_332 <- get
                                _ <- dv_spacesM
                                unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d366_332 ["spaces"]))
                                d368_333 <- get
                                xx367_334 <- dv_typTokenM
                                let v = xx367_334
                                unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d368_333 ["typToken"]))
                                return v]
p_peg_ = foldl1 mplus [do d370_335 <- get
                          _ <- dv_spacesM
                          unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d370_335 ["spaces"]))
                          d372_336 <- get
                          xx371_337 <- dv_definitionM
                          let d = xx371_337
                          unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d372_336 ["definition"]))
                          d374_338 <- get
                          xx373_339 <- dv_peg_M
                          let p = xx373_339
                          unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d374_338 ["peg_"]))
                          return (cons d p),
                       return emp]
p_definition = foldl1 mplus [do d376_340 <- get
                                xx375_341 <- dv_variableM
                                let v = xx375_341
                                unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d376_340 ["variable"]))
                                d378_342 <- get
                                _ <- dv_spacesM
                                unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d378_342 ["spaces"]))
                                d380_343 <- get
                                xx379_344 <- dvCharsM
                                case xx379_344 of
                                    ':' -> return ()
                                    _ -> gets dvPos >>= (throwError . ParseError "':'" "not match pattern: " "" d380_343 ["dvChars"])
                                let ':' = xx379_344
                                return ()
                                unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d380_343 ["dvChars"]))
                                d382_345 <- get
                                xx381_346 <- dvCharsM
                                case xx381_346 of
                                    ':' -> return ()
                                    _ -> gets dvPos >>= (throwError . ParseError "':'" "not match pattern: " "" d382_345 ["dvChars"])
                                let ':' = xx381_346
                                return ()
                                unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d382_345 ["dvChars"]))
                                d384_347 <- get
                                _ <- dv_spacesM
                                unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d384_347 ["spaces"]))
                                d386_348 <- get
                                xx385_349 <- dv_hsTypeArrM
                                let t = xx385_349
                                unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d386_348 ["hsTypeArr"]))
                                d388_350 <- get
                                _ <- dv_spacesM
                                unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d388_350 ["spaces"]))
                                d390_351 <- get
                                xx389_352 <- dvCharsM
                                case xx389_352 of
                                    '=' -> return ()
                                    _ -> gets dvPos >>= (throwError . ParseError "'='" "not match pattern: " "" d390_351 ["dvChars"])
                                let '=' = xx389_352
                                return ()
                                unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d390_351 ["dvChars"]))
                                d392_353 <- get
                                _ <- dv_spacesM
                                unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d392_353 ["spaces"]))
                                d394_354 <- get
                                xx393_355 <- dv_selectionM
                                let sel = xx393_355
                                unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d394_354 ["selection"]))
                                d396_356 <- get
                                _ <- dv_spacesM
                                unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d396_356 ["spaces"]))
                                d398_357 <- get
                                xx397_358 <- dvCharsM
                                case xx397_358 of
                                    ';' -> return ()
                                    _ -> gets dvPos >>= (throwError . ParseError "';'" "not match pattern: " "" d398_357 ["dvChars"])
                                let ';' = xx397_358
                                return ()
                                unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d398_357 ["dvChars"]))
                                return (mkDef v t sel)]
p_selection = foldl1 mplus [do d400_359 <- get
                               xx399_360 <- dv_expressionHsM
                               let ex = xx399_360
                               unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d400_359 ["expressionHs"]))
                               d402_361 <- get
                               _ <- dv_spacesM
                               unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d402_361 ["spaces"]))
                               d404_362 <- get
                               xx403_363 <- dvCharsM
                               case xx403_363 of
                                   '/' -> return ()
                                   _ -> gets dvPos >>= (throwError . ParseError "'/'" "not match pattern: " "" d404_362 ["dvChars"])
                               let '/' = xx403_363
                               return ()
                               unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d404_362 ["dvChars"]))
                               d406_364 <- get
                               _ <- dv_spacesM
                               unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d406_364 ["spaces"]))
                               d408_365 <- get
                               xx407_366 <- dv_selectionM
                               let sel = xx407_366
                               unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d408_365 ["selection"]))
                               return (cons ex sel),
                            do d410_367 <- get
                               xx409_368 <- dv_expressionHsM
                               let ex = xx409_368
                               unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d410_367 ["expressionHs"]))
                               return (cons ex emp)]
p_expressionHs = foldl1 mplus [do d412_369 <- get
                                  xx411_370 <- dv_expressionM
                                  let e = xx411_370
                                  unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d412_369 ["expression"]))
                                  d414_371 <- get
                                  _ <- dv_spacesM
                                  unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d414_371 ["spaces"]))
                                  d416_372 <- get
                                  xx415_373 <- dvCharsM
                                  case xx415_373 of
                                      '{' -> return ()
                                      _ -> gets dvPos >>= (throwError . ParseError "'{'" "not match pattern: " "" d416_372 ["dvChars"])
                                  let '{' = xx415_373
                                  return ()
                                  unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d416_372 ["dvChars"]))
                                  d418_374 <- get
                                  _ <- dv_spacesM
                                  unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d418_374 ["spaces"]))
                                  d420_375 <- get
                                  xx419_376 <- dv_hsExpLamM
                                  let h = xx419_376
                                  unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d420_375 ["hsExpLam"]))
                                  d422_377 <- get
                                  _ <- dv_spacesM
                                  unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d422_377 ["spaces"]))
                                  d424_378 <- get
                                  xx423_379 <- dvCharsM
                                  case xx423_379 of
                                      '}' -> return ()
                                      _ -> gets dvPos >>= (throwError . ParseError "'}'" "not match pattern: " "" d424_378 ["dvChars"])
                                  let '}' = xx423_379
                                  return ()
                                  unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d424_378 ["dvChars"]))
                                  return (mkExpressionHs e h)]
p_expression = foldl1 mplus [do d426_380 <- get
                                xx425_381 <- dv_nameLeaf_M
                                let l = xx425_381
                                unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d426_380 ["nameLeaf_"]))
                                d428_382 <- get
                                _ <- dv_spacesM
                                unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d428_382 ["spaces"]))
                                d430_383 <- get
                                xx429_384 <- dv_expressionM
                                let e = xx429_384
                                unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d430_383 ["expression"]))
                                return (cons l e),
                             return emp]
p_nameLeaf_ = foldl1 mplus [do d432_385 <- get
                               xx431_386 <- dvCharsM
                               case xx431_386 of
                                   '!' -> return ()
                                   _ -> gets dvPos >>= (throwError . ParseError "'!'" "not match pattern: " "" d432_385 ["dvChars"])
                               let '!' = xx431_386
                               return ()
                               unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d432_385 ["dvChars"]))
                               d434_387 <- get
                               xx433_388 <- dv_nameLeafNoComM
                               let nl = xx433_388
                               unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d434_387 ["nameLeafNoCom"]))
                               d436_389 <- get
                               _ <- dv_spacesM
                               unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d436_389 ["spaces"]))
                               d438_390 <- get
                               xx437_391 <- papOptional dv_comForErrM
                               let com = xx437_391
                               unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d438_390 ["comForErr"]))
                               return (NotAfter nl $ maybe "" id com),
                            do d440_392 <- get
                               xx439_393 <- dvCharsM
                               let c = xx439_393
                               unless (isAmp c) (gets dvPos >>= (throwError . ParseError "isAmp c" "not match: " "" d440_392 ["dvChars"]))
                               d442_394 <- get
                               xx441_395 <- dv_nameLeafM
                               let nl = xx441_395
                               unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d442_394 ["nameLeaf"]))
                               return (After nl),
                            do d444_396 <- get
                               xx443_397 <- dv_nameLeafM
                               let nl = xx443_397
                               unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d444_396 ["nameLeaf"]))
                               return (Here nl)]
p_nameLeaf = foldl1 mplus [do d446_398 <- get
                              xx445_399 <- dv_pat1M
                              let n = xx445_399
                              unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d446_398 ["pat1"]))
                              d448_400 <- get
                              _ <- dv_spacesM
                              unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d448_400 ["spaces"]))
                              d450_401 <- get
                              xx449_402 <- papOptional dv_comForErrM
                              let com = xx449_402
                              unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d450_401 ["comForErr"]))
                              d452_403 <- get
                              xx451_404 <- dvCharsM
                              case xx451_404 of
                                  ':' -> return ()
                                  _ -> gets dvPos >>= (throwError . ParseError "':'" "not match pattern: " "" d452_403 ["dvChars"])
                              let ':' = xx451_404
                              return ()
                              unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d452_403 ["dvChars"]))
                              d454_405 <- get
                              xx453_406 <- dv_leafM
                              let (rf, p) = xx453_406
                              unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d454_405 ["leaf"]))
                              return (NameLeaf (n, maybe "" id com) rf p),
                           do d456_407 <- get
                              xx455_408 <- dv_pat1M
                              let n = xx455_408
                              unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d456_407 ["pat1"]))
                              d458_409 <- get
                              _ <- dv_spacesM
                              unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d458_409 ["spaces"]))
                              d460_410 <- get
                              xx459_411 <- papOptional dv_comForErrM
                              let com = xx459_411
                              unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d460_410 ["comForErr"]))
                              return (NameLeaf (n,
                                                maybe "" id com) FromToken (conE $ mkName "True",
                                                                            ""))]
p_nameLeafNoCom = foldl1 mplus [do d462_412 <- get
                                   xx461_413 <- dv_pat1M
                                   let n = xx461_413
                                   unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d462_412 ["pat1"]))
                                   d464_414 <- get
                                   _ <- dv_spacesM
                                   unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d464_414 ["spaces"]))
                                   d466_415 <- get
                                   xx465_416 <- papOptional dv_comForErrM
                                   let com = xx465_416
                                   unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d466_415 ["comForErr"]))
                                   d468_417 <- get
                                   xx467_418 <- dvCharsM
                                   case xx467_418 of
                                       ':' -> return ()
                                       _ -> gets dvPos >>= (throwError . ParseError "':'" "not match pattern: " "" d468_417 ["dvChars"])
                                   let ':' = xx467_418
                                   return ()
                                   unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d468_417 ["dvChars"]))
                                   d470_419 <- get
                                   xx469_420 <- dv_leafM
                                   let (rf, p) = xx469_420
                                   unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d470_419 ["leaf"]))
                                   return (NameLeaf (n, maybe "" id com) rf p),
                                do d472_421 <- get
                                   xx471_422 <- dv_pat1M
                                   let n = xx471_422
                                   unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d472_421 ["pat1"]))
                                   d474_423 <- get
                                   _ <- dv_spacesM
                                   unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d474_423 ["spaces"]))
                                   return (NameLeaf (n, "") FromToken (conE $ mkName "True", ""))]
p_comForErr = foldl1 mplus [do d476_424 <- get
                               xx475_425 <- dvCharsM
                               case xx475_425 of
                                   '{' -> return ()
                                   _ -> gets dvPos >>= (throwError . ParseError "'{'" "not match pattern: " "" d476_424 ["dvChars"])
                               let '{' = xx475_425
                               return ()
                               unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d476_424 ["dvChars"]))
                               d478_426 <- get
                               xx477_427 <- dvCharsM
                               case xx477_427 of
                                   '-' -> return ()
                                   _ -> gets dvPos >>= (throwError . ParseError "'-'" "not match pattern: " "" d478_426 ["dvChars"])
                               let '-' = xx477_427
                               return ()
                               unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d478_426 ["dvChars"]))
                               d480_428 <- get
                               xx479_429 <- dvCharsM
                               case xx479_429 of
                                   '#' -> return ()
                                   _ -> gets dvPos >>= (throwError . ParseError "'#'" "not match pattern: " "" d480_428 ["dvChars"])
                               let '#' = xx479_429
                               return ()
                               unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d480_428 ["dvChars"]))
                               d482_430 <- get
                               _ <- dv_spacesM
                               unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d482_430 ["spaces"]))
                               d484_431 <- get
                               xx483_432 <- dvCharsM
                               case xx483_432 of
                                   '"' -> return ()
                                   _ -> gets dvPos >>= (throwError . ParseError "'\"'" "not match pattern: " "" d484_431 ["dvChars"])
                               let '"' = xx483_432
                               return ()
                               unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d484_431 ["dvChars"]))
                               d486_433 <- get
                               xx485_434 <- dv_stringLitM
                               let s = xx485_434
                               unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d486_433 ["stringLit"]))
                               d488_435 <- get
                               xx487_436 <- dvCharsM
                               case xx487_436 of
                                   '"' -> return ()
                                   _ -> gets dvPos >>= (throwError . ParseError "'\"'" "not match pattern: " "" d488_435 ["dvChars"])
                               let '"' = xx487_436
                               return ()
                               unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d488_435 ["dvChars"]))
                               d490_437 <- get
                               _ <- dv_spacesM
                               unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d490_437 ["spaces"]))
                               d492_438 <- get
                               xx491_439 <- dvCharsM
                               case xx491_439 of
                                   '#' -> return ()
                                   _ -> gets dvPos >>= (throwError . ParseError "'#'" "not match pattern: " "" d492_438 ["dvChars"])
                               let '#' = xx491_439
                               return ()
                               unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d492_438 ["dvChars"]))
                               d494_440 <- get
                               xx493_441 <- dvCharsM
                               case xx493_441 of
                                   '-' -> return ()
                                   _ -> gets dvPos >>= (throwError . ParseError "'-'" "not match pattern: " "" d494_440 ["dvChars"])
                               let '-' = xx493_441
                               return ()
                               unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d494_440 ["dvChars"]))
                               d496_442 <- get
                               xx495_443 <- dvCharsM
                               case xx495_443 of
                                   '}' -> return ()
                                   _ -> gets dvPos >>= (throwError . ParseError "'}'" "not match pattern: " "" d496_442 ["dvChars"])
                               let '}' = xx495_443
                               return ()
                               unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d496_442 ["dvChars"]))
                               d498_444 <- get
                               _ <- dv_spacesM
                               unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d498_444 ["spaces"]))
                               return s]
p_leaf = foldl1 mplus [do d500_445 <- get
                          xx499_446 <- dv_readFromLsM
                          let rf = xx499_446
                          unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d500_445 ["readFromLs"]))
                          d502_447 <- get
                          xx501_448 <- dv_testM
                          let t = xx501_448
                          unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d502_447 ["test"]))
                          return (rf, t),
                       do d504_449 <- get
                          xx503_450 <- dv_readFromLsM
                          let rf = xx503_450
                          unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d504_449 ["readFromLs"]))
                          return (rf, (true, "")),
                       do d506_451 <- get
                          xx505_452 <- dv_testM
                          let t = xx505_452
                          unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d506_451 ["test"]))
                          return (FromToken, t)]
p_patOp = foldl1 mplus [do d508_453 <- get
                           xx507_454 <- dv_patM
                           let p = xx507_454
                           unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d508_453 ["pat"]))
                           d510_455 <- get
                           xx509_456 <- dv_opConNameM
                           let o = xx509_456
                           unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d510_455 ["opConName"]))
                           d512_457 <- get
                           xx511_458 <- dv_patOpM
                           let po = xx511_458
                           unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d512_457 ["patOp"]))
                           return (uInfixP p o po),
                        do d514_459 <- get
                           xx513_460 <- dv_patM
                           let p = xx513_460
                           unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d514_459 ["pat"]))
                           d516_461 <- get
                           _ <- dv_spacesM
                           unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d516_461 ["spaces"]))
                           d518_462 <- get
                           xx517_463 <- dvCharsM
                           let q = xx517_463
                           unless (isBQ q) (gets dvPos >>= (throwError . ParseError "isBQ q" "not match: " "" d518_462 ["dvChars"]))
                           d520_464 <- get
                           xx519_465 <- dv_typM
                           let t = xx519_465
                           unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d520_464 ["typ"]))
                           d522_466 <- get
                           xx521_467 <- dvCharsM
                           let q_ = xx521_467
                           unless (isBQ q_) (gets dvPos >>= (throwError . ParseError "isBQ q_" "not match: " "" d522_466 ["dvChars"]))
                           d524_468 <- get
                           _ <- dv_spacesM
                           unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d524_468 ["spaces"]))
                           d526_469 <- get
                           xx525_470 <- dv_patOpM
                           let po = xx525_470
                           unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d526_469 ["patOp"]))
                           return (uInfixP p (mkName t) po),
                        do d528_471 <- get
                           xx527_472 <- dv_patM
                           let p = xx527_472
                           unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d528_471 ["pat"]))
                           return p]
p_pat = foldl1 mplus [do d530_473 <- get
                         xx529_474 <- dv_typM
                         let t = xx529_474
                         unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d530_473 ["typ"]))
                         d532_475 <- get
                         _ <- dv_spacesM
                         unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d532_475 ["spaces"]))
                         d534_476 <- get
                         xx533_477 <- dv_patsM
                         let ps = xx533_477
                         unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d534_476 ["pats"]))
                         return (conToPatQ t ps),
                      do d536_478 <- get
                         xx535_479 <- dvCharsM
                         case xx535_479 of
                             '(' -> return ()
                             _ -> gets dvPos >>= (throwError . ParseError "'('" "not match pattern: " "" d536_478 ["dvChars"])
                         let '(' = xx535_479
                         return ()
                         unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d536_478 ["dvChars"]))
                         d538_480 <- get
                         xx537_481 <- dv_opConNameM
                         let o = xx537_481
                         unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d538_480 ["opConName"]))
                         d540_482 <- get
                         xx539_483 <- dvCharsM
                         case xx539_483 of
                             ')' -> return ()
                             _ -> gets dvPos >>= (throwError . ParseError "')'" "not match pattern: " "" d540_482 ["dvChars"])
                         let ')' = xx539_483
                         return ()
                         unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d540_482 ["dvChars"]))
                         d542_484 <- get
                         _ <- dv_spacesM
                         unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d542_484 ["spaces"]))
                         d544_485 <- get
                         xx543_486 <- dv_patsM
                         let ps = xx543_486
                         unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d544_485 ["pats"]))
                         return (conP o ps),
                      do d546_487 <- get
                         xx545_488 <- dv_pat1M
                         let p = xx545_488
                         unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d546_487 ["pat1"]))
                         return p]
p_pat1 = foldl1 mplus [do d548_489 <- get
                          xx547_490 <- dv_typM
                          let t = xx547_490
                          unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d548_489 ["typ"]))
                          return (conToPatQ t emp),
                       do d550_491 <- get
                          xx549_492 <- dv_variableM
                          case xx549_492 of
                              "_" -> return ()
                              _ -> gets dvPos >>= (throwError . ParseError "\"_\"" "not match pattern: " "" d550_491 ["variable"])
                          let "_" = xx549_492
                          return ()
                          unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d550_491 ["variable"]))
                          return wildP,
                       do d552_493 <- get
                          xx551_494 <- dv_variableM
                          let n = xx551_494
                          unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d552_493 ["variable"]))
                          return (strToPatQ n),
                       do d554_495 <- get
                          xx553_496 <- dv_integerM
                          let i = xx553_496
                          unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d554_495 ["integer"]))
                          return (litP (integerL i)),
                       do d556_497 <- get
                          xx555_498 <- dvCharsM
                          case xx555_498 of
                              '-' -> return ()
                              _ -> gets dvPos >>= (throwError . ParseError "'-'" "not match pattern: " "" d556_497 ["dvChars"])
                          let '-' = xx555_498
                          return ()
                          unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d556_497 ["dvChars"]))
                          d558_499 <- get
                          _ <- dv_spacesM
                          unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d558_499 ["spaces"]))
                          d560_500 <- get
                          xx559_501 <- dv_integerM
                          let i = xx559_501
                          unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d560_500 ["integer"]))
                          return (litP (integerL $ negate i)),
                       do d562_502 <- get
                          xx561_503 <- dvCharsM
                          case xx561_503 of
                              '\'' -> return ()
                              _ -> gets dvPos >>= (throwError . ParseError "'\\''" "not match pattern: " "" d562_502 ["dvChars"])
                          let '\'' = xx561_503
                          return ()
                          unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d562_502 ["dvChars"]))
                          d564_504 <- get
                          xx563_505 <- dv_charLitM
                          let c = xx563_505
                          unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d564_504 ["charLit"]))
                          d566_506 <- get
                          xx565_507 <- dvCharsM
                          case xx565_507 of
                              '\'' -> return ()
                              _ -> gets dvPos >>= (throwError . ParseError "'\\''" "not match pattern: " "" d566_506 ["dvChars"])
                          let '\'' = xx565_507
                          return ()
                          unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d566_506 ["dvChars"]))
                          return (charP c),
                       do d568_508 <- get
                          xx567_509 <- dvCharsM
                          case xx567_509 of
                              '"' -> return ()
                              _ -> gets dvPos >>= (throwError . ParseError "'\"'" "not match pattern: " "" d568_508 ["dvChars"])
                          let '"' = xx567_509
                          return ()
                          unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d568_508 ["dvChars"]))
                          d570_510 <- get
                          xx569_511 <- dv_stringLitM
                          let s = xx569_511
                          unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d570_510 ["stringLit"]))
                          d572_512 <- get
                          xx571_513 <- dvCharsM
                          case xx571_513 of
                              '"' -> return ()
                              _ -> gets dvPos >>= (throwError . ParseError "'\"'" "not match pattern: " "" d572_512 ["dvChars"])
                          let '"' = xx571_513
                          return ()
                          unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d572_512 ["dvChars"]))
                          return (stringP s),
                       do d574_514 <- get
                          xx573_515 <- dvCharsM
                          case xx573_515 of
                              '(' -> return ()
                              _ -> gets dvPos >>= (throwError . ParseError "'('" "not match pattern: " "" d574_514 ["dvChars"])
                          let '(' = xx573_515
                          return ()
                          unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d574_514 ["dvChars"]))
                          d576_516 <- get
                          xx575_517 <- dv_patListM
                          let p = xx575_517
                          unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d576_516 ["patList"]))
                          d578_518 <- get
                          xx577_519 <- dvCharsM
                          case xx577_519 of
                              ')' -> return ()
                              _ -> gets dvPos >>= (throwError . ParseError "')'" "not match pattern: " "" d578_518 ["dvChars"])
                          let ')' = xx577_519
                          return ()
                          unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d578_518 ["dvChars"]))
                          return (tupP p),
                       do d580_520 <- get
                          xx579_521 <- dvCharsM
                          case xx579_521 of
                              '[' -> return ()
                              _ -> gets dvPos >>= (throwError . ParseError "'['" "not match pattern: " "" d580_520 ["dvChars"])
                          let '[' = xx579_521
                          return ()
                          unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d580_520 ["dvChars"]))
                          d582_522 <- get
                          xx581_523 <- dv_patListM
                          let p = xx581_523
                          unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d582_522 ["patList"]))
                          d584_524 <- get
                          xx583_525 <- dvCharsM
                          case xx583_525 of
                              ']' -> return ()
                              _ -> gets dvPos >>= (throwError . ParseError "']'" "not match pattern: " "" d584_524 ["dvChars"])
                          let ']' = xx583_525
                          return ()
                          unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d584_524 ["dvChars"]))
                          return (listP p)]
p_patList = foldl1 mplus [do d586_526 <- get
                             xx585_527 <- dv_patOpM
                             let p = xx585_527
                             unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d586_526 ["patOp"]))
                             d588_528 <- get
                             _ <- dv_spacesM
                             unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d588_528 ["spaces"]))
                             d590_529 <- get
                             xx589_530 <- dvCharsM
                             case xx589_530 of
                                 ',' -> return ()
                                 _ -> gets dvPos >>= (throwError . ParseError "','" "not match pattern: " "" d590_529 ["dvChars"])
                             let ',' = xx589_530
                             return ()
                             unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d590_529 ["dvChars"]))
                             d592_531 <- get
                             _ <- dv_spacesM
                             unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d592_531 ["spaces"]))
                             d594_532 <- get
                             xx593_533 <- dv_patListM
                             let ps = xx593_533
                             unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d594_532 ["patList"]))
                             return (p : ps),
                          do d596_534 <- get
                             xx595_535 <- dv_patOpM
                             let p = xx595_535
                             unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d596_534 ["patOp"]))
                             return [p],
                          return []]
p_opConName = foldl1 mplus [do d598_536 <- get
                               xx597_537 <- dvCharsM
                               case xx597_537 of
                                   ':' -> return ()
                                   _ -> gets dvPos >>= (throwError . ParseError "':'" "not match pattern: " "" d598_536 ["dvChars"])
                               let ':' = xx597_537
                               return ()
                               unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d598_536 ["dvChars"]))
                               d600_538 <- get
                               xx599_539 <- dv_opTailM
                               let ot = xx599_539
                               unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d600_538 ["opTail"]))
                               return (mkName $ colon : ot)]
p_charLit = foldl1 mplus [do d602_540 <- get
                             xx601_541 <- dvCharsM
                             let c = xx601_541
                             unless (isAlphaNumOt c) (gets dvPos >>= (throwError . ParseError "isAlphaNumOt c" "not match: " "" d602_540 ["dvChars"]))
                             return c,
                          do d604_542 <- get
                             xx603_543 <- dvCharsM
                             case xx603_543 of
                                 '\\' -> return ()
                                 _ -> gets dvPos >>= (throwError . ParseError "'\\\\'" "not match pattern: " "" d604_542 ["dvChars"])
                             let '\\' = xx603_543
                             return ()
                             unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d604_542 ["dvChars"]))
                             d606_544 <- get
                             xx605_545 <- dv_escapeCM
                             let c = xx605_545
                             unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d606_544 ["escapeC"]))
                             return c]
p_stringLit = foldl1 mplus [do d608_546 <- get
                               xx607_547 <- dvCharsM
                               let c = xx607_547
                               unless (isStrLitC c) (gets dvPos >>= (throwError . ParseError "isStrLitC c" "not match: " "" d608_546 ["dvChars"]))
                               d610_548 <- get
                               xx609_549 <- dv_stringLitM
                               let s = xx609_549
                               unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d610_548 ["stringLit"]))
                               return (cons c s),
                            do d612_550 <- get
                               xx611_551 <- dvCharsM
                               case xx611_551 of
                                   '\\' -> return ()
                                   _ -> gets dvPos >>= (throwError . ParseError "'\\\\'" "not match pattern: " "" d612_550 ["dvChars"])
                               let '\\' = xx611_551
                               return ()
                               unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d612_550 ["dvChars"]))
                               d614_552 <- get
                               xx613_553 <- dv_escapeCM
                               let c = xx613_553
                               unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d614_552 ["escapeC"]))
                               d616_554 <- get
                               xx615_555 <- dv_stringLitM
                               let s = xx615_555
                               unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d616_554 ["stringLit"]))
                               return (c : s),
                            return emp]
p_escapeC = foldl1 mplus [do d618_556 <- get
                             xx617_557 <- dvCharsM
                             case xx617_557 of
                                 '"' -> return ()
                                 _ -> gets dvPos >>= (throwError . ParseError "'\"'" "not match pattern: " "" d618_556 ["dvChars"])
                             let '"' = xx617_557
                             return ()
                             unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d618_556 ["dvChars"]))
                             return '"',
                          do d620_558 <- get
                             xx619_559 <- dvCharsM
                             case xx619_559 of
                                 '\'' -> return ()
                                 _ -> gets dvPos >>= (throwError . ParseError "'\\''" "not match pattern: " "" d620_558 ["dvChars"])
                             let '\'' = xx619_559
                             return ()
                             unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d620_558 ["dvChars"]))
                             return '\'',
                          do d622_560 <- get
                             xx621_561 <- dvCharsM
                             case xx621_561 of
                                 '\\' -> return ()
                                 _ -> gets dvPos >>= (throwError . ParseError "'\\\\'" "not match pattern: " "" d622_560 ["dvChars"])
                             let '\\' = xx621_561
                             return ()
                             unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d622_560 ["dvChars"]))
                             return '\\',
                          do d624_562 <- get
                             xx623_563 <- dvCharsM
                             case xx623_563 of
                                 'n' -> return ()
                                 _ -> gets dvPos >>= (throwError . ParseError "'n'" "not match pattern: " "" d624_562 ["dvChars"])
                             let 'n' = xx623_563
                             return ()
                             unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d624_562 ["dvChars"]))
                             return '\n',
                          do d626_564 <- get
                             xx625_565 <- dvCharsM
                             case xx625_565 of
                                 't' -> return ()
                                 _ -> gets dvPos >>= (throwError . ParseError "'t'" "not match pattern: " "" d626_564 ["dvChars"])
                             let 't' = xx625_565
                             return ()
                             unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d626_564 ["dvChars"]))
                             return tab]
p_pats = foldl1 mplus [do d628_566 <- get
                          xx627_567 <- dv_patM
                          let p = xx627_567
                          unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d628_566 ["pat"]))
                          d630_568 <- get
                          _ <- dv_spacesM
                          unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d630_568 ["spaces"]))
                          d632_569 <- get
                          xx631_570 <- dv_patsM
                          let ps = xx631_570
                          unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d632_569 ["pats"]))
                          return (cons p ps),
                       return emp]
p_readFromLs = foldl1 mplus [do d634_571 <- get
                                xx633_572 <- dv_readFromM
                                let rf = xx633_572
                                unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d634_571 ["readFrom"]))
                                d636_573 <- get
                                xx635_574 <- dvCharsM
                                case xx635_574 of
                                    '*' -> return ()
                                    _ -> gets dvPos >>= (throwError . ParseError "'*'" "not match pattern: " "" d636_573 ["dvChars"])
                                let '*' = xx635_574
                                return ()
                                unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d636_573 ["dvChars"]))
                                return (FromList rf),
                             do d638_575 <- get
                                xx637_576 <- dv_readFromM
                                let rf = xx637_576
                                unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d638_575 ["readFrom"]))
                                d640_577 <- get
                                xx639_578 <- dvCharsM
                                case xx639_578 of
                                    '+' -> return ()
                                    _ -> gets dvPos >>= (throwError . ParseError "'+'" "not match pattern: " "" d640_577 ["dvChars"])
                                let '+' = xx639_578
                                return ()
                                unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d640_577 ["dvChars"]))
                                return (FromList1 rf),
                             do d642_579 <- get
                                xx641_580 <- dv_readFromM
                                let rf = xx641_580
                                unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d642_579 ["readFrom"]))
                                d644_581 <- get
                                xx643_582 <- dvCharsM
                                case xx643_582 of
                                    '?' -> return ()
                                    _ -> gets dvPos >>= (throwError . ParseError "'?'" "not match pattern: " "" d644_581 ["dvChars"])
                                let '?' = xx643_582
                                return ()
                                unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d644_581 ["dvChars"]))
                                return (FromOptional rf),
                             do d646_583 <- get
                                xx645_584 <- dv_readFromM
                                let rf = xx645_584
                                unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d646_583 ["readFrom"]))
                                return rf]
p_readFrom = foldl1 mplus [do d648_585 <- get
                              xx647_586 <- dv_variableM
                              let v = xx647_586
                              unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d648_585 ["variable"]))
                              return (FromVariable v),
                           do d650_587 <- get
                              xx649_588 <- dvCharsM
                              case xx649_588 of
                                  '(' -> return ()
                                  _ -> gets dvPos >>= (throwError . ParseError "'('" "not match pattern: " "" d650_587 ["dvChars"])
                              let '(' = xx649_588
                              return ()
                              unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d650_587 ["dvChars"]))
                              d652_589 <- get
                              xx651_590 <- dv_selectionM
                              let s = xx651_590
                              unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d652_589 ["selection"]))
                              d654_591 <- get
                              xx653_592 <- dvCharsM
                              case xx653_592 of
                                  ')' -> return ()
                                  _ -> gets dvPos >>= (throwError . ParseError "')'" "not match pattern: " "" d654_591 ["dvChars"])
                              let ')' = xx653_592
                              return ()
                              unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d654_591 ["dvChars"]))
                              return (FromSelection s)]
p_test = foldl1 mplus [do d656_593 <- get
                          xx655_594 <- dvCharsM
                          case xx655_594 of
                              '[' -> return ()
                              _ -> gets dvPos >>= (throwError . ParseError "'['" "not match pattern: " "" d656_593 ["dvChars"])
                          let '[' = xx655_594
                          return ()
                          unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d656_593 ["dvChars"]))
                          d658_595 <- get
                          xx657_596 <- dv_hsExpLamM
                          let h = xx657_596
                          unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d658_595 ["hsExpLam"]))
                          d660_597 <- get
                          _ <- dv_spacesM
                          unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d660_597 ["spaces"]))
                          d662_598 <- get
                          xx661_599 <- papOptional dv_comForErrM
                          let com = xx661_599
                          unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d662_598 ["comForErr"]))
                          d664_600 <- get
                          xx663_601 <- dvCharsM
                          case xx663_601 of
                              ']' -> return ()
                              _ -> gets dvPos >>= (throwError . ParseError "']'" "not match pattern: " "" d664_600 ["dvChars"])
                          let ']' = xx663_601
                          return ()
                          unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d664_600 ["dvChars"]))
                          return (h, maybe "" id com)]
p_hsExpLam = foldl1 mplus [do d666_602 <- get
                              xx665_603 <- dvCharsM
                              case xx665_603 of
                                  '\\' -> return ()
                                  _ -> gets dvPos >>= (throwError . ParseError "'\\\\'" "not match pattern: " "" d666_602 ["dvChars"])
                              let '\\' = xx665_603
                              return ()
                              unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d666_602 ["dvChars"]))
                              d668_604 <- get
                              _ <- dv_spacesM
                              unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d668_604 ["spaces"]))
                              d670_605 <- get
                              xx669_606 <- dv_patsM
                              let ps = xx669_606
                              unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d670_605 ["pats"]))
                              d672_607 <- get
                              _ <- dv_spacesM
                              unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d672_607 ["spaces"]))
                              d674_608 <- get
                              xx673_609 <- dvCharsM
                              case xx673_609 of
                                  '-' -> return ()
                                  _ -> gets dvPos >>= (throwError . ParseError "'-'" "not match pattern: " "" d674_608 ["dvChars"])
                              let '-' = xx673_609
                              return ()
                              unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d674_608 ["dvChars"]))
                              d676_610 <- get
                              xx675_611 <- dvCharsM
                              let c = xx675_611
                              unless (isGt c) (gets dvPos >>= (throwError . ParseError "isGt c" "not match: " "" d676_610 ["dvChars"]))
                              d678_612 <- get
                              _ <- dv_spacesM
                              unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d678_612 ["spaces"]))
                              d680_613 <- get
                              xx679_614 <- dv_hsExpTypM
                              let e = xx679_614
                              unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d680_613 ["hsExpTyp"]))
                              return (lamE ps e),
                           do d682_615 <- get
                              xx681_616 <- dv_hsExpTypM
                              let e = xx681_616
                              unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d682_615 ["hsExpTyp"]))
                              return e]
p_hsExpTyp = foldl1 mplus [do d684_617 <- get
                              xx683_618 <- dv_hsExpOpM
                              let eo = xx683_618
                              unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d684_617 ["hsExpOp"]))
                              d686_619 <- get
                              xx685_620 <- dvCharsM
                              case xx685_620 of
                                  ':' -> return ()
                                  _ -> gets dvPos >>= (throwError . ParseError "':'" "not match pattern: " "" d686_619 ["dvChars"])
                              let ':' = xx685_620
                              return ()
                              unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d686_619 ["dvChars"]))
                              d688_621 <- get
                              xx687_622 <- dvCharsM
                              case xx687_622 of
                                  ':' -> return ()
                                  _ -> gets dvPos >>= (throwError . ParseError "':'" "not match pattern: " "" d688_621 ["dvChars"])
                              let ':' = xx687_622
                              return ()
                              unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d688_621 ["dvChars"]))
                              d690_623 <- get
                              _ <- dv_spacesM
                              unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d690_623 ["spaces"]))
                              d692_624 <- get
                              xx691_625 <- dv_hsTypeArrM
                              let t = xx691_625
                              unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d692_624 ["hsTypeArr"]))
                              return (sigE eo t),
                           do d694_626 <- get
                              xx693_627 <- dv_hsExpOpM
                              let eo = xx693_627
                              unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d694_626 ["hsExpOp"]))
                              return eo]
p_hsExpOp = foldl1 mplus [do d696_628 <- get
                             xx695_629 <- dv_hsExpM
                             let l = xx695_629
                             unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d696_628 ["hsExp"]))
                             d698_630 <- get
                             _ <- dv_spacesM
                             unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d698_630 ["spaces"]))
                             d700_631 <- get
                             xx699_632 <- dv_hsOpM
                             let o = xx699_632
                             unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d700_631 ["hsOp"]))
                             d702_633 <- get
                             _ <- dv_spacesM
                             unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d702_633 ["spaces"]))
                             d704_634 <- get
                             xx703_635 <- dv_hsExpOpM
                             let r = xx703_635
                             unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d704_634 ["hsExpOp"]))
                             return (uInfixE (getEx l) o r),
                          do d706_636 <- get
                             xx705_637 <- dv_hsExpM
                             let e = xx705_637
                             unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d706_636 ["hsExp"]))
                             return (getEx e)]
p_hsOp = foldl1 mplus [do d708_638 <- get
                          xx707_639 <- dvCharsM
                          let c = xx707_639
                          unless (isOpHeadChar c) (gets dvPos >>= (throwError . ParseError "isOpHeadChar c" "not match: " "" d708_638 ["dvChars"]))
                          d710_640 <- get
                          xx709_641 <- dv_opTailM
                          let o = xx709_641
                          unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d710_640 ["opTail"]))
                          return (varE (mkName (cons c o))),
                       do d712_642 <- get
                          xx711_643 <- dvCharsM
                          case xx711_643 of
                              ':' -> return ()
                              _ -> gets dvPos >>= (throwError . ParseError "':'" "not match pattern: " "" d712_642 ["dvChars"])
                          let ':' = xx711_643
                          return ()
                          unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d712_642 ["dvChars"]))
                          ddd713_644 <- get
                          do err <- ((do d715_645 <- get
                                         xx714_646 <- dvCharsM
                                         case xx714_646 of
                                             ':' -> return ()
                                             _ -> gets dvPos >>= (throwError . ParseError "':'" "not match pattern: " "" d715_645 ["dvChars"])
                                         let ':' = xx714_646
                                         return ()
                                         unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d715_645 ["dvChars"]))) >> return False) `catchError` const (return True)
                             unless err (gets dvPos >>= (throwError . ParseError ('!' : "':':[True]") "not match: " "" ddd713_644 ["dvChars"]))
                          put ddd713_644
                          d717_647 <- get
                          xx716_648 <- dv_opTailM
                          let o = xx716_648
                          unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d717_647 ["opTail"]))
                          return (conE (mkName (':' : o))),
                       do d719_649 <- get
                          xx718_650 <- dvCharsM
                          let c = xx718_650
                          unless (isBQ c) (gets dvPos >>= (throwError . ParseError "isBQ c" "not match: " "" d719_649 ["dvChars"]))
                          d721_651 <- get
                          xx720_652 <- dv_variableM
                          let v = xx720_652
                          unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d721_651 ["variable"]))
                          d723_653 <- get
                          xx722_654 <- dvCharsM
                          let c_ = xx722_654
                          unless (isBQ c_) (gets dvPos >>= (throwError . ParseError "isBQ c_" "not match: " "" d723_653 ["dvChars"]))
                          return (varE (mkName v)),
                       do d725_655 <- get
                          xx724_656 <- dvCharsM
                          let c = xx724_656
                          unless (isBQ c) (gets dvPos >>= (throwError . ParseError "isBQ c" "not match: " "" d725_655 ["dvChars"]))
                          d727_657 <- get
                          xx726_658 <- dv_typM
                          let t = xx726_658
                          unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d727_657 ["typ"]))
                          d729_659 <- get
                          xx728_660 <- dvCharsM
                          let c_ = xx728_660
                          unless (isBQ c_) (gets dvPos >>= (throwError . ParseError "isBQ c_" "not match: " "" d729_659 ["dvChars"]))
                          return (conE (mkName t))]
p_opTail = foldl1 mplus [do d731_661 <- get
                            xx730_662 <- dvCharsM
                            let c = xx730_662
                            unless (isOpTailChar c) (gets dvPos >>= (throwError . ParseError "isOpTailChar c" "not match: " "" d731_661 ["dvChars"]))
                            d733_663 <- get
                            xx732_664 <- dv_opTailM
                            let s = xx732_664
                            unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d733_663 ["opTail"]))
                            return (cons c s),
                         return emp]
p_hsExp = foldl1 mplus [do d735_665 <- get
                           xx734_666 <- dv_hsExp1M
                           let e = xx734_666
                           unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d735_665 ["hsExp1"]))
                           d737_667 <- get
                           _ <- dv_spacesM
                           unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d737_667 ["spaces"]))
                           d739_668 <- get
                           xx738_669 <- dv_hsExpM
                           let h = xx738_669
                           unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d739_668 ["hsExp"]))
                           return (applyExR e h),
                        do d741_670 <- get
                           xx740_671 <- dv_hsExp1M
                           let e = xx740_671
                           unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d741_670 ["hsExp1"]))
                           return (toEx e)]
p_hsExp1 = foldl1 mplus [do d743_672 <- get
                            xx742_673 <- dvCharsM
                            case xx742_673 of
                                '(' -> return ()
                                _ -> gets dvPos >>= (throwError . ParseError "'('" "not match pattern: " "" d743_672 ["dvChars"])
                            let '(' = xx742_673
                            return ()
                            unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d743_672 ["dvChars"]))
                            d745_674 <- get
                            xx744_675 <- papOptional (foldl1 mplus [do d747_676 <- get
                                                                       xx746_677 <- dv_hsExpTypM
                                                                       let e = xx746_677
                                                                       unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d747_676 ["hsExpTyp"]))
                                                                       return e])
                            let l = xx744_675
                            unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d745_674 ["hsExpTyp"]))
                            d749_678 <- get
                            _ <- dv_spacesM
                            unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d749_678 ["spaces"]))
                            d751_679 <- get
                            xx750_680 <- dv_hsOpM
                            let o = xx750_680
                            unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d751_679 ["hsOp"]))
                            d753_681 <- get
                            _ <- dv_spacesM
                            unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d753_681 ["spaces"]))
                            d755_682 <- get
                            xx754_683 <- papOptional (foldl1 mplus [do d757_684 <- get
                                                                       xx756_685 <- dv_hsExpTypM
                                                                       let e = xx756_685
                                                                       unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d757_684 ["hsExpTyp"]))
                                                                       return e])
                            let r = xx754_683
                            unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d755_682 ["hsExpTyp"]))
                            d759_686 <- get
                            xx758_687 <- dvCharsM
                            case xx758_687 of
                                ')' -> return ()
                                _ -> gets dvPos >>= (throwError . ParseError "')'" "not match pattern: " "" d759_686 ["dvChars"])
                            let ')' = xx758_687
                            return ()
                            unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d759_686 ["dvChars"]))
                            return (infixE l o r),
                         do d761_688 <- get
                            xx760_689 <- dvCharsM
                            case xx760_689 of
                                '(' -> return ()
                                _ -> gets dvPos >>= (throwError . ParseError "'('" "not match pattern: " "" d761_688 ["dvChars"])
                            let '(' = xx760_689
                            return ()
                            unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d761_688 ["dvChars"]))
                            d763_690 <- get
                            xx762_691 <- dv_hsExpTplM
                            let et = xx762_691
                            unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d763_690 ["hsExpTpl"]))
                            d765_692 <- get
                            xx764_693 <- dvCharsM
                            case xx764_693 of
                                ')' -> return ()
                                _ -> gets dvPos >>= (throwError . ParseError "')'" "not match pattern: " "" d765_692 ["dvChars"])
                            let ')' = xx764_693
                            return ()
                            unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d765_692 ["dvChars"]))
                            return (tupE et),
                         do d767_694 <- get
                            xx766_695 <- dvCharsM
                            case xx766_695 of
                                '[' -> return ()
                                _ -> gets dvPos >>= (throwError . ParseError "'['" "not match pattern: " "" d767_694 ["dvChars"])
                            let '[' = xx766_695
                            return ()
                            unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d767_694 ["dvChars"]))
                            d769_696 <- get
                            xx768_697 <- dv_hsExpTplM
                            let et = xx768_697
                            unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d769_696 ["hsExpTpl"]))
                            d771_698 <- get
                            xx770_699 <- dvCharsM
                            case xx770_699 of
                                ']' -> return ()
                                _ -> gets dvPos >>= (throwError . ParseError "']'" "not match pattern: " "" d771_698 ["dvChars"])
                            let ']' = xx770_699
                            return ()
                            unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d771_698 ["dvChars"]))
                            return (listE et),
                         do d773_700 <- get
                            xx772_701 <- dv_variableM
                            let v = xx772_701
                            unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d773_700 ["variable"]))
                            return (varE (mkName v)),
                         do d775_702 <- get
                            xx774_703 <- dv_typM
                            let t = xx774_703
                            unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d775_702 ["typ"]))
                            return (conE (mkName t)),
                         do d777_704 <- get
                            xx776_705 <- dv_integerM
                            let i = xx776_705
                            unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d777_704 ["integer"]))
                            d779_706 <- get
                            _ <- dv_spacesM
                            unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d779_706 ["spaces"]))
                            return (litE (integerL i)),
                         do d781_707 <- get
                            xx780_708 <- dvCharsM
                            case xx780_708 of
                                '\'' -> return ()
                                _ -> gets dvPos >>= (throwError . ParseError "'\\''" "not match pattern: " "" d781_707 ["dvChars"])
                            let '\'' = xx780_708
                            return ()
                            unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d781_707 ["dvChars"]))
                            d783_709 <- get
                            xx782_710 <- dv_charLitM
                            let c = xx782_710
                            unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d783_709 ["charLit"]))
                            d785_711 <- get
                            xx784_712 <- dvCharsM
                            case xx784_712 of
                                '\'' -> return ()
                                _ -> gets dvPos >>= (throwError . ParseError "'\\''" "not match pattern: " "" d785_711 ["dvChars"])
                            let '\'' = xx784_712
                            return ()
                            unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d785_711 ["dvChars"]))
                            return (litE (charL c)),
                         do d787_713 <- get
                            xx786_714 <- dvCharsM
                            case xx786_714 of
                                '"' -> return ()
                                _ -> gets dvPos >>= (throwError . ParseError "'\"'" "not match pattern: " "" d787_713 ["dvChars"])
                            let '"' = xx786_714
                            return ()
                            unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d787_713 ["dvChars"]))
                            d789_715 <- get
                            xx788_716 <- dv_stringLitM
                            let s = xx788_716
                            unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d789_715 ["stringLit"]))
                            d791_717 <- get
                            xx790_718 <- dvCharsM
                            case xx790_718 of
                                '"' -> return ()
                                _ -> gets dvPos >>= (throwError . ParseError "'\"'" "not match pattern: " "" d791_717 ["dvChars"])
                            let '"' = xx790_718
                            return ()
                            unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d791_717 ["dvChars"]))
                            return (litE (stringL s)),
                         do d793_719 <- get
                            xx792_720 <- dvCharsM
                            case xx792_720 of
                                '-' -> return ()
                                _ -> gets dvPos >>= (throwError . ParseError "'-'" "not match pattern: " "" d793_719 ["dvChars"])
                            let '-' = xx792_720
                            return ()
                            unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d793_719 ["dvChars"]))
                            d795_721 <- get
                            _ <- dv_spacesM
                            unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d795_721 ["spaces"]))
                            d797_722 <- get
                            xx796_723 <- dv_hsExp1M
                            let e = xx796_723
                            unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d797_722 ["hsExp1"]))
                            return (appE (varE $ mkName "negate") e)]
p_hsExpTpl = foldl1 mplus [do d799_724 <- get
                              xx798_725 <- dv_hsExpLamM
                              let e = xx798_725
                              unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d799_724 ["hsExpLam"]))
                              d801_726 <- get
                              _ <- dv_spacesM
                              unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d801_726 ["spaces"]))
                              d803_727 <- get
                              xx802_728 <- dvCharsM
                              let c = xx802_728
                              unless (isComma c) (gets dvPos >>= (throwError . ParseError "isComma c" "not match: " "" d803_727 ["dvChars"]))
                              d805_729 <- get
                              _ <- dv_spacesM
                              unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d805_729 ["spaces"]))
                              d807_730 <- get
                              xx806_731 <- dv_hsExpTplM
                              let et = xx806_731
                              unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d807_730 ["hsExpTpl"]))
                              return (cons e et),
                           do d809_732 <- get
                              xx808_733 <- dv_hsExpLamM
                              let e = xx808_733
                              unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d809_732 ["hsExpLam"]))
                              return (cons e emp),
                           return emp]
p_hsTypeArr = foldl1 mplus [do d811_734 <- get
                               xx810_735 <- dv_hsTypeM
                               let l = xx810_735
                               unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d811_734 ["hsType"]))
                               d813_736 <- get
                               xx812_737 <- dvCharsM
                               case xx812_737 of
                                   '-' -> return ()
                                   _ -> gets dvPos >>= (throwError . ParseError "'-'" "not match pattern: " "" d813_736 ["dvChars"])
                               let '-' = xx812_737
                               return ()
                               unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d813_736 ["dvChars"]))
                               d815_738 <- get
                               xx814_739 <- dvCharsM
                               let c = xx814_739
                               unless (isGt c) (gets dvPos >>= (throwError . ParseError "isGt c" "not match: " "" d815_738 ["dvChars"]))
                               d817_740 <- get
                               _ <- dv_spacesM
                               unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d817_740 ["spaces"]))
                               d819_741 <- get
                               xx818_742 <- dv_hsTypeArrM
                               let r = xx818_742
                               unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d819_741 ["hsTypeArr"]))
                               return (appT (appT arrowT (getTyp l)) r),
                            do d821_743 <- get
                               xx820_744 <- dv_hsTypeM
                               let t = xx820_744
                               unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d821_743 ["hsType"]))
                               return (getTyp t)]
p_hsType = foldl1 mplus [do d823_745 <- get
                            xx822_746 <- dv_hsType1M
                            let t = xx822_746
                            unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d823_745 ["hsType1"]))
                            d825_747 <- get
                            xx824_748 <- dv_hsTypeM
                            let ts = xx824_748
                            unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d825_747 ["hsType"]))
                            return (applyTyp (toTyp t) ts),
                         do d827_749 <- get
                            xx826_750 <- dv_hsType1M
                            let t = xx826_750
                            unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d827_749 ["hsType1"]))
                            return (toTyp t)]
p_hsType1 = foldl1 mplus [do d829_751 <- get
                             xx828_752 <- dvCharsM
                             case xx828_752 of
                                 '[' -> return ()
                                 _ -> gets dvPos >>= (throwError . ParseError "'['" "not match pattern: " "" d829_751 ["dvChars"])
                             let '[' = xx828_752
                             return ()
                             unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d829_751 ["dvChars"]))
                             d831_753 <- get
                             xx830_754 <- dvCharsM
                             case xx830_754 of
                                 ']' -> return ()
                                 _ -> gets dvPos >>= (throwError . ParseError "']'" "not match pattern: " "" d831_753 ["dvChars"])
                             let ']' = xx830_754
                             return ()
                             unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d831_753 ["dvChars"]))
                             d833_755 <- get
                             _ <- dv_spacesM
                             unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d833_755 ["spaces"]))
                             return listT,
                          do d835_756 <- get
                             xx834_757 <- dvCharsM
                             case xx834_757 of
                                 '[' -> return ()
                                 _ -> gets dvPos >>= (throwError . ParseError "'['" "not match pattern: " "" d835_756 ["dvChars"])
                             let '[' = xx834_757
                             return ()
                             unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d835_756 ["dvChars"]))
                             d837_758 <- get
                             xx836_759 <- dv_hsTypeArrM
                             let t = xx836_759
                             unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d837_758 ["hsTypeArr"]))
                             d839_760 <- get
                             xx838_761 <- dvCharsM
                             case xx838_761 of
                                 ']' -> return ()
                                 _ -> gets dvPos >>= (throwError . ParseError "']'" "not match pattern: " "" d839_760 ["dvChars"])
                             let ']' = xx838_761
                             return ()
                             unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d839_760 ["dvChars"]))
                             d841_762 <- get
                             _ <- dv_spacesM
                             unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d841_762 ["spaces"]))
                             return (appT listT t),
                          do d843_763 <- get
                             xx842_764 <- dvCharsM
                             case xx842_764 of
                                 '(' -> return ()
                                 _ -> gets dvPos >>= (throwError . ParseError "'('" "not match pattern: " "" d843_763 ["dvChars"])
                             let '(' = xx842_764
                             return ()
                             unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d843_763 ["dvChars"]))
                             d845_765 <- get
                             _ <- dv_spacesM
                             unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d845_765 ["spaces"]))
                             d847_766 <- get
                             xx846_767 <- dv_hsTypeTplM
                             let tt = xx846_767
                             unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d847_766 ["hsTypeTpl"]))
                             d849_768 <- get
                             xx848_769 <- dvCharsM
                             case xx848_769 of
                                 ')' -> return ()
                                 _ -> gets dvPos >>= (throwError . ParseError "')'" "not match pattern: " "" d849_768 ["dvChars"])
                             let ')' = xx848_769
                             return ()
                             unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d849_768 ["dvChars"]))
                             return (tupT tt),
                          do d851_770 <- get
                             xx850_771 <- dv_typTokenM
                             let t = xx850_771
                             unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d851_770 ["typToken"]))
                             return (conT (mkName t)),
                          do d853_772 <- get
                             xx852_773 <- dvCharsM
                             case xx852_773 of
                                 '(' -> return ()
                                 _ -> gets dvPos >>= (throwError . ParseError "'('" "not match pattern: " "" d853_772 ["dvChars"])
                             let '(' = xx852_773
                             return ()
                             unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d853_772 ["dvChars"]))
                             d855_774 <- get
                             xx854_775 <- dvCharsM
                             case xx854_775 of
                                 '-' -> return ()
                                 _ -> gets dvPos >>= (throwError . ParseError "'-'" "not match pattern: " "" d855_774 ["dvChars"])
                             let '-' = xx854_775
                             return ()
                             unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d855_774 ["dvChars"]))
                             d857_776 <- get
                             xx856_777 <- dvCharsM
                             let c = xx856_777
                             unless (isGt c) (gets dvPos >>= (throwError . ParseError "isGt c" "not match: " "" d857_776 ["dvChars"]))
                             d859_778 <- get
                             xx858_779 <- dvCharsM
                             case xx858_779 of
                                 ')' -> return ()
                                 _ -> gets dvPos >>= (throwError . ParseError "')'" "not match pattern: " "" d859_778 ["dvChars"])
                             let ')' = xx858_779
                             return ()
                             unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d859_778 ["dvChars"]))
                             d861_780 <- get
                             _ <- dv_spacesM
                             unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d861_780 ["spaces"]))
                             return arrowT]
p_hsTypeTpl = foldl1 mplus [do d863_781 <- get
                               xx862_782 <- dv_hsTypeArrM
                               let t = xx862_782
                               unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d863_781 ["hsTypeArr"]))
                               d865_783 <- get
                               xx864_784 <- dvCharsM
                               let c = xx864_784
                               unless (isComma c) (gets dvPos >>= (throwError . ParseError "isComma c" "not match: " "" d865_783 ["dvChars"]))
                               d867_785 <- get
                               _ <- dv_spacesM
                               unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d867_785 ["spaces"]))
                               d869_786 <- get
                               xx868_787 <- dv_hsTypeTplM
                               let tt = xx868_787
                               unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d869_786 ["hsTypeTpl"]))
                               return (cons t tt),
                            do d871_788 <- get
                               xx870_789 <- dv_hsTypeArrM
                               let t = xx870_789
                               unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d871_788 ["hsTypeArr"]))
                               return (cons t emp),
                            return emp]
p_typ = foldl1 mplus [do d873_790 <- get
                         xx872_791 <- dv_upperM
                         let u = xx872_791
                         unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d873_790 ["upper"]))
                         d875_792 <- get
                         xx874_793 <- dv_tvtailM
                         let t = xx874_793
                         unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d875_792 ["tvtail"]))
                         return (cons u t)]
p_variable = foldl1 mplus [do d877_794 <- get
                              xx876_795 <- dv_lowerM
                              let l = xx876_795
                              unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d877_794 ["lower"]))
                              d879_796 <- get
                              xx878_797 <- dv_tvtailM
                              let t = xx878_797
                              unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d879_796 ["tvtail"]))
                              return (cons l t)]
p_tvtail = foldl1 mplus [do d881_798 <- get
                            xx880_799 <- dv_alphaM
                            let a = xx880_799
                            unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d881_798 ["alpha"]))
                            d883_800 <- get
                            xx882_801 <- dv_tvtailM
                            let t = xx882_801
                            unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d883_800 ["tvtail"]))
                            return (cons a t),
                         return emp]
p_integer = foldl1 mplus [do d885_802 <- get
                             xx884_803 <- dv_digitM
                             let dh = xx884_803
                             unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d885_802 ["digit"]))
                             d887_804 <- get
                             xx886_805 <- list (foldl1 mplus [do d889_806 <- get
                                                                 xx888_807 <- dv_digitM
                                                                 let d = xx888_807
                                                                 unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d889_806 ["digit"]))
                                                                 return d])
                             let ds = xx886_805
                             unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d887_804 ["digit"]))
                             return (read (cons dh ds))]
p_alpha = foldl1 mplus [do d891_808 <- get
                           xx890_809 <- dv_upperM
                           let u = xx890_809
                           unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d891_808 ["upper"]))
                           return u,
                        do d893_810 <- get
                           xx892_811 <- dv_lowerM
                           let l = xx892_811
                           unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d893_810 ["lower"]))
                           return l,
                        do d895_812 <- get
                           xx894_813 <- dv_digitM
                           let d = xx894_813
                           unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d895_812 ["digit"]))
                           return d,
                        do d897_814 <- get
                           xx896_815 <- dvCharsM
                           case xx896_815 of
                               '\'' -> return ()
                               _ -> gets dvPos >>= (throwError . ParseError "'\\''" "not match pattern: " "" d897_814 ["dvChars"])
                           let '\'' = xx896_815
                           return ()
                           unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d897_814 ["dvChars"]))
                           return '\'']
p_upper = foldl1 mplus [do d899_816 <- get
                           xx898_817 <- dvCharsM
                           let u = xx898_817
                           unless (isUpper u) (gets dvPos >>= (throwError . ParseError "isUpper u" "not match: " "" d899_816 ["dvChars"]))
                           return u]
p_lower = foldl1 mplus [do d901_818 <- get
                           xx900_819 <- dvCharsM
                           let l = xx900_819
                           unless (isLowerU l) (gets dvPos >>= (throwError . ParseError "isLowerU l" "not match: " "" d901_818 ["dvChars"]))
                           return l]
p_digit = foldl1 mplus [do d903_820 <- get
                           xx902_821 <- dvCharsM
                           let d = xx902_821
                           unless (isDigit d) (gets dvPos >>= (throwError . ParseError "isDigit d" "not match: " "" d903_820 ["dvChars"]))
                           return d]
p_spaces = foldl1 mplus [do d905_822 <- get
                            _ <- dv_spaceM
                            unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d905_822 ["space"]))
                            d907_823 <- get
                            _ <- dv_spacesM
                            unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d907_823 ["spaces"]))
                            return (),
                         return ()]
p_space = foldl1 mplus [do d909_824 <- get
                           xx908_825 <- dvCharsM
                           let s = xx908_825
                           unless (isSpace s) (gets dvPos >>= (throwError . ParseError "isSpace s" "not match: " "" d909_824 ["dvChars"]))
                           return (),
                        do d911_826 <- get
                           xx910_827 <- dvCharsM
                           case xx910_827 of
                               '-' -> return ()
                               _ -> gets dvPos >>= (throwError . ParseError "'-'" "not match pattern: " "" d911_826 ["dvChars"])
                           let '-' = xx910_827
                           return ()
                           unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d911_826 ["dvChars"]))
                           d913_828 <- get
                           xx912_829 <- dvCharsM
                           case xx912_829 of
                               '-' -> return ()
                               _ -> gets dvPos >>= (throwError . ParseError "'-'" "not match pattern: " "" d913_828 ["dvChars"])
                           let '-' = xx912_829
                           return ()
                           unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d913_828 ["dvChars"]))
                           d915_830 <- get
                           _ <- dv_notNLStringM
                           unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d915_830 ["notNLString"]))
                           d917_831 <- get
                           _ <- dv_newLineM
                           unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d917_831 ["newLine"]))
                           return (),
                        do d919_832 <- get
                           _ <- dv_commentM
                           unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d919_832 ["comment"]))
                           return ()]
p_notNLString = foldl1 mplus [do ddd920_833 <- get
                                 do err <- ((do d922_834 <- get
                                                _ <- dv_newLineM
                                                unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d922_834 ["newLine"]))) >> return False) `catchError` const (return True)
                                    unless err (gets dvPos >>= (throwError . ParseError ('!' : "_:newLine[True]") "not match: " "" ddd920_833 ["newLine"]))
                                 put ddd920_833
                                 d924_835 <- get
                                 xx923_836 <- dvCharsM
                                 let c = xx923_836
                                 unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d924_835 ["dvChars"]))
                                 d926_837 <- get
                                 xx925_838 <- dv_notNLStringM
                                 let s = xx925_838
                                 unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d926_837 ["notNLString"]))
                                 return (cons c s),
                              return emp]
p_newLine = foldl1 mplus [do d928_839 <- get
                             xx927_840 <- dvCharsM
                             case xx927_840 of
                                 '\n' -> return ()
                                 _ -> gets dvPos >>= (throwError . ParseError "'\\n'" "not match pattern: " "" d928_839 ["dvChars"])
                             let '\n' = xx927_840
                             return ()
                             unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d928_839 ["dvChars"]))
                             return ()]
p_comment = foldl1 mplus [do d930_841 <- get
                             xx929_842 <- dvCharsM
                             case xx929_842 of
                                 '{' -> return ()
                                 _ -> gets dvPos >>= (throwError . ParseError "'{'" "not match pattern: " "" d930_841 ["dvChars"])
                             let '{' = xx929_842
                             return ()
                             unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d930_841 ["dvChars"]))
                             d932_843 <- get
                             xx931_844 <- dvCharsM
                             case xx931_844 of
                                 '-' -> return ()
                                 _ -> gets dvPos >>= (throwError . ParseError "'-'" "not match pattern: " "" d932_843 ["dvChars"])
                             let '-' = xx931_844
                             return ()
                             unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d932_843 ["dvChars"]))
                             ddd933_845 <- get
                             do err <- ((do d935_846 <- get
                                            xx934_847 <- dvCharsM
                                            case xx934_847 of
                                                '#' -> return ()
                                                _ -> gets dvPos >>= (throwError . ParseError "'#'" "not match pattern: " "" d935_846 ["dvChars"])
                                            let '#' = xx934_847
                                            return ()
                                            unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d935_846 ["dvChars"]))) >> return False) `catchError` const (return True)
                                unless err (gets dvPos >>= (throwError . ParseError ('!' : "'#':[True]") "not match: " "" ddd933_845 ["dvChars"]))
                             put ddd933_845
                             d937_848 <- get
                             _ <- dv_commentsM
                             unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d937_848 ["comments"]))
                             d939_849 <- get
                             _ <- dv_comEndM
                             unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d939_849 ["comEnd"]))
                             return ()]
p_comments = foldl1 mplus [do d941_850 <- get
                              _ <- dv_notComStrM
                              unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d941_850 ["notComStr"]))
                              d943_851 <- get
                              _ <- dv_commentM
                              unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d943_851 ["comment"]))
                              d945_852 <- get
                              _ <- dv_commentsM
                              unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d945_852 ["comments"]))
                              return (),
                           do d947_853 <- get
                              _ <- dv_notComStrM
                              unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d947_853 ["notComStr"]))
                              return ()]
p_notComStr = foldl1 mplus [do ddd948_854 <- get
                               do err <- ((do d950_855 <- get
                                              _ <- dv_commentM
                                              unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d950_855 ["comment"]))) >> return False) `catchError` const (return True)
                                  unless err (gets dvPos >>= (throwError . ParseError ('!' : "_:comment[True]") "not match: " "" ddd948_854 ["comment"]))
                               put ddd948_854
                               ddd951_856 <- get
                               do err <- ((do d953_857 <- get
                                              _ <- dv_comEndM
                                              unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d953_857 ["comEnd"]))) >> return False) `catchError` const (return True)
                                  unless err (gets dvPos >>= (throwError . ParseError ('!' : "_:comEnd[True]") "not match: " "" ddd951_856 ["comEnd"]))
                               put ddd951_856
                               d955_858 <- get
                               _ <- dvCharsM
                               unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d955_858 ["dvChars"]))
                               d957_859 <- get
                               _ <- dv_notComStrM
                               unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d957_859 ["notComStr"]))
                               return (),
                            return ()]
p_comEnd = foldl1 mplus [do d959_860 <- get
                            xx958_861 <- dvCharsM
                            case xx958_861 of
                                '-' -> return ()
                                _ -> gets dvPos >>= (throwError . ParseError "'-'" "not match pattern: " "" d959_860 ["dvChars"])
                            let '-' = xx958_861
                            return ()
                            unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d959_860 ["dvChars"]))
                            d961_862 <- get
                            xx960_863 <- dvCharsM
                            case xx960_863 of
                                '}' -> return ()
                                _ -> gets dvPos >>= (throwError . ParseError "'}'" "not match pattern: " "" d961_862 ["dvChars"])
                            let '}' = xx960_863
                            return ()
                            unless True (gets dvPos >>= (throwError . ParseError "True" "not match: " "" d961_862 ["dvChars"]))
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