{-# LANGUAGE FlexibleContexts, TemplateHaskell, UndecidableInstances FlexibleContexts, PackageImports, TypeFamilies, RankNTypes, FlexibleInstances #-}
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
	Derivs(..),
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
    = Derivs {dv_pegFile :: (Result PegFile),
              dv_pragma :: (Result (Maybe String)),
              dv_pragmaStr :: (Result String),
              dv_delPragmas :: (Result ()),
              dv_pragmaEnd :: (Result ()),
              dv_moduleDec :: (Result (Maybe String)),
              dv_moduleDecStr :: (Result String),
              dv_whr :: (Result ()),
              dv_preImpPap :: (Result String),
              dv_prePeg :: (Result String),
              dv_afterPeg :: (Result String),
              dv_importPapillon :: (Result ()),
              dv_varToken :: (Result String),
              dv_typToken :: (Result String),
              dv_pap :: (Result ()),
              dv_peg :: (Result TTPeg),
              dv_sourceType :: (Result String),
              dv_peg_ :: (Result Peg),
              dv_definition :: (Result Definition),
              dv_selection :: (Result Selection),
              dv_expressionHs :: (Result ExpressionHs),
              dv_expression :: (Result Expression),
              dv_nameLeaf_ :: (Result NameLeaf_),
              dv_nameLeaf :: (Result NameLeaf),
              dv_nameLeafNoCom :: (Result NameLeaf),
              dv_comForErr :: (Result String),
              dv_leaf :: (Result ((ReadFrom, (ExpQ, String)))),
              dv_patOp :: (Result PatQ),
              dv_pat :: (Result PatQ),
              dv_pat1 :: (Result PatQ),
              dv_patList :: (Result ([PatQ])),
              dv_opConName :: (Result Name),
              dv_charLit :: (Result Char),
              dv_stringLit :: (Result String),
              dv_escapeC :: (Result Char),
              dv_dq :: (Result ()),
              dv_pats :: (Result PatQs),
              dv_readFromLs :: (Result ReadFrom),
              dv_readFrom :: (Result ReadFrom),
              dv_test :: (Result ((ExR, String))),
              dv_hsExpLam :: (Result ExR),
              dv_hsExpTyp :: (Result ExR),
              dv_hsExpOp :: (Result ExR),
              dv_hsOp :: (Result ExR),
              dv_opTail :: (Result String),
              dv_hsExp :: (Result Ex),
              dv_hsExp1 :: (Result ExR),
              dv_hsExpTpl :: (Result ExRL),
              dv_hsTypeArr :: (Result TypeQ),
              dv_hsType :: (Result Typ),
              dv_hsType1 :: (Result TypeQ),
              dv_hsTypeTpl :: (Result TypeQL),
              dv_typ :: (Result String),
              dv_variable :: (Result String),
              dv_tvtail :: (Result String),
              dv_integer :: (Result Integer),
              dv_alpha :: (Result Char),
              dv_upper :: (Result Char),
              dv_lower :: (Result Char),
              dv_digit :: (Result Char),
              dv_spaces :: (Result ()),
              dv_space :: (Result ()),
              dv_notNLString :: (Result String),
              dv_nl :: (Result ()),
              dv_comment :: (Result ()),
              dv_comments :: (Result ()),
              dv_notComStr :: (Result ()),
              dv_comEnd :: (Result ()),
              dvChars :: (Result (Token String)),
              dvPos :: (Pos String)}
type Result v = Either (ParseError (Pos String)) ((v, Derivs))
type PackratM = StateT Derivs (Either (ParseError (Pos String)))
data ParseError pos
    = ParseError String String String pos Derivs ([String])
instance Error (ParseError pos)
    where strMsg msg = ParseError "" msg "" undefined undefined undefined
dv_pragmaM :: PackratM (Maybe String)
dv_pragmaStrM :: PackratM String
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
dv_nlM :: PackratM ()
dv_commentM :: PackratM ()
dv_commentsM :: PackratM ()
dv_notComStrM :: PackratM ()
dv_comEndM :: PackratM ()
dv_pragmaM = StateT dv_pragma
dv_pragmaStrM = StateT dv_pragmaStr
dv_delPragmasM = StateT dv_delPragmas
dv_pragmaEndM = StateT dv_pragmaEnd
dv_moduleDecM = StateT dv_moduleDec
dv_moduleDecStrM = StateT dv_moduleDecStr
dv_whrM = StateT dv_whr
dv_preImpPapM = StateT dv_preImpPap
dv_prePegM = StateT dv_prePeg
dv_afterPegM = StateT dv_afterPeg
dv_importPapillonM = StateT dv_importPapillon
dv_varTokenM = StateT dv_varToken
dv_typTokenM = StateT dv_typToken
dv_papM = StateT dv_pap
dv_pegM = StateT dv_peg
dv_sourceTypeM = StateT dv_sourceType
dv_peg_M = StateT dv_peg_
dv_definitionM = StateT dv_definition
dv_selectionM = StateT dv_selection
dv_expressionHsM = StateT dv_expressionHs
dv_expressionM = StateT dv_expression
dv_nameLeaf_M = StateT dv_nameLeaf_
dv_nameLeafM = StateT dv_nameLeaf
dv_nameLeafNoComM = StateT dv_nameLeafNoCom
dv_comForErrM = StateT dv_comForErr
dv_leafM = StateT dv_leaf
dv_patOpM = StateT dv_patOp
dv_patM = StateT dv_pat
dv_pat1M = StateT dv_pat1
dv_patListM = StateT dv_patList
dv_opConNameM = StateT dv_opConName
dv_charLitM = StateT dv_charLit
dv_stringLitM = StateT dv_stringLit
dv_escapeCM = StateT dv_escapeC
dv_patsM = StateT dv_pats
dv_readFromLsM = StateT dv_readFromLs
dv_readFromM = StateT dv_readFrom
dv_testM = StateT dv_test
dv_hsExpLamM = StateT dv_hsExpLam
dv_hsExpTypM = StateT dv_hsExpTyp
dv_hsExpOpM = StateT dv_hsExpOp
dv_hsOpM = StateT dv_hsOp
dv_opTailM = StateT dv_opTail
dv_hsExpM = StateT dv_hsExp
dv_hsExp1M = StateT dv_hsExp1
dv_hsExpTplM = StateT dv_hsExpTpl
dv_hsTypeArrM = StateT dv_hsTypeArr
dv_hsTypeM = StateT dv_hsType
dv_hsType1M = StateT dv_hsType1
dv_hsTypeTplM = StateT dv_hsTypeTpl
dv_typM = StateT dv_typ
dv_variableM = StateT dv_variable
dv_tvtailM = StateT dv_tvtail
dv_integerM = StateT dv_integer
dv_alphaM = StateT dv_alpha
dv_upperM = StateT dv_upper
dv_lowerM = StateT dv_lower
dv_digitM = StateT dv_digit
dv_spacesM = StateT dv_spaces
dv_spaceM = StateT dv_space
dv_notNLStringM = StateT dv_notNLString
dv_nlM = StateT dv_nl
dv_commentM = StateT dv_comment
dv_commentsM = StateT dv_comments
dv_notComStrM = StateT dv_notComStr
dv_comEndM = StateT dv_comEnd
dvCharsM :: PackratM (Token String)
dvCharsM = StateT dvChars
parse :: Pos String -> String -> Derivs
parse pos s = d
          where d = Derivs pegFile pragma pragmaStr delPragmas pragmaEnd moduleDec moduleDecStr whr preImpPap prePeg afterPeg importPapillon varToken typToken pap peg sourceType peg_ definition selection expressionHs expression nameLeaf_ nameLeaf nameLeafNoCom comForErr leaf patOp pat pat1 patList opConName charLit stringLit escapeC dq pats readFromLs readFrom test hsExpLam hsExpTyp hsExpOp hsOp opTail hsExp hsExp1 hsExpTpl hsTypeArr hsType hsType1 hsTypeTpl typ variable tvtail integer alpha upper lower digit spaces space notNLString nl comment comments notComStr comEnd char pos
                pegFile = runStateT p_pegFile d
                pragma = runStateT p_pragma d
                pragmaStr = runStateT p_pragmaStr d
                delPragmas = runStateT p_delPragmas d
                pragmaEnd = runStateT p_pragmaEnd d
                moduleDec = runStateT p_moduleDec d
                moduleDecStr = runStateT p_moduleDecStr d
                whr = runStateT p_whr d
                preImpPap = runStateT p_preImpPap d
                prePeg = runStateT p_prePeg d
                afterPeg = runStateT p_afterPeg d
                importPapillon = runStateT p_importPapillon d
                varToken = runStateT p_varToken d
                typToken = runStateT p_typToken d
                pap = runStateT p_pap d
                peg = runStateT p_peg d
                sourceType = runStateT p_sourceType d
                peg_ = runStateT p_peg_ d
                definition = runStateT p_definition d
                selection = runStateT p_selection d
                expressionHs = runStateT p_expressionHs d
                expression = runStateT p_expression d
                nameLeaf_ = runStateT p_nameLeaf_ d
                nameLeaf = runStateT p_nameLeaf d
                nameLeafNoCom = runStateT p_nameLeafNoCom d
                comForErr = runStateT p_comForErr d
                leaf = runStateT p_leaf d
                patOp = runStateT p_patOp d
                pat = runStateT p_pat d
                pat1 = runStateT p_pat1 d
                patList = runStateT p_patList d
                opConName = runStateT p_opConName d
                charLit = runStateT p_charLit d
                stringLit = runStateT p_stringLit d
                escapeC = runStateT p_escapeC d
                dq = runStateT p_dq d
                pats = runStateT p_pats d
                readFromLs = runStateT p_readFromLs d
                readFrom = runStateT p_readFrom d
                test = runStateT p_test d
                hsExpLam = runStateT p_hsExpLam d
                hsExpTyp = runStateT p_hsExpTyp d
                hsExpOp = runStateT p_hsExpOp d
                hsOp = runStateT p_hsOp d
                opTail = runStateT p_opTail d
                hsExp = runStateT p_hsExp d
                hsExp1 = runStateT p_hsExp1 d
                hsExpTpl = runStateT p_hsExpTpl d
                hsTypeArr = runStateT p_hsTypeArr d
                hsType = runStateT p_hsType d
                hsType1 = runStateT p_hsType1 d
                hsTypeTpl = runStateT p_hsTypeTpl d
                typ = runStateT p_typ d
                variable = runStateT p_variable d
                tvtail = runStateT p_tvtail d
                integer = runStateT p_integer d
                alpha = runStateT p_alpha d
                upper = runStateT p_upper d
                lower = runStateT p_lower d
                digit = runStateT p_digit d
                spaces = runStateT p_spaces d
                space = runStateT p_space d
                notNLString = runStateT p_notNLString d
                nl = runStateT p_nl d
                comment = runStateT p_comment d
                comments = runStateT p_comments d
                notComStr = runStateT p_notComStr d
                comEnd = runStateT p_comEnd d
                char = runStateT (case getToken s of
                                      Just (c, s') -> do put (parse (updatePos c pos) s')
                                                         return c
                                      _ -> throwErrorPackratM "" "end of input" [] undefined "") d
p_pegFile :: PackratM PegFile
p_pragma :: PackratM (Maybe String)
p_pragmaStr :: PackratM String
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
p_dq :: PackratM ()
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
p_nl :: PackratM ()
p_comment :: PackratM ()
p_comments :: PackratM ()
p_notComStr :: PackratM ()
p_comEnd :: PackratM ()
p_pegFile = foldl1 mplus [do d1_0 <- get
                             xx0_1 <- dv_pragmaM
                             let pr = xx0_1
                             unless True (throwErrorPackratM "True" "not match: " ["dv_pragma"] d1_0 "")
                             d3_2 <- get
                             xx2_3 <- dv_moduleDecM
                             let md = xx2_3
                             unless True (throwErrorPackratM "True" "not match: " ["dv_moduleDec"] d3_2 "")
                             d5_4 <- get
                             xx4_5 <- dv_preImpPapM
                             let pip = xx4_5
                             unless True (throwErrorPackratM "True" "not match: " ["dv_preImpPap"] d5_4 "")
                             d7_6 <- get
                             _ <- dv_importPapillonM
                             unless True (throwErrorPackratM "True" "not match: " ["dv_importPapillon"] d7_6 "")
                             d9_7 <- get
                             xx8_8 <- dv_prePegM
                             let pp = xx8_8
                             unless True (throwErrorPackratM "True" "not match: " ["dv_prePeg"] d9_7 "")
                             d11_9 <- get
                             _ <- dv_papM
                             unless True (throwErrorPackratM "True" "not match: " ["dv_pap"] d11_9 "")
                             d13_10 <- get
                             xx12_11 <- dv_pegM
                             let p = xx12_11
                             unless True (throwErrorPackratM "True" "not match: " ["dv_peg"] d13_10 "")
                             d15_12 <- get
                             _ <- dv_spacesM
                             unless True (throwErrorPackratM "True" "not match: " ["dv_spaces"] d15_12 "")
                             d17_13 <- get
                             xx16_14 <- dvCharsM
                             case xx16_14 of
                                 '|' -> return ()
                                 _ -> throwErrorPackratM "'|'" "not match pattern: " ["dvChars"] d17_13 ""
                             let '|' = xx16_14
                             return ()
                             unless True (throwErrorPackratM "True" "not match: " ["dvChars"] d17_13 "")
                             d19_15 <- get
                             xx18_16 <- dvCharsM
                             case xx18_16 of
                                 ']' -> return ()
                                 _ -> throwErrorPackratM "']'" "not match pattern: " ["dvChars"] d19_15 ""
                             let ']' = xx18_16
                             return ()
                             unless True (throwErrorPackratM "True" "not match: " ["dvChars"] d19_15 "")
                             d21_17 <- get
                             xx20_18 <- dvCharsM
                             case xx20_18 of
                                 '\n' -> return ()
                                 _ -> throwErrorPackratM "'\\n'" "not match pattern: " ["dvChars"] d21_17 ""
                             let '\n' = xx20_18
                             return ()
                             unless True (throwErrorPackratM "True" "not match: " ["dvChars"] d21_17 "")
                             d23_19 <- get
                             xx22_20 <- dv_afterPegM
                             let atp = xx22_20
                             unless True (throwErrorPackratM "True" "not match: " ["dv_afterPeg"] d23_19 "")
                             return (mkPegFile pr md pip pp p atp),
                          do d25_21 <- get
                             xx24_22 <- dv_pragmaM
                             let pr = xx24_22
                             unless True (throwErrorPackratM "True" "not match: " ["dv_pragma"] d25_21 "")
                             d27_23 <- get
                             xx26_24 <- dv_moduleDecM
                             let md = xx26_24
                             unless True (throwErrorPackratM "True" "not match: " ["dv_moduleDec"] d27_23 "")
                             d29_25 <- get
                             xx28_26 <- dv_prePegM
                             let pp = xx28_26
                             unless True (throwErrorPackratM "True" "not match: " ["dv_prePeg"] d29_25 "")
                             d31_27 <- get
                             _ <- dv_papM
                             unless True (throwErrorPackratM "True" "not match: " ["dv_pap"] d31_27 "")
                             d33_28 <- get
                             xx32_29 <- dv_pegM
                             let p = xx32_29
                             unless True (throwErrorPackratM "True" "not match: " ["dv_peg"] d33_28 "")
                             d35_30 <- get
                             _ <- dv_spacesM
                             unless True (throwErrorPackratM "True" "not match: " ["dv_spaces"] d35_30 "")
                             d37_31 <- get
                             xx36_32 <- dvCharsM
                             case xx36_32 of
                                 '|' -> return ()
                                 _ -> throwErrorPackratM "'|'" "not match pattern: " ["dvChars"] d37_31 ""
                             let '|' = xx36_32
                             return ()
                             unless True (throwErrorPackratM "True" "not match: " ["dvChars"] d37_31 "")
                             d39_33 <- get
                             xx38_34 <- dvCharsM
                             case xx38_34 of
                                 ']' -> return ()
                                 _ -> throwErrorPackratM "']'" "not match pattern: " ["dvChars"] d39_33 ""
                             let ']' = xx38_34
                             return ()
                             unless True (throwErrorPackratM "True" "not match: " ["dvChars"] d39_33 "")
                             d41_35 <- get
                             xx40_36 <- dvCharsM
                             case xx40_36 of
                                 '\n' -> return ()
                                 _ -> throwErrorPackratM "'\\n'" "not match pattern: " ["dvChars"] d41_35 ""
                             let '\n' = xx40_36
                             return ()
                             unless True (throwErrorPackratM "True" "not match: " ["dvChars"] d41_35 "")
                             d43_37 <- get
                             xx42_38 <- dv_afterPegM
                             let atp = xx42_38
                             unless True (throwErrorPackratM "True" "not match: " ["dv_afterPeg"] d43_37 "")
                             return (mkPegFile pr md emp pp p atp)]
p_pragma = foldl1 mplus [do d45_39 <- get
                            _ <- dv_spacesM
                            unless True (throwErrorPackratM "True" "not match: " ["dv_spaces"] d45_39 "")
                            d47_40 <- get
                            xx46_41 <- dvCharsM
                            case xx46_41 of
                                '{' -> return ()
                                _ -> throwErrorPackratM "'{'" "not match pattern: " ["dvChars"] d47_40 ""
                            let '{' = xx46_41
                            return ()
                            unless True (throwErrorPackratM "True" "not match: " ["dvChars"] d47_40 "")
                            d49_42 <- get
                            xx48_43 <- dvCharsM
                            case xx48_43 of
                                '-' -> return ()
                                _ -> throwErrorPackratM "'-'" "not match pattern: " ["dvChars"] d49_42 ""
                            let '-' = xx48_43
                            return ()
                            unless True (throwErrorPackratM "True" "not match: " ["dvChars"] d49_42 "")
                            d51_44 <- get
                            xx50_45 <- dvCharsM
                            case xx50_45 of
                                '#' -> return ()
                                _ -> throwErrorPackratM "'#'" "not match pattern: " ["dvChars"] d51_44 ""
                            let '#' = xx50_45
                            return ()
                            unless True (throwErrorPackratM "True" "not match: " ["dvChars"] d51_44 "")
                            d53_46 <- get
                            xx52_47 <- dv_pragmaStrM
                            let s = xx52_47
                            unless True (throwErrorPackratM "True" "not match: " ["dv_pragmaStr"] d53_46 "")
                            d55_48 <- get
                            _ <- dv_pragmaEndM
                            unless True (throwErrorPackratM "True" "not match: " ["dv_pragmaEnd"] d55_48 "")
                            d57_49 <- get
                            _ <- dv_spacesM
                            unless True (throwErrorPackratM "True" "not match: " ["dv_spaces"] d57_49 "")
                            return (just s),
                         do d59_50 <- get
                            _ <- dv_spacesM
                            unless True (throwErrorPackratM "True" "not match: " ["dv_spaces"] d59_50 "")
                            return nothing]
p_pragmaStr = foldl1 mplus [do d61_51 <- get
                               _ <- dv_delPragmasM
                               unless True (throwErrorPackratM "True" "not match: " ["dv_delPragmas"] d61_51 "")
                               d63_52 <- get
                               _ <- dv_spacesM
                               unless True (throwErrorPackratM "True" "not match: " ["dv_spaces"] d63_52 "")
                               d65_53 <- get
                               xx64_54 <- dvCharsM
                               case xx64_54 of
                                   ',' -> return ()
                                   _ -> throwErrorPackratM "','" "not match pattern: " ["dvChars"] d65_53 ""
                               let ',' = xx64_54
                               return ()
                               unless True (throwErrorPackratM "True" "not match: " ["dvChars"] d65_53 "")
                               d67_55 <- get
                               _ <- dv_spacesM
                               unless True (throwErrorPackratM "True" "not match: " ["dv_spaces"] d67_55 "")
                               d69_56 <- get
                               xx68_57 <- dv_pragmaStrM
                               let s = xx68_57
                               unless True (throwErrorPackratM "True" "not match: " ["dv_pragmaStr"] d69_56 "")
                               return (' ' : s),
                            do ddd70_58 <- get
                               flipMaybe "_:pragmaEnd[True]" ddd70_58 ["dv_pragmaEnd"] "" (do d72_59 <- get
                                                                                              _ <- dv_pragmaEndM
                                                                                              unless True (throwErrorPackratM "True" "not match: " ["dv_pragmaEnd"] d72_59 ""))
                               put ddd70_58
                               ddd73_60 <- get
                               flipMaybe "_:delPragmas[True]" ddd73_60 ["dv_delPragmas"] "" (do d75_61 <- get
                                                                                                _ <- dv_delPragmasM
                                                                                                unless True (throwErrorPackratM "True" "not match: " ["dv_delPragmas"] d75_61 ""))
                               put ddd73_60
                               d77_62 <- get
                               xx76_63 <- dvCharsM
                               let c = xx76_63
                               unless True (throwErrorPackratM "True" "not match: " ["dvChars"] d77_62 "")
                               d79_64 <- get
                               xx78_65 <- dv_pragmaStrM
                               let s = xx78_65
                               unless True (throwErrorPackratM "True" "not match: " ["dv_pragmaStr"] d79_64 "")
                               return (c : s),
                            do return emp]
p_delPragmas = foldl1 mplus [do d81_66 <- get
                                xx80_67 <- dvCharsM
                                case xx80_67 of
                                    'Q' -> return ()
                                    _ -> throwErrorPackratM "'Q'" "not match pattern: " ["dvChars"] d81_66 ""
                                let 'Q' = xx80_67
                                return ()
                                unless True (throwErrorPackratM "True" "not match: " ["dvChars"] d81_66 "")
                                d83_68 <- get
                                xx82_69 <- dvCharsM
                                case xx82_69 of
                                    'u' -> return ()
                                    _ -> throwErrorPackratM "'u'" "not match pattern: " ["dvChars"] d83_68 ""
                                let 'u' = xx82_69
                                return ()
                                unless True (throwErrorPackratM "True" "not match: " ["dvChars"] d83_68 "")
                                d85_70 <- get
                                xx84_71 <- dvCharsM
                                case xx84_71 of
                                    'a' -> return ()
                                    _ -> throwErrorPackratM "'a'" "not match pattern: " ["dvChars"] d85_70 ""
                                let 'a' = xx84_71
                                return ()
                                unless True (throwErrorPackratM "True" "not match: " ["dvChars"] d85_70 "")
                                d87_72 <- get
                                xx86_73 <- dvCharsM
                                case xx86_73 of
                                    's' -> return ()
                                    _ -> throwErrorPackratM "'s'" "not match pattern: " ["dvChars"] d87_72 ""
                                let 's' = xx86_73
                                return ()
                                unless True (throwErrorPackratM "True" "not match: " ["dvChars"] d87_72 "")
                                d89_74 <- get
                                xx88_75 <- dvCharsM
                                case xx88_75 of
                                    'i' -> return ()
                                    _ -> throwErrorPackratM "'i'" "not match pattern: " ["dvChars"] d89_74 ""
                                let 'i' = xx88_75
                                return ()
                                unless True (throwErrorPackratM "True" "not match: " ["dvChars"] d89_74 "")
                                d91_76 <- get
                                xx90_77 <- dvCharsM
                                case xx90_77 of
                                    'Q' -> return ()
                                    _ -> throwErrorPackratM "'Q'" "not match pattern: " ["dvChars"] d91_76 ""
                                let 'Q' = xx90_77
                                return ()
                                unless True (throwErrorPackratM "True" "not match: " ["dvChars"] d91_76 "")
                                d93_78 <- get
                                xx92_79 <- dvCharsM
                                case xx92_79 of
                                    'u' -> return ()
                                    _ -> throwErrorPackratM "'u'" "not match pattern: " ["dvChars"] d93_78 ""
                                let 'u' = xx92_79
                                return ()
                                unless True (throwErrorPackratM "True" "not match: " ["dvChars"] d93_78 "")
                                d95_80 <- get
                                xx94_81 <- dvCharsM
                                case xx94_81 of
                                    'o' -> return ()
                                    _ -> throwErrorPackratM "'o'" "not match pattern: " ["dvChars"] d95_80 ""
                                let 'o' = xx94_81
                                return ()
                                unless True (throwErrorPackratM "True" "not match: " ["dvChars"] d95_80 "")
                                d97_82 <- get
                                xx96_83 <- dvCharsM
                                case xx96_83 of
                                    't' -> return ()
                                    _ -> throwErrorPackratM "'t'" "not match pattern: " ["dvChars"] d97_82 ""
                                let 't' = xx96_83
                                return ()
                                unless True (throwErrorPackratM "True" "not match: " ["dvChars"] d97_82 "")
                                d99_84 <- get
                                xx98_85 <- dvCharsM
                                case xx98_85 of
                                    'e' -> return ()
                                    _ -> throwErrorPackratM "'e'" "not match pattern: " ["dvChars"] d99_84 ""
                                let 'e' = xx98_85
                                return ()
                                unless True (throwErrorPackratM "True" "not match: " ["dvChars"] d99_84 "")
                                d101_86 <- get
                                xx100_87 <- dvCharsM
                                case xx100_87 of
                                    's' -> return ()
                                    _ -> throwErrorPackratM "'s'" "not match pattern: " ["dvChars"] d101_86 ""
                                let 's' = xx100_87
                                return ()
                                unless True (throwErrorPackratM "True" "not match: " ["dvChars"] d101_86 "")
                                return (),
                             do d103_88 <- get
                                xx102_89 <- dvCharsM
                                case xx102_89 of
                                    'T' -> return ()
                                    _ -> throwErrorPackratM "'T'" "not match pattern: " ["dvChars"] d103_88 ""
                                let 'T' = xx102_89
                                return ()
                                unless True (throwErrorPackratM "True" "not match: " ["dvChars"] d103_88 "")
                                d105_90 <- get
                                xx104_91 <- dvCharsM
                                case xx104_91 of
                                    'y' -> return ()
                                    _ -> throwErrorPackratM "'y'" "not match pattern: " ["dvChars"] d105_90 ""
                                let 'y' = xx104_91
                                return ()
                                unless True (throwErrorPackratM "True" "not match: " ["dvChars"] d105_90 "")
                                d107_92 <- get
                                xx106_93 <- dvCharsM
                                case xx106_93 of
                                    'p' -> return ()
                                    _ -> throwErrorPackratM "'p'" "not match pattern: " ["dvChars"] d107_92 ""
                                let 'p' = xx106_93
                                return ()
                                unless True (throwErrorPackratM "True" "not match: " ["dvChars"] d107_92 "")
                                d109_94 <- get
                                xx108_95 <- dvCharsM
                                case xx108_95 of
                                    'e' -> return ()
                                    _ -> throwErrorPackratM "'e'" "not match pattern: " ["dvChars"] d109_94 ""
                                let 'e' = xx108_95
                                return ()
                                unless True (throwErrorPackratM "True" "not match: " ["dvChars"] d109_94 "")
                                d111_96 <- get
                                xx110_97 <- dvCharsM
                                case xx110_97 of
                                    'F' -> return ()
                                    _ -> throwErrorPackratM "'F'" "not match pattern: " ["dvChars"] d111_96 ""
                                let 'F' = xx110_97
                                return ()
                                unless True (throwErrorPackratM "True" "not match: " ["dvChars"] d111_96 "")
                                d113_98 <- get
                                xx112_99 <- dvCharsM
                                case xx112_99 of
                                    'a' -> return ()
                                    _ -> throwErrorPackratM "'a'" "not match pattern: " ["dvChars"] d113_98 ""
                                let 'a' = xx112_99
                                return ()
                                unless True (throwErrorPackratM "True" "not match: " ["dvChars"] d113_98 "")
                                d115_100 <- get
                                xx114_101 <- dvCharsM
                                case xx114_101 of
                                    'm' -> return ()
                                    _ -> throwErrorPackratM "'m'" "not match pattern: " ["dvChars"] d115_100 ""
                                let 'm' = xx114_101
                                return ()
                                unless True (throwErrorPackratM "True" "not match: " ["dvChars"] d115_100 "")
                                d117_102 <- get
                                xx116_103 <- dvCharsM
                                case xx116_103 of
                                    'i' -> return ()
                                    _ -> throwErrorPackratM "'i'" "not match pattern: " ["dvChars"] d117_102 ""
                                let 'i' = xx116_103
                                return ()
                                unless True (throwErrorPackratM "True" "not match: " ["dvChars"] d117_102 "")
                                d119_104 <- get
                                xx118_105 <- dvCharsM
                                case xx118_105 of
                                    'l' -> return ()
                                    _ -> throwErrorPackratM "'l'" "not match pattern: " ["dvChars"] d119_104 ""
                                let 'l' = xx118_105
                                return ()
                                unless True (throwErrorPackratM "True" "not match: " ["dvChars"] d119_104 "")
                                d121_106 <- get
                                xx120_107 <- dvCharsM
                                case xx120_107 of
                                    'i' -> return ()
                                    _ -> throwErrorPackratM "'i'" "not match pattern: " ["dvChars"] d121_106 ""
                                let 'i' = xx120_107
                                return ()
                                unless True (throwErrorPackratM "True" "not match: " ["dvChars"] d121_106 "")
                                d123_108 <- get
                                xx122_109 <- dvCharsM
                                case xx122_109 of
                                    'e' -> return ()
                                    _ -> throwErrorPackratM "'e'" "not match pattern: " ["dvChars"] d123_108 ""
                                let 'e' = xx122_109
                                return ()
                                unless True (throwErrorPackratM "True" "not match: " ["dvChars"] d123_108 "")
                                d125_110 <- get
                                xx124_111 <- dvCharsM
                                case xx124_111 of
                                    's' -> return ()
                                    _ -> throwErrorPackratM "'s'" "not match pattern: " ["dvChars"] d125_110 ""
                                let 's' = xx124_111
                                return ()
                                unless True (throwErrorPackratM "True" "not match: " ["dvChars"] d125_110 "")
                                return ()]
p_pragmaEnd = foldl1 mplus [do d127_112 <- get
                               _ <- dv_delPragmasM
                               unless True (throwErrorPackratM "True" "not match: " ["dv_delPragmas"] d127_112 "")
                               d129_113 <- get
                               _ <- dv_spacesM
                               unless True (throwErrorPackratM "True" "not match: " ["dv_spaces"] d129_113 "")
                               d131_114 <- get
                               xx130_115 <- dvCharsM
                               case xx130_115 of
                                   '#' -> return ()
                                   _ -> throwErrorPackratM "'#'" "not match pattern: " ["dvChars"] d131_114 ""
                               let '#' = xx130_115
                               return ()
                               unless True (throwErrorPackratM "True" "not match: " ["dvChars"] d131_114 "")
                               d133_116 <- get
                               xx132_117 <- dvCharsM
                               case xx132_117 of
                                   '-' -> return ()
                                   _ -> throwErrorPackratM "'-'" "not match pattern: " ["dvChars"] d133_116 ""
                               let '-' = xx132_117
                               return ()
                               unless True (throwErrorPackratM "True" "not match: " ["dvChars"] d133_116 "")
                               d135_118 <- get
                               xx134_119 <- dvCharsM
                               case xx134_119 of
                                   '}' -> return ()
                                   _ -> throwErrorPackratM "'}'" "not match pattern: " ["dvChars"] d135_118 ""
                               let '}' = xx134_119
                               return ()
                               unless True (throwErrorPackratM "True" "not match: " ["dvChars"] d135_118 "")
                               return (),
                            do d137_120 <- get
                               xx136_121 <- dvCharsM
                               case xx136_121 of
                                   '#' -> return ()
                                   _ -> throwErrorPackratM "'#'" "not match pattern: " ["dvChars"] d137_120 ""
                               let '#' = xx136_121
                               return ()
                               unless True (throwErrorPackratM "True" "not match: " ["dvChars"] d137_120 "")
                               d139_122 <- get
                               xx138_123 <- dvCharsM
                               case xx138_123 of
                                   '-' -> return ()
                                   _ -> throwErrorPackratM "'-'" "not match pattern: " ["dvChars"] d139_122 ""
                               let '-' = xx138_123
                               return ()
                               unless True (throwErrorPackratM "True" "not match: " ["dvChars"] d139_122 "")
                               d141_124 <- get
                               xx140_125 <- dvCharsM
                               case xx140_125 of
                                   '}' -> return ()
                                   _ -> throwErrorPackratM "'}'" "not match pattern: " ["dvChars"] d141_124 ""
                               let '}' = xx140_125
                               return ()
                               unless True (throwErrorPackratM "True" "not match: " ["dvChars"] d141_124 "")
                               return ()]
p_moduleDec = foldl1 mplus [do d143_126 <- get
                               xx142_127 <- dvCharsM
                               case xx142_127 of
                                   'm' -> return ()
                                   _ -> throwErrorPackratM "'m'" "not match pattern: " ["dvChars"] d143_126 ""
                               let 'm' = xx142_127
                               return ()
                               unless True (throwErrorPackratM "True" "not match: " ["dvChars"] d143_126 "")
                               d145_128 <- get
                               xx144_129 <- dvCharsM
                               case xx144_129 of
                                   'o' -> return ()
                                   _ -> throwErrorPackratM "'o'" "not match pattern: " ["dvChars"] d145_128 ""
                               let 'o' = xx144_129
                               return ()
                               unless True (throwErrorPackratM "True" "not match: " ["dvChars"] d145_128 "")
                               d147_130 <- get
                               xx146_131 <- dvCharsM
                               case xx146_131 of
                                   'd' -> return ()
                                   _ -> throwErrorPackratM "'d'" "not match pattern: " ["dvChars"] d147_130 ""
                               let 'd' = xx146_131
                               return ()
                               unless True (throwErrorPackratM "True" "not match: " ["dvChars"] d147_130 "")
                               d149_132 <- get
                               xx148_133 <- dvCharsM
                               case xx148_133 of
                                   'u' -> return ()
                                   _ -> throwErrorPackratM "'u'" "not match pattern: " ["dvChars"] d149_132 ""
                               let 'u' = xx148_133
                               return ()
                               unless True (throwErrorPackratM "True" "not match: " ["dvChars"] d149_132 "")
                               d151_134 <- get
                               xx150_135 <- dvCharsM
                               case xx150_135 of
                                   'l' -> return ()
                                   _ -> throwErrorPackratM "'l'" "not match pattern: " ["dvChars"] d151_134 ""
                               let 'l' = xx150_135
                               return ()
                               unless True (throwErrorPackratM "True" "not match: " ["dvChars"] d151_134 "")
                               d153_136 <- get
                               xx152_137 <- dvCharsM
                               case xx152_137 of
                                   'e' -> return ()
                                   _ -> throwErrorPackratM "'e'" "not match pattern: " ["dvChars"] d153_136 ""
                               let 'e' = xx152_137
                               return ()
                               unless True (throwErrorPackratM "True" "not match: " ["dvChars"] d153_136 "")
                               d155_138 <- get
                               xx154_139 <- dv_moduleDecStrM
                               let s = xx154_139
                               unless True (throwErrorPackratM "True" "not match: " ["dv_moduleDecStr"] d155_138 "")
                               d157_140 <- get
                               _ <- dv_whrM
                               unless True (throwErrorPackratM "True" "not match: " ["dv_whr"] d157_140 "")
                               return (just s),
                            do return nothing]
p_moduleDecStr = foldl1 mplus [do ddd158_141 <- get
                                  flipMaybe "_:whr[True]" ddd158_141 ["dv_whr"] "" (do d160_142 <- get
                                                                                       _ <- dv_whrM
                                                                                       unless True (throwErrorPackratM "True" "not match: " ["dv_whr"] d160_142 ""))
                                  put ddd158_141
                                  d162_143 <- get
                                  xx161_144 <- dvCharsM
                                  let c = xx161_144
                                  unless True (throwErrorPackratM "True" "not match: " ["dvChars"] d162_143 "")
                                  d164_145 <- get
                                  xx163_146 <- dv_moduleDecStrM
                                  let s = xx163_146
                                  unless True (throwErrorPackratM "True" "not match: " ["dv_moduleDecStr"] d164_145 "")
                                  return (cons c s),
                               do return emp]
p_whr = foldl1 mplus [do d166_147 <- get
                         xx165_148 <- dvCharsM
                         case xx165_148 of
                             'w' -> return ()
                             _ -> throwErrorPackratM "'w'" "not match pattern: " ["dvChars"] d166_147 ""
                         let 'w' = xx165_148
                         return ()
                         unless True (throwErrorPackratM "True" "not match: " ["dvChars"] d166_147 "")
                         d168_149 <- get
                         xx167_150 <- dvCharsM
                         case xx167_150 of
                             'h' -> return ()
                             _ -> throwErrorPackratM "'h'" "not match pattern: " ["dvChars"] d168_149 ""
                         let 'h' = xx167_150
                         return ()
                         unless True (throwErrorPackratM "True" "not match: " ["dvChars"] d168_149 "")
                         d170_151 <- get
                         xx169_152 <- dvCharsM
                         case xx169_152 of
                             'e' -> return ()
                             _ -> throwErrorPackratM "'e'" "not match pattern: " ["dvChars"] d170_151 ""
                         let 'e' = xx169_152
                         return ()
                         unless True (throwErrorPackratM "True" "not match: " ["dvChars"] d170_151 "")
                         d172_153 <- get
                         xx171_154 <- dvCharsM
                         case xx171_154 of
                             'r' -> return ()
                             _ -> throwErrorPackratM "'r'" "not match pattern: " ["dvChars"] d172_153 ""
                         let 'r' = xx171_154
                         return ()
                         unless True (throwErrorPackratM "True" "not match: " ["dvChars"] d172_153 "")
                         d174_155 <- get
                         xx173_156 <- dvCharsM
                         case xx173_156 of
                             'e' -> return ()
                             _ -> throwErrorPackratM "'e'" "not match pattern: " ["dvChars"] d174_155 ""
                         let 'e' = xx173_156
                         return ()
                         unless True (throwErrorPackratM "True" "not match: " ["dvChars"] d174_155 "")
                         return ()]
p_preImpPap = foldl1 mplus [do ddd175_157 <- get
                               flipMaybe "_:importPapillon[True]" ddd175_157 ["dv_importPapillon"] "" (do d177_158 <- get
                                                                                                          _ <- dv_importPapillonM
                                                                                                          unless True (throwErrorPackratM "True" "not match: " ["dv_importPapillon"] d177_158 ""))
                               put ddd175_157
                               ddd178_159 <- get
                               flipMaybe "_:pap[True]" ddd178_159 ["dv_pap"] "" (do d180_160 <- get
                                                                                    _ <- dv_papM
                                                                                    unless True (throwErrorPackratM "True" "not match: " ["dv_pap"] d180_160 ""))
                               put ddd178_159
                               d182_161 <- get
                               xx181_162 <- dvCharsM
                               let c = xx181_162
                               unless True (throwErrorPackratM "True" "not match: " ["dvChars"] d182_161 "")
                               d184_163 <- get
                               xx183_164 <- dv_preImpPapM
                               let pip = xx183_164
                               unless True (throwErrorPackratM "True" "not match: " ["dv_preImpPap"] d184_163 "")
                               return (cons c pip),
                            do return emp]
p_prePeg = foldl1 mplus [do ddd185_165 <- get
                            flipMaybe "_:pap[True]" ddd185_165 ["dv_pap"] "" (do d187_166 <- get
                                                                                 _ <- dv_papM
                                                                                 unless True (throwErrorPackratM "True" "not match: " ["dv_pap"] d187_166 ""))
                            put ddd185_165
                            d189_167 <- get
                            xx188_168 <- dvCharsM
                            let c = xx188_168
                            unless True (throwErrorPackratM "True" "not match: " ["dvChars"] d189_167 "")
                            d191_169 <- get
                            xx190_170 <- dv_prePegM
                            let pp = xx190_170
                            unless True (throwErrorPackratM "True" "not match: " ["dv_prePeg"] d191_169 "")
                            return (cons c pp),
                         do return emp]
p_afterPeg = foldl1 mplus [do d193_171 <- get
                              xx192_172 <- dvCharsM
                              let c = xx192_172
                              unless True (throwErrorPackratM "True" "not match: " ["dvChars"] d193_171 "")
                              d195_173 <- get
                              xx194_174 <- dv_afterPegM
                              let atp = xx194_174
                              unless True (throwErrorPackratM "True" "not match: " ["dv_afterPeg"] d195_173 "")
                              return (cons c atp),
                           do return emp]
p_importPapillon = foldl1 mplus [do d197_175 <- get
                                    xx196_176 <- dv_varTokenM
                                    case xx196_176 of
                                        "import" -> return ()
                                        _ -> throwErrorPackratM "\"import\"" "not match pattern: " ["dv_varToken"] d197_175 ""
                                    let "import" = xx196_176
                                    return ()
                                    unless True (throwErrorPackratM "True" "not match: " ["dv_varToken"] d197_175 "")
                                    d199_177 <- get
                                    xx198_178 <- dv_typTokenM
                                    case xx198_178 of
                                        "Text" -> return ()
                                        _ -> throwErrorPackratM "\"Text\"" "not match pattern: " ["dv_typToken"] d199_177 ""
                                    let "Text" = xx198_178
                                    return ()
                                    unless True (throwErrorPackratM "True" "not match: " ["dv_typToken"] d199_177 "")
                                    d201_179 <- get
                                    xx200_180 <- dvCharsM
                                    case xx200_180 of
                                        '.' -> return ()
                                        _ -> throwErrorPackratM "'.'" "not match pattern: " ["dvChars"] d201_179 ""
                                    let '.' = xx200_180
                                    return ()
                                    unless True (throwErrorPackratM "True" "not match: " ["dvChars"] d201_179 "")
                                    d203_181 <- get
                                    _ <- dv_spacesM
                                    unless True (throwErrorPackratM "True" "not match: " ["dv_spaces"] d203_181 "")
                                    d205_182 <- get
                                    xx204_183 <- dv_typTokenM
                                    case xx204_183 of
                                        "Papillon" -> return ()
                                        _ -> throwErrorPackratM "\"Papillon\"" "not match pattern: " ["dv_typToken"] d205_182 ""
                                    let "Papillon" = xx204_183
                                    return ()
                                    unless True (throwErrorPackratM "True" "not match: " ["dv_typToken"] d205_182 "")
                                    ddd206_184 <- get
                                    flipMaybe "'.':[True]" ddd206_184 ["dvChars"] "" (do d208_185 <- get
                                                                                         xx207_186 <- dvCharsM
                                                                                         case xx207_186 of
                                                                                             '.' -> return ()
                                                                                             _ -> throwErrorPackratM "'.'" "not match pattern: " ["dvChars"] d208_185 ""
                                                                                         let '.' = xx207_186
                                                                                         return ()
                                                                                         unless True (throwErrorPackratM "True" "not match: " ["dvChars"] d208_185 ""))
                                    put ddd206_184
                                    return ()]
p_varToken = foldl1 mplus [do d210_187 <- get
                              xx209_188 <- dv_variableM
                              let v = xx209_188
                              unless True (throwErrorPackratM "True" "not match: " ["dv_variable"] d210_187 "")
                              d212_189 <- get
                              _ <- dv_spacesM
                              unless True (throwErrorPackratM "True" "not match: " ["dv_spaces"] d212_189 "")
                              return v]
p_typToken = foldl1 mplus [do d214_190 <- get
                              xx213_191 <- dv_typM
                              let t = xx213_191
                              unless True (throwErrorPackratM "True" "not match: " ["dv_typ"] d214_190 "")
                              d216_192 <- get
                              _ <- dv_spacesM
                              unless True (throwErrorPackratM "True" "not match: " ["dv_spaces"] d216_192 "")
                              return t]
p_pap = foldl1 mplus [do d218_193 <- get
                         xx217_194 <- dvCharsM
                         case xx217_194 of
                             '\n' -> return ()
                             _ -> throwErrorPackratM "'\\n'" "not match pattern: " ["dvChars"] d218_193 ""
                         let '\n' = xx217_194
                         return ()
                         unless True (throwErrorPackratM "True" "not match: " ["dvChars"] d218_193 "")
                         d220_195 <- get
                         xx219_196 <- dvCharsM
                         case xx219_196 of
                             '[' -> return ()
                             _ -> throwErrorPackratM "'['" "not match pattern: " ["dvChars"] d220_195 ""
                         let '[' = xx219_196
                         return ()
                         unless True (throwErrorPackratM "True" "not match: " ["dvChars"] d220_195 "")
                         d222_197 <- get
                         xx221_198 <- dvCharsM
                         case xx221_198 of
                             'p' -> return ()
                             _ -> throwErrorPackratM "'p'" "not match pattern: " ["dvChars"] d222_197 ""
                         let 'p' = xx221_198
                         return ()
                         unless True (throwErrorPackratM "True" "not match: " ["dvChars"] d222_197 "")
                         d224_199 <- get
                         xx223_200 <- dvCharsM
                         case xx223_200 of
                             'a' -> return ()
                             _ -> throwErrorPackratM "'a'" "not match pattern: " ["dvChars"] d224_199 ""
                         let 'a' = xx223_200
                         return ()
                         unless True (throwErrorPackratM "True" "not match: " ["dvChars"] d224_199 "")
                         d226_201 <- get
                         xx225_202 <- dvCharsM
                         case xx225_202 of
                             'p' -> return ()
                             _ -> throwErrorPackratM "'p'" "not match pattern: " ["dvChars"] d226_201 ""
                         let 'p' = xx225_202
                         return ()
                         unless True (throwErrorPackratM "True" "not match: " ["dvChars"] d226_201 "")
                         d228_203 <- get
                         xx227_204 <- dvCharsM
                         case xx227_204 of
                             'i' -> return ()
                             _ -> throwErrorPackratM "'i'" "not match pattern: " ["dvChars"] d228_203 ""
                         let 'i' = xx227_204
                         return ()
                         unless True (throwErrorPackratM "True" "not match: " ["dvChars"] d228_203 "")
                         d230_205 <- get
                         xx229_206 <- dvCharsM
                         case xx229_206 of
                             'l' -> return ()
                             _ -> throwErrorPackratM "'l'" "not match pattern: " ["dvChars"] d230_205 ""
                         let 'l' = xx229_206
                         return ()
                         unless True (throwErrorPackratM "True" "not match: " ["dvChars"] d230_205 "")
                         d232_207 <- get
                         xx231_208 <- dvCharsM
                         case xx231_208 of
                             'l' -> return ()
                             _ -> throwErrorPackratM "'l'" "not match pattern: " ["dvChars"] d232_207 ""
                         let 'l' = xx231_208
                         return ()
                         unless True (throwErrorPackratM "True" "not match: " ["dvChars"] d232_207 "")
                         d234_209 <- get
                         xx233_210 <- dvCharsM
                         case xx233_210 of
                             'o' -> return ()
                             _ -> throwErrorPackratM "'o'" "not match pattern: " ["dvChars"] d234_209 ""
                         let 'o' = xx233_210
                         return ()
                         unless True (throwErrorPackratM "True" "not match: " ["dvChars"] d234_209 "")
                         d236_211 <- get
                         xx235_212 <- dvCharsM
                         case xx235_212 of
                             'n' -> return ()
                             _ -> throwErrorPackratM "'n'" "not match pattern: " ["dvChars"] d236_211 ""
                         let 'n' = xx235_212
                         return ()
                         unless True (throwErrorPackratM "True" "not match: " ["dvChars"] d236_211 "")
                         d238_213 <- get
                         xx237_214 <- dvCharsM
                         case xx237_214 of
                             '|' -> return ()
                             _ -> throwErrorPackratM "'|'" "not match pattern: " ["dvChars"] d238_213 ""
                         let '|' = xx237_214
                         return ()
                         unless True (throwErrorPackratM "True" "not match: " ["dvChars"] d238_213 "")
                         d240_215 <- get
                         xx239_216 <- dvCharsM
                         case xx239_216 of
                             '\n' -> return ()
                             _ -> throwErrorPackratM "'\\n'" "not match pattern: " ["dvChars"] d240_215 ""
                         let '\n' = xx239_216
                         return ()
                         unless True (throwErrorPackratM "True" "not match: " ["dvChars"] d240_215 "")
                         return ()]
p_peg = foldl1 mplus [do d242_217 <- get
                         _ <- dv_spacesM
                         unless True (throwErrorPackratM "True" "not match: " ["dv_spaces"] d242_217 "")
                         d244_218 <- get
                         xx243_219 <- dv_sourceTypeM
                         let s = xx243_219
                         unless True (throwErrorPackratM "True" "not match: " ["dv_sourceType"] d244_218 "")
                         d246_220 <- get
                         xx245_221 <- dv_peg_M
                         let p = xx245_221
                         unless True (throwErrorPackratM "True" "not match: " ["dv_peg_"] d246_220 "")
                         return (mkTTPeg s p),
                      do d248_222 <- get
                         xx247_223 <- dv_peg_M
                         let p = xx247_223
                         unless True (throwErrorPackratM "True" "not match: " ["dv_peg_"] d248_222 "")
                         return (mkTTPeg tString p)]
p_sourceType = foldl1 mplus [do d250_224 <- get
                                xx249_225 <- dv_varTokenM
                                case xx249_225 of
                                    "source" -> return ()
                                    _ -> throwErrorPackratM "\"source\"" "not match pattern: " ["dv_varToken"] d250_224 ""
                                let "source" = xx249_225
                                return ()
                                unless True (throwErrorPackratM "True" "not match: " ["dv_varToken"] d250_224 "")
                                d252_226 <- get
                                xx251_227 <- dvCharsM
                                case xx251_227 of
                                    ':' -> return ()
                                    _ -> throwErrorPackratM "':'" "not match pattern: " ["dvChars"] d252_226 ""
                                let ':' = xx251_227
                                return ()
                                unless True (throwErrorPackratM "True" "not match: " ["dvChars"] d252_226 "")
                                d254_228 <- get
                                _ <- dv_spacesM
                                unless True (throwErrorPackratM "True" "not match: " ["dv_spaces"] d254_228 "")
                                d256_229 <- get
                                xx255_230 <- dv_typTokenM
                                let v = xx255_230
                                unless True (throwErrorPackratM "True" "not match: " ["dv_typToken"] d256_229 "")
                                return v]
p_peg_ = foldl1 mplus [do d258_231 <- get
                          _ <- dv_spacesM
                          unless True (throwErrorPackratM "True" "not match: " ["dv_spaces"] d258_231 "")
                          d260_232 <- get
                          xx259_233 <- dv_definitionM
                          let d = xx259_233
                          unless True (throwErrorPackratM "True" "not match: " ["dv_definition"] d260_232 "")
                          d262_234 <- get
                          xx261_235 <- dv_peg_M
                          let p = xx261_235
                          unless True (throwErrorPackratM "True" "not match: " ["dv_peg_"] d262_234 "")
                          return (cons d p),
                       do return emp]
p_definition = foldl1 mplus [do d264_236 <- get
                                xx263_237 <- dv_variableM
                                let v = xx263_237
                                unless True (throwErrorPackratM "True" "not match: " ["dv_variable"] d264_236 "")
                                d266_238 <- get
                                _ <- dv_spacesM
                                unless True (throwErrorPackratM "True" "not match: " ["dv_spaces"] d266_238 "")
                                d268_239 <- get
                                xx267_240 <- dvCharsM
                                case xx267_240 of
                                    ':' -> return ()
                                    _ -> throwErrorPackratM "':'" "not match pattern: " ["dvChars"] d268_239 ""
                                let ':' = xx267_240
                                return ()
                                unless True (throwErrorPackratM "True" "not match: " ["dvChars"] d268_239 "")
                                d270_241 <- get
                                xx269_242 <- dvCharsM
                                case xx269_242 of
                                    ':' -> return ()
                                    _ -> throwErrorPackratM "':'" "not match pattern: " ["dvChars"] d270_241 ""
                                let ':' = xx269_242
                                return ()
                                unless True (throwErrorPackratM "True" "not match: " ["dvChars"] d270_241 "")
                                d272_243 <- get
                                _ <- dv_spacesM
                                unless True (throwErrorPackratM "True" "not match: " ["dv_spaces"] d272_243 "")
                                d274_244 <- get
                                xx273_245 <- dv_hsTypeArrM
                                let t = xx273_245
                                unless True (throwErrorPackratM "True" "not match: " ["dv_hsTypeArr"] d274_244 "")
                                d276_246 <- get
                                _ <- dv_spacesM
                                unless True (throwErrorPackratM "True" "not match: " ["dv_spaces"] d276_246 "")
                                d278_247 <- get
                                xx277_248 <- dvCharsM
                                case xx277_248 of
                                    '=' -> return ()
                                    _ -> throwErrorPackratM "'='" "not match pattern: " ["dvChars"] d278_247 ""
                                let '=' = xx277_248
                                return ()
                                unless True (throwErrorPackratM "True" "not match: " ["dvChars"] d278_247 "")
                                d280_249 <- get
                                _ <- dv_spacesM
                                unless True (throwErrorPackratM "True" "not match: " ["dv_spaces"] d280_249 "")
                                d282_250 <- get
                                xx281_251 <- dv_selectionM
                                let sel = xx281_251
                                unless True (throwErrorPackratM "True" "not match: " ["dv_selection"] d282_250 "")
                                d284_252 <- get
                                _ <- dv_spacesM
                                unless True (throwErrorPackratM "True" "not match: " ["dv_spaces"] d284_252 "")
                                d286_253 <- get
                                xx285_254 <- dvCharsM
                                case xx285_254 of
                                    ';' -> return ()
                                    _ -> throwErrorPackratM "';'" "not match pattern: " ["dvChars"] d286_253 ""
                                let ';' = xx285_254
                                return ()
                                unless True (throwErrorPackratM "True" "not match: " ["dvChars"] d286_253 "")
                                return (mkDef v t sel)]
p_selection = foldl1 mplus [do d288_255 <- get
                               xx287_256 <- dv_expressionHsM
                               let ex = xx287_256
                               unless True (throwErrorPackratM "True" "not match: " ["dv_expressionHs"] d288_255 "")
                               d290_257 <- get
                               _ <- dv_spacesM
                               unless True (throwErrorPackratM "True" "not match: " ["dv_spaces"] d290_257 "")
                               d292_258 <- get
                               xx291_259 <- dvCharsM
                               case xx291_259 of
                                   '/' -> return ()
                                   _ -> throwErrorPackratM "'/'" "not match pattern: " ["dvChars"] d292_258 ""
                               let '/' = xx291_259
                               return ()
                               unless True (throwErrorPackratM "True" "not match: " ["dvChars"] d292_258 "")
                               d294_260 <- get
                               _ <- dv_spacesM
                               unless True (throwErrorPackratM "True" "not match: " ["dv_spaces"] d294_260 "")
                               d296_261 <- get
                               xx295_262 <- dv_selectionM
                               let sel = xx295_262
                               unless True (throwErrorPackratM "True" "not match: " ["dv_selection"] d296_261 "")
                               return (cons ex sel),
                            do d298_263 <- get
                               xx297_264 <- dv_expressionHsM
                               let ex = xx297_264
                               unless True (throwErrorPackratM "True" "not match: " ["dv_expressionHs"] d298_263 "")
                               return (cons ex emp)]
p_expressionHs = foldl1 mplus [do d300_265 <- get
                                  xx299_266 <- dv_expressionM
                                  let e = xx299_266
                                  unless True (throwErrorPackratM "True" "not match: " ["dv_expression"] d300_265 "")
                                  d302_267 <- get
                                  _ <- dv_spacesM
                                  unless True (throwErrorPackratM "True" "not match: " ["dv_spaces"] d302_267 "")
                                  d304_268 <- get
                                  xx303_269 <- dvCharsM
                                  case xx303_269 of
                                      '{' -> return ()
                                      _ -> throwErrorPackratM "'{'" "not match pattern: " ["dvChars"] d304_268 ""
                                  let '{' = xx303_269
                                  return ()
                                  unless True (throwErrorPackratM "True" "not match: " ["dvChars"] d304_268 "")
                                  d306_270 <- get
                                  _ <- dv_spacesM
                                  unless True (throwErrorPackratM "True" "not match: " ["dv_spaces"] d306_270 "")
                                  d308_271 <- get
                                  xx307_272 <- dv_hsExpLamM
                                  let h = xx307_272
                                  unless True (throwErrorPackratM "True" "not match: " ["dv_hsExpLam"] d308_271 "")
                                  d310_273 <- get
                                  _ <- dv_spacesM
                                  unless True (throwErrorPackratM "True" "not match: " ["dv_spaces"] d310_273 "")
                                  d312_274 <- get
                                  xx311_275 <- dvCharsM
                                  case xx311_275 of
                                      '}' -> return ()
                                      _ -> throwErrorPackratM "'}'" "not match pattern: " ["dvChars"] d312_274 ""
                                  let '}' = xx311_275
                                  return ()
                                  unless True (throwErrorPackratM "True" "not match: " ["dvChars"] d312_274 "")
                                  return (mkExpressionHs e h)]
p_expression = foldl1 mplus [do d314_276 <- get
                                xx313_277 <- dv_nameLeaf_M
                                let l = xx313_277
                                unless True (throwErrorPackratM "True" "not match: " ["dv_nameLeaf_"] d314_276 "")
                                d316_278 <- get
                                _ <- dv_spacesM
                                unless True (throwErrorPackratM "True" "not match: " ["dv_spaces"] d316_278 "")
                                d318_279 <- get
                                xx317_280 <- dv_expressionM
                                let e = xx317_280
                                unless True (throwErrorPackratM "True" "not match: " ["dv_expression"] d318_279 "")
                                return (cons l e),
                             do return emp]
p_nameLeaf_ = foldl1 mplus [do d320_281 <- get
                               xx319_282 <- dvCharsM
                               case xx319_282 of
                                   '!' -> return ()
                                   _ -> throwErrorPackratM "'!'" "not match pattern: " ["dvChars"] d320_281 ""
                               let '!' = xx319_282
                               return ()
                               unless True (throwErrorPackratM "True" "not match: " ["dvChars"] d320_281 "")
                               d322_283 <- get
                               xx321_284 <- dv_nameLeafNoComM
                               let nl = xx321_284
                               unless True (throwErrorPackratM "True" "not match: " ["dv_nameLeafNoCom"] d322_283 "")
                               d324_285 <- get
                               _ <- dv_spacesM
                               unless True (throwErrorPackratM "True" "not match: " ["dv_spaces"] d324_285 "")
                               d326_286 <- get
                               xx325_287 <- papOptional dv_comForErrM
                               let com = xx325_287
                               unless True (throwErrorPackratM "True" "not match: " ["dv_comForErr"] d326_286 "")
                               return (NotAfter nl $ maybe "" id com),
                            do d328_288 <- get
                               xx327_289 <- dvCharsM
                               let c = xx327_289
                               unless (isAmp c) (throwErrorPackratM "isAmp c" "not match: " ["dvChars"] d328_288 "")
                               d330_290 <- get
                               xx329_291 <- dv_nameLeafM
                               let nl = xx329_291
                               unless True (throwErrorPackratM "True" "not match: " ["dv_nameLeaf"] d330_290 "")
                               return (After nl),
                            do d332_292 <- get
                               xx331_293 <- dv_nameLeafM
                               let nl = xx331_293
                               unless True (throwErrorPackratM "True" "not match: " ["dv_nameLeaf"] d332_292 "")
                               return (Here nl)]
p_nameLeaf = foldl1 mplus [do d334_294 <- get
                              xx333_295 <- dv_pat1M
                              let n = xx333_295
                              unless True (throwErrorPackratM "True" "not match: " ["dv_pat1"] d334_294 "")
                              d336_296 <- get
                              _ <- dv_spacesM
                              unless True (throwErrorPackratM "True" "not match: " ["dv_spaces"] d336_296 "")
                              d338_297 <- get
                              xx337_298 <- papOptional dv_comForErrM
                              let com = xx337_298
                              unless True (throwErrorPackratM "True" "not match: " ["dv_comForErr"] d338_297 "")
                              d340_299 <- get
                              xx339_300 <- dvCharsM
                              case xx339_300 of
                                  ':' -> return ()
                                  _ -> throwErrorPackratM "':'" "not match pattern: " ["dvChars"] d340_299 ""
                              let ':' = xx339_300
                              return ()
                              unless True (throwErrorPackratM "True" "not match: " ["dvChars"] d340_299 "")
                              d342_301 <- get
                              xx341_302 <- dv_leafM
                              let (rf, p) = xx341_302
                              unless True (throwErrorPackratM "True" "not match: " ["dv_leaf"] d342_301 "")
                              return (NameLeaf (n, maybe "" id com) rf p),
                           do d344_303 <- get
                              xx343_304 <- dv_pat1M
                              let n = xx343_304
                              unless True (throwErrorPackratM "True" "not match: " ["dv_pat1"] d344_303 "")
                              d346_305 <- get
                              _ <- dv_spacesM
                              unless True (throwErrorPackratM "True" "not match: " ["dv_spaces"] d346_305 "")
                              d348_306 <- get
                              xx347_307 <- papOptional dv_comForErrM
                              let com = xx347_307
                              unless True (throwErrorPackratM "True" "not match: " ["dv_comForErr"] d348_306 "")
                              return (NameLeaf (n,
                                                maybe "" id com) FromToken (conE $ mkName "True",
                                                                            ""))]
p_nameLeafNoCom = foldl1 mplus [do d350_308 <- get
                                   xx349_309 <- dv_pat1M
                                   let n = xx349_309
                                   unless True (throwErrorPackratM "True" "not match: " ["dv_pat1"] d350_308 "")
                                   d352_310 <- get
                                   _ <- dv_spacesM
                                   unless True (throwErrorPackratM "True" "not match: " ["dv_spaces"] d352_310 "")
                                   d354_311 <- get
                                   xx353_312 <- papOptional dv_comForErrM
                                   let com = xx353_312
                                   unless True (throwErrorPackratM "True" "not match: " ["dv_comForErr"] d354_311 "")
                                   d356_313 <- get
                                   xx355_314 <- dvCharsM
                                   case xx355_314 of
                                       ':' -> return ()
                                       _ -> throwErrorPackratM "':'" "not match pattern: " ["dvChars"] d356_313 ""
                                   let ':' = xx355_314
                                   return ()
                                   unless True (throwErrorPackratM "True" "not match: " ["dvChars"] d356_313 "")
                                   d358_315 <- get
                                   xx357_316 <- dv_leafM
                                   let (rf, p) = xx357_316
                                   unless True (throwErrorPackratM "True" "not match: " ["dv_leaf"] d358_315 "")
                                   return (NameLeaf (n, maybe "" id com) rf p),
                                do d360_317 <- get
                                   xx359_318 <- dv_pat1M
                                   let n = xx359_318
                                   unless True (throwErrorPackratM "True" "not match: " ["dv_pat1"] d360_317 "")
                                   d362_319 <- get
                                   _ <- dv_spacesM
                                   unless True (throwErrorPackratM "True" "not match: " ["dv_spaces"] d362_319 "")
                                   return (NameLeaf (n, "") FromToken (conE $ mkName "True", ""))]
p_comForErr = foldl1 mplus [do d364_320 <- get
                               xx363_321 <- dvCharsM
                               case xx363_321 of
                                   '{' -> return ()
                                   _ -> throwErrorPackratM "'{'" "not match pattern: " ["dvChars"] d364_320 ""
                               let '{' = xx363_321
                               return ()
                               unless True (throwErrorPackratM "True" "not match: " ["dvChars"] d364_320 "")
                               d366_322 <- get
                               xx365_323 <- dvCharsM
                               case xx365_323 of
                                   '-' -> return ()
                                   _ -> throwErrorPackratM "'-'" "not match pattern: " ["dvChars"] d366_322 ""
                               let '-' = xx365_323
                               return ()
                               unless True (throwErrorPackratM "True" "not match: " ["dvChars"] d366_322 "")
                               d368_324 <- get
                               xx367_325 <- dvCharsM
                               case xx367_325 of
                                   '#' -> return ()
                                   _ -> throwErrorPackratM "'#'" "not match pattern: " ["dvChars"] d368_324 ""
                               let '#' = xx367_325
                               return ()
                               unless True (throwErrorPackratM "True" "not match: " ["dvChars"] d368_324 "")
                               d370_326 <- get
                               _ <- dv_spacesM
                               unless True (throwErrorPackratM "True" "not match: " ["dv_spaces"] d370_326 "")
                               d372_327 <- get
                               xx371_328 <- dvCharsM
                               case xx371_328 of
                                   '"' -> return ()
                                   _ -> throwErrorPackratM "'\"'" "not match pattern: " ["dvChars"] d372_327 ""
                               let '"' = xx371_328
                               return ()
                               unless True (throwErrorPackratM "True" "not match: " ["dvChars"] d372_327 "")
                               d374_329 <- get
                               xx373_330 <- dv_stringLitM
                               let s = xx373_330
                               unless True (throwErrorPackratM "True" "not match: " ["dv_stringLit"] d374_329 "")
                               d376_331 <- get
                               xx375_332 <- dvCharsM
                               case xx375_332 of
                                   '"' -> return ()
                                   _ -> throwErrorPackratM "'\"'" "not match pattern: " ["dvChars"] d376_331 ""
                               let '"' = xx375_332
                               return ()
                               unless True (throwErrorPackratM "True" "not match: " ["dvChars"] d376_331 "")
                               d378_333 <- get
                               _ <- dv_spacesM
                               unless True (throwErrorPackratM "True" "not match: " ["dv_spaces"] d378_333 "")
                               d380_334 <- get
                               xx379_335 <- dvCharsM
                               case xx379_335 of
                                   '#' -> return ()
                                   _ -> throwErrorPackratM "'#'" "not match pattern: " ["dvChars"] d380_334 ""
                               let '#' = xx379_335
                               return ()
                               unless True (throwErrorPackratM "True" "not match: " ["dvChars"] d380_334 "")
                               d382_336 <- get
                               xx381_337 <- dvCharsM
                               case xx381_337 of
                                   '-' -> return ()
                                   _ -> throwErrorPackratM "'-'" "not match pattern: " ["dvChars"] d382_336 ""
                               let '-' = xx381_337
                               return ()
                               unless True (throwErrorPackratM "True" "not match: " ["dvChars"] d382_336 "")
                               d384_338 <- get
                               xx383_339 <- dvCharsM
                               case xx383_339 of
                                   '}' -> return ()
                                   _ -> throwErrorPackratM "'}'" "not match pattern: " ["dvChars"] d384_338 ""
                               let '}' = xx383_339
                               return ()
                               unless True (throwErrorPackratM "True" "not match: " ["dvChars"] d384_338 "")
                               d386_340 <- get
                               _ <- dv_spacesM
                               unless True (throwErrorPackratM "True" "not match: " ["dv_spaces"] d386_340 "")
                               return s]
p_leaf = foldl1 mplus [do d388_341 <- get
                          xx387_342 <- dv_readFromLsM
                          let rf = xx387_342
                          unless True (throwErrorPackratM "True" "not match: " ["dv_readFromLs"] d388_341 "")
                          d390_343 <- get
                          xx389_344 <- dv_testM
                          let t = xx389_344
                          unless True (throwErrorPackratM "True" "not match: " ["dv_test"] d390_343 "")
                          return (rf, t),
                       do d392_345 <- get
                          xx391_346 <- dv_readFromLsM
                          let rf = xx391_346
                          unless True (throwErrorPackratM "True" "not match: " ["dv_readFromLs"] d392_345 "")
                          return (rf, (true, "")),
                       do d394_347 <- get
                          xx393_348 <- dv_testM
                          let t = xx393_348
                          unless True (throwErrorPackratM "True" "not match: " ["dv_test"] d394_347 "")
                          return (FromToken, t)]
p_patOp = foldl1 mplus [do d396_349 <- get
                           xx395_350 <- dv_patM
                           let p = xx395_350
                           unless True (throwErrorPackratM "True" "not match: " ["dv_pat"] d396_349 "")
                           d398_351 <- get
                           xx397_352 <- dv_opConNameM
                           let o = xx397_352
                           unless True (throwErrorPackratM "True" "not match: " ["dv_opConName"] d398_351 "")
                           d400_353 <- get
                           xx399_354 <- dv_patOpM
                           let po = xx399_354
                           unless True (throwErrorPackratM "True" "not match: " ["dv_patOp"] d400_353 "")
                           return (uInfixP p o po),
                        do d402_355 <- get
                           xx401_356 <- dv_patM
                           let p = xx401_356
                           unless True (throwErrorPackratM "True" "not match: " ["dv_pat"] d402_355 "")
                           d404_357 <- get
                           _ <- dv_spacesM
                           unless True (throwErrorPackratM "True" "not match: " ["dv_spaces"] d404_357 "")
                           d406_358 <- get
                           xx405_359 <- dvCharsM
                           let q = xx405_359
                           unless (isBQ q) (throwErrorPackratM "isBQ q" "not match: " ["dvChars"] d406_358 "")
                           d408_360 <- get
                           xx407_361 <- dv_typM
                           let t = xx407_361
                           unless True (throwErrorPackratM "True" "not match: " ["dv_typ"] d408_360 "")
                           d410_362 <- get
                           xx409_363 <- dvCharsM
                           let q_ = xx409_363
                           unless (isBQ q_) (throwErrorPackratM "isBQ q_" "not match: " ["dvChars"] d410_362 "")
                           d412_364 <- get
                           _ <- dv_spacesM
                           unless True (throwErrorPackratM "True" "not match: " ["dv_spaces"] d412_364 "")
                           d414_365 <- get
                           xx413_366 <- dv_patOpM
                           let po = xx413_366
                           unless True (throwErrorPackratM "True" "not match: " ["dv_patOp"] d414_365 "")
                           return (uInfixP p (mkName t) po),
                        do d416_367 <- get
                           xx415_368 <- dv_patM
                           let p = xx415_368
                           unless True (throwErrorPackratM "True" "not match: " ["dv_pat"] d416_367 "")
                           return p]
p_pat = foldl1 mplus [do d418_369 <- get
                         xx417_370 <- dv_typM
                         let t = xx417_370
                         unless True (throwErrorPackratM "True" "not match: " ["dv_typ"] d418_369 "")
                         d420_371 <- get
                         _ <- dv_spacesM
                         unless True (throwErrorPackratM "True" "not match: " ["dv_spaces"] d420_371 "")
                         d422_372 <- get
                         xx421_373 <- dv_patsM
                         let ps = xx421_373
                         unless True (throwErrorPackratM "True" "not match: " ["dv_pats"] d422_372 "")
                         return (conToPatQ t ps),
                      do d424_374 <- get
                         xx423_375 <- dvCharsM
                         case xx423_375 of
                             '(' -> return ()
                             _ -> throwErrorPackratM "'('" "not match pattern: " ["dvChars"] d424_374 ""
                         let '(' = xx423_375
                         return ()
                         unless True (throwErrorPackratM "True" "not match: " ["dvChars"] d424_374 "")
                         d426_376 <- get
                         xx425_377 <- dv_opConNameM
                         let o = xx425_377
                         unless True (throwErrorPackratM "True" "not match: " ["dv_opConName"] d426_376 "")
                         d428_378 <- get
                         xx427_379 <- dvCharsM
                         case xx427_379 of
                             ')' -> return ()
                             _ -> throwErrorPackratM "')'" "not match pattern: " ["dvChars"] d428_378 ""
                         let ')' = xx427_379
                         return ()
                         unless True (throwErrorPackratM "True" "not match: " ["dvChars"] d428_378 "")
                         d430_380 <- get
                         _ <- dv_spacesM
                         unless True (throwErrorPackratM "True" "not match: " ["dv_spaces"] d430_380 "")
                         d432_381 <- get
                         xx431_382 <- dv_patsM
                         let ps = xx431_382
                         unless True (throwErrorPackratM "True" "not match: " ["dv_pats"] d432_381 "")
                         return (conP o ps),
                      do d434_383 <- get
                         xx433_384 <- dv_pat1M
                         let p = xx433_384
                         unless True (throwErrorPackratM "True" "not match: " ["dv_pat1"] d434_383 "")
                         return p]
p_pat1 = foldl1 mplus [do d436_385 <- get
                          xx435_386 <- dv_typM
                          let t = xx435_386
                          unless True (throwErrorPackratM "True" "not match: " ["dv_typ"] d436_385 "")
                          return (conToPatQ t emp),
                       do d438_387 <- get
                          xx437_388 <- dv_variableM
                          case xx437_388 of
                              "_" -> return ()
                              _ -> throwErrorPackratM "\"_\"" "not match pattern: " ["dv_variable"] d438_387 ""
                          let "_" = xx437_388
                          return ()
                          unless True (throwErrorPackratM "True" "not match: " ["dv_variable"] d438_387 "")
                          return wildP,
                       do d440_389 <- get
                          xx439_390 <- dv_variableM
                          let n = xx439_390
                          unless True (throwErrorPackratM "True" "not match: " ["dv_variable"] d440_389 "")
                          return (strToPatQ n),
                       do d442_391 <- get
                          xx441_392 <- dv_integerM
                          let i = xx441_392
                          unless True (throwErrorPackratM "True" "not match: " ["dv_integer"] d442_391 "")
                          return (litP (integerL i)),
                       do d444_393 <- get
                          xx443_394 <- dvCharsM
                          case xx443_394 of
                              '-' -> return ()
                              _ -> throwErrorPackratM "'-'" "not match pattern: " ["dvChars"] d444_393 ""
                          let '-' = xx443_394
                          return ()
                          unless True (throwErrorPackratM "True" "not match: " ["dvChars"] d444_393 "")
                          d446_395 <- get
                          _ <- dv_spacesM
                          unless True (throwErrorPackratM "True" "not match: " ["dv_spaces"] d446_395 "")
                          d448_396 <- get
                          xx447_397 <- dv_integerM
                          let i = xx447_397
                          unless True (throwErrorPackratM "True" "not match: " ["dv_integer"] d448_396 "")
                          return (litP (integerL $ negate i)),
                       do d450_398 <- get
                          xx449_399 <- dvCharsM
                          case xx449_399 of
                              '\'' -> return ()
                              _ -> throwErrorPackratM "'\\''" "not match pattern: " ["dvChars"] d450_398 ""
                          let '\'' = xx449_399
                          return ()
                          unless True (throwErrorPackratM "True" "not match: " ["dvChars"] d450_398 "")
                          d452_400 <- get
                          xx451_401 <- dv_charLitM
                          let c = xx451_401
                          unless True (throwErrorPackratM "True" "not match: " ["dv_charLit"] d452_400 "")
                          d454_402 <- get
                          xx453_403 <- dvCharsM
                          case xx453_403 of
                              '\'' -> return ()
                              _ -> throwErrorPackratM "'\\''" "not match pattern: " ["dvChars"] d454_402 ""
                          let '\'' = xx453_403
                          return ()
                          unless True (throwErrorPackratM "True" "not match: " ["dvChars"] d454_402 "")
                          return (charP c),
                       do d456_404 <- get
                          xx455_405 <- dvCharsM
                          case xx455_405 of
                              '"' -> return ()
                              _ -> throwErrorPackratM "'\"'" "not match pattern: " ["dvChars"] d456_404 ""
                          let '"' = xx455_405
                          return ()
                          unless True (throwErrorPackratM "True" "not match: " ["dvChars"] d456_404 "")
                          d458_406 <- get
                          xx457_407 <- dv_stringLitM
                          let s = xx457_407
                          unless True (throwErrorPackratM "True" "not match: " ["dv_stringLit"] d458_406 "")
                          d460_408 <- get
                          xx459_409 <- dvCharsM
                          case xx459_409 of
                              '"' -> return ()
                              _ -> throwErrorPackratM "'\"'" "not match pattern: " ["dvChars"] d460_408 ""
                          let '"' = xx459_409
                          return ()
                          unless True (throwErrorPackratM "True" "not match: " ["dvChars"] d460_408 "")
                          return (stringP s),
                       do d462_410 <- get
                          xx461_411 <- dvCharsM
                          case xx461_411 of
                              '(' -> return ()
                              _ -> throwErrorPackratM "'('" "not match pattern: " ["dvChars"] d462_410 ""
                          let '(' = xx461_411
                          return ()
                          unless True (throwErrorPackratM "True" "not match: " ["dvChars"] d462_410 "")
                          d464_412 <- get
                          xx463_413 <- dv_patListM
                          let p = xx463_413
                          unless True (throwErrorPackratM "True" "not match: " ["dv_patList"] d464_412 "")
                          d466_414 <- get
                          xx465_415 <- dvCharsM
                          case xx465_415 of
                              ')' -> return ()
                              _ -> throwErrorPackratM "')'" "not match pattern: " ["dvChars"] d466_414 ""
                          let ')' = xx465_415
                          return ()
                          unless True (throwErrorPackratM "True" "not match: " ["dvChars"] d466_414 "")
                          return (tupP p),
                       do d468_416 <- get
                          xx467_417 <- dvCharsM
                          case xx467_417 of
                              '[' -> return ()
                              _ -> throwErrorPackratM "'['" "not match pattern: " ["dvChars"] d468_416 ""
                          let '[' = xx467_417
                          return ()
                          unless True (throwErrorPackratM "True" "not match: " ["dvChars"] d468_416 "")
                          d470_418 <- get
                          xx469_419 <- dv_patListM
                          let p = xx469_419
                          unless True (throwErrorPackratM "True" "not match: " ["dv_patList"] d470_418 "")
                          d472_420 <- get
                          xx471_421 <- dvCharsM
                          case xx471_421 of
                              ']' -> return ()
                              _ -> throwErrorPackratM "']'" "not match pattern: " ["dvChars"] d472_420 ""
                          let ']' = xx471_421
                          return ()
                          unless True (throwErrorPackratM "True" "not match: " ["dvChars"] d472_420 "")
                          return (listP p)]
p_patList = foldl1 mplus [do d474_422 <- get
                             xx473_423 <- dv_patOpM
                             let p = xx473_423
                             unless True (throwErrorPackratM "True" "not match: " ["dv_patOp"] d474_422 "")
                             d476_424 <- get
                             _ <- dv_spacesM
                             unless True (throwErrorPackratM "True" "not match: " ["dv_spaces"] d476_424 "")
                             d478_425 <- get
                             xx477_426 <- dvCharsM
                             case xx477_426 of
                                 ',' -> return ()
                                 _ -> throwErrorPackratM "','" "not match pattern: " ["dvChars"] d478_425 ""
                             let ',' = xx477_426
                             return ()
                             unless True (throwErrorPackratM "True" "not match: " ["dvChars"] d478_425 "")
                             d480_427 <- get
                             _ <- dv_spacesM
                             unless True (throwErrorPackratM "True" "not match: " ["dv_spaces"] d480_427 "")
                             d482_428 <- get
                             xx481_429 <- dv_patListM
                             let ps = xx481_429
                             unless True (throwErrorPackratM "True" "not match: " ["dv_patList"] d482_428 "")
                             return (p : ps),
                          do d484_430 <- get
                             xx483_431 <- dv_patOpM
                             let p = xx483_431
                             unless True (throwErrorPackratM "True" "not match: " ["dv_patOp"] d484_430 "")
                             return [p],
                          do return []]
p_opConName = foldl1 mplus [do d486_432 <- get
                               xx485_433 <- dvCharsM
                               case xx485_433 of
                                   ':' -> return ()
                                   _ -> throwErrorPackratM "':'" "not match pattern: " ["dvChars"] d486_432 ""
                               let ':' = xx485_433
                               return ()
                               unless True (throwErrorPackratM "True" "not match: " ["dvChars"] d486_432 "")
                               d488_434 <- get
                               xx487_435 <- dv_opTailM
                               let ot = xx487_435
                               unless True (throwErrorPackratM "True" "not match: " ["dv_opTail"] d488_434 "")
                               return (mkName $ colon : ot)]
p_charLit = foldl1 mplus [do d490_436 <- get
                             xx489_437 <- dvCharsM
                             let c = xx489_437
                             unless (isAlphaNumOt c) (throwErrorPackratM "isAlphaNumOt c" "not match: " ["dvChars"] d490_436 "")
                             return c,
                          do d492_438 <- get
                             xx491_439 <- dvCharsM
                             case xx491_439 of
                                 '\\' -> return ()
                                 _ -> throwErrorPackratM "'\\\\'" "not match pattern: " ["dvChars"] d492_438 ""
                             let '\\' = xx491_439
                             return ()
                             unless True (throwErrorPackratM "True" "not match: " ["dvChars"] d492_438 "")
                             d494_440 <- get
                             xx493_441 <- dv_escapeCM
                             let c = xx493_441
                             unless True (throwErrorPackratM "True" "not match: " ["dv_escapeC"] d494_440 "")
                             return c]
p_stringLit = foldl1 mplus [do d496_442 <- get
                               xx495_443 <- dvCharsM
                               let c = xx495_443
                               unless (isStrLitC c) (throwErrorPackratM "isStrLitC c" "not match: " ["dvChars"] d496_442 "")
                               d498_444 <- get
                               xx497_445 <- dv_stringLitM
                               let s = xx497_445
                               unless True (throwErrorPackratM "True" "not match: " ["dv_stringLit"] d498_444 "")
                               return (cons c s),
                            do d500_446 <- get
                               xx499_447 <- dvCharsM
                               case xx499_447 of
                                   '\\' -> return ()
                                   _ -> throwErrorPackratM "'\\\\'" "not match pattern: " ["dvChars"] d500_446 ""
                               let '\\' = xx499_447
                               return ()
                               unless True (throwErrorPackratM "True" "not match: " ["dvChars"] d500_446 "")
                               d502_448 <- get
                               xx501_449 <- dv_escapeCM
                               let c = xx501_449
                               unless True (throwErrorPackratM "True" "not match: " ["dv_escapeC"] d502_448 "")
                               d504_450 <- get
                               xx503_451 <- dv_stringLitM
                               let s = xx503_451
                               unless True (throwErrorPackratM "True" "not match: " ["dv_stringLit"] d504_450 "")
                               return (c : s),
                            do return emp]
p_escapeC = foldl1 mplus [do d506_452 <- get
                             xx505_453 <- dvCharsM
                             case xx505_453 of
                                 '"' -> return ()
                                 _ -> throwErrorPackratM "'\"'" "not match pattern: " ["dvChars"] d506_452 ""
                             let '"' = xx505_453
                             return ()
                             unless True (throwErrorPackratM "True" "not match: " ["dvChars"] d506_452 "")
                             return '"',
                          do d508_454 <- get
                             xx507_455 <- dvCharsM
                             case xx507_455 of
                                 '\'' -> return ()
                                 _ -> throwErrorPackratM "'\\''" "not match pattern: " ["dvChars"] d508_454 ""
                             let '\'' = xx507_455
                             return ()
                             unless True (throwErrorPackratM "True" "not match: " ["dvChars"] d508_454 "")
                             return '\'',
                          do d510_456 <- get
                             xx509_457 <- dvCharsM
                             case xx509_457 of
                                 '\\' -> return ()
                                 _ -> throwErrorPackratM "'\\\\'" "not match pattern: " ["dvChars"] d510_456 ""
                             let '\\' = xx509_457
                             return ()
                             unless True (throwErrorPackratM "True" "not match: " ["dvChars"] d510_456 "")
                             return '\\',
                          do d512_458 <- get
                             xx511_459 <- dvCharsM
                             case xx511_459 of
                                 'n' -> return ()
                                 _ -> throwErrorPackratM "'n'" "not match pattern: " ["dvChars"] d512_458 ""
                             let 'n' = xx511_459
                             return ()
                             unless True (throwErrorPackratM "True" "not match: " ["dvChars"] d512_458 "")
                             return newLine,
                          do d514_460 <- get
                             xx513_461 <- dvCharsM
                             case xx513_461 of
                                 't' -> return ()
                                 _ -> throwErrorPackratM "'t'" "not match pattern: " ["dvChars"] d514_460 ""
                             let 't' = xx513_461
                             return ()
                             unless True (throwErrorPackratM "True" "not match: " ["dvChars"] d514_460 "")
                             return tab]
p_dq = foldl1 mplus [do d516_462 <- get
                        xx515_463 <- dvCharsM
                        case xx515_463 of
                            '"' -> return ()
                            _ -> throwErrorPackratM "'\"'" "not match pattern: " ["dvChars"] d516_462 ""
                        let '"' = xx515_463
                        return ()
                        unless True (throwErrorPackratM "True" "not match: " ["dvChars"] d516_462 "")
                        return ()]
p_pats = foldl1 mplus [do d518_464 <- get
                          xx517_465 <- dv_patM
                          let p = xx517_465
                          unless True (throwErrorPackratM "True" "not match: " ["dv_pat"] d518_464 "")
                          d520_466 <- get
                          _ <- dv_spacesM
                          unless True (throwErrorPackratM "True" "not match: " ["dv_spaces"] d520_466 "")
                          d522_467 <- get
                          xx521_468 <- dv_patsM
                          let ps = xx521_468
                          unless True (throwErrorPackratM "True" "not match: " ["dv_pats"] d522_467 "")
                          return (cons p ps),
                       do return emp]
p_readFromLs = foldl1 mplus [do d524_469 <- get
                                xx523_470 <- dv_readFromM
                                let rf = xx523_470
                                unless True (throwErrorPackratM "True" "not match: " ["dv_readFrom"] d524_469 "")
                                d526_471 <- get
                                xx525_472 <- dvCharsM
                                case xx525_472 of
                                    '*' -> return ()
                                    _ -> throwErrorPackratM "'*'" "not match pattern: " ["dvChars"] d526_471 ""
                                let '*' = xx525_472
                                return ()
                                unless True (throwErrorPackratM "True" "not match: " ["dvChars"] d526_471 "")
                                return (FromList rf),
                             do d528_473 <- get
                                xx527_474 <- dv_readFromM
                                let rf = xx527_474
                                unless True (throwErrorPackratM "True" "not match: " ["dv_readFrom"] d528_473 "")
                                d530_475 <- get
                                xx529_476 <- dvCharsM
                                case xx529_476 of
                                    '+' -> return ()
                                    _ -> throwErrorPackratM "'+'" "not match pattern: " ["dvChars"] d530_475 ""
                                let '+' = xx529_476
                                return ()
                                unless True (throwErrorPackratM "True" "not match: " ["dvChars"] d530_475 "")
                                return (FromList1 rf),
                             do d532_477 <- get
                                xx531_478 <- dv_readFromM
                                let rf = xx531_478
                                unless True (throwErrorPackratM "True" "not match: " ["dv_readFrom"] d532_477 "")
                                d534_479 <- get
                                xx533_480 <- dvCharsM
                                case xx533_480 of
                                    '?' -> return ()
                                    _ -> throwErrorPackratM "'?'" "not match pattern: " ["dvChars"] d534_479 ""
                                let '?' = xx533_480
                                return ()
                                unless True (throwErrorPackratM "True" "not match: " ["dvChars"] d534_479 "")
                                return (FromOptional rf),
                             do d536_481 <- get
                                xx535_482 <- dv_readFromM
                                let rf = xx535_482
                                unless True (throwErrorPackratM "True" "not match: " ["dv_readFrom"] d536_481 "")
                                return rf]
p_readFrom = foldl1 mplus [do d538_483 <- get
                              xx537_484 <- dv_variableM
                              let v = xx537_484
                              unless True (throwErrorPackratM "True" "not match: " ["dv_variable"] d538_483 "")
                              return (FromVariable v),
                           do d540_485 <- get
                              xx539_486 <- dvCharsM
                              case xx539_486 of
                                  '(' -> return ()
                                  _ -> throwErrorPackratM "'('" "not match pattern: " ["dvChars"] d540_485 ""
                              let '(' = xx539_486
                              return ()
                              unless True (throwErrorPackratM "True" "not match: " ["dvChars"] d540_485 "")
                              d542_487 <- get
                              xx541_488 <- dv_selectionM
                              let s = xx541_488
                              unless True (throwErrorPackratM "True" "not match: " ["dv_selection"] d542_487 "")
                              d544_489 <- get
                              xx543_490 <- dvCharsM
                              case xx543_490 of
                                  ')' -> return ()
                                  _ -> throwErrorPackratM "')'" "not match pattern: " ["dvChars"] d544_489 ""
                              let ')' = xx543_490
                              return ()
                              unless True (throwErrorPackratM "True" "not match: " ["dvChars"] d544_489 "")
                              return (FromSelection s)]
p_test = foldl1 mplus [do d546_491 <- get
                          xx545_492 <- dvCharsM
                          case xx545_492 of
                              '[' -> return ()
                              _ -> throwErrorPackratM "'['" "not match pattern: " ["dvChars"] d546_491 ""
                          let '[' = xx545_492
                          return ()
                          unless True (throwErrorPackratM "True" "not match: " ["dvChars"] d546_491 "")
                          d548_493 <- get
                          xx547_494 <- dv_hsExpLamM
                          let h = xx547_494
                          unless True (throwErrorPackratM "True" "not match: " ["dv_hsExpLam"] d548_493 "")
                          d550_495 <- get
                          _ <- dv_spacesM
                          unless True (throwErrorPackratM "True" "not match: " ["dv_spaces"] d550_495 "")
                          d552_496 <- get
                          xx551_497 <- papOptional dv_comForErrM
                          let com = xx551_497
                          unless True (throwErrorPackratM "True" "not match: " ["dv_comForErr"] d552_496 "")
                          d554_498 <- get
                          xx553_499 <- dvCharsM
                          case xx553_499 of
                              ']' -> return ()
                              _ -> throwErrorPackratM "']'" "not match pattern: " ["dvChars"] d554_498 ""
                          let ']' = xx553_499
                          return ()
                          unless True (throwErrorPackratM "True" "not match: " ["dvChars"] d554_498 "")
                          return (h, maybe "" id com)]
p_hsExpLam = foldl1 mplus [do d556_500 <- get
                              xx555_501 <- dvCharsM
                              case xx555_501 of
                                  '\\' -> return ()
                                  _ -> throwErrorPackratM "'\\\\'" "not match pattern: " ["dvChars"] d556_500 ""
                              let '\\' = xx555_501
                              return ()
                              unless True (throwErrorPackratM "True" "not match: " ["dvChars"] d556_500 "")
                              d558_502 <- get
                              _ <- dv_spacesM
                              unless True (throwErrorPackratM "True" "not match: " ["dv_spaces"] d558_502 "")
                              d560_503 <- get
                              xx559_504 <- dv_patsM
                              let ps = xx559_504
                              unless True (throwErrorPackratM "True" "not match: " ["dv_pats"] d560_503 "")
                              d562_505 <- get
                              _ <- dv_spacesM
                              unless True (throwErrorPackratM "True" "not match: " ["dv_spaces"] d562_505 "")
                              d564_506 <- get
                              xx563_507 <- dvCharsM
                              case xx563_507 of
                                  '-' -> return ()
                                  _ -> throwErrorPackratM "'-'" "not match pattern: " ["dvChars"] d564_506 ""
                              let '-' = xx563_507
                              return ()
                              unless True (throwErrorPackratM "True" "not match: " ["dvChars"] d564_506 "")
                              d566_508 <- get
                              xx565_509 <- dvCharsM
                              let c = xx565_509
                              unless (isGt c) (throwErrorPackratM "isGt c" "not match: " ["dvChars"] d566_508 "")
                              d568_510 <- get
                              _ <- dv_spacesM
                              unless True (throwErrorPackratM "True" "not match: " ["dv_spaces"] d568_510 "")
                              d570_511 <- get
                              xx569_512 <- dv_hsExpTypM
                              let e = xx569_512
                              unless True (throwErrorPackratM "True" "not match: " ["dv_hsExpTyp"] d570_511 "")
                              return (lamE ps e),
                           do d572_513 <- get
                              xx571_514 <- dv_hsExpTypM
                              let e = xx571_514
                              unless True (throwErrorPackratM "True" "not match: " ["dv_hsExpTyp"] d572_513 "")
                              return e]
p_hsExpTyp = foldl1 mplus [do d574_515 <- get
                              xx573_516 <- dv_hsExpOpM
                              let eo = xx573_516
                              unless True (throwErrorPackratM "True" "not match: " ["dv_hsExpOp"] d574_515 "")
                              d576_517 <- get
                              xx575_518 <- dvCharsM
                              case xx575_518 of
                                  ':' -> return ()
                                  _ -> throwErrorPackratM "':'" "not match pattern: " ["dvChars"] d576_517 ""
                              let ':' = xx575_518
                              return ()
                              unless True (throwErrorPackratM "True" "not match: " ["dvChars"] d576_517 "")
                              d578_519 <- get
                              xx577_520 <- dvCharsM
                              case xx577_520 of
                                  ':' -> return ()
                                  _ -> throwErrorPackratM "':'" "not match pattern: " ["dvChars"] d578_519 ""
                              let ':' = xx577_520
                              return ()
                              unless True (throwErrorPackratM "True" "not match: " ["dvChars"] d578_519 "")
                              d580_521 <- get
                              _ <- dv_spacesM
                              unless True (throwErrorPackratM "True" "not match: " ["dv_spaces"] d580_521 "")
                              d582_522 <- get
                              xx581_523 <- dv_hsTypeArrM
                              let t = xx581_523
                              unless True (throwErrorPackratM "True" "not match: " ["dv_hsTypeArr"] d582_522 "")
                              return (sigE eo t),
                           do d584_524 <- get
                              xx583_525 <- dv_hsExpOpM
                              let eo = xx583_525
                              unless True (throwErrorPackratM "True" "not match: " ["dv_hsExpOp"] d584_524 "")
                              return eo]
p_hsExpOp = foldl1 mplus [do d586_526 <- get
                             xx585_527 <- dv_hsExpM
                             let l = xx585_527
                             unless True (throwErrorPackratM "True" "not match: " ["dv_hsExp"] d586_526 "")
                             d588_528 <- get
                             _ <- dv_spacesM
                             unless True (throwErrorPackratM "True" "not match: " ["dv_spaces"] d588_528 "")
                             d590_529 <- get
                             xx589_530 <- dv_hsOpM
                             let o = xx589_530
                             unless True (throwErrorPackratM "True" "not match: " ["dv_hsOp"] d590_529 "")
                             d592_531 <- get
                             _ <- dv_spacesM
                             unless True (throwErrorPackratM "True" "not match: " ["dv_spaces"] d592_531 "")
                             d594_532 <- get
                             xx593_533 <- dv_hsExpOpM
                             let r = xx593_533
                             unless True (throwErrorPackratM "True" "not match: " ["dv_hsExpOp"] d594_532 "")
                             return (uInfixE (getEx l) o r),
                          do d596_534 <- get
                             xx595_535 <- dv_hsExpM
                             let e = xx595_535
                             unless True (throwErrorPackratM "True" "not match: " ["dv_hsExp"] d596_534 "")
                             return (getEx e)]
p_hsOp = foldl1 mplus [do d598_536 <- get
                          xx597_537 <- dvCharsM
                          let c = xx597_537
                          unless (isOpHeadChar c) (throwErrorPackratM "isOpHeadChar c" "not match: " ["dvChars"] d598_536 "")
                          d600_538 <- get
                          xx599_539 <- dv_opTailM
                          let o = xx599_539
                          unless True (throwErrorPackratM "True" "not match: " ["dv_opTail"] d600_538 "")
                          return (varE (mkName (cons c o))),
                       do d602_540 <- get
                          xx601_541 <- dvCharsM
                          case xx601_541 of
                              ':' -> return ()
                              _ -> throwErrorPackratM "':'" "not match pattern: " ["dvChars"] d602_540 ""
                          let ':' = xx601_541
                          return ()
                          unless True (throwErrorPackratM "True" "not match: " ["dvChars"] d602_540 "")
                          ddd603_542 <- get
                          flipMaybe "':':[True]" ddd603_542 ["dvChars"] "" (do d605_543 <- get
                                                                               xx604_544 <- dvCharsM
                                                                               case xx604_544 of
                                                                                   ':' -> return ()
                                                                                   _ -> throwErrorPackratM "':'" "not match pattern: " ["dvChars"] d605_543 ""
                                                                               let ':' = xx604_544
                                                                               return ()
                                                                               unless True (throwErrorPackratM "True" "not match: " ["dvChars"] d605_543 ""))
                          put ddd603_542
                          d607_545 <- get
                          xx606_546 <- dv_opTailM
                          let o = xx606_546
                          unless True (throwErrorPackratM "True" "not match: " ["dv_opTail"] d607_545 "")
                          return (conE (mkName (':' : o))),
                       do d609_547 <- get
                          xx608_548 <- dvCharsM
                          let c = xx608_548
                          unless (isBQ c) (throwErrorPackratM "isBQ c" "not match: " ["dvChars"] d609_547 "")
                          d611_549 <- get
                          xx610_550 <- dv_variableM
                          let v = xx610_550
                          unless True (throwErrorPackratM "True" "not match: " ["dv_variable"] d611_549 "")
                          d613_551 <- get
                          xx612_552 <- dvCharsM
                          let c_ = xx612_552
                          unless (isBQ c_) (throwErrorPackratM "isBQ c_" "not match: " ["dvChars"] d613_551 "")
                          return (varE (mkName v)),
                       do d615_553 <- get
                          xx614_554 <- dvCharsM
                          let c = xx614_554
                          unless (isBQ c) (throwErrorPackratM "isBQ c" "not match: " ["dvChars"] d615_553 "")
                          d617_555 <- get
                          xx616_556 <- dv_typM
                          let t = xx616_556
                          unless True (throwErrorPackratM "True" "not match: " ["dv_typ"] d617_555 "")
                          d619_557 <- get
                          xx618_558 <- dvCharsM
                          let c_ = xx618_558
                          unless (isBQ c_) (throwErrorPackratM "isBQ c_" "not match: " ["dvChars"] d619_557 "")
                          return (conE (mkName t))]
p_opTail = foldl1 mplus [do d621_559 <- get
                            xx620_560 <- dvCharsM
                            let c = xx620_560
                            unless (isOpTailChar c) (throwErrorPackratM "isOpTailChar c" "not match: " ["dvChars"] d621_559 "")
                            d623_561 <- get
                            xx622_562 <- dv_opTailM
                            let s = xx622_562
                            unless True (throwErrorPackratM "True" "not match: " ["dv_opTail"] d623_561 "")
                            return (cons c s),
                         do return emp]
p_hsExp = foldl1 mplus [do d625_563 <- get
                           xx624_564 <- dv_hsExp1M
                           let e = xx624_564
                           unless True (throwErrorPackratM "True" "not match: " ["dv_hsExp1"] d625_563 "")
                           d627_565 <- get
                           _ <- dv_spacesM
                           unless True (throwErrorPackratM "True" "not match: " ["dv_spaces"] d627_565 "")
                           d629_566 <- get
                           xx628_567 <- dv_hsExpM
                           let h = xx628_567
                           unless True (throwErrorPackratM "True" "not match: " ["dv_hsExp"] d629_566 "")
                           return (applyExR e h),
                        do d631_568 <- get
                           xx630_569 <- dv_hsExp1M
                           let e = xx630_569
                           unless True (throwErrorPackratM "True" "not match: " ["dv_hsExp1"] d631_568 "")
                           return (toEx e)]
p_hsExp1 = foldl1 mplus [do d633_570 <- get
                            xx632_571 <- dvCharsM
                            case xx632_571 of
                                '(' -> return ()
                                _ -> throwErrorPackratM "'('" "not match pattern: " ["dvChars"] d633_570 ""
                            let '(' = xx632_571
                            return ()
                            unless True (throwErrorPackratM "True" "not match: " ["dvChars"] d633_570 "")
                            d635_572 <- get
                            xx634_573 <- papOptional (foldl1 mplus [do d637_574 <- get
                                                                       xx636_575 <- dv_hsExpTypM
                                                                       let e = xx636_575
                                                                       unless True (throwErrorPackratM "True" "not match: " ["dv_hsExpTyp"] d637_574 "")
                                                                       return e])
                            let l = xx634_573
                            unless True (throwErrorPackratM "True" "not match: " ["dv_hsExpTyp"] d635_572 "")
                            d639_576 <- get
                            _ <- dv_spacesM
                            unless True (throwErrorPackratM "True" "not match: " ["dv_spaces"] d639_576 "")
                            d641_577 <- get
                            xx640_578 <- dv_hsOpM
                            let o = xx640_578
                            unless True (throwErrorPackratM "True" "not match: " ["dv_hsOp"] d641_577 "")
                            d643_579 <- get
                            _ <- dv_spacesM
                            unless True (throwErrorPackratM "True" "not match: " ["dv_spaces"] d643_579 "")
                            d645_580 <- get
                            xx644_581 <- papOptional (foldl1 mplus [do d647_582 <- get
                                                                       xx646_583 <- dv_hsExpTypM
                                                                       let e = xx646_583
                                                                       unless True (throwErrorPackratM "True" "not match: " ["dv_hsExpTyp"] d647_582 "")
                                                                       return e])
                            let r = xx644_581
                            unless True (throwErrorPackratM "True" "not match: " ["dv_hsExpTyp"] d645_580 "")
                            d649_584 <- get
                            xx648_585 <- dvCharsM
                            case xx648_585 of
                                ')' -> return ()
                                _ -> throwErrorPackratM "')'" "not match pattern: " ["dvChars"] d649_584 ""
                            let ')' = xx648_585
                            return ()
                            unless True (throwErrorPackratM "True" "not match: " ["dvChars"] d649_584 "")
                            return (infixE l o r),
                         do d651_586 <- get
                            xx650_587 <- dvCharsM
                            case xx650_587 of
                                '(' -> return ()
                                _ -> throwErrorPackratM "'('" "not match pattern: " ["dvChars"] d651_586 ""
                            let '(' = xx650_587
                            return ()
                            unless True (throwErrorPackratM "True" "not match: " ["dvChars"] d651_586 "")
                            d653_588 <- get
                            xx652_589 <- dv_hsExpTplM
                            let et = xx652_589
                            unless True (throwErrorPackratM "True" "not match: " ["dv_hsExpTpl"] d653_588 "")
                            d655_590 <- get
                            xx654_591 <- dvCharsM
                            case xx654_591 of
                                ')' -> return ()
                                _ -> throwErrorPackratM "')'" "not match pattern: " ["dvChars"] d655_590 ""
                            let ')' = xx654_591
                            return ()
                            unless True (throwErrorPackratM "True" "not match: " ["dvChars"] d655_590 "")
                            return (tupE et),
                         do d657_592 <- get
                            xx656_593 <- dvCharsM
                            case xx656_593 of
                                '[' -> return ()
                                _ -> throwErrorPackratM "'['" "not match pattern: " ["dvChars"] d657_592 ""
                            let '[' = xx656_593
                            return ()
                            unless True (throwErrorPackratM "True" "not match: " ["dvChars"] d657_592 "")
                            d659_594 <- get
                            xx658_595 <- dv_hsExpTplM
                            let et = xx658_595
                            unless True (throwErrorPackratM "True" "not match: " ["dv_hsExpTpl"] d659_594 "")
                            d661_596 <- get
                            xx660_597 <- dvCharsM
                            case xx660_597 of
                                ']' -> return ()
                                _ -> throwErrorPackratM "']'" "not match pattern: " ["dvChars"] d661_596 ""
                            let ']' = xx660_597
                            return ()
                            unless True (throwErrorPackratM "True" "not match: " ["dvChars"] d661_596 "")
                            return (listE et),
                         do d663_598 <- get
                            xx662_599 <- dv_variableM
                            let v = xx662_599
                            unless True (throwErrorPackratM "True" "not match: " ["dv_variable"] d663_598 "")
                            return (varE (mkName v)),
                         do d665_600 <- get
                            xx664_601 <- dv_typM
                            let t = xx664_601
                            unless True (throwErrorPackratM "True" "not match: " ["dv_typ"] d665_600 "")
                            return (conE (mkName t)),
                         do d667_602 <- get
                            xx666_603 <- dv_integerM
                            let i = xx666_603
                            unless True (throwErrorPackratM "True" "not match: " ["dv_integer"] d667_602 "")
                            d669_604 <- get
                            _ <- dv_spacesM
                            unless True (throwErrorPackratM "True" "not match: " ["dv_spaces"] d669_604 "")
                            return (litE (integerL i)),
                         do d671_605 <- get
                            xx670_606 <- dvCharsM
                            case xx670_606 of
                                '\'' -> return ()
                                _ -> throwErrorPackratM "'\\''" "not match pattern: " ["dvChars"] d671_605 ""
                            let '\'' = xx670_606
                            return ()
                            unless True (throwErrorPackratM "True" "not match: " ["dvChars"] d671_605 "")
                            d673_607 <- get
                            xx672_608 <- dv_charLitM
                            let c = xx672_608
                            unless True (throwErrorPackratM "True" "not match: " ["dv_charLit"] d673_607 "")
                            d675_609 <- get
                            xx674_610 <- dvCharsM
                            case xx674_610 of
                                '\'' -> return ()
                                _ -> throwErrorPackratM "'\\''" "not match pattern: " ["dvChars"] d675_609 ""
                            let '\'' = xx674_610
                            return ()
                            unless True (throwErrorPackratM "True" "not match: " ["dvChars"] d675_609 "")
                            return (litE (charL c)),
                         do d677_611 <- get
                            xx676_612 <- dvCharsM
                            case xx676_612 of
                                '"' -> return ()
                                _ -> throwErrorPackratM "'\"'" "not match pattern: " ["dvChars"] d677_611 ""
                            let '"' = xx676_612
                            return ()
                            unless True (throwErrorPackratM "True" "not match: " ["dvChars"] d677_611 "")
                            d679_613 <- get
                            xx678_614 <- dv_stringLitM
                            let s = xx678_614
                            unless True (throwErrorPackratM "True" "not match: " ["dv_stringLit"] d679_613 "")
                            d681_615 <- get
                            xx680_616 <- dvCharsM
                            case xx680_616 of
                                '"' -> return ()
                                _ -> throwErrorPackratM "'\"'" "not match pattern: " ["dvChars"] d681_615 ""
                            let '"' = xx680_616
                            return ()
                            unless True (throwErrorPackratM "True" "not match: " ["dvChars"] d681_615 "")
                            return (litE (stringL s)),
                         do d683_617 <- get
                            xx682_618 <- dvCharsM
                            case xx682_618 of
                                '-' -> return ()
                                _ -> throwErrorPackratM "'-'" "not match pattern: " ["dvChars"] d683_617 ""
                            let '-' = xx682_618
                            return ()
                            unless True (throwErrorPackratM "True" "not match: " ["dvChars"] d683_617 "")
                            d685_619 <- get
                            _ <- dv_spacesM
                            unless True (throwErrorPackratM "True" "not match: " ["dv_spaces"] d685_619 "")
                            d687_620 <- get
                            xx686_621 <- dv_hsExp1M
                            let e = xx686_621
                            unless True (throwErrorPackratM "True" "not match: " ["dv_hsExp1"] d687_620 "")
                            return (appE (varE $ mkName "negate") e)]
p_hsExpTpl = foldl1 mplus [do d689_622 <- get
                              xx688_623 <- dv_hsExpLamM
                              let e = xx688_623
                              unless True (throwErrorPackratM "True" "not match: " ["dv_hsExpLam"] d689_622 "")
                              d691_624 <- get
                              _ <- dv_spacesM
                              unless True (throwErrorPackratM "True" "not match: " ["dv_spaces"] d691_624 "")
                              d693_625 <- get
                              xx692_626 <- dvCharsM
                              let c = xx692_626
                              unless (isComma c) (throwErrorPackratM "isComma c" "not match: " ["dvChars"] d693_625 "")
                              d695_627 <- get
                              _ <- dv_spacesM
                              unless True (throwErrorPackratM "True" "not match: " ["dv_spaces"] d695_627 "")
                              d697_628 <- get
                              xx696_629 <- dv_hsExpTplM
                              let et = xx696_629
                              unless True (throwErrorPackratM "True" "not match: " ["dv_hsExpTpl"] d697_628 "")
                              return (cons e et),
                           do d699_630 <- get
                              xx698_631 <- dv_hsExpLamM
                              let e = xx698_631
                              unless True (throwErrorPackratM "True" "not match: " ["dv_hsExpLam"] d699_630 "")
                              return (cons e emp),
                           do return emp]
p_hsTypeArr = foldl1 mplus [do d701_632 <- get
                               xx700_633 <- dv_hsTypeM
                               let l = xx700_633
                               unless True (throwErrorPackratM "True" "not match: " ["dv_hsType"] d701_632 "")
                               d703_634 <- get
                               xx702_635 <- dvCharsM
                               case xx702_635 of
                                   '-' -> return ()
                                   _ -> throwErrorPackratM "'-'" "not match pattern: " ["dvChars"] d703_634 ""
                               let '-' = xx702_635
                               return ()
                               unless True (throwErrorPackratM "True" "not match: " ["dvChars"] d703_634 "")
                               d705_636 <- get
                               xx704_637 <- dvCharsM
                               let c = xx704_637
                               unless (isGt c) (throwErrorPackratM "isGt c" "not match: " ["dvChars"] d705_636 "")
                               d707_638 <- get
                               _ <- dv_spacesM
                               unless True (throwErrorPackratM "True" "not match: " ["dv_spaces"] d707_638 "")
                               d709_639 <- get
                               xx708_640 <- dv_hsTypeArrM
                               let r = xx708_640
                               unless True (throwErrorPackratM "True" "not match: " ["dv_hsTypeArr"] d709_639 "")
                               return (appT (appT arrowT (getTyp l)) r),
                            do d711_641 <- get
                               xx710_642 <- dv_hsTypeM
                               let t = xx710_642
                               unless True (throwErrorPackratM "True" "not match: " ["dv_hsType"] d711_641 "")
                               return (getTyp t)]
p_hsType = foldl1 mplus [do d713_643 <- get
                            xx712_644 <- dv_hsType1M
                            let t = xx712_644
                            unless True (throwErrorPackratM "True" "not match: " ["dv_hsType1"] d713_643 "")
                            d715_645 <- get
                            xx714_646 <- dv_hsTypeM
                            let ts = xx714_646
                            unless True (throwErrorPackratM "True" "not match: " ["dv_hsType"] d715_645 "")
                            return (applyTyp (toTyp t) ts),
                         do d717_647 <- get
                            xx716_648 <- dv_hsType1M
                            let t = xx716_648
                            unless True (throwErrorPackratM "True" "not match: " ["dv_hsType1"] d717_647 "")
                            return (toTyp t)]
p_hsType1 = foldl1 mplus [do d719_649 <- get
                             xx718_650 <- dvCharsM
                             case xx718_650 of
                                 '[' -> return ()
                                 _ -> throwErrorPackratM "'['" "not match pattern: " ["dvChars"] d719_649 ""
                             let '[' = xx718_650
                             return ()
                             unless True (throwErrorPackratM "True" "not match: " ["dvChars"] d719_649 "")
                             d721_651 <- get
                             xx720_652 <- dvCharsM
                             case xx720_652 of
                                 ']' -> return ()
                                 _ -> throwErrorPackratM "']'" "not match pattern: " ["dvChars"] d721_651 ""
                             let ']' = xx720_652
                             return ()
                             unless True (throwErrorPackratM "True" "not match: " ["dvChars"] d721_651 "")
                             d723_653 <- get
                             _ <- dv_spacesM
                             unless True (throwErrorPackratM "True" "not match: " ["dv_spaces"] d723_653 "")
                             return listT,
                          do d725_654 <- get
                             xx724_655 <- dvCharsM
                             case xx724_655 of
                                 '[' -> return ()
                                 _ -> throwErrorPackratM "'['" "not match pattern: " ["dvChars"] d725_654 ""
                             let '[' = xx724_655
                             return ()
                             unless True (throwErrorPackratM "True" "not match: " ["dvChars"] d725_654 "")
                             d727_656 <- get
                             xx726_657 <- dv_hsTypeArrM
                             let t = xx726_657
                             unless True (throwErrorPackratM "True" "not match: " ["dv_hsTypeArr"] d727_656 "")
                             d729_658 <- get
                             xx728_659 <- dvCharsM
                             case xx728_659 of
                                 ']' -> return ()
                                 _ -> throwErrorPackratM "']'" "not match pattern: " ["dvChars"] d729_658 ""
                             let ']' = xx728_659
                             return ()
                             unless True (throwErrorPackratM "True" "not match: " ["dvChars"] d729_658 "")
                             d731_660 <- get
                             _ <- dv_spacesM
                             unless True (throwErrorPackratM "True" "not match: " ["dv_spaces"] d731_660 "")
                             return (appT listT t),
                          do d733_661 <- get
                             xx732_662 <- dvCharsM
                             case xx732_662 of
                                 '(' -> return ()
                                 _ -> throwErrorPackratM "'('" "not match pattern: " ["dvChars"] d733_661 ""
                             let '(' = xx732_662
                             return ()
                             unless True (throwErrorPackratM "True" "not match: " ["dvChars"] d733_661 "")
                             d735_663 <- get
                             _ <- dv_spacesM
                             unless True (throwErrorPackratM "True" "not match: " ["dv_spaces"] d735_663 "")
                             d737_664 <- get
                             xx736_665 <- dv_hsTypeTplM
                             let tt = xx736_665
                             unless True (throwErrorPackratM "True" "not match: " ["dv_hsTypeTpl"] d737_664 "")
                             d739_666 <- get
                             xx738_667 <- dvCharsM
                             case xx738_667 of
                                 ')' -> return ()
                                 _ -> throwErrorPackratM "')'" "not match pattern: " ["dvChars"] d739_666 ""
                             let ')' = xx738_667
                             return ()
                             unless True (throwErrorPackratM "True" "not match: " ["dvChars"] d739_666 "")
                             return (tupT tt),
                          do d741_668 <- get
                             xx740_669 <- dv_typTokenM
                             let t = xx740_669
                             unless True (throwErrorPackratM "True" "not match: " ["dv_typToken"] d741_668 "")
                             return (conT (mkName t)),
                          do d743_670 <- get
                             xx742_671 <- dvCharsM
                             case xx742_671 of
                                 '(' -> return ()
                                 _ -> throwErrorPackratM "'('" "not match pattern: " ["dvChars"] d743_670 ""
                             let '(' = xx742_671
                             return ()
                             unless True (throwErrorPackratM "True" "not match: " ["dvChars"] d743_670 "")
                             d745_672 <- get
                             xx744_673 <- dvCharsM
                             case xx744_673 of
                                 '-' -> return ()
                                 _ -> throwErrorPackratM "'-'" "not match pattern: " ["dvChars"] d745_672 ""
                             let '-' = xx744_673
                             return ()
                             unless True (throwErrorPackratM "True" "not match: " ["dvChars"] d745_672 "")
                             d747_674 <- get
                             xx746_675 <- dvCharsM
                             let c = xx746_675
                             unless (isGt c) (throwErrorPackratM "isGt c" "not match: " ["dvChars"] d747_674 "")
                             d749_676 <- get
                             xx748_677 <- dvCharsM
                             case xx748_677 of
                                 ')' -> return ()
                                 _ -> throwErrorPackratM "')'" "not match pattern: " ["dvChars"] d749_676 ""
                             let ')' = xx748_677
                             return ()
                             unless True (throwErrorPackratM "True" "not match: " ["dvChars"] d749_676 "")
                             d751_678 <- get
                             _ <- dv_spacesM
                             unless True (throwErrorPackratM "True" "not match: " ["dv_spaces"] d751_678 "")
                             return arrowT]
p_hsTypeTpl = foldl1 mplus [do d753_679 <- get
                               xx752_680 <- dv_hsTypeArrM
                               let t = xx752_680
                               unless True (throwErrorPackratM "True" "not match: " ["dv_hsTypeArr"] d753_679 "")
                               d755_681 <- get
                               xx754_682 <- dvCharsM
                               let c = xx754_682
                               unless (isComma c) (throwErrorPackratM "isComma c" "not match: " ["dvChars"] d755_681 "")
                               d757_683 <- get
                               _ <- dv_spacesM
                               unless True (throwErrorPackratM "True" "not match: " ["dv_spaces"] d757_683 "")
                               d759_684 <- get
                               xx758_685 <- dv_hsTypeTplM
                               let tt = xx758_685
                               unless True (throwErrorPackratM "True" "not match: " ["dv_hsTypeTpl"] d759_684 "")
                               return (cons t tt),
                            do d761_686 <- get
                               xx760_687 <- dv_hsTypeArrM
                               let t = xx760_687
                               unless True (throwErrorPackratM "True" "not match: " ["dv_hsTypeArr"] d761_686 "")
                               return (cons t emp),
                            do return emp]
p_typ = foldl1 mplus [do d763_688 <- get
                         xx762_689 <- dv_upperM
                         let u = xx762_689
                         unless True (throwErrorPackratM "True" "not match: " ["dv_upper"] d763_688 "")
                         d765_690 <- get
                         xx764_691 <- dv_tvtailM
                         let t = xx764_691
                         unless True (throwErrorPackratM "True" "not match: " ["dv_tvtail"] d765_690 "")
                         return (cons u t)]
p_variable = foldl1 mplus [do d767_692 <- get
                              xx766_693 <- dv_lowerM
                              let l = xx766_693
                              unless True (throwErrorPackratM "True" "not match: " ["dv_lower"] d767_692 "")
                              d769_694 <- get
                              xx768_695 <- dv_tvtailM
                              let t = xx768_695
                              unless True (throwErrorPackratM "True" "not match: " ["dv_tvtail"] d769_694 "")
                              return (cons l t)]
p_tvtail = foldl1 mplus [do d771_696 <- get
                            xx770_697 <- dv_alphaM
                            let a = xx770_697
                            unless True (throwErrorPackratM "True" "not match: " ["dv_alpha"] d771_696 "")
                            d773_698 <- get
                            xx772_699 <- dv_tvtailM
                            let t = xx772_699
                            unless True (throwErrorPackratM "True" "not match: " ["dv_tvtail"] d773_698 "")
                            return (cons a t),
                         do return emp]
p_integer = foldl1 mplus [do d775_700 <- get
                             xx774_701 <- dv_digitM
                             let dh = xx774_701
                             unless True (throwErrorPackratM "True" "not match: " ["dv_digit"] d775_700 "")
                             d777_702 <- get
                             xx776_703 <- list (foldl1 mplus [do d779_704 <- get
                                                                 xx778_705 <- dv_digitM
                                                                 let d = xx778_705
                                                                 unless True (throwErrorPackratM "True" "not match: " ["dv_digit"] d779_704 "")
                                                                 return d])
                             let ds = xx776_703
                             unless True (throwErrorPackratM "True" "not match: " ["dv_digit"] d777_702 "")
                             return (read (cons dh ds))]
p_alpha = foldl1 mplus [do d781_706 <- get
                           xx780_707 <- dv_upperM
                           let u = xx780_707
                           unless True (throwErrorPackratM "True" "not match: " ["dv_upper"] d781_706 "")
                           return u,
                        do d783_708 <- get
                           xx782_709 <- dv_lowerM
                           let l = xx782_709
                           unless True (throwErrorPackratM "True" "not match: " ["dv_lower"] d783_708 "")
                           return l,
                        do d785_710 <- get
                           xx784_711 <- dv_digitM
                           let d = xx784_711
                           unless True (throwErrorPackratM "True" "not match: " ["dv_digit"] d785_710 "")
                           return d,
                        do d787_712 <- get
                           xx786_713 <- dvCharsM
                           case xx786_713 of
                               '\'' -> return ()
                               _ -> throwErrorPackratM "'\\''" "not match pattern: " ["dvChars"] d787_712 ""
                           let '\'' = xx786_713
                           return ()
                           unless True (throwErrorPackratM "True" "not match: " ["dvChars"] d787_712 "")
                           return '\'']
p_upper = foldl1 mplus [do d789_714 <- get
                           xx788_715 <- dvCharsM
                           let u = xx788_715
                           unless (isUpper u) (throwErrorPackratM "isUpper u" "not match: " ["dvChars"] d789_714 "")
                           return u]
p_lower = foldl1 mplus [do d791_716 <- get
                           xx790_717 <- dvCharsM
                           let l = xx790_717
                           unless (isLowerU l) (throwErrorPackratM "isLowerU l" "not match: " ["dvChars"] d791_716 "")
                           return l]
p_digit = foldl1 mplus [do d793_718 <- get
                           xx792_719 <- dvCharsM
                           let d = xx792_719
                           unless (isDigit d) (throwErrorPackratM "isDigit d" "not match: " ["dvChars"] d793_718 "")
                           return d]
p_spaces = foldl1 mplus [do d795_720 <- get
                            _ <- dv_spaceM
                            unless True (throwErrorPackratM "True" "not match: " ["dv_space"] d795_720 "")
                            d797_721 <- get
                            _ <- dv_spacesM
                            unless True (throwErrorPackratM "True" "not match: " ["dv_spaces"] d797_721 "")
                            return (),
                         do return ()]
p_space = foldl1 mplus [do d799_722 <- get
                           xx798_723 <- dvCharsM
                           let s = xx798_723
                           unless (isSpace s) (throwErrorPackratM "isSpace s" "not match: " ["dvChars"] d799_722 "")
                           return (),
                        do d801_724 <- get
                           xx800_725 <- dvCharsM
                           case xx800_725 of
                               '-' -> return ()
                               _ -> throwErrorPackratM "'-'" "not match pattern: " ["dvChars"] d801_724 ""
                           let '-' = xx800_725
                           return ()
                           unless True (throwErrorPackratM "True" "not match: " ["dvChars"] d801_724 "")
                           d803_726 <- get
                           xx802_727 <- dvCharsM
                           case xx802_727 of
                               '-' -> return ()
                               _ -> throwErrorPackratM "'-'" "not match pattern: " ["dvChars"] d803_726 ""
                           let '-' = xx802_727
                           return ()
                           unless True (throwErrorPackratM "True" "not match: " ["dvChars"] d803_726 "")
                           d805_728 <- get
                           _ <- dv_notNLStringM
                           unless True (throwErrorPackratM "True" "not match: " ["dv_notNLString"] d805_728 "")
                           d807_729 <- get
                           _ <- dv_nlM
                           unless True (throwErrorPackratM "True" "not match: " ["dv_nl"] d807_729 "")
                           return (),
                        do d809_730 <- get
                           _ <- dv_commentM
                           unless True (throwErrorPackratM "True" "not match: " ["dv_comment"] d809_730 "")
                           return ()]
p_notNLString = foldl1 mplus [do ddd810_731 <- get
                                 flipMaybe "_:nl[True]" ddd810_731 ["dv_nl"] "" (do d812_732 <- get
                                                                                    _ <- dv_nlM
                                                                                    unless True (throwErrorPackratM "True" "not match: " ["dv_nl"] d812_732 ""))
                                 put ddd810_731
                                 d814_733 <- get
                                 xx813_734 <- dvCharsM
                                 let c = xx813_734
                                 unless True (throwErrorPackratM "True" "not match: " ["dvChars"] d814_733 "")
                                 d816_735 <- get
                                 xx815_736 <- dv_notNLStringM
                                 let s = xx815_736
                                 unless True (throwErrorPackratM "True" "not match: " ["dv_notNLString"] d816_735 "")
                                 return (cons c s),
                              do return emp]
p_nl = foldl1 mplus [do d818_737 <- get
                        xx817_738 <- dvCharsM
                        case xx817_738 of
                            '\n' -> return ()
                            _ -> throwErrorPackratM "'\\n'" "not match pattern: " ["dvChars"] d818_737 ""
                        let '\n' = xx817_738
                        return ()
                        unless True (throwErrorPackratM "True" "not match: " ["dvChars"] d818_737 "")
                        return ()]
p_comment = foldl1 mplus [do d820_739 <- get
                             xx819_740 <- dvCharsM
                             case xx819_740 of
                                 '{' -> return ()
                                 _ -> throwErrorPackratM "'{'" "not match pattern: " ["dvChars"] d820_739 ""
                             let '{' = xx819_740
                             return ()
                             unless True (throwErrorPackratM "True" "not match: " ["dvChars"] d820_739 "")
                             d822_741 <- get
                             xx821_742 <- dvCharsM
                             case xx821_742 of
                                 '-' -> return ()
                                 _ -> throwErrorPackratM "'-'" "not match pattern: " ["dvChars"] d822_741 ""
                             let '-' = xx821_742
                             return ()
                             unless True (throwErrorPackratM "True" "not match: " ["dvChars"] d822_741 "")
                             ddd823_743 <- get
                             flipMaybe "'#':[True]" ddd823_743 ["dvChars"] "" (do d825_744 <- get
                                                                                  xx824_745 <- dvCharsM
                                                                                  case xx824_745 of
                                                                                      '#' -> return ()
                                                                                      _ -> throwErrorPackratM "'#'" "not match pattern: " ["dvChars"] d825_744 ""
                                                                                  let '#' = xx824_745
                                                                                  return ()
                                                                                  unless True (throwErrorPackratM "True" "not match: " ["dvChars"] d825_744 ""))
                             put ddd823_743
                             d827_746 <- get
                             _ <- dv_commentsM
                             unless True (throwErrorPackratM "True" "not match: " ["dv_comments"] d827_746 "")
                             d829_747 <- get
                             _ <- dv_comEndM
                             unless True (throwErrorPackratM "True" "not match: " ["dv_comEnd"] d829_747 "")
                             return ()]
p_comments = foldl1 mplus [do d831_748 <- get
                              _ <- dv_notComStrM
                              unless True (throwErrorPackratM "True" "not match: " ["dv_notComStr"] d831_748 "")
                              d833_749 <- get
                              _ <- dv_commentM
                              unless True (throwErrorPackratM "True" "not match: " ["dv_comment"] d833_749 "")
                              d835_750 <- get
                              _ <- dv_commentsM
                              unless True (throwErrorPackratM "True" "not match: " ["dv_comments"] d835_750 "")
                              return (),
                           do d837_751 <- get
                              _ <- dv_notComStrM
                              unless True (throwErrorPackratM "True" "not match: " ["dv_notComStr"] d837_751 "")
                              return ()]
p_notComStr = foldl1 mplus [do ddd838_752 <- get
                               flipMaybe "_:comment[True]" ddd838_752 ["dv_comment"] "" (do d840_753 <- get
                                                                                            _ <- dv_commentM
                                                                                            unless True (throwErrorPackratM "True" "not match: " ["dv_comment"] d840_753 ""))
                               put ddd838_752
                               ddd841_754 <- get
                               flipMaybe "_:comEnd[True]" ddd841_754 ["dv_comEnd"] "" (do d843_755 <- get
                                                                                          _ <- dv_comEndM
                                                                                          unless True (throwErrorPackratM "True" "not match: " ["dv_comEnd"] d843_755 ""))
                               put ddd841_754
                               d845_756 <- get
                               _ <- dvCharsM
                               unless True (throwErrorPackratM "True" "not match: " ["dvChars"] d845_756 "")
                               d847_757 <- get
                               _ <- dv_notComStrM
                               unless True (throwErrorPackratM "True" "not match: " ["dv_notComStr"] d847_757 "")
                               return (),
                            do return ()]
p_comEnd = foldl1 mplus [do d849_758 <- get
                            xx848_759 <- dvCharsM
                            case xx848_759 of
                                '-' -> return ()
                                _ -> throwErrorPackratM "'-'" "not match pattern: " ["dvChars"] d849_758 ""
                            let '-' = xx848_759
                            return ()
                            unless True (throwErrorPackratM "True" "not match: " ["dvChars"] d849_758 "")
                            d851_760 <- get
                            xx850_761 <- dvCharsM
                            case xx850_761 of
                                '}' -> return ()
                                _ -> throwErrorPackratM "'}'" "not match pattern: " ["dvChars"] d851_760 ""
                            let '}' = xx850_761
                            return ()
                            unless True (throwErrorPackratM "True" "not match: " ["dvChars"] d851_760 "")
                            return ()]
throwErrorPackratM :: forall a . String ->
                                 String -> [String] -> Derivs -> String -> PackratM a
throwErrorPackratM code msg ns d com = do pos <- gets dvPos
                                          throwError (ParseError code msg com pos d ns)
flipMaybe :: forall a . String ->
                        Derivs -> [String] -> String -> PackratM a -> PackratM ()
flipMaybe errMsg d ns com act = do err <- (act >> return False) `catchError` const (return True)
                                   unless err (throwErrorPackratM ('!' : errMsg) "not match: " ns d com)

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