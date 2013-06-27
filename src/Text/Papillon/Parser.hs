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
              dv_pragmaItems :: (Result ([String])),
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
dv_nlM :: PackratM ()
dv_commentM :: PackratM ()
dv_commentsM :: PackratM ()
dv_notComStrM :: PackratM ()
dv_comEndM :: PackratM ()
dv_pragmaM = StateT dv_pragma
dv_pragmaStrM = StateT dv_pragmaStr
dv_pragmaItemsM = StateT dv_pragmaItems
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
          where d = Derivs pegFile pragma pragmaStr pragmaItems delPragmas pragmaEnd moduleDec moduleDecStr whr preImpPap prePeg afterPeg importPapillon varToken typToken pap peg sourceType peg_ definition selection expressionHs expression nameLeaf_ nameLeaf nameLeafNoCom comForErr leaf patOp pat pat1 patList opConName charLit stringLit escapeC dq pats readFromLs readFrom test hsExpLam hsExpTyp hsExpOp hsOp opTail hsExp hsExp1 hsExpTpl hsTypeArr hsType hsType1 hsTypeTpl typ variable tvtail integer alpha upper lower digit spaces space notNLString nl comment comments notComStr comEnd char pos
                pegFile = runStateT p_pegFile d
                pragma = runStateT p_pragma d
                pragmaStr = runStateT p_pragmaStr d
                pragmaItems = runStateT p_pragmaItems d
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
                            _ <- dv_spacesM
                            unless True (throwErrorPackratM "True" "not match: " ["dv_spaces"] d53_46 "")
                            d55_47 <- get
                            xx54_48 <- dvCharsM
                            case xx54_48 of
                                'L' -> return ()
                                _ -> throwErrorPackratM "'L'" "not match pattern: " ["dvChars"] d55_47 ""
                            let 'L' = xx54_48
                            return ()
                            unless True (throwErrorPackratM "True" "not match: " ["dvChars"] d55_47 "")
                            d57_49 <- get
                            xx56_50 <- dvCharsM
                            case xx56_50 of
                                'A' -> return ()
                                _ -> throwErrorPackratM "'A'" "not match pattern: " ["dvChars"] d57_49 ""
                            let 'A' = xx56_50
                            return ()
                            unless True (throwErrorPackratM "True" "not match: " ["dvChars"] d57_49 "")
                            d59_51 <- get
                            xx58_52 <- dvCharsM
                            case xx58_52 of
                                'N' -> return ()
                                _ -> throwErrorPackratM "'N'" "not match pattern: " ["dvChars"] d59_51 ""
                            let 'N' = xx58_52
                            return ()
                            unless True (throwErrorPackratM "True" "not match: " ["dvChars"] d59_51 "")
                            d61_53 <- get
                            xx60_54 <- dvCharsM
                            case xx60_54 of
                                'G' -> return ()
                                _ -> throwErrorPackratM "'G'" "not match pattern: " ["dvChars"] d61_53 ""
                            let 'G' = xx60_54
                            return ()
                            unless True (throwErrorPackratM "True" "not match: " ["dvChars"] d61_53 "")
                            d63_55 <- get
                            xx62_56 <- dvCharsM
                            case xx62_56 of
                                'U' -> return ()
                                _ -> throwErrorPackratM "'U'" "not match pattern: " ["dvChars"] d63_55 ""
                            let 'U' = xx62_56
                            return ()
                            unless True (throwErrorPackratM "True" "not match: " ["dvChars"] d63_55 "")
                            d65_57 <- get
                            xx64_58 <- dvCharsM
                            case xx64_58 of
                                'A' -> return ()
                                _ -> throwErrorPackratM "'A'" "not match pattern: " ["dvChars"] d65_57 ""
                            let 'A' = xx64_58
                            return ()
                            unless True (throwErrorPackratM "True" "not match: " ["dvChars"] d65_57 "")
                            d67_59 <- get
                            xx66_60 <- dvCharsM
                            case xx66_60 of
                                'G' -> return ()
                                _ -> throwErrorPackratM "'G'" "not match pattern: " ["dvChars"] d67_59 ""
                            let 'G' = xx66_60
                            return ()
                            unless True (throwErrorPackratM "True" "not match: " ["dvChars"] d67_59 "")
                            d69_61 <- get
                            xx68_62 <- dvCharsM
                            case xx68_62 of
                                'E' -> return ()
                                _ -> throwErrorPackratM "'E'" "not match pattern: " ["dvChars"] d69_61 ""
                            let 'E' = xx68_62
                            return ()
                            unless True (throwErrorPackratM "True" "not match: " ["dvChars"] d69_61 "")
                            d71_63 <- get
                            _ <- dv_spacesM
                            unless True (throwErrorPackratM "True" "not match: " ["dv_spaces"] d71_63 "")
                            d73_64 <- get
                            xx72_65 <- dv_pragmaItemsM
                            let s = xx72_65
                            unless True (throwErrorPackratM "True" "not match: " ["dv_pragmaItems"] d73_64 "")
                            d75_66 <- get
                            _ <- dv_pragmaEndM
                            unless True (throwErrorPackratM "True" "not match: " ["dv_pragmaEnd"] d75_66 "")
                            d77_67 <- get
                            _ <- dv_spacesM
                            unless True (throwErrorPackratM "True" "not match: " ["dv_spaces"] d77_67 "")
                            return (just $ " LANGUAGE " ++ concatMap (++ ", ") s),
                         do d79_68 <- get
                            _ <- dv_spacesM
                            unless True (throwErrorPackratM "True" "not match: " ["dv_spaces"] d79_68 "")
                            return nothing]
p_pragmaStr = foldl1 mplus [do d81_69 <- get
                               _ <- dv_delPragmasM
                               unless True (throwErrorPackratM "True" "not match: " ["dv_delPragmas"] d81_69 "")
                               d83_70 <- get
                               _ <- dv_spacesM
                               unless True (throwErrorPackratM "True" "not match: " ["dv_spaces"] d83_70 "")
                               d85_71 <- get
                               xx84_72 <- dvCharsM
                               case xx84_72 of
                                   ',' -> return ()
                                   _ -> throwErrorPackratM "','" "not match pattern: " ["dvChars"] d85_71 ""
                               let ',' = xx84_72
                               return ()
                               unless True (throwErrorPackratM "True" "not match: " ["dvChars"] d85_71 "")
                               d87_73 <- get
                               _ <- dv_spacesM
                               unless True (throwErrorPackratM "True" "not match: " ["dv_spaces"] d87_73 "")
                               d89_74 <- get
                               xx88_75 <- dv_pragmaStrM
                               let s = xx88_75
                               unless True (throwErrorPackratM "True" "not match: " ["dv_pragmaStr"] d89_74 "")
                               return (' ' : s),
                            do ddd90_76 <- get
                               flipMaybe "_:pragmaEnd[True]" ddd90_76 ["dv_pragmaEnd"] "" (do d92_77 <- get
                                                                                              _ <- dv_pragmaEndM
                                                                                              unless True (throwErrorPackratM "True" "not match: " ["dv_pragmaEnd"] d92_77 ""))
                               put ddd90_76
                               ddd93_78 <- get
                               flipMaybe "_:delPragmas[True]" ddd93_78 ["dv_delPragmas"] "" (do d95_79 <- get
                                                                                                _ <- dv_delPragmasM
                                                                                                unless True (throwErrorPackratM "True" "not match: " ["dv_delPragmas"] d95_79 ""))
                               put ddd93_78
                               d97_80 <- get
                               xx96_81 <- dvCharsM
                               let c = xx96_81
                               unless True (throwErrorPackratM "True" "not match: " ["dvChars"] d97_80 "")
                               d99_82 <- get
                               xx98_83 <- dv_pragmaStrM
                               let s = xx98_83
                               unless True (throwErrorPackratM "True" "not match: " ["dv_pragmaStr"] d99_82 "")
                               return (c : s),
                            do return emp]
p_pragmaItems = foldl1 mplus [do d101_84 <- get
                                 _ <- dv_delPragmasM
                                 unless True (throwErrorPackratM "True" "not match: " ["dv_delPragmas"] d101_84 "")
                                 d103_85 <- get
                                 _ <- dv_spacesM
                                 unless True (throwErrorPackratM "True" "not match: " ["dv_spaces"] d103_85 "")
                                 d105_86 <- get
                                 xx104_87 <- dvCharsM
                                 case xx104_87 of
                                     ',' -> return ()
                                     _ -> throwErrorPackratM "','" "not match pattern: " ["dvChars"] d105_86 ""
                                 let ',' = xx104_87
                                 return ()
                                 unless True (throwErrorPackratM "True" "not match: " ["dvChars"] d105_86 "")
                                 d107_88 <- get
                                 _ <- dv_spacesM
                                 unless True (throwErrorPackratM "True" "not match: " ["dv_spaces"] d107_88 "")
                                 d109_89 <- get
                                 xx108_90 <- dv_pragmaItemsM
                                 let i = xx108_90
                                 unless True (throwErrorPackratM "True" "not match: " ["dv_pragmaItems"] d109_89 "")
                                 return i,
                              do d111_91 <- get
                                 xx110_92 <- dv_typTokenM
                                 let t = xx110_92
                                 unless True (throwErrorPackratM "True" "not match: " ["dv_typToken"] d111_91 "")
                                 d113_93 <- get
                                 xx112_94 <- dvCharsM
                                 case xx112_94 of
                                     ',' -> return ()
                                     _ -> throwErrorPackratM "','" "not match pattern: " ["dvChars"] d113_93 ""
                                 let ',' = xx112_94
                                 return ()
                                 unless True (throwErrorPackratM "True" "not match: " ["dvChars"] d113_93 "")
                                 d115_95 <- get
                                 _ <- dv_spacesM
                                 unless True (throwErrorPackratM "True" "not match: " ["dv_spaces"] d115_95 "")
                                 d117_96 <- get
                                 xx116_97 <- dv_pragmaItemsM
                                 let i = xx116_97
                                 unless True (throwErrorPackratM "True" "not match: " ["dv_pragmaItems"] d117_96 "")
                                 return (t : i),
                              do d119_98 <- get
                                 _ <- dv_delPragmasM
                                 unless True (throwErrorPackratM "True" "not match: " ["dv_delPragmas"] d119_98 "")
                                 d121_99 <- get
                                 _ <- dv_spacesM
                                 unless True (throwErrorPackratM "True" "not match: " ["dv_spaces"] d121_99 "")
                                 return [],
                              do d123_100 <- get
                                 xx122_101 <- dv_typTokenM
                                 let t = xx122_101
                                 unless True (throwErrorPackratM "True" "not match: " ["dv_typToken"] d123_100 "")
                                 return [t]]
p_delPragmas = foldl1 mplus [do d125_102 <- get
                                xx124_103 <- dvCharsM
                                case xx124_103 of
                                    'Q' -> return ()
                                    _ -> throwErrorPackratM "'Q'" "not match pattern: " ["dvChars"] d125_102 ""
                                let 'Q' = xx124_103
                                return ()
                                unless True (throwErrorPackratM "True" "not match: " ["dvChars"] d125_102 "")
                                d127_104 <- get
                                xx126_105 <- dvCharsM
                                case xx126_105 of
                                    'u' -> return ()
                                    _ -> throwErrorPackratM "'u'" "not match pattern: " ["dvChars"] d127_104 ""
                                let 'u' = xx126_105
                                return ()
                                unless True (throwErrorPackratM "True" "not match: " ["dvChars"] d127_104 "")
                                d129_106 <- get
                                xx128_107 <- dvCharsM
                                case xx128_107 of
                                    'a' -> return ()
                                    _ -> throwErrorPackratM "'a'" "not match pattern: " ["dvChars"] d129_106 ""
                                let 'a' = xx128_107
                                return ()
                                unless True (throwErrorPackratM "True" "not match: " ["dvChars"] d129_106 "")
                                d131_108 <- get
                                xx130_109 <- dvCharsM
                                case xx130_109 of
                                    's' -> return ()
                                    _ -> throwErrorPackratM "'s'" "not match pattern: " ["dvChars"] d131_108 ""
                                let 's' = xx130_109
                                return ()
                                unless True (throwErrorPackratM "True" "not match: " ["dvChars"] d131_108 "")
                                d133_110 <- get
                                xx132_111 <- dvCharsM
                                case xx132_111 of
                                    'i' -> return ()
                                    _ -> throwErrorPackratM "'i'" "not match pattern: " ["dvChars"] d133_110 ""
                                let 'i' = xx132_111
                                return ()
                                unless True (throwErrorPackratM "True" "not match: " ["dvChars"] d133_110 "")
                                d135_112 <- get
                                xx134_113 <- dvCharsM
                                case xx134_113 of
                                    'Q' -> return ()
                                    _ -> throwErrorPackratM "'Q'" "not match pattern: " ["dvChars"] d135_112 ""
                                let 'Q' = xx134_113
                                return ()
                                unless True (throwErrorPackratM "True" "not match: " ["dvChars"] d135_112 "")
                                d137_114 <- get
                                xx136_115 <- dvCharsM
                                case xx136_115 of
                                    'u' -> return ()
                                    _ -> throwErrorPackratM "'u'" "not match pattern: " ["dvChars"] d137_114 ""
                                let 'u' = xx136_115
                                return ()
                                unless True (throwErrorPackratM "True" "not match: " ["dvChars"] d137_114 "")
                                d139_116 <- get
                                xx138_117 <- dvCharsM
                                case xx138_117 of
                                    'o' -> return ()
                                    _ -> throwErrorPackratM "'o'" "not match pattern: " ["dvChars"] d139_116 ""
                                let 'o' = xx138_117
                                return ()
                                unless True (throwErrorPackratM "True" "not match: " ["dvChars"] d139_116 "")
                                d141_118 <- get
                                xx140_119 <- dvCharsM
                                case xx140_119 of
                                    't' -> return ()
                                    _ -> throwErrorPackratM "'t'" "not match pattern: " ["dvChars"] d141_118 ""
                                let 't' = xx140_119
                                return ()
                                unless True (throwErrorPackratM "True" "not match: " ["dvChars"] d141_118 "")
                                d143_120 <- get
                                xx142_121 <- dvCharsM
                                case xx142_121 of
                                    'e' -> return ()
                                    _ -> throwErrorPackratM "'e'" "not match pattern: " ["dvChars"] d143_120 ""
                                let 'e' = xx142_121
                                return ()
                                unless True (throwErrorPackratM "True" "not match: " ["dvChars"] d143_120 "")
                                d145_122 <- get
                                xx144_123 <- dvCharsM
                                case xx144_123 of
                                    's' -> return ()
                                    _ -> throwErrorPackratM "'s'" "not match pattern: " ["dvChars"] d145_122 ""
                                let 's' = xx144_123
                                return ()
                                unless True (throwErrorPackratM "True" "not match: " ["dvChars"] d145_122 "")
                                return (),
                             do d147_124 <- get
                                xx146_125 <- dvCharsM
                                case xx146_125 of
                                    'T' -> return ()
                                    _ -> throwErrorPackratM "'T'" "not match pattern: " ["dvChars"] d147_124 ""
                                let 'T' = xx146_125
                                return ()
                                unless True (throwErrorPackratM "True" "not match: " ["dvChars"] d147_124 "")
                                d149_126 <- get
                                xx148_127 <- dvCharsM
                                case xx148_127 of
                                    'y' -> return ()
                                    _ -> throwErrorPackratM "'y'" "not match pattern: " ["dvChars"] d149_126 ""
                                let 'y' = xx148_127
                                return ()
                                unless True (throwErrorPackratM "True" "not match: " ["dvChars"] d149_126 "")
                                d151_128 <- get
                                xx150_129 <- dvCharsM
                                case xx150_129 of
                                    'p' -> return ()
                                    _ -> throwErrorPackratM "'p'" "not match pattern: " ["dvChars"] d151_128 ""
                                let 'p' = xx150_129
                                return ()
                                unless True (throwErrorPackratM "True" "not match: " ["dvChars"] d151_128 "")
                                d153_130 <- get
                                xx152_131 <- dvCharsM
                                case xx152_131 of
                                    'e' -> return ()
                                    _ -> throwErrorPackratM "'e'" "not match pattern: " ["dvChars"] d153_130 ""
                                let 'e' = xx152_131
                                return ()
                                unless True (throwErrorPackratM "True" "not match: " ["dvChars"] d153_130 "")
                                d155_132 <- get
                                xx154_133 <- dvCharsM
                                case xx154_133 of
                                    'F' -> return ()
                                    _ -> throwErrorPackratM "'F'" "not match pattern: " ["dvChars"] d155_132 ""
                                let 'F' = xx154_133
                                return ()
                                unless True (throwErrorPackratM "True" "not match: " ["dvChars"] d155_132 "")
                                d157_134 <- get
                                xx156_135 <- dvCharsM
                                case xx156_135 of
                                    'a' -> return ()
                                    _ -> throwErrorPackratM "'a'" "not match pattern: " ["dvChars"] d157_134 ""
                                let 'a' = xx156_135
                                return ()
                                unless True (throwErrorPackratM "True" "not match: " ["dvChars"] d157_134 "")
                                d159_136 <- get
                                xx158_137 <- dvCharsM
                                case xx158_137 of
                                    'm' -> return ()
                                    _ -> throwErrorPackratM "'m'" "not match pattern: " ["dvChars"] d159_136 ""
                                let 'm' = xx158_137
                                return ()
                                unless True (throwErrorPackratM "True" "not match: " ["dvChars"] d159_136 "")
                                d161_138 <- get
                                xx160_139 <- dvCharsM
                                case xx160_139 of
                                    'i' -> return ()
                                    _ -> throwErrorPackratM "'i'" "not match pattern: " ["dvChars"] d161_138 ""
                                let 'i' = xx160_139
                                return ()
                                unless True (throwErrorPackratM "True" "not match: " ["dvChars"] d161_138 "")
                                d163_140 <- get
                                xx162_141 <- dvCharsM
                                case xx162_141 of
                                    'l' -> return ()
                                    _ -> throwErrorPackratM "'l'" "not match pattern: " ["dvChars"] d163_140 ""
                                let 'l' = xx162_141
                                return ()
                                unless True (throwErrorPackratM "True" "not match: " ["dvChars"] d163_140 "")
                                d165_142 <- get
                                xx164_143 <- dvCharsM
                                case xx164_143 of
                                    'i' -> return ()
                                    _ -> throwErrorPackratM "'i'" "not match pattern: " ["dvChars"] d165_142 ""
                                let 'i' = xx164_143
                                return ()
                                unless True (throwErrorPackratM "True" "not match: " ["dvChars"] d165_142 "")
                                d167_144 <- get
                                xx166_145 <- dvCharsM
                                case xx166_145 of
                                    'e' -> return ()
                                    _ -> throwErrorPackratM "'e'" "not match pattern: " ["dvChars"] d167_144 ""
                                let 'e' = xx166_145
                                return ()
                                unless True (throwErrorPackratM "True" "not match: " ["dvChars"] d167_144 "")
                                d169_146 <- get
                                xx168_147 <- dvCharsM
                                case xx168_147 of
                                    's' -> return ()
                                    _ -> throwErrorPackratM "'s'" "not match pattern: " ["dvChars"] d169_146 ""
                                let 's' = xx168_147
                                return ()
                                unless True (throwErrorPackratM "True" "not match: " ["dvChars"] d169_146 "")
                                return ()]
p_pragmaEnd = foldl1 mplus [do d171_148 <- get
                               _ <- dv_delPragmasM
                               unless True (throwErrorPackratM "True" "not match: " ["dv_delPragmas"] d171_148 "")
                               d173_149 <- get
                               _ <- dv_spacesM
                               unless True (throwErrorPackratM "True" "not match: " ["dv_spaces"] d173_149 "")
                               d175_150 <- get
                               xx174_151 <- dvCharsM
                               case xx174_151 of
                                   '#' -> return ()
                                   _ -> throwErrorPackratM "'#'" "not match pattern: " ["dvChars"] d175_150 ""
                               let '#' = xx174_151
                               return ()
                               unless True (throwErrorPackratM "True" "not match: " ["dvChars"] d175_150 "")
                               d177_152 <- get
                               xx176_153 <- dvCharsM
                               case xx176_153 of
                                   '-' -> return ()
                                   _ -> throwErrorPackratM "'-'" "not match pattern: " ["dvChars"] d177_152 ""
                               let '-' = xx176_153
                               return ()
                               unless True (throwErrorPackratM "True" "not match: " ["dvChars"] d177_152 "")
                               d179_154 <- get
                               xx178_155 <- dvCharsM
                               case xx178_155 of
                                   '}' -> return ()
                                   _ -> throwErrorPackratM "'}'" "not match pattern: " ["dvChars"] d179_154 ""
                               let '}' = xx178_155
                               return ()
                               unless True (throwErrorPackratM "True" "not match: " ["dvChars"] d179_154 "")
                               return (),
                            do d181_156 <- get
                               xx180_157 <- dvCharsM
                               case xx180_157 of
                                   '#' -> return ()
                                   _ -> throwErrorPackratM "'#'" "not match pattern: " ["dvChars"] d181_156 ""
                               let '#' = xx180_157
                               return ()
                               unless True (throwErrorPackratM "True" "not match: " ["dvChars"] d181_156 "")
                               d183_158 <- get
                               xx182_159 <- dvCharsM
                               case xx182_159 of
                                   '-' -> return ()
                                   _ -> throwErrorPackratM "'-'" "not match pattern: " ["dvChars"] d183_158 ""
                               let '-' = xx182_159
                               return ()
                               unless True (throwErrorPackratM "True" "not match: " ["dvChars"] d183_158 "")
                               d185_160 <- get
                               xx184_161 <- dvCharsM
                               case xx184_161 of
                                   '}' -> return ()
                                   _ -> throwErrorPackratM "'}'" "not match pattern: " ["dvChars"] d185_160 ""
                               let '}' = xx184_161
                               return ()
                               unless True (throwErrorPackratM "True" "not match: " ["dvChars"] d185_160 "")
                               return ()]
p_moduleDec = foldl1 mplus [do d187_162 <- get
                               xx186_163 <- dvCharsM
                               case xx186_163 of
                                   'm' -> return ()
                                   _ -> throwErrorPackratM "'m'" "not match pattern: " ["dvChars"] d187_162 ""
                               let 'm' = xx186_163
                               return ()
                               unless True (throwErrorPackratM "True" "not match: " ["dvChars"] d187_162 "")
                               d189_164 <- get
                               xx188_165 <- dvCharsM
                               case xx188_165 of
                                   'o' -> return ()
                                   _ -> throwErrorPackratM "'o'" "not match pattern: " ["dvChars"] d189_164 ""
                               let 'o' = xx188_165
                               return ()
                               unless True (throwErrorPackratM "True" "not match: " ["dvChars"] d189_164 "")
                               d191_166 <- get
                               xx190_167 <- dvCharsM
                               case xx190_167 of
                                   'd' -> return ()
                                   _ -> throwErrorPackratM "'d'" "not match pattern: " ["dvChars"] d191_166 ""
                               let 'd' = xx190_167
                               return ()
                               unless True (throwErrorPackratM "True" "not match: " ["dvChars"] d191_166 "")
                               d193_168 <- get
                               xx192_169 <- dvCharsM
                               case xx192_169 of
                                   'u' -> return ()
                                   _ -> throwErrorPackratM "'u'" "not match pattern: " ["dvChars"] d193_168 ""
                               let 'u' = xx192_169
                               return ()
                               unless True (throwErrorPackratM "True" "not match: " ["dvChars"] d193_168 "")
                               d195_170 <- get
                               xx194_171 <- dvCharsM
                               case xx194_171 of
                                   'l' -> return ()
                                   _ -> throwErrorPackratM "'l'" "not match pattern: " ["dvChars"] d195_170 ""
                               let 'l' = xx194_171
                               return ()
                               unless True (throwErrorPackratM "True" "not match: " ["dvChars"] d195_170 "")
                               d197_172 <- get
                               xx196_173 <- dvCharsM
                               case xx196_173 of
                                   'e' -> return ()
                                   _ -> throwErrorPackratM "'e'" "not match pattern: " ["dvChars"] d197_172 ""
                               let 'e' = xx196_173
                               return ()
                               unless True (throwErrorPackratM "True" "not match: " ["dvChars"] d197_172 "")
                               d199_174 <- get
                               xx198_175 <- dv_moduleDecStrM
                               let s = xx198_175
                               unless True (throwErrorPackratM "True" "not match: " ["dv_moduleDecStr"] d199_174 "")
                               d201_176 <- get
                               _ <- dv_whrM
                               unless True (throwErrorPackratM "True" "not match: " ["dv_whr"] d201_176 "")
                               return (just s),
                            do return nothing]
p_moduleDecStr = foldl1 mplus [do ddd202_177 <- get
                                  flipMaybe "_:whr[True]" ddd202_177 ["dv_whr"] "" (do d204_178 <- get
                                                                                       _ <- dv_whrM
                                                                                       unless True (throwErrorPackratM "True" "not match: " ["dv_whr"] d204_178 ""))
                                  put ddd202_177
                                  d206_179 <- get
                                  xx205_180 <- dvCharsM
                                  let c = xx205_180
                                  unless True (throwErrorPackratM "True" "not match: " ["dvChars"] d206_179 "")
                                  d208_181 <- get
                                  xx207_182 <- dv_moduleDecStrM
                                  let s = xx207_182
                                  unless True (throwErrorPackratM "True" "not match: " ["dv_moduleDecStr"] d208_181 "")
                                  return (cons c s),
                               do return emp]
p_whr = foldl1 mplus [do d210_183 <- get
                         xx209_184 <- dvCharsM
                         case xx209_184 of
                             'w' -> return ()
                             _ -> throwErrorPackratM "'w'" "not match pattern: " ["dvChars"] d210_183 ""
                         let 'w' = xx209_184
                         return ()
                         unless True (throwErrorPackratM "True" "not match: " ["dvChars"] d210_183 "")
                         d212_185 <- get
                         xx211_186 <- dvCharsM
                         case xx211_186 of
                             'h' -> return ()
                             _ -> throwErrorPackratM "'h'" "not match pattern: " ["dvChars"] d212_185 ""
                         let 'h' = xx211_186
                         return ()
                         unless True (throwErrorPackratM "True" "not match: " ["dvChars"] d212_185 "")
                         d214_187 <- get
                         xx213_188 <- dvCharsM
                         case xx213_188 of
                             'e' -> return ()
                             _ -> throwErrorPackratM "'e'" "not match pattern: " ["dvChars"] d214_187 ""
                         let 'e' = xx213_188
                         return ()
                         unless True (throwErrorPackratM "True" "not match: " ["dvChars"] d214_187 "")
                         d216_189 <- get
                         xx215_190 <- dvCharsM
                         case xx215_190 of
                             'r' -> return ()
                             _ -> throwErrorPackratM "'r'" "not match pattern: " ["dvChars"] d216_189 ""
                         let 'r' = xx215_190
                         return ()
                         unless True (throwErrorPackratM "True" "not match: " ["dvChars"] d216_189 "")
                         d218_191 <- get
                         xx217_192 <- dvCharsM
                         case xx217_192 of
                             'e' -> return ()
                             _ -> throwErrorPackratM "'e'" "not match pattern: " ["dvChars"] d218_191 ""
                         let 'e' = xx217_192
                         return ()
                         unless True (throwErrorPackratM "True" "not match: " ["dvChars"] d218_191 "")
                         return ()]
p_preImpPap = foldl1 mplus [do ddd219_193 <- get
                               flipMaybe "_:importPapillon[True]" ddd219_193 ["dv_importPapillon"] "" (do d221_194 <- get
                                                                                                          _ <- dv_importPapillonM
                                                                                                          unless True (throwErrorPackratM "True" "not match: " ["dv_importPapillon"] d221_194 ""))
                               put ddd219_193
                               ddd222_195 <- get
                               flipMaybe "_:pap[True]" ddd222_195 ["dv_pap"] "" (do d224_196 <- get
                                                                                    _ <- dv_papM
                                                                                    unless True (throwErrorPackratM "True" "not match: " ["dv_pap"] d224_196 ""))
                               put ddd222_195
                               d226_197 <- get
                               xx225_198 <- dvCharsM
                               let c = xx225_198
                               unless True (throwErrorPackratM "True" "not match: " ["dvChars"] d226_197 "")
                               d228_199 <- get
                               xx227_200 <- dv_preImpPapM
                               let pip = xx227_200
                               unless True (throwErrorPackratM "True" "not match: " ["dv_preImpPap"] d228_199 "")
                               return (cons c pip),
                            do return emp]
p_prePeg = foldl1 mplus [do ddd229_201 <- get
                            flipMaybe "_:pap[True]" ddd229_201 ["dv_pap"] "" (do d231_202 <- get
                                                                                 _ <- dv_papM
                                                                                 unless True (throwErrorPackratM "True" "not match: " ["dv_pap"] d231_202 ""))
                            put ddd229_201
                            d233_203 <- get
                            xx232_204 <- dvCharsM
                            let c = xx232_204
                            unless True (throwErrorPackratM "True" "not match: " ["dvChars"] d233_203 "")
                            d235_205 <- get
                            xx234_206 <- dv_prePegM
                            let pp = xx234_206
                            unless True (throwErrorPackratM "True" "not match: " ["dv_prePeg"] d235_205 "")
                            return (cons c pp),
                         do return emp]
p_afterPeg = foldl1 mplus [do d237_207 <- get
                              xx236_208 <- dvCharsM
                              let c = xx236_208
                              unless True (throwErrorPackratM "True" "not match: " ["dvChars"] d237_207 "")
                              d239_209 <- get
                              xx238_210 <- dv_afterPegM
                              let atp = xx238_210
                              unless True (throwErrorPackratM "True" "not match: " ["dv_afterPeg"] d239_209 "")
                              return (cons c atp),
                           do return emp]
p_importPapillon = foldl1 mplus [do d241_211 <- get
                                    xx240_212 <- dv_varTokenM
                                    case xx240_212 of
                                        "import" -> return ()
                                        _ -> throwErrorPackratM "\"import\"" "not match pattern: " ["dv_varToken"] d241_211 ""
                                    let "import" = xx240_212
                                    return ()
                                    unless True (throwErrorPackratM "True" "not match: " ["dv_varToken"] d241_211 "")
                                    d243_213 <- get
                                    xx242_214 <- dv_typTokenM
                                    case xx242_214 of
                                        "Text" -> return ()
                                        _ -> throwErrorPackratM "\"Text\"" "not match pattern: " ["dv_typToken"] d243_213 ""
                                    let "Text" = xx242_214
                                    return ()
                                    unless True (throwErrorPackratM "True" "not match: " ["dv_typToken"] d243_213 "")
                                    d245_215 <- get
                                    xx244_216 <- dvCharsM
                                    case xx244_216 of
                                        '.' -> return ()
                                        _ -> throwErrorPackratM "'.'" "not match pattern: " ["dvChars"] d245_215 ""
                                    let '.' = xx244_216
                                    return ()
                                    unless True (throwErrorPackratM "True" "not match: " ["dvChars"] d245_215 "")
                                    d247_217 <- get
                                    _ <- dv_spacesM
                                    unless True (throwErrorPackratM "True" "not match: " ["dv_spaces"] d247_217 "")
                                    d249_218 <- get
                                    xx248_219 <- dv_typTokenM
                                    case xx248_219 of
                                        "Papillon" -> return ()
                                        _ -> throwErrorPackratM "\"Papillon\"" "not match pattern: " ["dv_typToken"] d249_218 ""
                                    let "Papillon" = xx248_219
                                    return ()
                                    unless True (throwErrorPackratM "True" "not match: " ["dv_typToken"] d249_218 "")
                                    ddd250_220 <- get
                                    flipMaybe "'.':[True]" ddd250_220 ["dvChars"] "" (do d252_221 <- get
                                                                                         xx251_222 <- dvCharsM
                                                                                         case xx251_222 of
                                                                                             '.' -> return ()
                                                                                             _ -> throwErrorPackratM "'.'" "not match pattern: " ["dvChars"] d252_221 ""
                                                                                         let '.' = xx251_222
                                                                                         return ()
                                                                                         unless True (throwErrorPackratM "True" "not match: " ["dvChars"] d252_221 ""))
                                    put ddd250_220
                                    return ()]
p_varToken = foldl1 mplus [do d254_223 <- get
                              xx253_224 <- dv_variableM
                              let v = xx253_224
                              unless True (throwErrorPackratM "True" "not match: " ["dv_variable"] d254_223 "")
                              d256_225 <- get
                              _ <- dv_spacesM
                              unless True (throwErrorPackratM "True" "not match: " ["dv_spaces"] d256_225 "")
                              return v]
p_typToken = foldl1 mplus [do d258_226 <- get
                              xx257_227 <- dv_typM
                              let t = xx257_227
                              unless True (throwErrorPackratM "True" "not match: " ["dv_typ"] d258_226 "")
                              d260_228 <- get
                              _ <- dv_spacesM
                              unless True (throwErrorPackratM "True" "not match: " ["dv_spaces"] d260_228 "")
                              return t]
p_pap = foldl1 mplus [do d262_229 <- get
                         xx261_230 <- dvCharsM
                         case xx261_230 of
                             '\n' -> return ()
                             _ -> throwErrorPackratM "'\\n'" "not match pattern: " ["dvChars"] d262_229 ""
                         let '\n' = xx261_230
                         return ()
                         unless True (throwErrorPackratM "True" "not match: " ["dvChars"] d262_229 "")
                         d264_231 <- get
                         xx263_232 <- dvCharsM
                         case xx263_232 of
                             '[' -> return ()
                             _ -> throwErrorPackratM "'['" "not match pattern: " ["dvChars"] d264_231 ""
                         let '[' = xx263_232
                         return ()
                         unless True (throwErrorPackratM "True" "not match: " ["dvChars"] d264_231 "")
                         d266_233 <- get
                         xx265_234 <- dvCharsM
                         case xx265_234 of
                             'p' -> return ()
                             _ -> throwErrorPackratM "'p'" "not match pattern: " ["dvChars"] d266_233 ""
                         let 'p' = xx265_234
                         return ()
                         unless True (throwErrorPackratM "True" "not match: " ["dvChars"] d266_233 "")
                         d268_235 <- get
                         xx267_236 <- dvCharsM
                         case xx267_236 of
                             'a' -> return ()
                             _ -> throwErrorPackratM "'a'" "not match pattern: " ["dvChars"] d268_235 ""
                         let 'a' = xx267_236
                         return ()
                         unless True (throwErrorPackratM "True" "not match: " ["dvChars"] d268_235 "")
                         d270_237 <- get
                         xx269_238 <- dvCharsM
                         case xx269_238 of
                             'p' -> return ()
                             _ -> throwErrorPackratM "'p'" "not match pattern: " ["dvChars"] d270_237 ""
                         let 'p' = xx269_238
                         return ()
                         unless True (throwErrorPackratM "True" "not match: " ["dvChars"] d270_237 "")
                         d272_239 <- get
                         xx271_240 <- dvCharsM
                         case xx271_240 of
                             'i' -> return ()
                             _ -> throwErrorPackratM "'i'" "not match pattern: " ["dvChars"] d272_239 ""
                         let 'i' = xx271_240
                         return ()
                         unless True (throwErrorPackratM "True" "not match: " ["dvChars"] d272_239 "")
                         d274_241 <- get
                         xx273_242 <- dvCharsM
                         case xx273_242 of
                             'l' -> return ()
                             _ -> throwErrorPackratM "'l'" "not match pattern: " ["dvChars"] d274_241 ""
                         let 'l' = xx273_242
                         return ()
                         unless True (throwErrorPackratM "True" "not match: " ["dvChars"] d274_241 "")
                         d276_243 <- get
                         xx275_244 <- dvCharsM
                         case xx275_244 of
                             'l' -> return ()
                             _ -> throwErrorPackratM "'l'" "not match pattern: " ["dvChars"] d276_243 ""
                         let 'l' = xx275_244
                         return ()
                         unless True (throwErrorPackratM "True" "not match: " ["dvChars"] d276_243 "")
                         d278_245 <- get
                         xx277_246 <- dvCharsM
                         case xx277_246 of
                             'o' -> return ()
                             _ -> throwErrorPackratM "'o'" "not match pattern: " ["dvChars"] d278_245 ""
                         let 'o' = xx277_246
                         return ()
                         unless True (throwErrorPackratM "True" "not match: " ["dvChars"] d278_245 "")
                         d280_247 <- get
                         xx279_248 <- dvCharsM
                         case xx279_248 of
                             'n' -> return ()
                             _ -> throwErrorPackratM "'n'" "not match pattern: " ["dvChars"] d280_247 ""
                         let 'n' = xx279_248
                         return ()
                         unless True (throwErrorPackratM "True" "not match: " ["dvChars"] d280_247 "")
                         d282_249 <- get
                         xx281_250 <- dvCharsM
                         case xx281_250 of
                             '|' -> return ()
                             _ -> throwErrorPackratM "'|'" "not match pattern: " ["dvChars"] d282_249 ""
                         let '|' = xx281_250
                         return ()
                         unless True (throwErrorPackratM "True" "not match: " ["dvChars"] d282_249 "")
                         d284_251 <- get
                         xx283_252 <- dvCharsM
                         case xx283_252 of
                             '\n' -> return ()
                             _ -> throwErrorPackratM "'\\n'" "not match pattern: " ["dvChars"] d284_251 ""
                         let '\n' = xx283_252
                         return ()
                         unless True (throwErrorPackratM "True" "not match: " ["dvChars"] d284_251 "")
                         return ()]
p_peg = foldl1 mplus [do d286_253 <- get
                         _ <- dv_spacesM
                         unless True (throwErrorPackratM "True" "not match: " ["dv_spaces"] d286_253 "")
                         d288_254 <- get
                         xx287_255 <- dv_sourceTypeM
                         let s = xx287_255
                         unless True (throwErrorPackratM "True" "not match: " ["dv_sourceType"] d288_254 "")
                         d290_256 <- get
                         xx289_257 <- dv_peg_M
                         let p = xx289_257
                         unless True (throwErrorPackratM "True" "not match: " ["dv_peg_"] d290_256 "")
                         return (mkTTPeg s p),
                      do d292_258 <- get
                         xx291_259 <- dv_peg_M
                         let p = xx291_259
                         unless True (throwErrorPackratM "True" "not match: " ["dv_peg_"] d292_258 "")
                         return (mkTTPeg tString p)]
p_sourceType = foldl1 mplus [do d294_260 <- get
                                xx293_261 <- dv_varTokenM
                                case xx293_261 of
                                    "source" -> return ()
                                    _ -> throwErrorPackratM "\"source\"" "not match pattern: " ["dv_varToken"] d294_260 ""
                                let "source" = xx293_261
                                return ()
                                unless True (throwErrorPackratM "True" "not match: " ["dv_varToken"] d294_260 "")
                                d296_262 <- get
                                xx295_263 <- dvCharsM
                                case xx295_263 of
                                    ':' -> return ()
                                    _ -> throwErrorPackratM "':'" "not match pattern: " ["dvChars"] d296_262 ""
                                let ':' = xx295_263
                                return ()
                                unless True (throwErrorPackratM "True" "not match: " ["dvChars"] d296_262 "")
                                d298_264 <- get
                                _ <- dv_spacesM
                                unless True (throwErrorPackratM "True" "not match: " ["dv_spaces"] d298_264 "")
                                d300_265 <- get
                                xx299_266 <- dv_typTokenM
                                let v = xx299_266
                                unless True (throwErrorPackratM "True" "not match: " ["dv_typToken"] d300_265 "")
                                return v]
p_peg_ = foldl1 mplus [do d302_267 <- get
                          _ <- dv_spacesM
                          unless True (throwErrorPackratM "True" "not match: " ["dv_spaces"] d302_267 "")
                          d304_268 <- get
                          xx303_269 <- dv_definitionM
                          let d = xx303_269
                          unless True (throwErrorPackratM "True" "not match: " ["dv_definition"] d304_268 "")
                          d306_270 <- get
                          xx305_271 <- dv_peg_M
                          let p = xx305_271
                          unless True (throwErrorPackratM "True" "not match: " ["dv_peg_"] d306_270 "")
                          return (cons d p),
                       do return emp]
p_definition = foldl1 mplus [do d308_272 <- get
                                xx307_273 <- dv_variableM
                                let v = xx307_273
                                unless True (throwErrorPackratM "True" "not match: " ["dv_variable"] d308_272 "")
                                d310_274 <- get
                                _ <- dv_spacesM
                                unless True (throwErrorPackratM "True" "not match: " ["dv_spaces"] d310_274 "")
                                d312_275 <- get
                                xx311_276 <- dvCharsM
                                case xx311_276 of
                                    ':' -> return ()
                                    _ -> throwErrorPackratM "':'" "not match pattern: " ["dvChars"] d312_275 ""
                                let ':' = xx311_276
                                return ()
                                unless True (throwErrorPackratM "True" "not match: " ["dvChars"] d312_275 "")
                                d314_277 <- get
                                xx313_278 <- dvCharsM
                                case xx313_278 of
                                    ':' -> return ()
                                    _ -> throwErrorPackratM "':'" "not match pattern: " ["dvChars"] d314_277 ""
                                let ':' = xx313_278
                                return ()
                                unless True (throwErrorPackratM "True" "not match: " ["dvChars"] d314_277 "")
                                d316_279 <- get
                                _ <- dv_spacesM
                                unless True (throwErrorPackratM "True" "not match: " ["dv_spaces"] d316_279 "")
                                d318_280 <- get
                                xx317_281 <- dv_hsTypeArrM
                                let t = xx317_281
                                unless True (throwErrorPackratM "True" "not match: " ["dv_hsTypeArr"] d318_280 "")
                                d320_282 <- get
                                _ <- dv_spacesM
                                unless True (throwErrorPackratM "True" "not match: " ["dv_spaces"] d320_282 "")
                                d322_283 <- get
                                xx321_284 <- dvCharsM
                                case xx321_284 of
                                    '=' -> return ()
                                    _ -> throwErrorPackratM "'='" "not match pattern: " ["dvChars"] d322_283 ""
                                let '=' = xx321_284
                                return ()
                                unless True (throwErrorPackratM "True" "not match: " ["dvChars"] d322_283 "")
                                d324_285 <- get
                                _ <- dv_spacesM
                                unless True (throwErrorPackratM "True" "not match: " ["dv_spaces"] d324_285 "")
                                d326_286 <- get
                                xx325_287 <- dv_selectionM
                                let sel = xx325_287
                                unless True (throwErrorPackratM "True" "not match: " ["dv_selection"] d326_286 "")
                                d328_288 <- get
                                _ <- dv_spacesM
                                unless True (throwErrorPackratM "True" "not match: " ["dv_spaces"] d328_288 "")
                                d330_289 <- get
                                xx329_290 <- dvCharsM
                                case xx329_290 of
                                    ';' -> return ()
                                    _ -> throwErrorPackratM "';'" "not match pattern: " ["dvChars"] d330_289 ""
                                let ';' = xx329_290
                                return ()
                                unless True (throwErrorPackratM "True" "not match: " ["dvChars"] d330_289 "")
                                return (mkDef v t sel)]
p_selection = foldl1 mplus [do d332_291 <- get
                               xx331_292 <- dv_expressionHsM
                               let ex = xx331_292
                               unless True (throwErrorPackratM "True" "not match: " ["dv_expressionHs"] d332_291 "")
                               d334_293 <- get
                               _ <- dv_spacesM
                               unless True (throwErrorPackratM "True" "not match: " ["dv_spaces"] d334_293 "")
                               d336_294 <- get
                               xx335_295 <- dvCharsM
                               case xx335_295 of
                                   '/' -> return ()
                                   _ -> throwErrorPackratM "'/'" "not match pattern: " ["dvChars"] d336_294 ""
                               let '/' = xx335_295
                               return ()
                               unless True (throwErrorPackratM "True" "not match: " ["dvChars"] d336_294 "")
                               d338_296 <- get
                               _ <- dv_spacesM
                               unless True (throwErrorPackratM "True" "not match: " ["dv_spaces"] d338_296 "")
                               d340_297 <- get
                               xx339_298 <- dv_selectionM
                               let sel = xx339_298
                               unless True (throwErrorPackratM "True" "not match: " ["dv_selection"] d340_297 "")
                               return (cons ex sel),
                            do d342_299 <- get
                               xx341_300 <- dv_expressionHsM
                               let ex = xx341_300
                               unless True (throwErrorPackratM "True" "not match: " ["dv_expressionHs"] d342_299 "")
                               return (cons ex emp)]
p_expressionHs = foldl1 mplus [do d344_301 <- get
                                  xx343_302 <- dv_expressionM
                                  let e = xx343_302
                                  unless True (throwErrorPackratM "True" "not match: " ["dv_expression"] d344_301 "")
                                  d346_303 <- get
                                  _ <- dv_spacesM
                                  unless True (throwErrorPackratM "True" "not match: " ["dv_spaces"] d346_303 "")
                                  d348_304 <- get
                                  xx347_305 <- dvCharsM
                                  case xx347_305 of
                                      '{' -> return ()
                                      _ -> throwErrorPackratM "'{'" "not match pattern: " ["dvChars"] d348_304 ""
                                  let '{' = xx347_305
                                  return ()
                                  unless True (throwErrorPackratM "True" "not match: " ["dvChars"] d348_304 "")
                                  d350_306 <- get
                                  _ <- dv_spacesM
                                  unless True (throwErrorPackratM "True" "not match: " ["dv_spaces"] d350_306 "")
                                  d352_307 <- get
                                  xx351_308 <- dv_hsExpLamM
                                  let h = xx351_308
                                  unless True (throwErrorPackratM "True" "not match: " ["dv_hsExpLam"] d352_307 "")
                                  d354_309 <- get
                                  _ <- dv_spacesM
                                  unless True (throwErrorPackratM "True" "not match: " ["dv_spaces"] d354_309 "")
                                  d356_310 <- get
                                  xx355_311 <- dvCharsM
                                  case xx355_311 of
                                      '}' -> return ()
                                      _ -> throwErrorPackratM "'}'" "not match pattern: " ["dvChars"] d356_310 ""
                                  let '}' = xx355_311
                                  return ()
                                  unless True (throwErrorPackratM "True" "not match: " ["dvChars"] d356_310 "")
                                  return (mkExpressionHs e h)]
p_expression = foldl1 mplus [do d358_312 <- get
                                xx357_313 <- dv_nameLeaf_M
                                let l = xx357_313
                                unless True (throwErrorPackratM "True" "not match: " ["dv_nameLeaf_"] d358_312 "")
                                d360_314 <- get
                                _ <- dv_spacesM
                                unless True (throwErrorPackratM "True" "not match: " ["dv_spaces"] d360_314 "")
                                d362_315 <- get
                                xx361_316 <- dv_expressionM
                                let e = xx361_316
                                unless True (throwErrorPackratM "True" "not match: " ["dv_expression"] d362_315 "")
                                return (cons l e),
                             do return emp]
p_nameLeaf_ = foldl1 mplus [do d364_317 <- get
                               xx363_318 <- dvCharsM
                               case xx363_318 of
                                   '!' -> return ()
                                   _ -> throwErrorPackratM "'!'" "not match pattern: " ["dvChars"] d364_317 ""
                               let '!' = xx363_318
                               return ()
                               unless True (throwErrorPackratM "True" "not match: " ["dvChars"] d364_317 "")
                               d366_319 <- get
                               xx365_320 <- dv_nameLeafNoComM
                               let nl = xx365_320
                               unless True (throwErrorPackratM "True" "not match: " ["dv_nameLeafNoCom"] d366_319 "")
                               d368_321 <- get
                               _ <- dv_spacesM
                               unless True (throwErrorPackratM "True" "not match: " ["dv_spaces"] d368_321 "")
                               d370_322 <- get
                               xx369_323 <- papOptional dv_comForErrM
                               let com = xx369_323
                               unless True (throwErrorPackratM "True" "not match: " ["dv_comForErr"] d370_322 "")
                               return (NotAfter nl $ maybe "" id com),
                            do d372_324 <- get
                               xx371_325 <- dvCharsM
                               let c = xx371_325
                               unless (isAmp c) (throwErrorPackratM "isAmp c" "not match: " ["dvChars"] d372_324 "")
                               d374_326 <- get
                               xx373_327 <- dv_nameLeafM
                               let nl = xx373_327
                               unless True (throwErrorPackratM "True" "not match: " ["dv_nameLeaf"] d374_326 "")
                               return (After nl),
                            do d376_328 <- get
                               xx375_329 <- dv_nameLeafM
                               let nl = xx375_329
                               unless True (throwErrorPackratM "True" "not match: " ["dv_nameLeaf"] d376_328 "")
                               return (Here nl)]
p_nameLeaf = foldl1 mplus [do d378_330 <- get
                              xx377_331 <- dv_pat1M
                              let n = xx377_331
                              unless True (throwErrorPackratM "True" "not match: " ["dv_pat1"] d378_330 "")
                              d380_332 <- get
                              _ <- dv_spacesM
                              unless True (throwErrorPackratM "True" "not match: " ["dv_spaces"] d380_332 "")
                              d382_333 <- get
                              xx381_334 <- papOptional dv_comForErrM
                              let com = xx381_334
                              unless True (throwErrorPackratM "True" "not match: " ["dv_comForErr"] d382_333 "")
                              d384_335 <- get
                              xx383_336 <- dvCharsM
                              case xx383_336 of
                                  ':' -> return ()
                                  _ -> throwErrorPackratM "':'" "not match pattern: " ["dvChars"] d384_335 ""
                              let ':' = xx383_336
                              return ()
                              unless True (throwErrorPackratM "True" "not match: " ["dvChars"] d384_335 "")
                              d386_337 <- get
                              xx385_338 <- dv_leafM
                              let (rf, p) = xx385_338
                              unless True (throwErrorPackratM "True" "not match: " ["dv_leaf"] d386_337 "")
                              return (NameLeaf (n, maybe "" id com) rf p),
                           do d388_339 <- get
                              xx387_340 <- dv_pat1M
                              let n = xx387_340
                              unless True (throwErrorPackratM "True" "not match: " ["dv_pat1"] d388_339 "")
                              d390_341 <- get
                              _ <- dv_spacesM
                              unless True (throwErrorPackratM "True" "not match: " ["dv_spaces"] d390_341 "")
                              d392_342 <- get
                              xx391_343 <- papOptional dv_comForErrM
                              let com = xx391_343
                              unless True (throwErrorPackratM "True" "not match: " ["dv_comForErr"] d392_342 "")
                              return (NameLeaf (n,
                                                maybe "" id com) FromToken (conE $ mkName "True",
                                                                            ""))]
p_nameLeafNoCom = foldl1 mplus [do d394_344 <- get
                                   xx393_345 <- dv_pat1M
                                   let n = xx393_345
                                   unless True (throwErrorPackratM "True" "not match: " ["dv_pat1"] d394_344 "")
                                   d396_346 <- get
                                   _ <- dv_spacesM
                                   unless True (throwErrorPackratM "True" "not match: " ["dv_spaces"] d396_346 "")
                                   d398_347 <- get
                                   xx397_348 <- papOptional dv_comForErrM
                                   let com = xx397_348
                                   unless True (throwErrorPackratM "True" "not match: " ["dv_comForErr"] d398_347 "")
                                   d400_349 <- get
                                   xx399_350 <- dvCharsM
                                   case xx399_350 of
                                       ':' -> return ()
                                       _ -> throwErrorPackratM "':'" "not match pattern: " ["dvChars"] d400_349 ""
                                   let ':' = xx399_350
                                   return ()
                                   unless True (throwErrorPackratM "True" "not match: " ["dvChars"] d400_349 "")
                                   d402_351 <- get
                                   xx401_352 <- dv_leafM
                                   let (rf, p) = xx401_352
                                   unless True (throwErrorPackratM "True" "not match: " ["dv_leaf"] d402_351 "")
                                   return (NameLeaf (n, maybe "" id com) rf p),
                                do d404_353 <- get
                                   xx403_354 <- dv_pat1M
                                   let n = xx403_354
                                   unless True (throwErrorPackratM "True" "not match: " ["dv_pat1"] d404_353 "")
                                   d406_355 <- get
                                   _ <- dv_spacesM
                                   unless True (throwErrorPackratM "True" "not match: " ["dv_spaces"] d406_355 "")
                                   return (NameLeaf (n, "") FromToken (conE $ mkName "True", ""))]
p_comForErr = foldl1 mplus [do d408_356 <- get
                               xx407_357 <- dvCharsM
                               case xx407_357 of
                                   '{' -> return ()
                                   _ -> throwErrorPackratM "'{'" "not match pattern: " ["dvChars"] d408_356 ""
                               let '{' = xx407_357
                               return ()
                               unless True (throwErrorPackratM "True" "not match: " ["dvChars"] d408_356 "")
                               d410_358 <- get
                               xx409_359 <- dvCharsM
                               case xx409_359 of
                                   '-' -> return ()
                                   _ -> throwErrorPackratM "'-'" "not match pattern: " ["dvChars"] d410_358 ""
                               let '-' = xx409_359
                               return ()
                               unless True (throwErrorPackratM "True" "not match: " ["dvChars"] d410_358 "")
                               d412_360 <- get
                               xx411_361 <- dvCharsM
                               case xx411_361 of
                                   '#' -> return ()
                                   _ -> throwErrorPackratM "'#'" "not match pattern: " ["dvChars"] d412_360 ""
                               let '#' = xx411_361
                               return ()
                               unless True (throwErrorPackratM "True" "not match: " ["dvChars"] d412_360 "")
                               d414_362 <- get
                               _ <- dv_spacesM
                               unless True (throwErrorPackratM "True" "not match: " ["dv_spaces"] d414_362 "")
                               d416_363 <- get
                               xx415_364 <- dvCharsM
                               case xx415_364 of
                                   '"' -> return ()
                                   _ -> throwErrorPackratM "'\"'" "not match pattern: " ["dvChars"] d416_363 ""
                               let '"' = xx415_364
                               return ()
                               unless True (throwErrorPackratM "True" "not match: " ["dvChars"] d416_363 "")
                               d418_365 <- get
                               xx417_366 <- dv_stringLitM
                               let s = xx417_366
                               unless True (throwErrorPackratM "True" "not match: " ["dv_stringLit"] d418_365 "")
                               d420_367 <- get
                               xx419_368 <- dvCharsM
                               case xx419_368 of
                                   '"' -> return ()
                                   _ -> throwErrorPackratM "'\"'" "not match pattern: " ["dvChars"] d420_367 ""
                               let '"' = xx419_368
                               return ()
                               unless True (throwErrorPackratM "True" "not match: " ["dvChars"] d420_367 "")
                               d422_369 <- get
                               _ <- dv_spacesM
                               unless True (throwErrorPackratM "True" "not match: " ["dv_spaces"] d422_369 "")
                               d424_370 <- get
                               xx423_371 <- dvCharsM
                               case xx423_371 of
                                   '#' -> return ()
                                   _ -> throwErrorPackratM "'#'" "not match pattern: " ["dvChars"] d424_370 ""
                               let '#' = xx423_371
                               return ()
                               unless True (throwErrorPackratM "True" "not match: " ["dvChars"] d424_370 "")
                               d426_372 <- get
                               xx425_373 <- dvCharsM
                               case xx425_373 of
                                   '-' -> return ()
                                   _ -> throwErrorPackratM "'-'" "not match pattern: " ["dvChars"] d426_372 ""
                               let '-' = xx425_373
                               return ()
                               unless True (throwErrorPackratM "True" "not match: " ["dvChars"] d426_372 "")
                               d428_374 <- get
                               xx427_375 <- dvCharsM
                               case xx427_375 of
                                   '}' -> return ()
                                   _ -> throwErrorPackratM "'}'" "not match pattern: " ["dvChars"] d428_374 ""
                               let '}' = xx427_375
                               return ()
                               unless True (throwErrorPackratM "True" "not match: " ["dvChars"] d428_374 "")
                               d430_376 <- get
                               _ <- dv_spacesM
                               unless True (throwErrorPackratM "True" "not match: " ["dv_spaces"] d430_376 "")
                               return s]
p_leaf = foldl1 mplus [do d432_377 <- get
                          xx431_378 <- dv_readFromLsM
                          let rf = xx431_378
                          unless True (throwErrorPackratM "True" "not match: " ["dv_readFromLs"] d432_377 "")
                          d434_379 <- get
                          xx433_380 <- dv_testM
                          let t = xx433_380
                          unless True (throwErrorPackratM "True" "not match: " ["dv_test"] d434_379 "")
                          return (rf, t),
                       do d436_381 <- get
                          xx435_382 <- dv_readFromLsM
                          let rf = xx435_382
                          unless True (throwErrorPackratM "True" "not match: " ["dv_readFromLs"] d436_381 "")
                          return (rf, (true, "")),
                       do d438_383 <- get
                          xx437_384 <- dv_testM
                          let t = xx437_384
                          unless True (throwErrorPackratM "True" "not match: " ["dv_test"] d438_383 "")
                          return (FromToken, t)]
p_patOp = foldl1 mplus [do d440_385 <- get
                           xx439_386 <- dv_patM
                           let p = xx439_386
                           unless True (throwErrorPackratM "True" "not match: " ["dv_pat"] d440_385 "")
                           d442_387 <- get
                           xx441_388 <- dv_opConNameM
                           let o = xx441_388
                           unless True (throwErrorPackratM "True" "not match: " ["dv_opConName"] d442_387 "")
                           d444_389 <- get
                           xx443_390 <- dv_patOpM
                           let po = xx443_390
                           unless True (throwErrorPackratM "True" "not match: " ["dv_patOp"] d444_389 "")
                           return (uInfixP p o po),
                        do d446_391 <- get
                           xx445_392 <- dv_patM
                           let p = xx445_392
                           unless True (throwErrorPackratM "True" "not match: " ["dv_pat"] d446_391 "")
                           d448_393 <- get
                           _ <- dv_spacesM
                           unless True (throwErrorPackratM "True" "not match: " ["dv_spaces"] d448_393 "")
                           d450_394 <- get
                           xx449_395 <- dvCharsM
                           let q = xx449_395
                           unless (isBQ q) (throwErrorPackratM "isBQ q" "not match: " ["dvChars"] d450_394 "")
                           d452_396 <- get
                           xx451_397 <- dv_typM
                           let t = xx451_397
                           unless True (throwErrorPackratM "True" "not match: " ["dv_typ"] d452_396 "")
                           d454_398 <- get
                           xx453_399 <- dvCharsM
                           let q_ = xx453_399
                           unless (isBQ q_) (throwErrorPackratM "isBQ q_" "not match: " ["dvChars"] d454_398 "")
                           d456_400 <- get
                           _ <- dv_spacesM
                           unless True (throwErrorPackratM "True" "not match: " ["dv_spaces"] d456_400 "")
                           d458_401 <- get
                           xx457_402 <- dv_patOpM
                           let po = xx457_402
                           unless True (throwErrorPackratM "True" "not match: " ["dv_patOp"] d458_401 "")
                           return (uInfixP p (mkName t) po),
                        do d460_403 <- get
                           xx459_404 <- dv_patM
                           let p = xx459_404
                           unless True (throwErrorPackratM "True" "not match: " ["dv_pat"] d460_403 "")
                           return p]
p_pat = foldl1 mplus [do d462_405 <- get
                         xx461_406 <- dv_typM
                         let t = xx461_406
                         unless True (throwErrorPackratM "True" "not match: " ["dv_typ"] d462_405 "")
                         d464_407 <- get
                         _ <- dv_spacesM
                         unless True (throwErrorPackratM "True" "not match: " ["dv_spaces"] d464_407 "")
                         d466_408 <- get
                         xx465_409 <- dv_patsM
                         let ps = xx465_409
                         unless True (throwErrorPackratM "True" "not match: " ["dv_pats"] d466_408 "")
                         return (conToPatQ t ps),
                      do d468_410 <- get
                         xx467_411 <- dvCharsM
                         case xx467_411 of
                             '(' -> return ()
                             _ -> throwErrorPackratM "'('" "not match pattern: " ["dvChars"] d468_410 ""
                         let '(' = xx467_411
                         return ()
                         unless True (throwErrorPackratM "True" "not match: " ["dvChars"] d468_410 "")
                         d470_412 <- get
                         xx469_413 <- dv_opConNameM
                         let o = xx469_413
                         unless True (throwErrorPackratM "True" "not match: " ["dv_opConName"] d470_412 "")
                         d472_414 <- get
                         xx471_415 <- dvCharsM
                         case xx471_415 of
                             ')' -> return ()
                             _ -> throwErrorPackratM "')'" "not match pattern: " ["dvChars"] d472_414 ""
                         let ')' = xx471_415
                         return ()
                         unless True (throwErrorPackratM "True" "not match: " ["dvChars"] d472_414 "")
                         d474_416 <- get
                         _ <- dv_spacesM
                         unless True (throwErrorPackratM "True" "not match: " ["dv_spaces"] d474_416 "")
                         d476_417 <- get
                         xx475_418 <- dv_patsM
                         let ps = xx475_418
                         unless True (throwErrorPackratM "True" "not match: " ["dv_pats"] d476_417 "")
                         return (conP o ps),
                      do d478_419 <- get
                         xx477_420 <- dv_pat1M
                         let p = xx477_420
                         unless True (throwErrorPackratM "True" "not match: " ["dv_pat1"] d478_419 "")
                         return p]
p_pat1 = foldl1 mplus [do d480_421 <- get
                          xx479_422 <- dv_typM
                          let t = xx479_422
                          unless True (throwErrorPackratM "True" "not match: " ["dv_typ"] d480_421 "")
                          return (conToPatQ t emp),
                       do d482_423 <- get
                          xx481_424 <- dv_variableM
                          case xx481_424 of
                              "_" -> return ()
                              _ -> throwErrorPackratM "\"_\"" "not match pattern: " ["dv_variable"] d482_423 ""
                          let "_" = xx481_424
                          return ()
                          unless True (throwErrorPackratM "True" "not match: " ["dv_variable"] d482_423 "")
                          return wildP,
                       do d484_425 <- get
                          xx483_426 <- dv_variableM
                          let n = xx483_426
                          unless True (throwErrorPackratM "True" "not match: " ["dv_variable"] d484_425 "")
                          return (strToPatQ n),
                       do d486_427 <- get
                          xx485_428 <- dv_integerM
                          let i = xx485_428
                          unless True (throwErrorPackratM "True" "not match: " ["dv_integer"] d486_427 "")
                          return (litP (integerL i)),
                       do d488_429 <- get
                          xx487_430 <- dvCharsM
                          case xx487_430 of
                              '-' -> return ()
                              _ -> throwErrorPackratM "'-'" "not match pattern: " ["dvChars"] d488_429 ""
                          let '-' = xx487_430
                          return ()
                          unless True (throwErrorPackratM "True" "not match: " ["dvChars"] d488_429 "")
                          d490_431 <- get
                          _ <- dv_spacesM
                          unless True (throwErrorPackratM "True" "not match: " ["dv_spaces"] d490_431 "")
                          d492_432 <- get
                          xx491_433 <- dv_integerM
                          let i = xx491_433
                          unless True (throwErrorPackratM "True" "not match: " ["dv_integer"] d492_432 "")
                          return (litP (integerL $ negate i)),
                       do d494_434 <- get
                          xx493_435 <- dvCharsM
                          case xx493_435 of
                              '\'' -> return ()
                              _ -> throwErrorPackratM "'\\''" "not match pattern: " ["dvChars"] d494_434 ""
                          let '\'' = xx493_435
                          return ()
                          unless True (throwErrorPackratM "True" "not match: " ["dvChars"] d494_434 "")
                          d496_436 <- get
                          xx495_437 <- dv_charLitM
                          let c = xx495_437
                          unless True (throwErrorPackratM "True" "not match: " ["dv_charLit"] d496_436 "")
                          d498_438 <- get
                          xx497_439 <- dvCharsM
                          case xx497_439 of
                              '\'' -> return ()
                              _ -> throwErrorPackratM "'\\''" "not match pattern: " ["dvChars"] d498_438 ""
                          let '\'' = xx497_439
                          return ()
                          unless True (throwErrorPackratM "True" "not match: " ["dvChars"] d498_438 "")
                          return (charP c),
                       do d500_440 <- get
                          xx499_441 <- dvCharsM
                          case xx499_441 of
                              '"' -> return ()
                              _ -> throwErrorPackratM "'\"'" "not match pattern: " ["dvChars"] d500_440 ""
                          let '"' = xx499_441
                          return ()
                          unless True (throwErrorPackratM "True" "not match: " ["dvChars"] d500_440 "")
                          d502_442 <- get
                          xx501_443 <- dv_stringLitM
                          let s = xx501_443
                          unless True (throwErrorPackratM "True" "not match: " ["dv_stringLit"] d502_442 "")
                          d504_444 <- get
                          xx503_445 <- dvCharsM
                          case xx503_445 of
                              '"' -> return ()
                              _ -> throwErrorPackratM "'\"'" "not match pattern: " ["dvChars"] d504_444 ""
                          let '"' = xx503_445
                          return ()
                          unless True (throwErrorPackratM "True" "not match: " ["dvChars"] d504_444 "")
                          return (stringP s),
                       do d506_446 <- get
                          xx505_447 <- dvCharsM
                          case xx505_447 of
                              '(' -> return ()
                              _ -> throwErrorPackratM "'('" "not match pattern: " ["dvChars"] d506_446 ""
                          let '(' = xx505_447
                          return ()
                          unless True (throwErrorPackratM "True" "not match: " ["dvChars"] d506_446 "")
                          d508_448 <- get
                          xx507_449 <- dv_patListM
                          let p = xx507_449
                          unless True (throwErrorPackratM "True" "not match: " ["dv_patList"] d508_448 "")
                          d510_450 <- get
                          xx509_451 <- dvCharsM
                          case xx509_451 of
                              ')' -> return ()
                              _ -> throwErrorPackratM "')'" "not match pattern: " ["dvChars"] d510_450 ""
                          let ')' = xx509_451
                          return ()
                          unless True (throwErrorPackratM "True" "not match: " ["dvChars"] d510_450 "")
                          return (tupP p),
                       do d512_452 <- get
                          xx511_453 <- dvCharsM
                          case xx511_453 of
                              '[' -> return ()
                              _ -> throwErrorPackratM "'['" "not match pattern: " ["dvChars"] d512_452 ""
                          let '[' = xx511_453
                          return ()
                          unless True (throwErrorPackratM "True" "not match: " ["dvChars"] d512_452 "")
                          d514_454 <- get
                          xx513_455 <- dv_patListM
                          let p = xx513_455
                          unless True (throwErrorPackratM "True" "not match: " ["dv_patList"] d514_454 "")
                          d516_456 <- get
                          xx515_457 <- dvCharsM
                          case xx515_457 of
                              ']' -> return ()
                              _ -> throwErrorPackratM "']'" "not match pattern: " ["dvChars"] d516_456 ""
                          let ']' = xx515_457
                          return ()
                          unless True (throwErrorPackratM "True" "not match: " ["dvChars"] d516_456 "")
                          return (listP p)]
p_patList = foldl1 mplus [do d518_458 <- get
                             xx517_459 <- dv_patOpM
                             let p = xx517_459
                             unless True (throwErrorPackratM "True" "not match: " ["dv_patOp"] d518_458 "")
                             d520_460 <- get
                             _ <- dv_spacesM
                             unless True (throwErrorPackratM "True" "not match: " ["dv_spaces"] d520_460 "")
                             d522_461 <- get
                             xx521_462 <- dvCharsM
                             case xx521_462 of
                                 ',' -> return ()
                                 _ -> throwErrorPackratM "','" "not match pattern: " ["dvChars"] d522_461 ""
                             let ',' = xx521_462
                             return ()
                             unless True (throwErrorPackratM "True" "not match: " ["dvChars"] d522_461 "")
                             d524_463 <- get
                             _ <- dv_spacesM
                             unless True (throwErrorPackratM "True" "not match: " ["dv_spaces"] d524_463 "")
                             d526_464 <- get
                             xx525_465 <- dv_patListM
                             let ps = xx525_465
                             unless True (throwErrorPackratM "True" "not match: " ["dv_patList"] d526_464 "")
                             return (p : ps),
                          do d528_466 <- get
                             xx527_467 <- dv_patOpM
                             let p = xx527_467
                             unless True (throwErrorPackratM "True" "not match: " ["dv_patOp"] d528_466 "")
                             return [p],
                          do return []]
p_opConName = foldl1 mplus [do d530_468 <- get
                               xx529_469 <- dvCharsM
                               case xx529_469 of
                                   ':' -> return ()
                                   _ -> throwErrorPackratM "':'" "not match pattern: " ["dvChars"] d530_468 ""
                               let ':' = xx529_469
                               return ()
                               unless True (throwErrorPackratM "True" "not match: " ["dvChars"] d530_468 "")
                               d532_470 <- get
                               xx531_471 <- dv_opTailM
                               let ot = xx531_471
                               unless True (throwErrorPackratM "True" "not match: " ["dv_opTail"] d532_470 "")
                               return (mkName $ colon : ot)]
p_charLit = foldl1 mplus [do d534_472 <- get
                             xx533_473 <- dvCharsM
                             let c = xx533_473
                             unless (isAlphaNumOt c) (throwErrorPackratM "isAlphaNumOt c" "not match: " ["dvChars"] d534_472 "")
                             return c,
                          do d536_474 <- get
                             xx535_475 <- dvCharsM
                             case xx535_475 of
                                 '\\' -> return ()
                                 _ -> throwErrorPackratM "'\\\\'" "not match pattern: " ["dvChars"] d536_474 ""
                             let '\\' = xx535_475
                             return ()
                             unless True (throwErrorPackratM "True" "not match: " ["dvChars"] d536_474 "")
                             d538_476 <- get
                             xx537_477 <- dv_escapeCM
                             let c = xx537_477
                             unless True (throwErrorPackratM "True" "not match: " ["dv_escapeC"] d538_476 "")
                             return c]
p_stringLit = foldl1 mplus [do d540_478 <- get
                               xx539_479 <- dvCharsM
                               let c = xx539_479
                               unless (isStrLitC c) (throwErrorPackratM "isStrLitC c" "not match: " ["dvChars"] d540_478 "")
                               d542_480 <- get
                               xx541_481 <- dv_stringLitM
                               let s = xx541_481
                               unless True (throwErrorPackratM "True" "not match: " ["dv_stringLit"] d542_480 "")
                               return (cons c s),
                            do d544_482 <- get
                               xx543_483 <- dvCharsM
                               case xx543_483 of
                                   '\\' -> return ()
                                   _ -> throwErrorPackratM "'\\\\'" "not match pattern: " ["dvChars"] d544_482 ""
                               let '\\' = xx543_483
                               return ()
                               unless True (throwErrorPackratM "True" "not match: " ["dvChars"] d544_482 "")
                               d546_484 <- get
                               xx545_485 <- dv_escapeCM
                               let c = xx545_485
                               unless True (throwErrorPackratM "True" "not match: " ["dv_escapeC"] d546_484 "")
                               d548_486 <- get
                               xx547_487 <- dv_stringLitM
                               let s = xx547_487
                               unless True (throwErrorPackratM "True" "not match: " ["dv_stringLit"] d548_486 "")
                               return (c : s),
                            do return emp]
p_escapeC = foldl1 mplus [do d550_488 <- get
                             xx549_489 <- dvCharsM
                             case xx549_489 of
                                 '"' -> return ()
                                 _ -> throwErrorPackratM "'\"'" "not match pattern: " ["dvChars"] d550_488 ""
                             let '"' = xx549_489
                             return ()
                             unless True (throwErrorPackratM "True" "not match: " ["dvChars"] d550_488 "")
                             return '"',
                          do d552_490 <- get
                             xx551_491 <- dvCharsM
                             case xx551_491 of
                                 '\'' -> return ()
                                 _ -> throwErrorPackratM "'\\''" "not match pattern: " ["dvChars"] d552_490 ""
                             let '\'' = xx551_491
                             return ()
                             unless True (throwErrorPackratM "True" "not match: " ["dvChars"] d552_490 "")
                             return '\'',
                          do d554_492 <- get
                             xx553_493 <- dvCharsM
                             case xx553_493 of
                                 '\\' -> return ()
                                 _ -> throwErrorPackratM "'\\\\'" "not match pattern: " ["dvChars"] d554_492 ""
                             let '\\' = xx553_493
                             return ()
                             unless True (throwErrorPackratM "True" "not match: " ["dvChars"] d554_492 "")
                             return '\\',
                          do d556_494 <- get
                             xx555_495 <- dvCharsM
                             case xx555_495 of
                                 'n' -> return ()
                                 _ -> throwErrorPackratM "'n'" "not match pattern: " ["dvChars"] d556_494 ""
                             let 'n' = xx555_495
                             return ()
                             unless True (throwErrorPackratM "True" "not match: " ["dvChars"] d556_494 "")
                             return newLine,
                          do d558_496 <- get
                             xx557_497 <- dvCharsM
                             case xx557_497 of
                                 't' -> return ()
                                 _ -> throwErrorPackratM "'t'" "not match pattern: " ["dvChars"] d558_496 ""
                             let 't' = xx557_497
                             return ()
                             unless True (throwErrorPackratM "True" "not match: " ["dvChars"] d558_496 "")
                             return tab]
p_dq = foldl1 mplus [do d560_498 <- get
                        xx559_499 <- dvCharsM
                        case xx559_499 of
                            '"' -> return ()
                            _ -> throwErrorPackratM "'\"'" "not match pattern: " ["dvChars"] d560_498 ""
                        let '"' = xx559_499
                        return ()
                        unless True (throwErrorPackratM "True" "not match: " ["dvChars"] d560_498 "")
                        return ()]
p_pats = foldl1 mplus [do d562_500 <- get
                          xx561_501 <- dv_patM
                          let p = xx561_501
                          unless True (throwErrorPackratM "True" "not match: " ["dv_pat"] d562_500 "")
                          d564_502 <- get
                          _ <- dv_spacesM
                          unless True (throwErrorPackratM "True" "not match: " ["dv_spaces"] d564_502 "")
                          d566_503 <- get
                          xx565_504 <- dv_patsM
                          let ps = xx565_504
                          unless True (throwErrorPackratM "True" "not match: " ["dv_pats"] d566_503 "")
                          return (cons p ps),
                       do return emp]
p_readFromLs = foldl1 mplus [do d568_505 <- get
                                xx567_506 <- dv_readFromM
                                let rf = xx567_506
                                unless True (throwErrorPackratM "True" "not match: " ["dv_readFrom"] d568_505 "")
                                d570_507 <- get
                                xx569_508 <- dvCharsM
                                case xx569_508 of
                                    '*' -> return ()
                                    _ -> throwErrorPackratM "'*'" "not match pattern: " ["dvChars"] d570_507 ""
                                let '*' = xx569_508
                                return ()
                                unless True (throwErrorPackratM "True" "not match: " ["dvChars"] d570_507 "")
                                return (FromList rf),
                             do d572_509 <- get
                                xx571_510 <- dv_readFromM
                                let rf = xx571_510
                                unless True (throwErrorPackratM "True" "not match: " ["dv_readFrom"] d572_509 "")
                                d574_511 <- get
                                xx573_512 <- dvCharsM
                                case xx573_512 of
                                    '+' -> return ()
                                    _ -> throwErrorPackratM "'+'" "not match pattern: " ["dvChars"] d574_511 ""
                                let '+' = xx573_512
                                return ()
                                unless True (throwErrorPackratM "True" "not match: " ["dvChars"] d574_511 "")
                                return (FromList1 rf),
                             do d576_513 <- get
                                xx575_514 <- dv_readFromM
                                let rf = xx575_514
                                unless True (throwErrorPackratM "True" "not match: " ["dv_readFrom"] d576_513 "")
                                d578_515 <- get
                                xx577_516 <- dvCharsM
                                case xx577_516 of
                                    '?' -> return ()
                                    _ -> throwErrorPackratM "'?'" "not match pattern: " ["dvChars"] d578_515 ""
                                let '?' = xx577_516
                                return ()
                                unless True (throwErrorPackratM "True" "not match: " ["dvChars"] d578_515 "")
                                return (FromOptional rf),
                             do d580_517 <- get
                                xx579_518 <- dv_readFromM
                                let rf = xx579_518
                                unless True (throwErrorPackratM "True" "not match: " ["dv_readFrom"] d580_517 "")
                                return rf]
p_readFrom = foldl1 mplus [do d582_519 <- get
                              xx581_520 <- dv_variableM
                              let v = xx581_520
                              unless True (throwErrorPackratM "True" "not match: " ["dv_variable"] d582_519 "")
                              return (FromVariable v),
                           do d584_521 <- get
                              xx583_522 <- dvCharsM
                              case xx583_522 of
                                  '(' -> return ()
                                  _ -> throwErrorPackratM "'('" "not match pattern: " ["dvChars"] d584_521 ""
                              let '(' = xx583_522
                              return ()
                              unless True (throwErrorPackratM "True" "not match: " ["dvChars"] d584_521 "")
                              d586_523 <- get
                              xx585_524 <- dv_selectionM
                              let s = xx585_524
                              unless True (throwErrorPackratM "True" "not match: " ["dv_selection"] d586_523 "")
                              d588_525 <- get
                              xx587_526 <- dvCharsM
                              case xx587_526 of
                                  ')' -> return ()
                                  _ -> throwErrorPackratM "')'" "not match pattern: " ["dvChars"] d588_525 ""
                              let ')' = xx587_526
                              return ()
                              unless True (throwErrorPackratM "True" "not match: " ["dvChars"] d588_525 "")
                              return (FromSelection s)]
p_test = foldl1 mplus [do d590_527 <- get
                          xx589_528 <- dvCharsM
                          case xx589_528 of
                              '[' -> return ()
                              _ -> throwErrorPackratM "'['" "not match pattern: " ["dvChars"] d590_527 ""
                          let '[' = xx589_528
                          return ()
                          unless True (throwErrorPackratM "True" "not match: " ["dvChars"] d590_527 "")
                          d592_529 <- get
                          xx591_530 <- dv_hsExpLamM
                          let h = xx591_530
                          unless True (throwErrorPackratM "True" "not match: " ["dv_hsExpLam"] d592_529 "")
                          d594_531 <- get
                          _ <- dv_spacesM
                          unless True (throwErrorPackratM "True" "not match: " ["dv_spaces"] d594_531 "")
                          d596_532 <- get
                          xx595_533 <- papOptional dv_comForErrM
                          let com = xx595_533
                          unless True (throwErrorPackratM "True" "not match: " ["dv_comForErr"] d596_532 "")
                          d598_534 <- get
                          xx597_535 <- dvCharsM
                          case xx597_535 of
                              ']' -> return ()
                              _ -> throwErrorPackratM "']'" "not match pattern: " ["dvChars"] d598_534 ""
                          let ']' = xx597_535
                          return ()
                          unless True (throwErrorPackratM "True" "not match: " ["dvChars"] d598_534 "")
                          return (h, maybe "" id com)]
p_hsExpLam = foldl1 mplus [do d600_536 <- get
                              xx599_537 <- dvCharsM
                              case xx599_537 of
                                  '\\' -> return ()
                                  _ -> throwErrorPackratM "'\\\\'" "not match pattern: " ["dvChars"] d600_536 ""
                              let '\\' = xx599_537
                              return ()
                              unless True (throwErrorPackratM "True" "not match: " ["dvChars"] d600_536 "")
                              d602_538 <- get
                              _ <- dv_spacesM
                              unless True (throwErrorPackratM "True" "not match: " ["dv_spaces"] d602_538 "")
                              d604_539 <- get
                              xx603_540 <- dv_patsM
                              let ps = xx603_540
                              unless True (throwErrorPackratM "True" "not match: " ["dv_pats"] d604_539 "")
                              d606_541 <- get
                              _ <- dv_spacesM
                              unless True (throwErrorPackratM "True" "not match: " ["dv_spaces"] d606_541 "")
                              d608_542 <- get
                              xx607_543 <- dvCharsM
                              case xx607_543 of
                                  '-' -> return ()
                                  _ -> throwErrorPackratM "'-'" "not match pattern: " ["dvChars"] d608_542 ""
                              let '-' = xx607_543
                              return ()
                              unless True (throwErrorPackratM "True" "not match: " ["dvChars"] d608_542 "")
                              d610_544 <- get
                              xx609_545 <- dvCharsM
                              let c = xx609_545
                              unless (isGt c) (throwErrorPackratM "isGt c" "not match: " ["dvChars"] d610_544 "")
                              d612_546 <- get
                              _ <- dv_spacesM
                              unless True (throwErrorPackratM "True" "not match: " ["dv_spaces"] d612_546 "")
                              d614_547 <- get
                              xx613_548 <- dv_hsExpTypM
                              let e = xx613_548
                              unless True (throwErrorPackratM "True" "not match: " ["dv_hsExpTyp"] d614_547 "")
                              return (lamE ps e),
                           do d616_549 <- get
                              xx615_550 <- dv_hsExpTypM
                              let e = xx615_550
                              unless True (throwErrorPackratM "True" "not match: " ["dv_hsExpTyp"] d616_549 "")
                              return e]
p_hsExpTyp = foldl1 mplus [do d618_551 <- get
                              xx617_552 <- dv_hsExpOpM
                              let eo = xx617_552
                              unless True (throwErrorPackratM "True" "not match: " ["dv_hsExpOp"] d618_551 "")
                              d620_553 <- get
                              xx619_554 <- dvCharsM
                              case xx619_554 of
                                  ':' -> return ()
                                  _ -> throwErrorPackratM "':'" "not match pattern: " ["dvChars"] d620_553 ""
                              let ':' = xx619_554
                              return ()
                              unless True (throwErrorPackratM "True" "not match: " ["dvChars"] d620_553 "")
                              d622_555 <- get
                              xx621_556 <- dvCharsM
                              case xx621_556 of
                                  ':' -> return ()
                                  _ -> throwErrorPackratM "':'" "not match pattern: " ["dvChars"] d622_555 ""
                              let ':' = xx621_556
                              return ()
                              unless True (throwErrorPackratM "True" "not match: " ["dvChars"] d622_555 "")
                              d624_557 <- get
                              _ <- dv_spacesM
                              unless True (throwErrorPackratM "True" "not match: " ["dv_spaces"] d624_557 "")
                              d626_558 <- get
                              xx625_559 <- dv_hsTypeArrM
                              let t = xx625_559
                              unless True (throwErrorPackratM "True" "not match: " ["dv_hsTypeArr"] d626_558 "")
                              return (sigE eo t),
                           do d628_560 <- get
                              xx627_561 <- dv_hsExpOpM
                              let eo = xx627_561
                              unless True (throwErrorPackratM "True" "not match: " ["dv_hsExpOp"] d628_560 "")
                              return eo]
p_hsExpOp = foldl1 mplus [do d630_562 <- get
                             xx629_563 <- dv_hsExpM
                             let l = xx629_563
                             unless True (throwErrorPackratM "True" "not match: " ["dv_hsExp"] d630_562 "")
                             d632_564 <- get
                             _ <- dv_spacesM
                             unless True (throwErrorPackratM "True" "not match: " ["dv_spaces"] d632_564 "")
                             d634_565 <- get
                             xx633_566 <- dv_hsOpM
                             let o = xx633_566
                             unless True (throwErrorPackratM "True" "not match: " ["dv_hsOp"] d634_565 "")
                             d636_567 <- get
                             _ <- dv_spacesM
                             unless True (throwErrorPackratM "True" "not match: " ["dv_spaces"] d636_567 "")
                             d638_568 <- get
                             xx637_569 <- dv_hsExpOpM
                             let r = xx637_569
                             unless True (throwErrorPackratM "True" "not match: " ["dv_hsExpOp"] d638_568 "")
                             return (uInfixE (getEx l) o r),
                          do d640_570 <- get
                             xx639_571 <- dv_hsExpM
                             let e = xx639_571
                             unless True (throwErrorPackratM "True" "not match: " ["dv_hsExp"] d640_570 "")
                             return (getEx e)]
p_hsOp = foldl1 mplus [do d642_572 <- get
                          xx641_573 <- dvCharsM
                          let c = xx641_573
                          unless (isOpHeadChar c) (throwErrorPackratM "isOpHeadChar c" "not match: " ["dvChars"] d642_572 "")
                          d644_574 <- get
                          xx643_575 <- dv_opTailM
                          let o = xx643_575
                          unless True (throwErrorPackratM "True" "not match: " ["dv_opTail"] d644_574 "")
                          return (varE (mkName (cons c o))),
                       do d646_576 <- get
                          xx645_577 <- dvCharsM
                          case xx645_577 of
                              ':' -> return ()
                              _ -> throwErrorPackratM "':'" "not match pattern: " ["dvChars"] d646_576 ""
                          let ':' = xx645_577
                          return ()
                          unless True (throwErrorPackratM "True" "not match: " ["dvChars"] d646_576 "")
                          ddd647_578 <- get
                          flipMaybe "':':[True]" ddd647_578 ["dvChars"] "" (do d649_579 <- get
                                                                               xx648_580 <- dvCharsM
                                                                               case xx648_580 of
                                                                                   ':' -> return ()
                                                                                   _ -> throwErrorPackratM "':'" "not match pattern: " ["dvChars"] d649_579 ""
                                                                               let ':' = xx648_580
                                                                               return ()
                                                                               unless True (throwErrorPackratM "True" "not match: " ["dvChars"] d649_579 ""))
                          put ddd647_578
                          d651_581 <- get
                          xx650_582 <- dv_opTailM
                          let o = xx650_582
                          unless True (throwErrorPackratM "True" "not match: " ["dv_opTail"] d651_581 "")
                          return (conE (mkName (':' : o))),
                       do d653_583 <- get
                          xx652_584 <- dvCharsM
                          let c = xx652_584
                          unless (isBQ c) (throwErrorPackratM "isBQ c" "not match: " ["dvChars"] d653_583 "")
                          d655_585 <- get
                          xx654_586 <- dv_variableM
                          let v = xx654_586
                          unless True (throwErrorPackratM "True" "not match: " ["dv_variable"] d655_585 "")
                          d657_587 <- get
                          xx656_588 <- dvCharsM
                          let c_ = xx656_588
                          unless (isBQ c_) (throwErrorPackratM "isBQ c_" "not match: " ["dvChars"] d657_587 "")
                          return (varE (mkName v)),
                       do d659_589 <- get
                          xx658_590 <- dvCharsM
                          let c = xx658_590
                          unless (isBQ c) (throwErrorPackratM "isBQ c" "not match: " ["dvChars"] d659_589 "")
                          d661_591 <- get
                          xx660_592 <- dv_typM
                          let t = xx660_592
                          unless True (throwErrorPackratM "True" "not match: " ["dv_typ"] d661_591 "")
                          d663_593 <- get
                          xx662_594 <- dvCharsM
                          let c_ = xx662_594
                          unless (isBQ c_) (throwErrorPackratM "isBQ c_" "not match: " ["dvChars"] d663_593 "")
                          return (conE (mkName t))]
p_opTail = foldl1 mplus [do d665_595 <- get
                            xx664_596 <- dvCharsM
                            let c = xx664_596
                            unless (isOpTailChar c) (throwErrorPackratM "isOpTailChar c" "not match: " ["dvChars"] d665_595 "")
                            d667_597 <- get
                            xx666_598 <- dv_opTailM
                            let s = xx666_598
                            unless True (throwErrorPackratM "True" "not match: " ["dv_opTail"] d667_597 "")
                            return (cons c s),
                         do return emp]
p_hsExp = foldl1 mplus [do d669_599 <- get
                           xx668_600 <- dv_hsExp1M
                           let e = xx668_600
                           unless True (throwErrorPackratM "True" "not match: " ["dv_hsExp1"] d669_599 "")
                           d671_601 <- get
                           _ <- dv_spacesM
                           unless True (throwErrorPackratM "True" "not match: " ["dv_spaces"] d671_601 "")
                           d673_602 <- get
                           xx672_603 <- dv_hsExpM
                           let h = xx672_603
                           unless True (throwErrorPackratM "True" "not match: " ["dv_hsExp"] d673_602 "")
                           return (applyExR e h),
                        do d675_604 <- get
                           xx674_605 <- dv_hsExp1M
                           let e = xx674_605
                           unless True (throwErrorPackratM "True" "not match: " ["dv_hsExp1"] d675_604 "")
                           return (toEx e)]
p_hsExp1 = foldl1 mplus [do d677_606 <- get
                            xx676_607 <- dvCharsM
                            case xx676_607 of
                                '(' -> return ()
                                _ -> throwErrorPackratM "'('" "not match pattern: " ["dvChars"] d677_606 ""
                            let '(' = xx676_607
                            return ()
                            unless True (throwErrorPackratM "True" "not match: " ["dvChars"] d677_606 "")
                            d679_608 <- get
                            xx678_609 <- papOptional (foldl1 mplus [do d681_610 <- get
                                                                       xx680_611 <- dv_hsExpTypM
                                                                       let e = xx680_611
                                                                       unless True (throwErrorPackratM "True" "not match: " ["dv_hsExpTyp"] d681_610 "")
                                                                       return e])
                            let l = xx678_609
                            unless True (throwErrorPackratM "True" "not match: " ["dv_hsExpTyp"] d679_608 "")
                            d683_612 <- get
                            _ <- dv_spacesM
                            unless True (throwErrorPackratM "True" "not match: " ["dv_spaces"] d683_612 "")
                            d685_613 <- get
                            xx684_614 <- dv_hsOpM
                            let o = xx684_614
                            unless True (throwErrorPackratM "True" "not match: " ["dv_hsOp"] d685_613 "")
                            d687_615 <- get
                            _ <- dv_spacesM
                            unless True (throwErrorPackratM "True" "not match: " ["dv_spaces"] d687_615 "")
                            d689_616 <- get
                            xx688_617 <- papOptional (foldl1 mplus [do d691_618 <- get
                                                                       xx690_619 <- dv_hsExpTypM
                                                                       let e = xx690_619
                                                                       unless True (throwErrorPackratM "True" "not match: " ["dv_hsExpTyp"] d691_618 "")
                                                                       return e])
                            let r = xx688_617
                            unless True (throwErrorPackratM "True" "not match: " ["dv_hsExpTyp"] d689_616 "")
                            d693_620 <- get
                            xx692_621 <- dvCharsM
                            case xx692_621 of
                                ')' -> return ()
                                _ -> throwErrorPackratM "')'" "not match pattern: " ["dvChars"] d693_620 ""
                            let ')' = xx692_621
                            return ()
                            unless True (throwErrorPackratM "True" "not match: " ["dvChars"] d693_620 "")
                            return (infixE l o r),
                         do d695_622 <- get
                            xx694_623 <- dvCharsM
                            case xx694_623 of
                                '(' -> return ()
                                _ -> throwErrorPackratM "'('" "not match pattern: " ["dvChars"] d695_622 ""
                            let '(' = xx694_623
                            return ()
                            unless True (throwErrorPackratM "True" "not match: " ["dvChars"] d695_622 "")
                            d697_624 <- get
                            xx696_625 <- dv_hsExpTplM
                            let et = xx696_625
                            unless True (throwErrorPackratM "True" "not match: " ["dv_hsExpTpl"] d697_624 "")
                            d699_626 <- get
                            xx698_627 <- dvCharsM
                            case xx698_627 of
                                ')' -> return ()
                                _ -> throwErrorPackratM "')'" "not match pattern: " ["dvChars"] d699_626 ""
                            let ')' = xx698_627
                            return ()
                            unless True (throwErrorPackratM "True" "not match: " ["dvChars"] d699_626 "")
                            return (tupE et),
                         do d701_628 <- get
                            xx700_629 <- dvCharsM
                            case xx700_629 of
                                '[' -> return ()
                                _ -> throwErrorPackratM "'['" "not match pattern: " ["dvChars"] d701_628 ""
                            let '[' = xx700_629
                            return ()
                            unless True (throwErrorPackratM "True" "not match: " ["dvChars"] d701_628 "")
                            d703_630 <- get
                            xx702_631 <- dv_hsExpTplM
                            let et = xx702_631
                            unless True (throwErrorPackratM "True" "not match: " ["dv_hsExpTpl"] d703_630 "")
                            d705_632 <- get
                            xx704_633 <- dvCharsM
                            case xx704_633 of
                                ']' -> return ()
                                _ -> throwErrorPackratM "']'" "not match pattern: " ["dvChars"] d705_632 ""
                            let ']' = xx704_633
                            return ()
                            unless True (throwErrorPackratM "True" "not match: " ["dvChars"] d705_632 "")
                            return (listE et),
                         do d707_634 <- get
                            xx706_635 <- dv_variableM
                            let v = xx706_635
                            unless True (throwErrorPackratM "True" "not match: " ["dv_variable"] d707_634 "")
                            return (varE (mkName v)),
                         do d709_636 <- get
                            xx708_637 <- dv_typM
                            let t = xx708_637
                            unless True (throwErrorPackratM "True" "not match: " ["dv_typ"] d709_636 "")
                            return (conE (mkName t)),
                         do d711_638 <- get
                            xx710_639 <- dv_integerM
                            let i = xx710_639
                            unless True (throwErrorPackratM "True" "not match: " ["dv_integer"] d711_638 "")
                            d713_640 <- get
                            _ <- dv_spacesM
                            unless True (throwErrorPackratM "True" "not match: " ["dv_spaces"] d713_640 "")
                            return (litE (integerL i)),
                         do d715_641 <- get
                            xx714_642 <- dvCharsM
                            case xx714_642 of
                                '\'' -> return ()
                                _ -> throwErrorPackratM "'\\''" "not match pattern: " ["dvChars"] d715_641 ""
                            let '\'' = xx714_642
                            return ()
                            unless True (throwErrorPackratM "True" "not match: " ["dvChars"] d715_641 "")
                            d717_643 <- get
                            xx716_644 <- dv_charLitM
                            let c = xx716_644
                            unless True (throwErrorPackratM "True" "not match: " ["dv_charLit"] d717_643 "")
                            d719_645 <- get
                            xx718_646 <- dvCharsM
                            case xx718_646 of
                                '\'' -> return ()
                                _ -> throwErrorPackratM "'\\''" "not match pattern: " ["dvChars"] d719_645 ""
                            let '\'' = xx718_646
                            return ()
                            unless True (throwErrorPackratM "True" "not match: " ["dvChars"] d719_645 "")
                            return (litE (charL c)),
                         do d721_647 <- get
                            xx720_648 <- dvCharsM
                            case xx720_648 of
                                '"' -> return ()
                                _ -> throwErrorPackratM "'\"'" "not match pattern: " ["dvChars"] d721_647 ""
                            let '"' = xx720_648
                            return ()
                            unless True (throwErrorPackratM "True" "not match: " ["dvChars"] d721_647 "")
                            d723_649 <- get
                            xx722_650 <- dv_stringLitM
                            let s = xx722_650
                            unless True (throwErrorPackratM "True" "not match: " ["dv_stringLit"] d723_649 "")
                            d725_651 <- get
                            xx724_652 <- dvCharsM
                            case xx724_652 of
                                '"' -> return ()
                                _ -> throwErrorPackratM "'\"'" "not match pattern: " ["dvChars"] d725_651 ""
                            let '"' = xx724_652
                            return ()
                            unless True (throwErrorPackratM "True" "not match: " ["dvChars"] d725_651 "")
                            return (litE (stringL s)),
                         do d727_653 <- get
                            xx726_654 <- dvCharsM
                            case xx726_654 of
                                '-' -> return ()
                                _ -> throwErrorPackratM "'-'" "not match pattern: " ["dvChars"] d727_653 ""
                            let '-' = xx726_654
                            return ()
                            unless True (throwErrorPackratM "True" "not match: " ["dvChars"] d727_653 "")
                            d729_655 <- get
                            _ <- dv_spacesM
                            unless True (throwErrorPackratM "True" "not match: " ["dv_spaces"] d729_655 "")
                            d731_656 <- get
                            xx730_657 <- dv_hsExp1M
                            let e = xx730_657
                            unless True (throwErrorPackratM "True" "not match: " ["dv_hsExp1"] d731_656 "")
                            return (appE (varE $ mkName "negate") e)]
p_hsExpTpl = foldl1 mplus [do d733_658 <- get
                              xx732_659 <- dv_hsExpLamM
                              let e = xx732_659
                              unless True (throwErrorPackratM "True" "not match: " ["dv_hsExpLam"] d733_658 "")
                              d735_660 <- get
                              _ <- dv_spacesM
                              unless True (throwErrorPackratM "True" "not match: " ["dv_spaces"] d735_660 "")
                              d737_661 <- get
                              xx736_662 <- dvCharsM
                              let c = xx736_662
                              unless (isComma c) (throwErrorPackratM "isComma c" "not match: " ["dvChars"] d737_661 "")
                              d739_663 <- get
                              _ <- dv_spacesM
                              unless True (throwErrorPackratM "True" "not match: " ["dv_spaces"] d739_663 "")
                              d741_664 <- get
                              xx740_665 <- dv_hsExpTplM
                              let et = xx740_665
                              unless True (throwErrorPackratM "True" "not match: " ["dv_hsExpTpl"] d741_664 "")
                              return (cons e et),
                           do d743_666 <- get
                              xx742_667 <- dv_hsExpLamM
                              let e = xx742_667
                              unless True (throwErrorPackratM "True" "not match: " ["dv_hsExpLam"] d743_666 "")
                              return (cons e emp),
                           do return emp]
p_hsTypeArr = foldl1 mplus [do d745_668 <- get
                               xx744_669 <- dv_hsTypeM
                               let l = xx744_669
                               unless True (throwErrorPackratM "True" "not match: " ["dv_hsType"] d745_668 "")
                               d747_670 <- get
                               xx746_671 <- dvCharsM
                               case xx746_671 of
                                   '-' -> return ()
                                   _ -> throwErrorPackratM "'-'" "not match pattern: " ["dvChars"] d747_670 ""
                               let '-' = xx746_671
                               return ()
                               unless True (throwErrorPackratM "True" "not match: " ["dvChars"] d747_670 "")
                               d749_672 <- get
                               xx748_673 <- dvCharsM
                               let c = xx748_673
                               unless (isGt c) (throwErrorPackratM "isGt c" "not match: " ["dvChars"] d749_672 "")
                               d751_674 <- get
                               _ <- dv_spacesM
                               unless True (throwErrorPackratM "True" "not match: " ["dv_spaces"] d751_674 "")
                               d753_675 <- get
                               xx752_676 <- dv_hsTypeArrM
                               let r = xx752_676
                               unless True (throwErrorPackratM "True" "not match: " ["dv_hsTypeArr"] d753_675 "")
                               return (appT (appT arrowT (getTyp l)) r),
                            do d755_677 <- get
                               xx754_678 <- dv_hsTypeM
                               let t = xx754_678
                               unless True (throwErrorPackratM "True" "not match: " ["dv_hsType"] d755_677 "")
                               return (getTyp t)]
p_hsType = foldl1 mplus [do d757_679 <- get
                            xx756_680 <- dv_hsType1M
                            let t = xx756_680
                            unless True (throwErrorPackratM "True" "not match: " ["dv_hsType1"] d757_679 "")
                            d759_681 <- get
                            xx758_682 <- dv_hsTypeM
                            let ts = xx758_682
                            unless True (throwErrorPackratM "True" "not match: " ["dv_hsType"] d759_681 "")
                            return (applyTyp (toTyp t) ts),
                         do d761_683 <- get
                            xx760_684 <- dv_hsType1M
                            let t = xx760_684
                            unless True (throwErrorPackratM "True" "not match: " ["dv_hsType1"] d761_683 "")
                            return (toTyp t)]
p_hsType1 = foldl1 mplus [do d763_685 <- get
                             xx762_686 <- dvCharsM
                             case xx762_686 of
                                 '[' -> return ()
                                 _ -> throwErrorPackratM "'['" "not match pattern: " ["dvChars"] d763_685 ""
                             let '[' = xx762_686
                             return ()
                             unless True (throwErrorPackratM "True" "not match: " ["dvChars"] d763_685 "")
                             d765_687 <- get
                             xx764_688 <- dvCharsM
                             case xx764_688 of
                                 ']' -> return ()
                                 _ -> throwErrorPackratM "']'" "not match pattern: " ["dvChars"] d765_687 ""
                             let ']' = xx764_688
                             return ()
                             unless True (throwErrorPackratM "True" "not match: " ["dvChars"] d765_687 "")
                             d767_689 <- get
                             _ <- dv_spacesM
                             unless True (throwErrorPackratM "True" "not match: " ["dv_spaces"] d767_689 "")
                             return listT,
                          do d769_690 <- get
                             xx768_691 <- dvCharsM
                             case xx768_691 of
                                 '[' -> return ()
                                 _ -> throwErrorPackratM "'['" "not match pattern: " ["dvChars"] d769_690 ""
                             let '[' = xx768_691
                             return ()
                             unless True (throwErrorPackratM "True" "not match: " ["dvChars"] d769_690 "")
                             d771_692 <- get
                             xx770_693 <- dv_hsTypeArrM
                             let t = xx770_693
                             unless True (throwErrorPackratM "True" "not match: " ["dv_hsTypeArr"] d771_692 "")
                             d773_694 <- get
                             xx772_695 <- dvCharsM
                             case xx772_695 of
                                 ']' -> return ()
                                 _ -> throwErrorPackratM "']'" "not match pattern: " ["dvChars"] d773_694 ""
                             let ']' = xx772_695
                             return ()
                             unless True (throwErrorPackratM "True" "not match: " ["dvChars"] d773_694 "")
                             d775_696 <- get
                             _ <- dv_spacesM
                             unless True (throwErrorPackratM "True" "not match: " ["dv_spaces"] d775_696 "")
                             return (appT listT t),
                          do d777_697 <- get
                             xx776_698 <- dvCharsM
                             case xx776_698 of
                                 '(' -> return ()
                                 _ -> throwErrorPackratM "'('" "not match pattern: " ["dvChars"] d777_697 ""
                             let '(' = xx776_698
                             return ()
                             unless True (throwErrorPackratM "True" "not match: " ["dvChars"] d777_697 "")
                             d779_699 <- get
                             _ <- dv_spacesM
                             unless True (throwErrorPackratM "True" "not match: " ["dv_spaces"] d779_699 "")
                             d781_700 <- get
                             xx780_701 <- dv_hsTypeTplM
                             let tt = xx780_701
                             unless True (throwErrorPackratM "True" "not match: " ["dv_hsTypeTpl"] d781_700 "")
                             d783_702 <- get
                             xx782_703 <- dvCharsM
                             case xx782_703 of
                                 ')' -> return ()
                                 _ -> throwErrorPackratM "')'" "not match pattern: " ["dvChars"] d783_702 ""
                             let ')' = xx782_703
                             return ()
                             unless True (throwErrorPackratM "True" "not match: " ["dvChars"] d783_702 "")
                             return (tupT tt),
                          do d785_704 <- get
                             xx784_705 <- dv_typTokenM
                             let t = xx784_705
                             unless True (throwErrorPackratM "True" "not match: " ["dv_typToken"] d785_704 "")
                             return (conT (mkName t)),
                          do d787_706 <- get
                             xx786_707 <- dvCharsM
                             case xx786_707 of
                                 '(' -> return ()
                                 _ -> throwErrorPackratM "'('" "not match pattern: " ["dvChars"] d787_706 ""
                             let '(' = xx786_707
                             return ()
                             unless True (throwErrorPackratM "True" "not match: " ["dvChars"] d787_706 "")
                             d789_708 <- get
                             xx788_709 <- dvCharsM
                             case xx788_709 of
                                 '-' -> return ()
                                 _ -> throwErrorPackratM "'-'" "not match pattern: " ["dvChars"] d789_708 ""
                             let '-' = xx788_709
                             return ()
                             unless True (throwErrorPackratM "True" "not match: " ["dvChars"] d789_708 "")
                             d791_710 <- get
                             xx790_711 <- dvCharsM
                             let c = xx790_711
                             unless (isGt c) (throwErrorPackratM "isGt c" "not match: " ["dvChars"] d791_710 "")
                             d793_712 <- get
                             xx792_713 <- dvCharsM
                             case xx792_713 of
                                 ')' -> return ()
                                 _ -> throwErrorPackratM "')'" "not match pattern: " ["dvChars"] d793_712 ""
                             let ')' = xx792_713
                             return ()
                             unless True (throwErrorPackratM "True" "not match: " ["dvChars"] d793_712 "")
                             d795_714 <- get
                             _ <- dv_spacesM
                             unless True (throwErrorPackratM "True" "not match: " ["dv_spaces"] d795_714 "")
                             return arrowT]
p_hsTypeTpl = foldl1 mplus [do d797_715 <- get
                               xx796_716 <- dv_hsTypeArrM
                               let t = xx796_716
                               unless True (throwErrorPackratM "True" "not match: " ["dv_hsTypeArr"] d797_715 "")
                               d799_717 <- get
                               xx798_718 <- dvCharsM
                               let c = xx798_718
                               unless (isComma c) (throwErrorPackratM "isComma c" "not match: " ["dvChars"] d799_717 "")
                               d801_719 <- get
                               _ <- dv_spacesM
                               unless True (throwErrorPackratM "True" "not match: " ["dv_spaces"] d801_719 "")
                               d803_720 <- get
                               xx802_721 <- dv_hsTypeTplM
                               let tt = xx802_721
                               unless True (throwErrorPackratM "True" "not match: " ["dv_hsTypeTpl"] d803_720 "")
                               return (cons t tt),
                            do d805_722 <- get
                               xx804_723 <- dv_hsTypeArrM
                               let t = xx804_723
                               unless True (throwErrorPackratM "True" "not match: " ["dv_hsTypeArr"] d805_722 "")
                               return (cons t emp),
                            do return emp]
p_typ = foldl1 mplus [do d807_724 <- get
                         xx806_725 <- dv_upperM
                         let u = xx806_725
                         unless True (throwErrorPackratM "True" "not match: " ["dv_upper"] d807_724 "")
                         d809_726 <- get
                         xx808_727 <- dv_tvtailM
                         let t = xx808_727
                         unless True (throwErrorPackratM "True" "not match: " ["dv_tvtail"] d809_726 "")
                         return (cons u t)]
p_variable = foldl1 mplus [do d811_728 <- get
                              xx810_729 <- dv_lowerM
                              let l = xx810_729
                              unless True (throwErrorPackratM "True" "not match: " ["dv_lower"] d811_728 "")
                              d813_730 <- get
                              xx812_731 <- dv_tvtailM
                              let t = xx812_731
                              unless True (throwErrorPackratM "True" "not match: " ["dv_tvtail"] d813_730 "")
                              return (cons l t)]
p_tvtail = foldl1 mplus [do d815_732 <- get
                            xx814_733 <- dv_alphaM
                            let a = xx814_733
                            unless True (throwErrorPackratM "True" "not match: " ["dv_alpha"] d815_732 "")
                            d817_734 <- get
                            xx816_735 <- dv_tvtailM
                            let t = xx816_735
                            unless True (throwErrorPackratM "True" "not match: " ["dv_tvtail"] d817_734 "")
                            return (cons a t),
                         do return emp]
p_integer = foldl1 mplus [do d819_736 <- get
                             xx818_737 <- dv_digitM
                             let dh = xx818_737
                             unless True (throwErrorPackratM "True" "not match: " ["dv_digit"] d819_736 "")
                             d821_738 <- get
                             xx820_739 <- list (foldl1 mplus [do d823_740 <- get
                                                                 xx822_741 <- dv_digitM
                                                                 let d = xx822_741
                                                                 unless True (throwErrorPackratM "True" "not match: " ["dv_digit"] d823_740 "")
                                                                 return d])
                             let ds = xx820_739
                             unless True (throwErrorPackratM "True" "not match: " ["dv_digit"] d821_738 "")
                             return (read (cons dh ds))]
p_alpha = foldl1 mplus [do d825_742 <- get
                           xx824_743 <- dv_upperM
                           let u = xx824_743
                           unless True (throwErrorPackratM "True" "not match: " ["dv_upper"] d825_742 "")
                           return u,
                        do d827_744 <- get
                           xx826_745 <- dv_lowerM
                           let l = xx826_745
                           unless True (throwErrorPackratM "True" "not match: " ["dv_lower"] d827_744 "")
                           return l,
                        do d829_746 <- get
                           xx828_747 <- dv_digitM
                           let d = xx828_747
                           unless True (throwErrorPackratM "True" "not match: " ["dv_digit"] d829_746 "")
                           return d,
                        do d831_748 <- get
                           xx830_749 <- dvCharsM
                           case xx830_749 of
                               '\'' -> return ()
                               _ -> throwErrorPackratM "'\\''" "not match pattern: " ["dvChars"] d831_748 ""
                           let '\'' = xx830_749
                           return ()
                           unless True (throwErrorPackratM "True" "not match: " ["dvChars"] d831_748 "")
                           return '\'']
p_upper = foldl1 mplus [do d833_750 <- get
                           xx832_751 <- dvCharsM
                           let u = xx832_751
                           unless (isUpper u) (throwErrorPackratM "isUpper u" "not match: " ["dvChars"] d833_750 "")
                           return u]
p_lower = foldl1 mplus [do d835_752 <- get
                           xx834_753 <- dvCharsM
                           let l = xx834_753
                           unless (isLowerU l) (throwErrorPackratM "isLowerU l" "not match: " ["dvChars"] d835_752 "")
                           return l]
p_digit = foldl1 mplus [do d837_754 <- get
                           xx836_755 <- dvCharsM
                           let d = xx836_755
                           unless (isDigit d) (throwErrorPackratM "isDigit d" "not match: " ["dvChars"] d837_754 "")
                           return d]
p_spaces = foldl1 mplus [do d839_756 <- get
                            _ <- dv_spaceM
                            unless True (throwErrorPackratM "True" "not match: " ["dv_space"] d839_756 "")
                            d841_757 <- get
                            _ <- dv_spacesM
                            unless True (throwErrorPackratM "True" "not match: " ["dv_spaces"] d841_757 "")
                            return (),
                         do return ()]
p_space = foldl1 mplus [do d843_758 <- get
                           xx842_759 <- dvCharsM
                           let s = xx842_759
                           unless (isSpace s) (throwErrorPackratM "isSpace s" "not match: " ["dvChars"] d843_758 "")
                           return (),
                        do d845_760 <- get
                           xx844_761 <- dvCharsM
                           case xx844_761 of
                               '-' -> return ()
                               _ -> throwErrorPackratM "'-'" "not match pattern: " ["dvChars"] d845_760 ""
                           let '-' = xx844_761
                           return ()
                           unless True (throwErrorPackratM "True" "not match: " ["dvChars"] d845_760 "")
                           d847_762 <- get
                           xx846_763 <- dvCharsM
                           case xx846_763 of
                               '-' -> return ()
                               _ -> throwErrorPackratM "'-'" "not match pattern: " ["dvChars"] d847_762 ""
                           let '-' = xx846_763
                           return ()
                           unless True (throwErrorPackratM "True" "not match: " ["dvChars"] d847_762 "")
                           d849_764 <- get
                           _ <- dv_notNLStringM
                           unless True (throwErrorPackratM "True" "not match: " ["dv_notNLString"] d849_764 "")
                           d851_765 <- get
                           _ <- dv_nlM
                           unless True (throwErrorPackratM "True" "not match: " ["dv_nl"] d851_765 "")
                           return (),
                        do d853_766 <- get
                           _ <- dv_commentM
                           unless True (throwErrorPackratM "True" "not match: " ["dv_comment"] d853_766 "")
                           return ()]
p_notNLString = foldl1 mplus [do ddd854_767 <- get
                                 flipMaybe "_:nl[True]" ddd854_767 ["dv_nl"] "" (do d856_768 <- get
                                                                                    _ <- dv_nlM
                                                                                    unless True (throwErrorPackratM "True" "not match: " ["dv_nl"] d856_768 ""))
                                 put ddd854_767
                                 d858_769 <- get
                                 xx857_770 <- dvCharsM
                                 let c = xx857_770
                                 unless True (throwErrorPackratM "True" "not match: " ["dvChars"] d858_769 "")
                                 d860_771 <- get
                                 xx859_772 <- dv_notNLStringM
                                 let s = xx859_772
                                 unless True (throwErrorPackratM "True" "not match: " ["dv_notNLString"] d860_771 "")
                                 return (cons c s),
                              do return emp]
p_nl = foldl1 mplus [do d862_773 <- get
                        xx861_774 <- dvCharsM
                        case xx861_774 of
                            '\n' -> return ()
                            _ -> throwErrorPackratM "'\\n'" "not match pattern: " ["dvChars"] d862_773 ""
                        let '\n' = xx861_774
                        return ()
                        unless True (throwErrorPackratM "True" "not match: " ["dvChars"] d862_773 "")
                        return ()]
p_comment = foldl1 mplus [do d864_775 <- get
                             xx863_776 <- dvCharsM
                             case xx863_776 of
                                 '{' -> return ()
                                 _ -> throwErrorPackratM "'{'" "not match pattern: " ["dvChars"] d864_775 ""
                             let '{' = xx863_776
                             return ()
                             unless True (throwErrorPackratM "True" "not match: " ["dvChars"] d864_775 "")
                             d866_777 <- get
                             xx865_778 <- dvCharsM
                             case xx865_778 of
                                 '-' -> return ()
                                 _ -> throwErrorPackratM "'-'" "not match pattern: " ["dvChars"] d866_777 ""
                             let '-' = xx865_778
                             return ()
                             unless True (throwErrorPackratM "True" "not match: " ["dvChars"] d866_777 "")
                             ddd867_779 <- get
                             flipMaybe "'#':[True]" ddd867_779 ["dvChars"] "" (do d869_780 <- get
                                                                                  xx868_781 <- dvCharsM
                                                                                  case xx868_781 of
                                                                                      '#' -> return ()
                                                                                      _ -> throwErrorPackratM "'#'" "not match pattern: " ["dvChars"] d869_780 ""
                                                                                  let '#' = xx868_781
                                                                                  return ()
                                                                                  unless True (throwErrorPackratM "True" "not match: " ["dvChars"] d869_780 ""))
                             put ddd867_779
                             d871_782 <- get
                             _ <- dv_commentsM
                             unless True (throwErrorPackratM "True" "not match: " ["dv_comments"] d871_782 "")
                             d873_783 <- get
                             _ <- dv_comEndM
                             unless True (throwErrorPackratM "True" "not match: " ["dv_comEnd"] d873_783 "")
                             return ()]
p_comments = foldl1 mplus [do d875_784 <- get
                              _ <- dv_notComStrM
                              unless True (throwErrorPackratM "True" "not match: " ["dv_notComStr"] d875_784 "")
                              d877_785 <- get
                              _ <- dv_commentM
                              unless True (throwErrorPackratM "True" "not match: " ["dv_comment"] d877_785 "")
                              d879_786 <- get
                              _ <- dv_commentsM
                              unless True (throwErrorPackratM "True" "not match: " ["dv_comments"] d879_786 "")
                              return (),
                           do d881_787 <- get
                              _ <- dv_notComStrM
                              unless True (throwErrorPackratM "True" "not match: " ["dv_notComStr"] d881_787 "")
                              return ()]
p_notComStr = foldl1 mplus [do ddd882_788 <- get
                               flipMaybe "_:comment[True]" ddd882_788 ["dv_comment"] "" (do d884_789 <- get
                                                                                            _ <- dv_commentM
                                                                                            unless True (throwErrorPackratM "True" "not match: " ["dv_comment"] d884_789 ""))
                               put ddd882_788
                               ddd885_790 <- get
                               flipMaybe "_:comEnd[True]" ddd885_790 ["dv_comEnd"] "" (do d887_791 <- get
                                                                                          _ <- dv_comEndM
                                                                                          unless True (throwErrorPackratM "True" "not match: " ["dv_comEnd"] d887_791 ""))
                               put ddd885_790
                               d889_792 <- get
                               _ <- dvCharsM
                               unless True (throwErrorPackratM "True" "not match: " ["dvChars"] d889_792 "")
                               d891_793 <- get
                               _ <- dv_notComStrM
                               unless True (throwErrorPackratM "True" "not match: " ["dv_notComStr"] d891_793 "")
                               return (),
                            do return ()]
p_comEnd = foldl1 mplus [do d893_794 <- get
                            xx892_795 <- dvCharsM
                            case xx892_795 of
                                '-' -> return ()
                                _ -> throwErrorPackratM "'-'" "not match pattern: " ["dvChars"] d893_794 ""
                            let '-' = xx892_795
                            return ()
                            unless True (throwErrorPackratM "True" "not match: " ["dvChars"] d893_794 "")
                            d895_796 <- get
                            xx894_797 <- dvCharsM
                            case xx894_797 of
                                '}' -> return ()
                                _ -> throwErrorPackratM "'}'" "not match pattern: " ["dvChars"] d895_796 ""
                            let '}' = xx894_797
                            return ()
                            unless True (throwErrorPackratM "True" "not match: " ["dvChars"] d895_796 "")
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