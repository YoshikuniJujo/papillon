{-# LANGUAGE FlexibleContexts, TemplateHaskell, UndecidableInstances , FlexibleContexts, PackageImports, TypeFamilies, RankNTypes, FlexibleInstances #-}
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

data ParseError pos
    = ParseError String String String pos Derivs ([String])
throwErrorPackratM :: forall a . String ->
                                 String -> [String] -> Derivs -> PackratM a
throwErrorPackratM code msg ns d = do pos <- gets dvPos
                                      throwError (ParseError code msg undefined pos d ns)
instance (Source s, Pos s ~ pos) => Error (ParseError pos)
    where strMsg msg = ParseError "" msg "" initialPos undefined undefined
flipMaybe :: forall a . String ->
                        Derivs -> [String] -> PackratM a -> PackratM ()
flipMaybe errMsg d ns act = do err <- (act >> return False) `catchError` const (return True)
                               unless err (throwErrorPackratM ('!' : errMsg) "not match: " ns d)
type PackratM = StateT Derivs (Either (ParseError (Pos String)))
type Result v = Either (ParseError (Pos String)) ((v, Derivs))
data Derivs
    = Derivs {dv_pegFile :: (Result PegFile),
              dv_pragma :: (Result (Maybe String)),
              dv_pragmaStr :: (Result String),
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
parse :: Pos String -> String -> Derivs
parse pos___hoge s = d
          where d = Derivs pegFile pragma pragmaStr pragmaEnd moduleDec moduleDecStr whr preImpPap prePeg afterPeg importPapillon varToken typToken pap peg sourceType peg_ definition selection expressionHs expression nameLeaf_ nameLeaf nameLeafNoCom comForErr leaf patOp pat pat1 patList opConName charLit stringLit dq pats readFromLs readFrom test hsExpLam hsExpTyp hsExpOp hsOp opTail hsExp hsExp1 hsExpTpl hsTypeArr hsType hsType1 hsTypeTpl typ variable tvtail integer alpha upper lower digit spaces space notNLString nl comment comments notComStr comEnd char pos___hoge
                pegFile = runStateT p_pegFile d
                pragma = runStateT p_pragma d
                pragmaStr = runStateT p_pragmaStr d
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
                char = flip runStateT d (case getToken s of
                                             Just (c,
                                                   s') -> do put (parse (updatePos c pos___hoge) s')
                                                             return c
                                             _ -> throwErrorPackratM "" "end of input" [] undefined)
dv_pragmaM :: PackratM (Maybe String)
dv_pragmaStrM :: PackratM String
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
dv_dqM :: PackratM ()
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
dv_dqM = StateT dv_dq
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
p_pegFile :: PackratM PegFile
p_pragma :: PackratM (Maybe String)
p_pragmaStr :: PackratM String
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
                             if True
                              then return ()
                              else throwErrorPackratM "True" "not match: " ["dv_pragma"] d1_0
                             d3_2 <- get
                             xx2_3 <- dv_moduleDecM
                             let md = xx2_3
                             if True
                              then return ()
                              else throwErrorPackratM "True" "not match: " ["dv_moduleDec"] d3_2
                             d5_4 <- get
                             xx4_5 <- dv_preImpPapM
                             let pip = xx4_5
                             if True
                              then return ()
                              else throwErrorPackratM "True" "not match: " ["dv_preImpPap"] d5_4
                             d7_6 <- get
                             _ <- dv_importPapillonM
                             if True
                              then return ()
                              else throwErrorPackratM "True" "not match: " ["dv_importPapillon"] d7_6
                             d9_7 <- get
                             xx8_8 <- dv_prePegM
                             let pp = xx8_8
                             if True
                              then return ()
                              else throwErrorPackratM "True" "not match: " ["dv_prePeg"] d9_7
                             d11_9 <- get
                             _ <- dv_papM
                             if True
                              then return ()
                              else throwErrorPackratM "True" "not match: " ["dv_pap"] d11_9
                             d13_10 <- get
                             xx12_11 <- dv_pegM
                             let p = xx12_11
                             if True
                              then return ()
                              else throwErrorPackratM "True" "not match: " ["dv_peg"] d13_10
                             d15_12 <- get
                             _ <- dv_spacesM
                             if True
                              then return ()
                              else throwErrorPackratM "True" "not match: " ["dv_spaces"] d15_12
                             d17_13 <- get
                             xx16_14 <- dvCharsM
                             case xx16_14 of
                                 '|' -> return ()
                                 _ -> throwErrorPackratM "'|'" "not match pattern: " ["dvChars"] d17_13
                             let '|' = xx16_14
                             return ()
                             if True
                              then return ()
                              else throwErrorPackratM "True" "not match: " ["dvChars"] d17_13
                             d19_15 <- get
                             xx18_16 <- dvCharsM
                             case xx18_16 of
                                 ']' -> return ()
                                 _ -> throwErrorPackratM "']'" "not match pattern: " ["dvChars"] d19_15
                             let ']' = xx18_16
                             return ()
                             if True
                              then return ()
                              else throwErrorPackratM "True" "not match: " ["dvChars"] d19_15
                             d21_17 <- get
                             xx20_18 <- dvCharsM
                             case xx20_18 of
                                 '\n' -> return ()
                                 _ -> throwErrorPackratM "'\\n'" "not match pattern: " ["dvChars"] d21_17
                             let '\n' = xx20_18
                             return ()
                             if True
                              then return ()
                              else throwErrorPackratM "True" "not match: " ["dvChars"] d21_17
                             d23_19 <- get
                             xx22_20 <- dv_afterPegM
                             let atp = xx22_20
                             if True
                              then return ()
                              else throwErrorPackratM "True" "not match: " ["dv_afterPeg"] d23_19
                             return (mkPegFile pr md pip pp p atp),
                          do d25_21 <- get
                             xx24_22 <- dv_pragmaM
                             let pr = xx24_22
                             if True
                              then return ()
                              else throwErrorPackratM "True" "not match: " ["dv_pragma"] d25_21
                             d27_23 <- get
                             xx26_24 <- dv_moduleDecM
                             let md = xx26_24
                             if True
                              then return ()
                              else throwErrorPackratM "True" "not match: " ["dv_moduleDec"] d27_23
                             d29_25 <- get
                             xx28_26 <- dv_prePegM
                             let pp = xx28_26
                             if True
                              then return ()
                              else throwErrorPackratM "True" "not match: " ["dv_prePeg"] d29_25
                             d31_27 <- get
                             _ <- dv_papM
                             if True
                              then return ()
                              else throwErrorPackratM "True" "not match: " ["dv_pap"] d31_27
                             d33_28 <- get
                             xx32_29 <- dv_pegM
                             let p = xx32_29
                             if True
                              then return ()
                              else throwErrorPackratM "True" "not match: " ["dv_peg"] d33_28
                             d35_30 <- get
                             _ <- dv_spacesM
                             if True
                              then return ()
                              else throwErrorPackratM "True" "not match: " ["dv_spaces"] d35_30
                             d37_31 <- get
                             xx36_32 <- dvCharsM
                             case xx36_32 of
                                 '|' -> return ()
                                 _ -> throwErrorPackratM "'|'" "not match pattern: " ["dvChars"] d37_31
                             let '|' = xx36_32
                             return ()
                             if True
                              then return ()
                              else throwErrorPackratM "True" "not match: " ["dvChars"] d37_31
                             d39_33 <- get
                             xx38_34 <- dvCharsM
                             case xx38_34 of
                                 ']' -> return ()
                                 _ -> throwErrorPackratM "']'" "not match pattern: " ["dvChars"] d39_33
                             let ']' = xx38_34
                             return ()
                             if True
                              then return ()
                              else throwErrorPackratM "True" "not match: " ["dvChars"] d39_33
                             d41_35 <- get
                             xx40_36 <- dvCharsM
                             case xx40_36 of
                                 '\n' -> return ()
                                 _ -> throwErrorPackratM "'\\n'" "not match pattern: " ["dvChars"] d41_35
                             let '\n' = xx40_36
                             return ()
                             if True
                              then return ()
                              else throwErrorPackratM "True" "not match: " ["dvChars"] d41_35
                             d43_37 <- get
                             xx42_38 <- dv_afterPegM
                             let atp = xx42_38
                             if True
                              then return ()
                              else throwErrorPackratM "True" "not match: " ["dv_afterPeg"] d43_37
                             return (mkPegFile pr md emp pp p atp)]
p_pragma = foldl1 mplus [do d45_39 <- get
                            _ <- dv_spacesM
                            if True
                             then return ()
                             else throwErrorPackratM "True" "not match: " ["dv_spaces"] d45_39
                            d47_40 <- get
                            xx46_41 <- dvCharsM
                            case xx46_41 of
                                '{' -> return ()
                                _ -> throwErrorPackratM "'{'" "not match pattern: " ["dvChars"] d47_40
                            let '{' = xx46_41
                            return ()
                            if True
                             then return ()
                             else throwErrorPackratM "True" "not match: " ["dvChars"] d47_40
                            d49_42 <- get
                            xx48_43 <- dvCharsM
                            case xx48_43 of
                                '-' -> return ()
                                _ -> throwErrorPackratM "'-'" "not match pattern: " ["dvChars"] d49_42
                            let '-' = xx48_43
                            return ()
                            if True
                             then return ()
                             else throwErrorPackratM "True" "not match: " ["dvChars"] d49_42
                            d51_44 <- get
                            xx50_45 <- dvCharsM
                            case xx50_45 of
                                '#' -> return ()
                                _ -> throwErrorPackratM "'#'" "not match pattern: " ["dvChars"] d51_44
                            let '#' = xx50_45
                            return ()
                            if True
                             then return ()
                             else throwErrorPackratM "True" "not match: " ["dvChars"] d51_44
                            d53_46 <- get
                            xx52_47 <- dv_pragmaStrM
                            let s = xx52_47
                            if True
                             then return ()
                             else throwErrorPackratM "True" "not match: " ["dv_pragmaStr"] d53_46
                            d55_48 <- get
                            _ <- dv_pragmaEndM
                            if True
                             then return ()
                             else throwErrorPackratM "True" "not match: " ["dv_pragmaEnd"] d55_48
                            d57_49 <- get
                            _ <- dv_spacesM
                            if True
                             then return ()
                             else throwErrorPackratM "True" "not match: " ["dv_spaces"] d57_49
                            return (just s),
                         do d59_50 <- get
                            _ <- dv_spacesM
                            if True
                             then return ()
                             else throwErrorPackratM "True" "not match: " ["dv_spaces"] d59_50
                            return nothing]
p_pragmaStr = foldl1 mplus [do ddd60_51 <- get
                               flipMaybe "_:pragmaEnd[True]" ddd60_51 ["dv_pragmaEnd"] (do d62_52 <- get
                                                                                           _ <- dv_pragmaEndM
                                                                                           if True
                                                                                            then return ()
                                                                                            else throwErrorPackratM "True" "not match: " ["dv_pragmaEnd"] d62_52)
                               put ddd60_51
                               d64_53 <- get
                               xx63_54 <- dvCharsM
                               let c = xx63_54
                               if True
                                then return ()
                                else throwErrorPackratM "True" "not match: " ["dvChars"] d64_53
                               d66_55 <- get
                               xx65_56 <- dv_pragmaStrM
                               let s = xx65_56
                               if True
                                then return ()
                                else throwErrorPackratM "True" "not match: " ["dv_pragmaStr"] d66_55
                               return (cons c s),
                            do return emp]
p_pragmaEnd = foldl1 mplus [do d68_57 <- get
                               xx67_58 <- dvCharsM
                               case xx67_58 of
                                   '#' -> return ()
                                   _ -> throwErrorPackratM "'#'" "not match pattern: " ["dvChars"] d68_57
                               let '#' = xx67_58
                               return ()
                               if True
                                then return ()
                                else throwErrorPackratM "True" "not match: " ["dvChars"] d68_57
                               d70_59 <- get
                               xx69_60 <- dvCharsM
                               case xx69_60 of
                                   '-' -> return ()
                                   _ -> throwErrorPackratM "'-'" "not match pattern: " ["dvChars"] d70_59
                               let '-' = xx69_60
                               return ()
                               if True
                                then return ()
                                else throwErrorPackratM "True" "not match: " ["dvChars"] d70_59
                               d72_61 <- get
                               xx71_62 <- dvCharsM
                               case xx71_62 of
                                   '}' -> return ()
                                   _ -> throwErrorPackratM "'}'" "not match pattern: " ["dvChars"] d72_61
                               let '}' = xx71_62
                               return ()
                               if True
                                then return ()
                                else throwErrorPackratM "True" "not match: " ["dvChars"] d72_61
                               return ()]
p_moduleDec = foldl1 mplus [do d74_63 <- get
                               xx73_64 <- dvCharsM
                               case xx73_64 of
                                   'm' -> return ()
                                   _ -> throwErrorPackratM "'m'" "not match pattern: " ["dvChars"] d74_63
                               let 'm' = xx73_64
                               return ()
                               if True
                                then return ()
                                else throwErrorPackratM "True" "not match: " ["dvChars"] d74_63
                               d76_65 <- get
                               xx75_66 <- dvCharsM
                               case xx75_66 of
                                   'o' -> return ()
                                   _ -> throwErrorPackratM "'o'" "not match pattern: " ["dvChars"] d76_65
                               let 'o' = xx75_66
                               return ()
                               if True
                                then return ()
                                else throwErrorPackratM "True" "not match: " ["dvChars"] d76_65
                               d78_67 <- get
                               xx77_68 <- dvCharsM
                               case xx77_68 of
                                   'd' -> return ()
                                   _ -> throwErrorPackratM "'d'" "not match pattern: " ["dvChars"] d78_67
                               let 'd' = xx77_68
                               return ()
                               if True
                                then return ()
                                else throwErrorPackratM "True" "not match: " ["dvChars"] d78_67
                               d80_69 <- get
                               xx79_70 <- dvCharsM
                               case xx79_70 of
                                   'u' -> return ()
                                   _ -> throwErrorPackratM "'u'" "not match pattern: " ["dvChars"] d80_69
                               let 'u' = xx79_70
                               return ()
                               if True
                                then return ()
                                else throwErrorPackratM "True" "not match: " ["dvChars"] d80_69
                               d82_71 <- get
                               xx81_72 <- dvCharsM
                               case xx81_72 of
                                   'l' -> return ()
                                   _ -> throwErrorPackratM "'l'" "not match pattern: " ["dvChars"] d82_71
                               let 'l' = xx81_72
                               return ()
                               if True
                                then return ()
                                else throwErrorPackratM "True" "not match: " ["dvChars"] d82_71
                               d84_73 <- get
                               xx83_74 <- dvCharsM
                               case xx83_74 of
                                   'e' -> return ()
                                   _ -> throwErrorPackratM "'e'" "not match pattern: " ["dvChars"] d84_73
                               let 'e' = xx83_74
                               return ()
                               if True
                                then return ()
                                else throwErrorPackratM "True" "not match: " ["dvChars"] d84_73
                               d86_75 <- get
                               xx85_76 <- dv_moduleDecStrM
                               let s = xx85_76
                               if True
                                then return ()
                                else throwErrorPackratM "True" "not match: " ["dv_moduleDecStr"] d86_75
                               d88_77 <- get
                               _ <- dv_whrM
                               if True
                                then return ()
                                else throwErrorPackratM "True" "not match: " ["dv_whr"] d88_77
                               return (just s),
                            do return nothing]
p_moduleDecStr = foldl1 mplus [do ddd89_78 <- get
                                  flipMaybe "_:whr[True]" ddd89_78 ["dv_whr"] (do d91_79 <- get
                                                                                  _ <- dv_whrM
                                                                                  if True
                                                                                   then return ()
                                                                                   else throwErrorPackratM "True" "not match: " ["dv_whr"] d91_79)
                                  put ddd89_78
                                  d93_80 <- get
                                  xx92_81 <- dvCharsM
                                  let c = xx92_81
                                  if True
                                   then return ()
                                   else throwErrorPackratM "True" "not match: " ["dvChars"] d93_80
                                  d95_82 <- get
                                  xx94_83 <- dv_moduleDecStrM
                                  let s = xx94_83
                                  if True
                                   then return ()
                                   else throwErrorPackratM "True" "not match: " ["dv_moduleDecStr"] d95_82
                                  return (cons c s),
                               do return emp]
p_whr = foldl1 mplus [do d97_84 <- get
                         xx96_85 <- dvCharsM
                         case xx96_85 of
                             'w' -> return ()
                             _ -> throwErrorPackratM "'w'" "not match pattern: " ["dvChars"] d97_84
                         let 'w' = xx96_85
                         return ()
                         if True
                          then return ()
                          else throwErrorPackratM "True" "not match: " ["dvChars"] d97_84
                         d99_86 <- get
                         xx98_87 <- dvCharsM
                         case xx98_87 of
                             'h' -> return ()
                             _ -> throwErrorPackratM "'h'" "not match pattern: " ["dvChars"] d99_86
                         let 'h' = xx98_87
                         return ()
                         if True
                          then return ()
                          else throwErrorPackratM "True" "not match: " ["dvChars"] d99_86
                         d101_88 <- get
                         xx100_89 <- dvCharsM
                         case xx100_89 of
                             'e' -> return ()
                             _ -> throwErrorPackratM "'e'" "not match pattern: " ["dvChars"] d101_88
                         let 'e' = xx100_89
                         return ()
                         if True
                          then return ()
                          else throwErrorPackratM "True" "not match: " ["dvChars"] d101_88
                         d103_90 <- get
                         xx102_91 <- dvCharsM
                         case xx102_91 of
                             'r' -> return ()
                             _ -> throwErrorPackratM "'r'" "not match pattern: " ["dvChars"] d103_90
                         let 'r' = xx102_91
                         return ()
                         if True
                          then return ()
                          else throwErrorPackratM "True" "not match: " ["dvChars"] d103_90
                         d105_92 <- get
                         xx104_93 <- dvCharsM
                         case xx104_93 of
                             'e' -> return ()
                             _ -> throwErrorPackratM "'e'" "not match pattern: " ["dvChars"] d105_92
                         let 'e' = xx104_93
                         return ()
                         if True
                          then return ()
                          else throwErrorPackratM "True" "not match: " ["dvChars"] d105_92
                         return ()]
p_preImpPap = foldl1 mplus [do ddd106_94 <- get
                               flipMaybe "_:importPapillon[True]" ddd106_94 ["dv_importPapillon"] (do d108_95 <- get
                                                                                                      _ <- dv_importPapillonM
                                                                                                      if True
                                                                                                       then return ()
                                                                                                       else throwErrorPackratM "True" "not match: " ["dv_importPapillon"] d108_95)
                               put ddd106_94
                               ddd109_96 <- get
                               flipMaybe "_:pap[True]" ddd109_96 ["dv_pap"] (do d111_97 <- get
                                                                                _ <- dv_papM
                                                                                if True
                                                                                 then return ()
                                                                                 else throwErrorPackratM "True" "not match: " ["dv_pap"] d111_97)
                               put ddd109_96
                               d113_98 <- get
                               xx112_99 <- dvCharsM
                               let c = xx112_99
                               if True
                                then return ()
                                else throwErrorPackratM "True" "not match: " ["dvChars"] d113_98
                               d115_100 <- get
                               xx114_101 <- dv_preImpPapM
                               let pip = xx114_101
                               if True
                                then return ()
                                else throwErrorPackratM "True" "not match: " ["dv_preImpPap"] d115_100
                               return (cons c pip),
                            do return emp]
p_prePeg = foldl1 mplus [do ddd116_102 <- get
                            flipMaybe "_:pap[True]" ddd116_102 ["dv_pap"] (do d118_103 <- get
                                                                              _ <- dv_papM
                                                                              if True
                                                                               then return ()
                                                                               else throwErrorPackratM "True" "not match: " ["dv_pap"] d118_103)
                            put ddd116_102
                            d120_104 <- get
                            xx119_105 <- dvCharsM
                            let c = xx119_105
                            if True
                             then return ()
                             else throwErrorPackratM "True" "not match: " ["dvChars"] d120_104
                            d122_106 <- get
                            xx121_107 <- dv_prePegM
                            let pp = xx121_107
                            if True
                             then return ()
                             else throwErrorPackratM "True" "not match: " ["dv_prePeg"] d122_106
                            return (cons c pp),
                         do return emp]
p_afterPeg = foldl1 mplus [do d124_108 <- get
                              xx123_109 <- dvCharsM
                              let c = xx123_109
                              if True
                               then return ()
                               else throwErrorPackratM "True" "not match: " ["dvChars"] d124_108
                              d126_110 <- get
                              xx125_111 <- dv_afterPegM
                              let atp = xx125_111
                              if True
                               then return ()
                               else throwErrorPackratM "True" "not match: " ["dv_afterPeg"] d126_110
                              return (cons c atp),
                           do return emp]
p_importPapillon = foldl1 mplus [do d128_112 <- get
                                    xx127_113 <- dv_varTokenM
                                    case xx127_113 of
                                        "import" -> return ()
                                        _ -> throwErrorPackratM "\"import\"" "not match pattern: " ["dv_varToken"] d128_112
                                    let "import" = xx127_113
                                    return ()
                                    if True
                                     then return ()
                                     else throwErrorPackratM "True" "not match: " ["dv_varToken"] d128_112
                                    d130_114 <- get
                                    xx129_115 <- dv_typTokenM
                                    case xx129_115 of
                                        "Text" -> return ()
                                        _ -> throwErrorPackratM "\"Text\"" "not match pattern: " ["dv_typToken"] d130_114
                                    let "Text" = xx129_115
                                    return ()
                                    if True
                                     then return ()
                                     else throwErrorPackratM "True" "not match: " ["dv_typToken"] d130_114
                                    d132_116 <- get
                                    xx131_117 <- dvCharsM
                                    case xx131_117 of
                                        '.' -> return ()
                                        _ -> throwErrorPackratM "'.'" "not match pattern: " ["dvChars"] d132_116
                                    let '.' = xx131_117
                                    return ()
                                    if True
                                     then return ()
                                     else throwErrorPackratM "True" "not match: " ["dvChars"] d132_116
                                    d134_118 <- get
                                    _ <- dv_spacesM
                                    if True
                                     then return ()
                                     else throwErrorPackratM "True" "not match: " ["dv_spaces"] d134_118
                                    d136_119 <- get
                                    xx135_120 <- dv_typTokenM
                                    case xx135_120 of
                                        "Papillon" -> return ()
                                        _ -> throwErrorPackratM "\"Papillon\"" "not match pattern: " ["dv_typToken"] d136_119
                                    let "Papillon" = xx135_120
                                    return ()
                                    if True
                                     then return ()
                                     else throwErrorPackratM "True" "not match: " ["dv_typToken"] d136_119
                                    ddd137_121 <- get
                                    flipMaybe "'.':[True]" ddd137_121 ["dvChars"] (do d139_122 <- get
                                                                                      xx138_123 <- dvCharsM
                                                                                      case xx138_123 of
                                                                                          '.' -> return ()
                                                                                          _ -> throwErrorPackratM "'.'" "not match pattern: " ["dvChars"] d139_122
                                                                                      let '.' = xx138_123
                                                                                      return ()
                                                                                      if True
                                                                                       then return ()
                                                                                       else throwErrorPackratM "True" "not match: " ["dvChars"] d139_122)
                                    put ddd137_121
                                    return ()]
p_varToken = foldl1 mplus [do d141_124 <- get
                              xx140_125 <- dv_variableM
                              let v = xx140_125
                              if True
                               then return ()
                               else throwErrorPackratM "True" "not match: " ["dv_variable"] d141_124
                              d143_126 <- get
                              _ <- dv_spacesM
                              if True
                               then return ()
                               else throwErrorPackratM "True" "not match: " ["dv_spaces"] d143_126
                              return v]
p_typToken = foldl1 mplus [do d145_127 <- get
                              xx144_128 <- dv_typM
                              let t = xx144_128
                              if True
                               then return ()
                               else throwErrorPackratM "True" "not match: " ["dv_typ"] d145_127
                              d147_129 <- get
                              _ <- dv_spacesM
                              if True
                               then return ()
                               else throwErrorPackratM "True" "not match: " ["dv_spaces"] d147_129
                              return t]
p_pap = foldl1 mplus [do d149_130 <- get
                         xx148_131 <- dvCharsM
                         case xx148_131 of
                             '\n' -> return ()
                             _ -> throwErrorPackratM "'\\n'" "not match pattern: " ["dvChars"] d149_130
                         let '\n' = xx148_131
                         return ()
                         if True
                          then return ()
                          else throwErrorPackratM "True" "not match: " ["dvChars"] d149_130
                         d151_132 <- get
                         xx150_133 <- dvCharsM
                         case xx150_133 of
                             '[' -> return ()
                             _ -> throwErrorPackratM "'['" "not match pattern: " ["dvChars"] d151_132
                         let '[' = xx150_133
                         return ()
                         if True
                          then return ()
                          else throwErrorPackratM "True" "not match: " ["dvChars"] d151_132
                         d153_134 <- get
                         xx152_135 <- dvCharsM
                         case xx152_135 of
                             'p' -> return ()
                             _ -> throwErrorPackratM "'p'" "not match pattern: " ["dvChars"] d153_134
                         let 'p' = xx152_135
                         return ()
                         if True
                          then return ()
                          else throwErrorPackratM "True" "not match: " ["dvChars"] d153_134
                         d155_136 <- get
                         xx154_137 <- dvCharsM
                         case xx154_137 of
                             'a' -> return ()
                             _ -> throwErrorPackratM "'a'" "not match pattern: " ["dvChars"] d155_136
                         let 'a' = xx154_137
                         return ()
                         if True
                          then return ()
                          else throwErrorPackratM "True" "not match: " ["dvChars"] d155_136
                         d157_138 <- get
                         xx156_139 <- dvCharsM
                         case xx156_139 of
                             'p' -> return ()
                             _ -> throwErrorPackratM "'p'" "not match pattern: " ["dvChars"] d157_138
                         let 'p' = xx156_139
                         return ()
                         if True
                          then return ()
                          else throwErrorPackratM "True" "not match: " ["dvChars"] d157_138
                         d159_140 <- get
                         xx158_141 <- dvCharsM
                         case xx158_141 of
                             'i' -> return ()
                             _ -> throwErrorPackratM "'i'" "not match pattern: " ["dvChars"] d159_140
                         let 'i' = xx158_141
                         return ()
                         if True
                          then return ()
                          else throwErrorPackratM "True" "not match: " ["dvChars"] d159_140
                         d161_142 <- get
                         xx160_143 <- dvCharsM
                         case xx160_143 of
                             'l' -> return ()
                             _ -> throwErrorPackratM "'l'" "not match pattern: " ["dvChars"] d161_142
                         let 'l' = xx160_143
                         return ()
                         if True
                          then return ()
                          else throwErrorPackratM "True" "not match: " ["dvChars"] d161_142
                         d163_144 <- get
                         xx162_145 <- dvCharsM
                         case xx162_145 of
                             'l' -> return ()
                             _ -> throwErrorPackratM "'l'" "not match pattern: " ["dvChars"] d163_144
                         let 'l' = xx162_145
                         return ()
                         if True
                          then return ()
                          else throwErrorPackratM "True" "not match: " ["dvChars"] d163_144
                         d165_146 <- get
                         xx164_147 <- dvCharsM
                         case xx164_147 of
                             'o' -> return ()
                             _ -> throwErrorPackratM "'o'" "not match pattern: " ["dvChars"] d165_146
                         let 'o' = xx164_147
                         return ()
                         if True
                          then return ()
                          else throwErrorPackratM "True" "not match: " ["dvChars"] d165_146
                         d167_148 <- get
                         xx166_149 <- dvCharsM
                         case xx166_149 of
                             'n' -> return ()
                             _ -> throwErrorPackratM "'n'" "not match pattern: " ["dvChars"] d167_148
                         let 'n' = xx166_149
                         return ()
                         if True
                          then return ()
                          else throwErrorPackratM "True" "not match: " ["dvChars"] d167_148
                         d169_150 <- get
                         xx168_151 <- dvCharsM
                         case xx168_151 of
                             '|' -> return ()
                             _ -> throwErrorPackratM "'|'" "not match pattern: " ["dvChars"] d169_150
                         let '|' = xx168_151
                         return ()
                         if True
                          then return ()
                          else throwErrorPackratM "True" "not match: " ["dvChars"] d169_150
                         d171_152 <- get
                         xx170_153 <- dvCharsM
                         case xx170_153 of
                             '\n' -> return ()
                             _ -> throwErrorPackratM "'\\n'" "not match pattern: " ["dvChars"] d171_152
                         let '\n' = xx170_153
                         return ()
                         if True
                          then return ()
                          else throwErrorPackratM "True" "not match: " ["dvChars"] d171_152
                         return ()]
p_peg = foldl1 mplus [do d173_154 <- get
                         _ <- dv_spacesM
                         if True
                          then return ()
                          else throwErrorPackratM "True" "not match: " ["dv_spaces"] d173_154
                         d175_155 <- get
                         xx174_156 <- dv_sourceTypeM
                         let s = xx174_156
                         if True
                          then return ()
                          else throwErrorPackratM "True" "not match: " ["dv_sourceType"] d175_155
                         d177_157 <- get
                         xx176_158 <- dv_peg_M
                         let p = xx176_158
                         if True
                          then return ()
                          else throwErrorPackratM "True" "not match: " ["dv_peg_"] d177_157
                         return (mkTTPeg s p),
                      do d179_159 <- get
                         xx178_160 <- dv_peg_M
                         let p = xx178_160
                         if True
                          then return ()
                          else throwErrorPackratM "True" "not match: " ["dv_peg_"] d179_159
                         return (mkTTPeg tString p)]
p_sourceType = foldl1 mplus [do d181_161 <- get
                                xx180_162 <- dv_varTokenM
                                case xx180_162 of
                                    "source" -> return ()
                                    _ -> throwErrorPackratM "\"source\"" "not match pattern: " ["dv_varToken"] d181_161
                                let "source" = xx180_162
                                return ()
                                if True
                                 then return ()
                                 else throwErrorPackratM "True" "not match: " ["dv_varToken"] d181_161
                                d183_163 <- get
                                xx182_164 <- dvCharsM
                                case xx182_164 of
                                    ':' -> return ()
                                    _ -> throwErrorPackratM "':'" "not match pattern: " ["dvChars"] d183_163
                                let ':' = xx182_164
                                return ()
                                if True
                                 then return ()
                                 else throwErrorPackratM "True" "not match: " ["dvChars"] d183_163
                                d185_165 <- get
                                _ <- dv_spacesM
                                if True
                                 then return ()
                                 else throwErrorPackratM "True" "not match: " ["dv_spaces"] d185_165
                                d187_166 <- get
                                xx186_167 <- dv_typTokenM
                                let v = xx186_167
                                if True
                                 then return ()
                                 else throwErrorPackratM "True" "not match: " ["dv_typToken"] d187_166
                                return v]
p_peg_ = foldl1 mplus [do d189_168 <- get
                          _ <- dv_spacesM
                          if True
                           then return ()
                           else throwErrorPackratM "True" "not match: " ["dv_spaces"] d189_168
                          d191_169 <- get
                          xx190_170 <- dv_definitionM
                          let d = xx190_170
                          if True
                           then return ()
                           else throwErrorPackratM "True" "not match: " ["dv_definition"] d191_169
                          d193_171 <- get
                          xx192_172 <- dv_peg_M
                          let p = xx192_172
                          if True
                           then return ()
                           else throwErrorPackratM "True" "not match: " ["dv_peg_"] d193_171
                          return (cons d p),
                       do return emp]
p_definition = foldl1 mplus [do d195_173 <- get
                                xx194_174 <- dv_variableM
                                let v = xx194_174
                                if True
                                 then return ()
                                 else throwErrorPackratM "True" "not match: " ["dv_variable"] d195_173
                                d197_175 <- get
                                _ <- dv_spacesM
                                if True
                                 then return ()
                                 else throwErrorPackratM "True" "not match: " ["dv_spaces"] d197_175
                                d199_176 <- get
                                xx198_177 <- dvCharsM
                                case xx198_177 of
                                    ':' -> return ()
                                    _ -> throwErrorPackratM "':'" "not match pattern: " ["dvChars"] d199_176
                                let ':' = xx198_177
                                return ()
                                if True
                                 then return ()
                                 else throwErrorPackratM "True" "not match: " ["dvChars"] d199_176
                                d201_178 <- get
                                xx200_179 <- dvCharsM
                                case xx200_179 of
                                    ':' -> return ()
                                    _ -> throwErrorPackratM "':'" "not match pattern: " ["dvChars"] d201_178
                                let ':' = xx200_179
                                return ()
                                if True
                                 then return ()
                                 else throwErrorPackratM "True" "not match: " ["dvChars"] d201_178
                                d203_180 <- get
                                _ <- dv_spacesM
                                if True
                                 then return ()
                                 else throwErrorPackratM "True" "not match: " ["dv_spaces"] d203_180
                                d205_181 <- get
                                xx204_182 <- dv_hsTypeArrM
                                let t = xx204_182
                                if True
                                 then return ()
                                 else throwErrorPackratM "True" "not match: " ["dv_hsTypeArr"] d205_181
                                d207_183 <- get
                                _ <- dv_spacesM
                                if True
                                 then return ()
                                 else throwErrorPackratM "True" "not match: " ["dv_spaces"] d207_183
                                d209_184 <- get
                                xx208_185 <- dvCharsM
                                case xx208_185 of
                                    '=' -> return ()
                                    _ -> throwErrorPackratM "'='" "not match pattern: " ["dvChars"] d209_184
                                let '=' = xx208_185
                                return ()
                                if True
                                 then return ()
                                 else throwErrorPackratM "True" "not match: " ["dvChars"] d209_184
                                d211_186 <- get
                                _ <- dv_spacesM
                                if True
                                 then return ()
                                 else throwErrorPackratM "True" "not match: " ["dv_spaces"] d211_186
                                d213_187 <- get
                                xx212_188 <- dv_selectionM
                                let sel = xx212_188
                                if True
                                 then return ()
                                 else throwErrorPackratM "True" "not match: " ["dv_selection"] d213_187
                                d215_189 <- get
                                _ <- dv_spacesM
                                if True
                                 then return ()
                                 else throwErrorPackratM "True" "not match: " ["dv_spaces"] d215_189
                                d217_190 <- get
                                xx216_191 <- dvCharsM
                                case xx216_191 of
                                    ';' -> return ()
                                    _ -> throwErrorPackratM "';'" "not match pattern: " ["dvChars"] d217_190
                                let ';' = xx216_191
                                return ()
                                if True
                                 then return ()
                                 else throwErrorPackratM "True" "not match: " ["dvChars"] d217_190
                                return (mkDef v t sel)]
p_selection = foldl1 mplus [do d219_192 <- get
                               xx218_193 <- dv_expressionHsM
                               let ex = xx218_193
                               if True
                                then return ()
                                else throwErrorPackratM "True" "not match: " ["dv_expressionHs"] d219_192
                               d221_194 <- get
                               _ <- dv_spacesM
                               if True
                                then return ()
                                else throwErrorPackratM "True" "not match: " ["dv_spaces"] d221_194
                               d223_195 <- get
                               xx222_196 <- dvCharsM
                               case xx222_196 of
                                   '/' -> return ()
                                   _ -> throwErrorPackratM "'/'" "not match pattern: " ["dvChars"] d223_195
                               let '/' = xx222_196
                               return ()
                               if True
                                then return ()
                                else throwErrorPackratM "True" "not match: " ["dvChars"] d223_195
                               d225_197 <- get
                               _ <- dv_spacesM
                               if True
                                then return ()
                                else throwErrorPackratM "True" "not match: " ["dv_spaces"] d225_197
                               d227_198 <- get
                               xx226_199 <- dv_selectionM
                               let sel = xx226_199
                               if True
                                then return ()
                                else throwErrorPackratM "True" "not match: " ["dv_selection"] d227_198
                               return (cons ex sel),
                            do d229_200 <- get
                               xx228_201 <- dv_expressionHsM
                               let ex = xx228_201
                               if True
                                then return ()
                                else throwErrorPackratM "True" "not match: " ["dv_expressionHs"] d229_200
                               return (cons ex emp)]
p_expressionHs = foldl1 mplus [do d231_202 <- get
                                  xx230_203 <- dv_expressionM
                                  let e = xx230_203
                                  if True
                                   then return ()
                                   else throwErrorPackratM "True" "not match: " ["dv_expression"] d231_202
                                  d233_204 <- get
                                  _ <- dv_spacesM
                                  if True
                                   then return ()
                                   else throwErrorPackratM "True" "not match: " ["dv_spaces"] d233_204
                                  d235_205 <- get
                                  xx234_206 <- dvCharsM
                                  case xx234_206 of
                                      '{' -> return ()
                                      _ -> throwErrorPackratM "'{'" "not match pattern: " ["dvChars"] d235_205
                                  let '{' = xx234_206
                                  return ()
                                  if True
                                   then return ()
                                   else throwErrorPackratM "True" "not match: " ["dvChars"] d235_205
                                  d237_207 <- get
                                  _ <- dv_spacesM
                                  if True
                                   then return ()
                                   else throwErrorPackratM "True" "not match: " ["dv_spaces"] d237_207
                                  d239_208 <- get
                                  xx238_209 <- dv_hsExpLamM
                                  let h = xx238_209
                                  if True
                                   then return ()
                                   else throwErrorPackratM "True" "not match: " ["dv_hsExpLam"] d239_208
                                  d241_210 <- get
                                  _ <- dv_spacesM
                                  if True
                                   then return ()
                                   else throwErrorPackratM "True" "not match: " ["dv_spaces"] d241_210
                                  d243_211 <- get
                                  xx242_212 <- dvCharsM
                                  case xx242_212 of
                                      '}' -> return ()
                                      _ -> throwErrorPackratM "'}'" "not match pattern: " ["dvChars"] d243_211
                                  let '}' = xx242_212
                                  return ()
                                  if True
                                   then return ()
                                   else throwErrorPackratM "True" "not match: " ["dvChars"] d243_211
                                  return (mkExpressionHs e h)]
p_expression = foldl1 mplus [do d245_213 <- get
                                xx244_214 <- dv_nameLeaf_M
                                let l = xx244_214
                                if True
                                 then return ()
                                 else throwErrorPackratM "True" "not match: " ["dv_nameLeaf_"] d245_213
                                d247_215 <- get
                                _ <- dv_spacesM
                                if True
                                 then return ()
                                 else throwErrorPackratM "True" "not match: " ["dv_spaces"] d247_215
                                d249_216 <- get
                                xx248_217 <- dv_expressionM
                                let e = xx248_217
                                if True
                                 then return ()
                                 else throwErrorPackratM "True" "not match: " ["dv_expression"] d249_216
                                return (cons l e),
                             do return emp]
p_nameLeaf_ = foldl1 mplus [do d251_218 <- get
                               xx250_219 <- dvCharsM
                               case xx250_219 of
                                   '!' -> return ()
                                   _ -> throwErrorPackratM "'!'" "not match pattern: " ["dvChars"] d251_218
                               let '!' = xx250_219
                               return ()
                               if True
                                then return ()
                                else throwErrorPackratM "True" "not match: " ["dvChars"] d251_218
                               d253_220 <- get
                               xx252_221 <- dv_nameLeafNoComM
                               let nl = xx252_221
                               if True
                                then return ()
                                else throwErrorPackratM "True" "not match: " ["dv_nameLeafNoCom"] d253_220
                               d255_222 <- get
                               _ <- dv_spacesM
                               if True
                                then return ()
                                else throwErrorPackratM "True" "not match: " ["dv_spaces"] d255_222
                               d257_223 <- get
                               xx256_224 <- papOptional dv_comForErrM
                               let com = xx256_224
                               if True
                                then return ()
                                else throwErrorPackratM "True" "not match: " ["dv_comForErr"] d257_223
                               return (NotAfter nl $ maybe "" id com),
                            do d259_225 <- get
                               xx258_226 <- dvCharsM
                               let c = xx258_226
                               if isAmp c
                                then return ()
                                else throwErrorPackratM "isAmp c" "not match: " ["dvChars"] d259_225
                               d261_227 <- get
                               xx260_228 <- dv_nameLeafM
                               let nl = xx260_228
                               if True
                                then return ()
                                else throwErrorPackratM "True" "not match: " ["dv_nameLeaf"] d261_227
                               return (After nl),
                            do d263_229 <- get
                               xx262_230 <- dv_nameLeafM
                               let nl = xx262_230
                               if True
                                then return ()
                                else throwErrorPackratM "True" "not match: " ["dv_nameLeaf"] d263_229
                               return (Here nl)]
p_nameLeaf = foldl1 mplus [do d265_231 <- get
                              xx264_232 <- dv_pat1M
                              let n = xx264_232
                              if True
                               then return ()
                               else throwErrorPackratM "True" "not match: " ["dv_pat1"] d265_231
                              d267_233 <- get
                              _ <- dv_spacesM
                              if True
                               then return ()
                               else throwErrorPackratM "True" "not match: " ["dv_spaces"] d267_233
                              d269_234 <- get
                              xx268_235 <- papOptional dv_comForErrM
                              let com = xx268_235
                              if True
                               then return ()
                               else throwErrorPackratM "True" "not match: " ["dv_comForErr"] d269_234
                              d271_236 <- get
                              xx270_237 <- dvCharsM
                              case xx270_237 of
                                  ':' -> return ()
                                  _ -> throwErrorPackratM "':'" "not match pattern: " ["dvChars"] d271_236
                              let ':' = xx270_237
                              return ()
                              if True
                               then return ()
                               else throwErrorPackratM "True" "not match: " ["dvChars"] d271_236
                              d273_238 <- get
                              xx272_239 <- dv_leafM
                              let (rf, p) = xx272_239
                              if True
                               then return ()
                               else throwErrorPackratM "True" "not match: " ["dv_leaf"] d273_238
                              return (NameLeaf (n, maybe "" id com) rf p),
                           do d275_240 <- get
                              xx274_241 <- dv_pat1M
                              let n = xx274_241
                              if True
                               then return ()
                               else throwErrorPackratM "True" "not match: " ["dv_pat1"] d275_240
                              d277_242 <- get
                              _ <- dv_spacesM
                              if True
                               then return ()
                               else throwErrorPackratM "True" "not match: " ["dv_spaces"] d277_242
                              d279_243 <- get
                              xx278_244 <- papOptional dv_comForErrM
                              let com = xx278_244
                              if True
                               then return ()
                               else throwErrorPackratM "True" "not match: " ["dv_comForErr"] d279_243
                              return (NameLeaf (n,
                                                maybe "" id com) FromToken (conE $ mkName "True",
                                                                            ""))]
p_nameLeafNoCom = foldl1 mplus [do d281_245 <- get
                                   xx280_246 <- dv_pat1M
                                   let n = xx280_246
                                   if True
                                    then return ()
                                    else throwErrorPackratM "True" "not match: " ["dv_pat1"] d281_245
                                   d283_247 <- get
                                   _ <- dv_spacesM
                                   if True
                                    then return ()
                                    else throwErrorPackratM "True" "not match: " ["dv_spaces"] d283_247
                                   d285_248 <- get
                                   xx284_249 <- papOptional dv_comForErrM
                                   let com = xx284_249
                                   if True
                                    then return ()
                                    else throwErrorPackratM "True" "not match: " ["dv_comForErr"] d285_248
                                   d287_250 <- get
                                   xx286_251 <- dvCharsM
                                   case xx286_251 of
                                       ':' -> return ()
                                       _ -> throwErrorPackratM "':'" "not match pattern: " ["dvChars"] d287_250
                                   let ':' = xx286_251
                                   return ()
                                   if True
                                    then return ()
                                    else throwErrorPackratM "True" "not match: " ["dvChars"] d287_250
                                   d289_252 <- get
                                   xx288_253 <- dv_leafM
                                   let (rf, p) = xx288_253
                                   if True
                                    then return ()
                                    else throwErrorPackratM "True" "not match: " ["dv_leaf"] d289_252
                                   return (NameLeaf (n, maybe "" id com) rf p),
                                do d291_254 <- get
                                   xx290_255 <- dv_pat1M
                                   let n = xx290_255
                                   if True
                                    then return ()
                                    else throwErrorPackratM "True" "not match: " ["dv_pat1"] d291_254
                                   d293_256 <- get
                                   _ <- dv_spacesM
                                   if True
                                    then return ()
                                    else throwErrorPackratM "True" "not match: " ["dv_spaces"] d293_256
                                   return (NameLeaf (n, "") FromToken (conE $ mkName "True", ""))]
p_comForErr = foldl1 mplus [do d295_257 <- get
                               xx294_258 <- dvCharsM
                               case xx294_258 of
                                   '{' -> return ()
                                   _ -> throwErrorPackratM "'{'" "not match pattern: " ["dvChars"] d295_257
                               let '{' = xx294_258
                               return ()
                               if True
                                then return ()
                                else throwErrorPackratM "True" "not match: " ["dvChars"] d295_257
                               d297_259 <- get
                               xx296_260 <- dvCharsM
                               case xx296_260 of
                                   '-' -> return ()
                                   _ -> throwErrorPackratM "'-'" "not match pattern: " ["dvChars"] d297_259
                               let '-' = xx296_260
                               return ()
                               if True
                                then return ()
                                else throwErrorPackratM "True" "not match: " ["dvChars"] d297_259
                               d299_261 <- get
                               xx298_262 <- dvCharsM
                               case xx298_262 of
                                   '#' -> return ()
                                   _ -> throwErrorPackratM "'#'" "not match pattern: " ["dvChars"] d299_261
                               let '#' = xx298_262
                               return ()
                               if True
                                then return ()
                                else throwErrorPackratM "True" "not match: " ["dvChars"] d299_261
                               d301_263 <- get
                               _ <- dv_spacesM
                               if True
                                then return ()
                                else throwErrorPackratM "True" "not match: " ["dv_spaces"] d301_263
                               d303_264 <- get
                               xx302_265 <- dvCharsM
                               case xx302_265 of
                                   '"' -> return ()
                                   _ -> throwErrorPackratM "'\"'" "not match pattern: " ["dvChars"] d303_264
                               let '"' = xx302_265
                               return ()
                               if True
                                then return ()
                                else throwErrorPackratM "True" "not match: " ["dvChars"] d303_264
                               d305_266 <- get
                               xx304_267 <- dv_stringLitM
                               let s = xx304_267
                               if True
                                then return ()
                                else throwErrorPackratM "True" "not match: " ["dv_stringLit"] d305_266
                               d307_268 <- get
                               xx306_269 <- dvCharsM
                               case xx306_269 of
                                   '"' -> return ()
                                   _ -> throwErrorPackratM "'\"'" "not match pattern: " ["dvChars"] d307_268
                               let '"' = xx306_269
                               return ()
                               if True
                                then return ()
                                else throwErrorPackratM "True" "not match: " ["dvChars"] d307_268
                               d309_270 <- get
                               _ <- dv_spacesM
                               if True
                                then return ()
                                else throwErrorPackratM "True" "not match: " ["dv_spaces"] d309_270
                               d311_271 <- get
                               xx310_272 <- dvCharsM
                               case xx310_272 of
                                   '#' -> return ()
                                   _ -> throwErrorPackratM "'#'" "not match pattern: " ["dvChars"] d311_271
                               let '#' = xx310_272
                               return ()
                               if True
                                then return ()
                                else throwErrorPackratM "True" "not match: " ["dvChars"] d311_271
                               d313_273 <- get
                               xx312_274 <- dvCharsM
                               case xx312_274 of
                                   '-' -> return ()
                                   _ -> throwErrorPackratM "'-'" "not match pattern: " ["dvChars"] d313_273
                               let '-' = xx312_274
                               return ()
                               if True
                                then return ()
                                else throwErrorPackratM "True" "not match: " ["dvChars"] d313_273
                               d315_275 <- get
                               xx314_276 <- dvCharsM
                               case xx314_276 of
                                   '}' -> return ()
                                   _ -> throwErrorPackratM "'}'" "not match pattern: " ["dvChars"] d315_275
                               let '}' = xx314_276
                               return ()
                               if True
                                then return ()
                                else throwErrorPackratM "True" "not match: " ["dvChars"] d315_275
                               d317_277 <- get
                               _ <- dv_spacesM
                               if True
                                then return ()
                                else throwErrorPackratM "True" "not match: " ["dv_spaces"] d317_277
                               return s]
p_leaf = foldl1 mplus [do d319_278 <- get
                          xx318_279 <- dv_readFromLsM
                          let rf = xx318_279
                          if True
                           then return ()
                           else throwErrorPackratM "True" "not match: " ["dv_readFromLs"] d319_278
                          d321_280 <- get
                          xx320_281 <- dv_testM
                          let t = xx320_281
                          if True
                           then return ()
                           else throwErrorPackratM "True" "not match: " ["dv_test"] d321_280
                          return (rf, t),
                       do d323_282 <- get
                          xx322_283 <- dv_readFromLsM
                          let rf = xx322_283
                          if True
                           then return ()
                           else throwErrorPackratM "True" "not match: " ["dv_readFromLs"] d323_282
                          return (rf, (true, "")),
                       do d325_284 <- get
                          xx324_285 <- dv_testM
                          let t = xx324_285
                          if True
                           then return ()
                           else throwErrorPackratM "True" "not match: " ["dv_test"] d325_284
                          return (FromToken, t)]
p_patOp = foldl1 mplus [do d327_286 <- get
                           xx326_287 <- dv_patM
                           let p = xx326_287
                           if True
                            then return ()
                            else throwErrorPackratM "True" "not match: " ["dv_pat"] d327_286
                           d329_288 <- get
                           xx328_289 <- dv_opConNameM
                           let o = xx328_289
                           if True
                            then return ()
                            else throwErrorPackratM "True" "not match: " ["dv_opConName"] d329_288
                           d331_290 <- get
                           xx330_291 <- dv_patOpM
                           let po = xx330_291
                           if True
                            then return ()
                            else throwErrorPackratM "True" "not match: " ["dv_patOp"] d331_290
                           return (uInfixP p o po),
                        do d333_292 <- get
                           xx332_293 <- dv_patM
                           let p = xx332_293
                           if True
                            then return ()
                            else throwErrorPackratM "True" "not match: " ["dv_pat"] d333_292
                           d335_294 <- get
                           _ <- dv_spacesM
                           if True
                            then return ()
                            else throwErrorPackratM "True" "not match: " ["dv_spaces"] d335_294
                           d337_295 <- get
                           xx336_296 <- dvCharsM
                           let q = xx336_296
                           if isBQ q
                            then return ()
                            else throwErrorPackratM "isBQ q" "not match: " ["dvChars"] d337_295
                           d339_297 <- get
                           xx338_298 <- dv_typM
                           let t = xx338_298
                           if True
                            then return ()
                            else throwErrorPackratM "True" "not match: " ["dv_typ"] d339_297
                           d341_299 <- get
                           xx340_300 <- dvCharsM
                           let q_ = xx340_300
                           if isBQ q_
                            then return ()
                            else throwErrorPackratM "isBQ q_" "not match: " ["dvChars"] d341_299
                           d343_301 <- get
                           _ <- dv_spacesM
                           if True
                            then return ()
                            else throwErrorPackratM "True" "not match: " ["dv_spaces"] d343_301
                           d345_302 <- get
                           xx344_303 <- dv_patOpM
                           let po = xx344_303
                           if True
                            then return ()
                            else throwErrorPackratM "True" "not match: " ["dv_patOp"] d345_302
                           return (uInfixP p (mkName t) po),
                        do d347_304 <- get
                           xx346_305 <- dv_patM
                           let p = xx346_305
                           if True
                            then return ()
                            else throwErrorPackratM "True" "not match: " ["dv_pat"] d347_304
                           return p]
p_pat = foldl1 mplus [do d349_306 <- get
                         xx348_307 <- dv_typM
                         let t = xx348_307
                         if True
                          then return ()
                          else throwErrorPackratM "True" "not match: " ["dv_typ"] d349_306
                         d351_308 <- get
                         _ <- dv_spacesM
                         if True
                          then return ()
                          else throwErrorPackratM "True" "not match: " ["dv_spaces"] d351_308
                         d353_309 <- get
                         xx352_310 <- dv_patsM
                         let ps = xx352_310
                         if True
                          then return ()
                          else throwErrorPackratM "True" "not match: " ["dv_pats"] d353_309
                         return (conToPatQ t ps),
                      do d355_311 <- get
                         xx354_312 <- dvCharsM
                         case xx354_312 of
                             '(' -> return ()
                             _ -> throwErrorPackratM "'('" "not match pattern: " ["dvChars"] d355_311
                         let '(' = xx354_312
                         return ()
                         if True
                          then return ()
                          else throwErrorPackratM "True" "not match: " ["dvChars"] d355_311
                         d357_313 <- get
                         xx356_314 <- dv_opConNameM
                         let o = xx356_314
                         if True
                          then return ()
                          else throwErrorPackratM "True" "not match: " ["dv_opConName"] d357_313
                         d359_315 <- get
                         xx358_316 <- dvCharsM
                         case xx358_316 of
                             ')' -> return ()
                             _ -> throwErrorPackratM "')'" "not match pattern: " ["dvChars"] d359_315
                         let ')' = xx358_316
                         return ()
                         if True
                          then return ()
                          else throwErrorPackratM "True" "not match: " ["dvChars"] d359_315
                         d361_317 <- get
                         _ <- dv_spacesM
                         if True
                          then return ()
                          else throwErrorPackratM "True" "not match: " ["dv_spaces"] d361_317
                         d363_318 <- get
                         xx362_319 <- dv_patsM
                         let ps = xx362_319
                         if True
                          then return ()
                          else throwErrorPackratM "True" "not match: " ["dv_pats"] d363_318
                         return (conP o ps),
                      do d365_320 <- get
                         xx364_321 <- dv_pat1M
                         let p = xx364_321
                         if True
                          then return ()
                          else throwErrorPackratM "True" "not match: " ["dv_pat1"] d365_320
                         return p]
p_pat1 = foldl1 mplus [do d367_322 <- get
                          xx366_323 <- dv_typM
                          let t = xx366_323
                          if True
                           then return ()
                           else throwErrorPackratM "True" "not match: " ["dv_typ"] d367_322
                          return (conToPatQ t emp),
                       do d369_324 <- get
                          xx368_325 <- dv_variableM
                          case xx368_325 of
                              "_" -> return ()
                              _ -> throwErrorPackratM "\"_\"" "not match pattern: " ["dv_variable"] d369_324
                          let "_" = xx368_325
                          return ()
                          if True
                           then return ()
                           else throwErrorPackratM "True" "not match: " ["dv_variable"] d369_324
                          return wildP,
                       do d371_326 <- get
                          xx370_327 <- dv_variableM
                          let n = xx370_327
                          if True
                           then return ()
                           else throwErrorPackratM "True" "not match: " ["dv_variable"] d371_326
                          return (strToPatQ n),
                       do d373_328 <- get
                          xx372_329 <- dv_integerM
                          let i = xx372_329
                          if True
                           then return ()
                           else throwErrorPackratM "True" "not match: " ["dv_integer"] d373_328
                          return (litP (integerL i)),
                       do d375_330 <- get
                          xx374_331 <- dvCharsM
                          case xx374_331 of
                              '-' -> return ()
                              _ -> throwErrorPackratM "'-'" "not match pattern: " ["dvChars"] d375_330
                          let '-' = xx374_331
                          return ()
                          if True
                           then return ()
                           else throwErrorPackratM "True" "not match: " ["dvChars"] d375_330
                          d377_332 <- get
                          _ <- dv_spacesM
                          if True
                           then return ()
                           else throwErrorPackratM "True" "not match: " ["dv_spaces"] d377_332
                          d379_333 <- get
                          xx378_334 <- dv_integerM
                          let i = xx378_334
                          if True
                           then return ()
                           else throwErrorPackratM "True" "not match: " ["dv_integer"] d379_333
                          return (litP (integerL $ negate i)),
                       do d381_335 <- get
                          xx380_336 <- dvCharsM
                          case xx380_336 of
                              '\'' -> return ()
                              _ -> throwErrorPackratM "'\\''" "not match pattern: " ["dvChars"] d381_335
                          let '\'' = xx380_336
                          return ()
                          if True
                           then return ()
                           else throwErrorPackratM "True" "not match: " ["dvChars"] d381_335
                          d383_337 <- get
                          xx382_338 <- dv_charLitM
                          let c = xx382_338
                          if True
                           then return ()
                           else throwErrorPackratM "True" "not match: " ["dv_charLit"] d383_337
                          d385_339 <- get
                          xx384_340 <- dvCharsM
                          case xx384_340 of
                              '\'' -> return ()
                              _ -> throwErrorPackratM "'\\''" "not match pattern: " ["dvChars"] d385_339
                          let '\'' = xx384_340
                          return ()
                          if True
                           then return ()
                           else throwErrorPackratM "True" "not match: " ["dvChars"] d385_339
                          return (charP c),
                       do d387_341 <- get
                          xx386_342 <- dvCharsM
                          case xx386_342 of
                              '"' -> return ()
                              _ -> throwErrorPackratM "'\"'" "not match pattern: " ["dvChars"] d387_341
                          let '"' = xx386_342
                          return ()
                          if True
                           then return ()
                           else throwErrorPackratM "True" "not match: " ["dvChars"] d387_341
                          d389_343 <- get
                          xx388_344 <- dv_stringLitM
                          let s = xx388_344
                          if True
                           then return ()
                           else throwErrorPackratM "True" "not match: " ["dv_stringLit"] d389_343
                          d391_345 <- get
                          xx390_346 <- dvCharsM
                          case xx390_346 of
                              '"' -> return ()
                              _ -> throwErrorPackratM "'\"'" "not match pattern: " ["dvChars"] d391_345
                          let '"' = xx390_346
                          return ()
                          if True
                           then return ()
                           else throwErrorPackratM "True" "not match: " ["dvChars"] d391_345
                          return (stringP s),
                       do d393_347 <- get
                          xx392_348 <- dvCharsM
                          case xx392_348 of
                              '(' -> return ()
                              _ -> throwErrorPackratM "'('" "not match pattern: " ["dvChars"] d393_347
                          let '(' = xx392_348
                          return ()
                          if True
                           then return ()
                           else throwErrorPackratM "True" "not match: " ["dvChars"] d393_347
                          d395_349 <- get
                          xx394_350 <- dv_patListM
                          let p = xx394_350
                          if True
                           then return ()
                           else throwErrorPackratM "True" "not match: " ["dv_patList"] d395_349
                          d397_351 <- get
                          xx396_352 <- dvCharsM
                          case xx396_352 of
                              ')' -> return ()
                              _ -> throwErrorPackratM "')'" "not match pattern: " ["dvChars"] d397_351
                          let ')' = xx396_352
                          return ()
                          if True
                           then return ()
                           else throwErrorPackratM "True" "not match: " ["dvChars"] d397_351
                          return (tupP p),
                       do d399_353 <- get
                          xx398_354 <- dvCharsM
                          case xx398_354 of
                              '[' -> return ()
                              _ -> throwErrorPackratM "'['" "not match pattern: " ["dvChars"] d399_353
                          let '[' = xx398_354
                          return ()
                          if True
                           then return ()
                           else throwErrorPackratM "True" "not match: " ["dvChars"] d399_353
                          d401_355 <- get
                          xx400_356 <- dv_patListM
                          let p = xx400_356
                          if True
                           then return ()
                           else throwErrorPackratM "True" "not match: " ["dv_patList"] d401_355
                          d403_357 <- get
                          xx402_358 <- dvCharsM
                          case xx402_358 of
                              ']' -> return ()
                              _ -> throwErrorPackratM "']'" "not match pattern: " ["dvChars"] d403_357
                          let ']' = xx402_358
                          return ()
                          if True
                           then return ()
                           else throwErrorPackratM "True" "not match: " ["dvChars"] d403_357
                          return (listP p)]
p_patList = foldl1 mplus [do d405_359 <- get
                             xx404_360 <- dv_patOpM
                             let p = xx404_360
                             if True
                              then return ()
                              else throwErrorPackratM "True" "not match: " ["dv_patOp"] d405_359
                             d407_361 <- get
                             _ <- dv_spacesM
                             if True
                              then return ()
                              else throwErrorPackratM "True" "not match: " ["dv_spaces"] d407_361
                             d409_362 <- get
                             xx408_363 <- dvCharsM
                             case xx408_363 of
                                 ',' -> return ()
                                 _ -> throwErrorPackratM "','" "not match pattern: " ["dvChars"] d409_362
                             let ',' = xx408_363
                             return ()
                             if True
                              then return ()
                              else throwErrorPackratM "True" "not match: " ["dvChars"] d409_362
                             d411_364 <- get
                             _ <- dv_spacesM
                             if True
                              then return ()
                              else throwErrorPackratM "True" "not match: " ["dv_spaces"] d411_364
                             d413_365 <- get
                             xx412_366 <- dv_patListM
                             let ps = xx412_366
                             if True
                              then return ()
                              else throwErrorPackratM "True" "not match: " ["dv_patList"] d413_365
                             return (p : ps),
                          do d415_367 <- get
                             xx414_368 <- dv_patOpM
                             let p = xx414_368
                             if True
                              then return ()
                              else throwErrorPackratM "True" "not match: " ["dv_patOp"] d415_367
                             return [p],
                          do return []]
p_opConName = foldl1 mplus [do d417_369 <- get
                               xx416_370 <- dvCharsM
                               case xx416_370 of
                                   ':' -> return ()
                                   _ -> throwErrorPackratM "':'" "not match pattern: " ["dvChars"] d417_369
                               let ':' = xx416_370
                               return ()
                               if True
                                then return ()
                                else throwErrorPackratM "True" "not match: " ["dvChars"] d417_369
                               d419_371 <- get
                               xx418_372 <- dv_opTailM
                               let ot = xx418_372
                               if True
                                then return ()
                                else throwErrorPackratM "True" "not match: " ["dv_opTail"] d419_371
                               return (mkName $ colon : ot)]
p_charLit = foldl1 mplus [do d421_373 <- get
                             xx420_374 <- dvCharsM
                             let c = xx420_374
                             if isAlphaNumOt c
                              then return ()
                              else throwErrorPackratM "isAlphaNumOt c" "not match: " ["dvChars"] d421_373
                             return c,
                          do d423_375 <- get
                             xx422_376 <- dvCharsM
                             case xx422_376 of
                                 '\\' -> return ()
                                 _ -> throwErrorPackratM "'\\\\'" "not match pattern: " ["dvChars"] d423_375
                             let '\\' = xx422_376
                             return ()
                             if True
                              then return ()
                              else throwErrorPackratM "True" "not match: " ["dvChars"] d423_375
                             d425_377 <- get
                             xx424_378 <- dvCharsM
                             let c = xx424_378
                             if elemNTs c
                              then return ()
                              else throwErrorPackratM "elemNTs c" "not match: " ["dvChars"] d425_377
                             return (getNTs c)]
p_stringLit = foldl1 mplus [do ddd426_379 <- get
                               flipMaybe "_:dq[True]" ddd426_379 ["dv_dq"] (do d428_380 <- get
                                                                               _ <- dv_dqM
                                                                               if True
                                                                                then return ()
                                                                                else throwErrorPackratM "True" "not match: " ["dv_dq"] d428_380)
                               put ddd426_379
                               d430_381 <- get
                               xx429_382 <- dvCharsM
                               let c = xx429_382
                               if True
                                then return ()
                                else throwErrorPackratM "True" "not match: " ["dvChars"] d430_381
                               d432_383 <- get
                               xx431_384 <- dv_stringLitM
                               let s = xx431_384
                               if True
                                then return ()
                                else throwErrorPackratM "True" "not match: " ["dv_stringLit"] d432_383
                               return (cons c s),
                            do return emp]
p_dq = foldl1 mplus [do d434_385 <- get
                        xx433_386 <- dvCharsM
                        case xx433_386 of
                            '"' -> return ()
                            _ -> throwErrorPackratM "'\"'" "not match pattern: " ["dvChars"] d434_385
                        let '"' = xx433_386
                        return ()
                        if True
                         then return ()
                         else throwErrorPackratM "True" "not match: " ["dvChars"] d434_385
                        return ()]
p_pats = foldl1 mplus [do d436_387 <- get
                          xx435_388 <- dv_patM
                          let p = xx435_388
                          if True
                           then return ()
                           else throwErrorPackratM "True" "not match: " ["dv_pat"] d436_387
                          d438_389 <- get
                          _ <- dv_spacesM
                          if True
                           then return ()
                           else throwErrorPackratM "True" "not match: " ["dv_spaces"] d438_389
                          d440_390 <- get
                          xx439_391 <- dv_patsM
                          let ps = xx439_391
                          if True
                           then return ()
                           else throwErrorPackratM "True" "not match: " ["dv_pats"] d440_390
                          return (cons p ps),
                       do return emp]
p_readFromLs = foldl1 mplus [do d442_392 <- get
                                xx441_393 <- dv_readFromM
                                let rf = xx441_393
                                if True
                                 then return ()
                                 else throwErrorPackratM "True" "not match: " ["dv_readFrom"] d442_392
                                d444_394 <- get
                                xx443_395 <- dvCharsM
                                case xx443_395 of
                                    '*' -> return ()
                                    _ -> throwErrorPackratM "'*'" "not match pattern: " ["dvChars"] d444_394
                                let '*' = xx443_395
                                return ()
                                if True
                                 then return ()
                                 else throwErrorPackratM "True" "not match: " ["dvChars"] d444_394
                                return (FromList rf),
                             do d446_396 <- get
                                xx445_397 <- dv_readFromM
                                let rf = xx445_397
                                if True
                                 then return ()
                                 else throwErrorPackratM "True" "not match: " ["dv_readFrom"] d446_396
                                d448_398 <- get
                                xx447_399 <- dvCharsM
                                case xx447_399 of
                                    '+' -> return ()
                                    _ -> throwErrorPackratM "'+'" "not match pattern: " ["dvChars"] d448_398
                                let '+' = xx447_399
                                return ()
                                if True
                                 then return ()
                                 else throwErrorPackratM "True" "not match: " ["dvChars"] d448_398
                                return (FromList1 rf),
                             do d450_400 <- get
                                xx449_401 <- dv_readFromM
                                let rf = xx449_401
                                if True
                                 then return ()
                                 else throwErrorPackratM "True" "not match: " ["dv_readFrom"] d450_400
                                d452_402 <- get
                                xx451_403 <- dvCharsM
                                case xx451_403 of
                                    '?' -> return ()
                                    _ -> throwErrorPackratM "'?'" "not match pattern: " ["dvChars"] d452_402
                                let '?' = xx451_403
                                return ()
                                if True
                                 then return ()
                                 else throwErrorPackratM "True" "not match: " ["dvChars"] d452_402
                                return (FromOptional rf),
                             do d454_404 <- get
                                xx453_405 <- dv_readFromM
                                let rf = xx453_405
                                if True
                                 then return ()
                                 else throwErrorPackratM "True" "not match: " ["dv_readFrom"] d454_404
                                return rf]
p_readFrom = foldl1 mplus [do d456_406 <- get
                              xx455_407 <- dv_variableM
                              let v = xx455_407
                              if True
                               then return ()
                               else throwErrorPackratM "True" "not match: " ["dv_variable"] d456_406
                              return (FromVariable v),
                           do d458_408 <- get
                              xx457_409 <- dvCharsM
                              case xx457_409 of
                                  '(' -> return ()
                                  _ -> throwErrorPackratM "'('" "not match pattern: " ["dvChars"] d458_408
                              let '(' = xx457_409
                              return ()
                              if True
                               then return ()
                               else throwErrorPackratM "True" "not match: " ["dvChars"] d458_408
                              d460_410 <- get
                              xx459_411 <- dv_selectionM
                              let s = xx459_411
                              if True
                               then return ()
                               else throwErrorPackratM "True" "not match: " ["dv_selection"] d460_410
                              d462_412 <- get
                              xx461_413 <- dvCharsM
                              case xx461_413 of
                                  ')' -> return ()
                                  _ -> throwErrorPackratM "')'" "not match pattern: " ["dvChars"] d462_412
                              let ')' = xx461_413
                              return ()
                              if True
                               then return ()
                               else throwErrorPackratM "True" "not match: " ["dvChars"] d462_412
                              return (FromSelection s)]
p_test = foldl1 mplus [do d464_414 <- get
                          xx463_415 <- dvCharsM
                          case xx463_415 of
                              '[' -> return ()
                              _ -> throwErrorPackratM "'['" "not match pattern: " ["dvChars"] d464_414
                          let '[' = xx463_415
                          return ()
                          if True
                           then return ()
                           else throwErrorPackratM "True" "not match: " ["dvChars"] d464_414
                          d466_416 <- get
                          xx465_417 <- dv_hsExpLamM
                          let h = xx465_417
                          if True
                           then return ()
                           else throwErrorPackratM "True" "not match: " ["dv_hsExpLam"] d466_416
                          d468_418 <- get
                          _ <- dv_spacesM
                          if True
                           then return ()
                           else throwErrorPackratM "True" "not match: " ["dv_spaces"] d468_418
                          d470_419 <- get
                          xx469_420 <- papOptional dv_comForErrM
                          let com = xx469_420
                          if True
                           then return ()
                           else throwErrorPackratM "True" "not match: " ["dv_comForErr"] d470_419
                          d472_421 <- get
                          xx471_422 <- dvCharsM
                          case xx471_422 of
                              ']' -> return ()
                              _ -> throwErrorPackratM "']'" "not match pattern: " ["dvChars"] d472_421
                          let ']' = xx471_422
                          return ()
                          if True
                           then return ()
                           else throwErrorPackratM "True" "not match: " ["dvChars"] d472_421
                          return (h, maybe "" id com)]
p_hsExpLam = foldl1 mplus [do d474_423 <- get
                              xx473_424 <- dvCharsM
                              case xx473_424 of
                                  '\\' -> return ()
                                  _ -> throwErrorPackratM "'\\\\'" "not match pattern: " ["dvChars"] d474_423
                              let '\\' = xx473_424
                              return ()
                              if True
                               then return ()
                               else throwErrorPackratM "True" "not match: " ["dvChars"] d474_423
                              d476_425 <- get
                              _ <- dv_spacesM
                              if True
                               then return ()
                               else throwErrorPackratM "True" "not match: " ["dv_spaces"] d476_425
                              d478_426 <- get
                              xx477_427 <- dv_patsM
                              let ps = xx477_427
                              if True
                               then return ()
                               else throwErrorPackratM "True" "not match: " ["dv_pats"] d478_426
                              d480_428 <- get
                              _ <- dv_spacesM
                              if True
                               then return ()
                               else throwErrorPackratM "True" "not match: " ["dv_spaces"] d480_428
                              d482_429 <- get
                              xx481_430 <- dvCharsM
                              case xx481_430 of
                                  '-' -> return ()
                                  _ -> throwErrorPackratM "'-'" "not match pattern: " ["dvChars"] d482_429
                              let '-' = xx481_430
                              return ()
                              if True
                               then return ()
                               else throwErrorPackratM "True" "not match: " ["dvChars"] d482_429
                              d484_431 <- get
                              xx483_432 <- dvCharsM
                              let c = xx483_432
                              if isGt c
                               then return ()
                               else throwErrorPackratM "isGt c" "not match: " ["dvChars"] d484_431
                              d486_433 <- get
                              _ <- dv_spacesM
                              if True
                               then return ()
                               else throwErrorPackratM "True" "not match: " ["dv_spaces"] d486_433
                              d488_434 <- get
                              xx487_435 <- dv_hsExpTypM
                              let e = xx487_435
                              if True
                               then return ()
                               else throwErrorPackratM "True" "not match: " ["dv_hsExpTyp"] d488_434
                              return (lamE ps e),
                           do d490_436 <- get
                              xx489_437 <- dv_hsExpTypM
                              let e = xx489_437
                              if True
                               then return ()
                               else throwErrorPackratM "True" "not match: " ["dv_hsExpTyp"] d490_436
                              return e]
p_hsExpTyp = foldl1 mplus [do d492_438 <- get
                              xx491_439 <- dv_hsExpOpM
                              let eo = xx491_439
                              if True
                               then return ()
                               else throwErrorPackratM "True" "not match: " ["dv_hsExpOp"] d492_438
                              d494_440 <- get
                              xx493_441 <- dvCharsM
                              case xx493_441 of
                                  ':' -> return ()
                                  _ -> throwErrorPackratM "':'" "not match pattern: " ["dvChars"] d494_440
                              let ':' = xx493_441
                              return ()
                              if True
                               then return ()
                               else throwErrorPackratM "True" "not match: " ["dvChars"] d494_440
                              d496_442 <- get
                              xx495_443 <- dvCharsM
                              case xx495_443 of
                                  ':' -> return ()
                                  _ -> throwErrorPackratM "':'" "not match pattern: " ["dvChars"] d496_442
                              let ':' = xx495_443
                              return ()
                              if True
                               then return ()
                               else throwErrorPackratM "True" "not match: " ["dvChars"] d496_442
                              d498_444 <- get
                              _ <- dv_spacesM
                              if True
                               then return ()
                               else throwErrorPackratM "True" "not match: " ["dv_spaces"] d498_444
                              d500_445 <- get
                              xx499_446 <- dv_hsTypeArrM
                              let t = xx499_446
                              if True
                               then return ()
                               else throwErrorPackratM "True" "not match: " ["dv_hsTypeArr"] d500_445
                              return (sigE eo t),
                           do d502_447 <- get
                              xx501_448 <- dv_hsExpOpM
                              let eo = xx501_448
                              if True
                               then return ()
                               else throwErrorPackratM "True" "not match: " ["dv_hsExpOp"] d502_447
                              return eo]
p_hsExpOp = foldl1 mplus [do d504_449 <- get
                             xx503_450 <- dv_hsExpM
                             let l = xx503_450
                             if True
                              then return ()
                              else throwErrorPackratM "True" "not match: " ["dv_hsExp"] d504_449
                             d506_451 <- get
                             _ <- dv_spacesM
                             if True
                              then return ()
                              else throwErrorPackratM "True" "not match: " ["dv_spaces"] d506_451
                             d508_452 <- get
                             xx507_453 <- dv_hsOpM
                             let o = xx507_453
                             if True
                              then return ()
                              else throwErrorPackratM "True" "not match: " ["dv_hsOp"] d508_452
                             d510_454 <- get
                             _ <- dv_spacesM
                             if True
                              then return ()
                              else throwErrorPackratM "True" "not match: " ["dv_spaces"] d510_454
                             d512_455 <- get
                             xx511_456 <- dv_hsExpOpM
                             let r = xx511_456
                             if True
                              then return ()
                              else throwErrorPackratM "True" "not match: " ["dv_hsExpOp"] d512_455
                             return (uInfixE (getEx l) o r),
                          do d514_457 <- get
                             xx513_458 <- dv_hsExpM
                             let e = xx513_458
                             if True
                              then return ()
                              else throwErrorPackratM "True" "not match: " ["dv_hsExp"] d514_457
                             return (getEx e)]
p_hsOp = foldl1 mplus [do d516_459 <- get
                          xx515_460 <- dvCharsM
                          let c = xx515_460
                          if isOpHeadChar c
                           then return ()
                           else throwErrorPackratM "isOpHeadChar c" "not match: " ["dvChars"] d516_459
                          d518_461 <- get
                          xx517_462 <- dv_opTailM
                          let o = xx517_462
                          if True
                           then return ()
                           else throwErrorPackratM "True" "not match: " ["dv_opTail"] d518_461
                          return (varE (mkName (cons c o))),
                       do d520_463 <- get
                          xx519_464 <- dvCharsM
                          case xx519_464 of
                              ':' -> return ()
                              _ -> throwErrorPackratM "':'" "not match pattern: " ["dvChars"] d520_463
                          let ':' = xx519_464
                          return ()
                          if True
                           then return ()
                           else throwErrorPackratM "True" "not match: " ["dvChars"] d520_463
                          ddd521_465 <- get
                          flipMaybe "':':[True]" ddd521_465 ["dvChars"] (do d523_466 <- get
                                                                            xx522_467 <- dvCharsM
                                                                            case xx522_467 of
                                                                                ':' -> return ()
                                                                                _ -> throwErrorPackratM "':'" "not match pattern: " ["dvChars"] d523_466
                                                                            let ':' = xx522_467
                                                                            return ()
                                                                            if True
                                                                             then return ()
                                                                             else throwErrorPackratM "True" "not match: " ["dvChars"] d523_466)
                          put ddd521_465
                          d525_468 <- get
                          xx524_469 <- dv_opTailM
                          let o = xx524_469
                          if True
                           then return ()
                           else throwErrorPackratM "True" "not match: " ["dv_opTail"] d525_468
                          return (conE (mkName (':' : o))),
                       do d527_470 <- get
                          xx526_471 <- dvCharsM
                          let c = xx526_471
                          if isBQ c
                           then return ()
                           else throwErrorPackratM "isBQ c" "not match: " ["dvChars"] d527_470
                          d529_472 <- get
                          xx528_473 <- dv_variableM
                          let v = xx528_473
                          if True
                           then return ()
                           else throwErrorPackratM "True" "not match: " ["dv_variable"] d529_472
                          d531_474 <- get
                          xx530_475 <- dvCharsM
                          let c_ = xx530_475
                          if isBQ c_
                           then return ()
                           else throwErrorPackratM "isBQ c_" "not match: " ["dvChars"] d531_474
                          return (varE (mkName v)),
                       do d533_476 <- get
                          xx532_477 <- dvCharsM
                          let c = xx532_477
                          if isBQ c
                           then return ()
                           else throwErrorPackratM "isBQ c" "not match: " ["dvChars"] d533_476
                          d535_478 <- get
                          xx534_479 <- dv_typM
                          let t = xx534_479
                          if True
                           then return ()
                           else throwErrorPackratM "True" "not match: " ["dv_typ"] d535_478
                          d537_480 <- get
                          xx536_481 <- dvCharsM
                          let c_ = xx536_481
                          if isBQ c_
                           then return ()
                           else throwErrorPackratM "isBQ c_" "not match: " ["dvChars"] d537_480
                          return (conE (mkName t))]
p_opTail = foldl1 mplus [do d539_482 <- get
                            xx538_483 <- dvCharsM
                            let c = xx538_483
                            if isOpTailChar c
                             then return ()
                             else throwErrorPackratM "isOpTailChar c" "not match: " ["dvChars"] d539_482
                            d541_484 <- get
                            xx540_485 <- dv_opTailM
                            let s = xx540_485
                            if True
                             then return ()
                             else throwErrorPackratM "True" "not match: " ["dv_opTail"] d541_484
                            return (cons c s),
                         do return emp]
p_hsExp = foldl1 mplus [do d543_486 <- get
                           xx542_487 <- dv_hsExp1M
                           let e = xx542_487
                           if True
                            then return ()
                            else throwErrorPackratM "True" "not match: " ["dv_hsExp1"] d543_486
                           d545_488 <- get
                           _ <- dv_spacesM
                           if True
                            then return ()
                            else throwErrorPackratM "True" "not match: " ["dv_spaces"] d545_488
                           d547_489 <- get
                           xx546_490 <- dv_hsExpM
                           let h = xx546_490
                           if True
                            then return ()
                            else throwErrorPackratM "True" "not match: " ["dv_hsExp"] d547_489
                           return (applyExR e h),
                        do d549_491 <- get
                           xx548_492 <- dv_hsExp1M
                           let e = xx548_492
                           if True
                            then return ()
                            else throwErrorPackratM "True" "not match: " ["dv_hsExp1"] d549_491
                           return (toEx e)]
p_hsExp1 = foldl1 mplus [do d551_493 <- get
                            xx550_494 <- dvCharsM
                            case xx550_494 of
                                '(' -> return ()
                                _ -> throwErrorPackratM "'('" "not match pattern: " ["dvChars"] d551_493
                            let '(' = xx550_494
                            return ()
                            if True
                             then return ()
                             else throwErrorPackratM "True" "not match: " ["dvChars"] d551_493
                            d553_495 <- get
                            xx552_496 <- papOptional (foldl1 mplus [do d555_497 <- get
                                                                       xx554_498 <- dv_hsExpTypM
                                                                       let e = xx554_498
                                                                       if True
                                                                        then return ()
                                                                        else throwErrorPackratM "True" "not match: " ["dv_hsExpTyp"] d555_497
                                                                       return e])
                            let l = xx552_496
                            if True
                             then return ()
                             else throwErrorPackratM "True" "not match: " ["dv_hsExpTyp"] d553_495
                            d557_499 <- get
                            _ <- dv_spacesM
                            if True
                             then return ()
                             else throwErrorPackratM "True" "not match: " ["dv_spaces"] d557_499
                            d559_500 <- get
                            xx558_501 <- dv_hsOpM
                            let o = xx558_501
                            if True
                             then return ()
                             else throwErrorPackratM "True" "not match: " ["dv_hsOp"] d559_500
                            d561_502 <- get
                            _ <- dv_spacesM
                            if True
                             then return ()
                             else throwErrorPackratM "True" "not match: " ["dv_spaces"] d561_502
                            d563_503 <- get
                            xx562_504 <- papOptional (foldl1 mplus [do d565_505 <- get
                                                                       xx564_506 <- dv_hsExpTypM
                                                                       let e = xx564_506
                                                                       if True
                                                                        then return ()
                                                                        else throwErrorPackratM "True" "not match: " ["dv_hsExpTyp"] d565_505
                                                                       return e])
                            let r = xx562_504
                            if True
                             then return ()
                             else throwErrorPackratM "True" "not match: " ["dv_hsExpTyp"] d563_503
                            d567_507 <- get
                            xx566_508 <- dvCharsM
                            case xx566_508 of
                                ')' -> return ()
                                _ -> throwErrorPackratM "')'" "not match pattern: " ["dvChars"] d567_507
                            let ')' = xx566_508
                            return ()
                            if True
                             then return ()
                             else throwErrorPackratM "True" "not match: " ["dvChars"] d567_507
                            return (infixE l o r),
                         do d569_509 <- get
                            xx568_510 <- dvCharsM
                            case xx568_510 of
                                '(' -> return ()
                                _ -> throwErrorPackratM "'('" "not match pattern: " ["dvChars"] d569_509
                            let '(' = xx568_510
                            return ()
                            if True
                             then return ()
                             else throwErrorPackratM "True" "not match: " ["dvChars"] d569_509
                            d571_511 <- get
                            xx570_512 <- dv_hsExpTplM
                            let et = xx570_512
                            if True
                             then return ()
                             else throwErrorPackratM "True" "not match: " ["dv_hsExpTpl"] d571_511
                            d573_513 <- get
                            xx572_514 <- dvCharsM
                            case xx572_514 of
                                ')' -> return ()
                                _ -> throwErrorPackratM "')'" "not match pattern: " ["dvChars"] d573_513
                            let ')' = xx572_514
                            return ()
                            if True
                             then return ()
                             else throwErrorPackratM "True" "not match: " ["dvChars"] d573_513
                            return (tupE et),
                         do d575_515 <- get
                            xx574_516 <- dvCharsM
                            case xx574_516 of
                                '[' -> return ()
                                _ -> throwErrorPackratM "'['" "not match pattern: " ["dvChars"] d575_515
                            let '[' = xx574_516
                            return ()
                            if True
                             then return ()
                             else throwErrorPackratM "True" "not match: " ["dvChars"] d575_515
                            d577_517 <- get
                            xx576_518 <- dv_hsExpTplM
                            let et = xx576_518
                            if True
                             then return ()
                             else throwErrorPackratM "True" "not match: " ["dv_hsExpTpl"] d577_517
                            d579_519 <- get
                            xx578_520 <- dvCharsM
                            case xx578_520 of
                                ']' -> return ()
                                _ -> throwErrorPackratM "']'" "not match pattern: " ["dvChars"] d579_519
                            let ']' = xx578_520
                            return ()
                            if True
                             then return ()
                             else throwErrorPackratM "True" "not match: " ["dvChars"] d579_519
                            return (listE et),
                         do d581_521 <- get
                            xx580_522 <- dv_variableM
                            let v = xx580_522
                            if True
                             then return ()
                             else throwErrorPackratM "True" "not match: " ["dv_variable"] d581_521
                            return (varE (mkName v)),
                         do d583_523 <- get
                            xx582_524 <- dv_typM
                            let t = xx582_524
                            if True
                             then return ()
                             else throwErrorPackratM "True" "not match: " ["dv_typ"] d583_523
                            return (conE (mkName t)),
                         do d585_525 <- get
                            xx584_526 <- dv_integerM
                            let i = xx584_526
                            if True
                             then return ()
                             else throwErrorPackratM "True" "not match: " ["dv_integer"] d585_525
                            d587_527 <- get
                            _ <- dv_spacesM
                            if True
                             then return ()
                             else throwErrorPackratM "True" "not match: " ["dv_spaces"] d587_527
                            return (litE (integerL i)),
                         do d589_528 <- get
                            xx588_529 <- dvCharsM
                            case xx588_529 of
                                '\'' -> return ()
                                _ -> throwErrorPackratM "'\\''" "not match pattern: " ["dvChars"] d589_528
                            let '\'' = xx588_529
                            return ()
                            if True
                             then return ()
                             else throwErrorPackratM "True" "not match: " ["dvChars"] d589_528
                            d591_530 <- get
                            xx590_531 <- dv_charLitM
                            let c = xx590_531
                            if True
                             then return ()
                             else throwErrorPackratM "True" "not match: " ["dv_charLit"] d591_530
                            d593_532 <- get
                            xx592_533 <- dvCharsM
                            case xx592_533 of
                                '\'' -> return ()
                                _ -> throwErrorPackratM "'\\''" "not match pattern: " ["dvChars"] d593_532
                            let '\'' = xx592_533
                            return ()
                            if True
                             then return ()
                             else throwErrorPackratM "True" "not match: " ["dvChars"] d593_532
                            return (litE (charL c)),
                         do d595_534 <- get
                            xx594_535 <- dvCharsM
                            case xx594_535 of
                                '"' -> return ()
                                _ -> throwErrorPackratM "'\"'" "not match pattern: " ["dvChars"] d595_534
                            let '"' = xx594_535
                            return ()
                            if True
                             then return ()
                             else throwErrorPackratM "True" "not match: " ["dvChars"] d595_534
                            d597_536 <- get
                            xx596_537 <- dv_stringLitM
                            let s = xx596_537
                            if True
                             then return ()
                             else throwErrorPackratM "True" "not match: " ["dv_stringLit"] d597_536
                            d599_538 <- get
                            xx598_539 <- dvCharsM
                            case xx598_539 of
                                '"' -> return ()
                                _ -> throwErrorPackratM "'\"'" "not match pattern: " ["dvChars"] d599_538
                            let '"' = xx598_539
                            return ()
                            if True
                             then return ()
                             else throwErrorPackratM "True" "not match: " ["dvChars"] d599_538
                            return (litE (stringL s)),
                         do d601_540 <- get
                            xx600_541 <- dvCharsM
                            case xx600_541 of
                                '-' -> return ()
                                _ -> throwErrorPackratM "'-'" "not match pattern: " ["dvChars"] d601_540
                            let '-' = xx600_541
                            return ()
                            if True
                             then return ()
                             else throwErrorPackratM "True" "not match: " ["dvChars"] d601_540
                            d603_542 <- get
                            _ <- dv_spacesM
                            if True
                             then return ()
                             else throwErrorPackratM "True" "not match: " ["dv_spaces"] d603_542
                            d605_543 <- get
                            xx604_544 <- dv_hsExp1M
                            let e = xx604_544
                            if True
                             then return ()
                             else throwErrorPackratM "True" "not match: " ["dv_hsExp1"] d605_543
                            return (appE (varE $ mkName "negate") e)]
p_hsExpTpl = foldl1 mplus [do d607_545 <- get
                              xx606_546 <- dv_hsExpLamM
                              let e = xx606_546
                              if True
                               then return ()
                               else throwErrorPackratM "True" "not match: " ["dv_hsExpLam"] d607_545
                              d609_547 <- get
                              _ <- dv_spacesM
                              if True
                               then return ()
                               else throwErrorPackratM "True" "not match: " ["dv_spaces"] d609_547
                              d611_548 <- get
                              xx610_549 <- dvCharsM
                              let c = xx610_549
                              if isComma c
                               then return ()
                               else throwErrorPackratM "isComma c" "not match: " ["dvChars"] d611_548
                              d613_550 <- get
                              _ <- dv_spacesM
                              if True
                               then return ()
                               else throwErrorPackratM "True" "not match: " ["dv_spaces"] d613_550
                              d615_551 <- get
                              xx614_552 <- dv_hsExpTplM
                              let et = xx614_552
                              if True
                               then return ()
                               else throwErrorPackratM "True" "not match: " ["dv_hsExpTpl"] d615_551
                              return (cons e et),
                           do d617_553 <- get
                              xx616_554 <- dv_hsExpLamM
                              let e = xx616_554
                              if True
                               then return ()
                               else throwErrorPackratM "True" "not match: " ["dv_hsExpLam"] d617_553
                              return (cons e emp),
                           do return emp]
p_hsTypeArr = foldl1 mplus [do d619_555 <- get
                               xx618_556 <- dv_hsTypeM
                               let l = xx618_556
                               if True
                                then return ()
                                else throwErrorPackratM "True" "not match: " ["dv_hsType"] d619_555
                               d621_557 <- get
                               xx620_558 <- dvCharsM
                               case xx620_558 of
                                   '-' -> return ()
                                   _ -> throwErrorPackratM "'-'" "not match pattern: " ["dvChars"] d621_557
                               let '-' = xx620_558
                               return ()
                               if True
                                then return ()
                                else throwErrorPackratM "True" "not match: " ["dvChars"] d621_557
                               d623_559 <- get
                               xx622_560 <- dvCharsM
                               let c = xx622_560
                               if isGt c
                                then return ()
                                else throwErrorPackratM "isGt c" "not match: " ["dvChars"] d623_559
                               d625_561 <- get
                               _ <- dv_spacesM
                               if True
                                then return ()
                                else throwErrorPackratM "True" "not match: " ["dv_spaces"] d625_561
                               d627_562 <- get
                               xx626_563 <- dv_hsTypeArrM
                               let r = xx626_563
                               if True
                                then return ()
                                else throwErrorPackratM "True" "not match: " ["dv_hsTypeArr"] d627_562
                               return (appT (appT arrowT (getTyp l)) r),
                            do d629_564 <- get
                               xx628_565 <- dv_hsTypeM
                               let t = xx628_565
                               if True
                                then return ()
                                else throwErrorPackratM "True" "not match: " ["dv_hsType"] d629_564
                               return (getTyp t)]
p_hsType = foldl1 mplus [do d631_566 <- get
                            xx630_567 <- dv_hsType1M
                            let t = xx630_567
                            if True
                             then return ()
                             else throwErrorPackratM "True" "not match: " ["dv_hsType1"] d631_566
                            d633_568 <- get
                            xx632_569 <- dv_hsTypeM
                            let ts = xx632_569
                            if True
                             then return ()
                             else throwErrorPackratM "True" "not match: " ["dv_hsType"] d633_568
                            return (applyTyp (toTyp t) ts),
                         do d635_570 <- get
                            xx634_571 <- dv_hsType1M
                            let t = xx634_571
                            if True
                             then return ()
                             else throwErrorPackratM "True" "not match: " ["dv_hsType1"] d635_570
                            return (toTyp t)]
p_hsType1 = foldl1 mplus [do d637_572 <- get
                             xx636_573 <- dvCharsM
                             case xx636_573 of
                                 '[' -> return ()
                                 _ -> throwErrorPackratM "'['" "not match pattern: " ["dvChars"] d637_572
                             let '[' = xx636_573
                             return ()
                             if True
                              then return ()
                              else throwErrorPackratM "True" "not match: " ["dvChars"] d637_572
                             d639_574 <- get
                             xx638_575 <- dvCharsM
                             case xx638_575 of
                                 ']' -> return ()
                                 _ -> throwErrorPackratM "']'" "not match pattern: " ["dvChars"] d639_574
                             let ']' = xx638_575
                             return ()
                             if True
                              then return ()
                              else throwErrorPackratM "True" "not match: " ["dvChars"] d639_574
                             d641_576 <- get
                             _ <- dv_spacesM
                             if True
                              then return ()
                              else throwErrorPackratM "True" "not match: " ["dv_spaces"] d641_576
                             return listT,
                          do d643_577 <- get
                             xx642_578 <- dvCharsM
                             case xx642_578 of
                                 '[' -> return ()
                                 _ -> throwErrorPackratM "'['" "not match pattern: " ["dvChars"] d643_577
                             let '[' = xx642_578
                             return ()
                             if True
                              then return ()
                              else throwErrorPackratM "True" "not match: " ["dvChars"] d643_577
                             d645_579 <- get
                             xx644_580 <- dv_hsTypeArrM
                             let t = xx644_580
                             if True
                              then return ()
                              else throwErrorPackratM "True" "not match: " ["dv_hsTypeArr"] d645_579
                             d647_581 <- get
                             xx646_582 <- dvCharsM
                             case xx646_582 of
                                 ']' -> return ()
                                 _ -> throwErrorPackratM "']'" "not match pattern: " ["dvChars"] d647_581
                             let ']' = xx646_582
                             return ()
                             if True
                              then return ()
                              else throwErrorPackratM "True" "not match: " ["dvChars"] d647_581
                             d649_583 <- get
                             _ <- dv_spacesM
                             if True
                              then return ()
                              else throwErrorPackratM "True" "not match: " ["dv_spaces"] d649_583
                             return (appT listT t),
                          do d651_584 <- get
                             xx650_585 <- dvCharsM
                             case xx650_585 of
                                 '(' -> return ()
                                 _ -> throwErrorPackratM "'('" "not match pattern: " ["dvChars"] d651_584
                             let '(' = xx650_585
                             return ()
                             if True
                              then return ()
                              else throwErrorPackratM "True" "not match: " ["dvChars"] d651_584
                             d653_586 <- get
                             _ <- dv_spacesM
                             if True
                              then return ()
                              else throwErrorPackratM "True" "not match: " ["dv_spaces"] d653_586
                             d655_587 <- get
                             xx654_588 <- dv_hsTypeTplM
                             let tt = xx654_588
                             if True
                              then return ()
                              else throwErrorPackratM "True" "not match: " ["dv_hsTypeTpl"] d655_587
                             d657_589 <- get
                             xx656_590 <- dvCharsM
                             case xx656_590 of
                                 ')' -> return ()
                                 _ -> throwErrorPackratM "')'" "not match pattern: " ["dvChars"] d657_589
                             let ')' = xx656_590
                             return ()
                             if True
                              then return ()
                              else throwErrorPackratM "True" "not match: " ["dvChars"] d657_589
                             return (tupT tt),
                          do d659_591 <- get
                             xx658_592 <- dv_typTokenM
                             let t = xx658_592
                             if True
                              then return ()
                              else throwErrorPackratM "True" "not match: " ["dv_typToken"] d659_591
                             return (conT (mkName t)),
                          do d661_593 <- get
                             xx660_594 <- dvCharsM
                             case xx660_594 of
                                 '(' -> return ()
                                 _ -> throwErrorPackratM "'('" "not match pattern: " ["dvChars"] d661_593
                             let '(' = xx660_594
                             return ()
                             if True
                              then return ()
                              else throwErrorPackratM "True" "not match: " ["dvChars"] d661_593
                             d663_595 <- get
                             xx662_596 <- dvCharsM
                             case xx662_596 of
                                 '-' -> return ()
                                 _ -> throwErrorPackratM "'-'" "not match pattern: " ["dvChars"] d663_595
                             let '-' = xx662_596
                             return ()
                             if True
                              then return ()
                              else throwErrorPackratM "True" "not match: " ["dvChars"] d663_595
                             d665_597 <- get
                             xx664_598 <- dvCharsM
                             let c = xx664_598
                             if isGt c
                              then return ()
                              else throwErrorPackratM "isGt c" "not match: " ["dvChars"] d665_597
                             d667_599 <- get
                             xx666_600 <- dvCharsM
                             case xx666_600 of
                                 ')' -> return ()
                                 _ -> throwErrorPackratM "')'" "not match pattern: " ["dvChars"] d667_599
                             let ')' = xx666_600
                             return ()
                             if True
                              then return ()
                              else throwErrorPackratM "True" "not match: " ["dvChars"] d667_599
                             d669_601 <- get
                             _ <- dv_spacesM
                             if True
                              then return ()
                              else throwErrorPackratM "True" "not match: " ["dv_spaces"] d669_601
                             return arrowT]
p_hsTypeTpl = foldl1 mplus [do d671_602 <- get
                               xx670_603 <- dv_hsTypeArrM
                               let t = xx670_603
                               if True
                                then return ()
                                else throwErrorPackratM "True" "not match: " ["dv_hsTypeArr"] d671_602
                               d673_604 <- get
                               xx672_605 <- dvCharsM
                               let c = xx672_605
                               if isComma c
                                then return ()
                                else throwErrorPackratM "isComma c" "not match: " ["dvChars"] d673_604
                               d675_606 <- get
                               _ <- dv_spacesM
                               if True
                                then return ()
                                else throwErrorPackratM "True" "not match: " ["dv_spaces"] d675_606
                               d677_607 <- get
                               xx676_608 <- dv_hsTypeTplM
                               let tt = xx676_608
                               if True
                                then return ()
                                else throwErrorPackratM "True" "not match: " ["dv_hsTypeTpl"] d677_607
                               return (cons t tt),
                            do d679_609 <- get
                               xx678_610 <- dv_hsTypeArrM
                               let t = xx678_610
                               if True
                                then return ()
                                else throwErrorPackratM "True" "not match: " ["dv_hsTypeArr"] d679_609
                               return (cons t emp),
                            do return emp]
p_typ = foldl1 mplus [do d681_611 <- get
                         xx680_612 <- dv_upperM
                         let u = xx680_612
                         if True
                          then return ()
                          else throwErrorPackratM "True" "not match: " ["dv_upper"] d681_611
                         d683_613 <- get
                         xx682_614 <- dv_tvtailM
                         let t = xx682_614
                         if True
                          then return ()
                          else throwErrorPackratM "True" "not match: " ["dv_tvtail"] d683_613
                         return (cons u t)]
p_variable = foldl1 mplus [do d685_615 <- get
                              xx684_616 <- dv_lowerM
                              let l = xx684_616
                              if True
                               then return ()
                               else throwErrorPackratM "True" "not match: " ["dv_lower"] d685_615
                              d687_617 <- get
                              xx686_618 <- dv_tvtailM
                              let t = xx686_618
                              if True
                               then return ()
                               else throwErrorPackratM "True" "not match: " ["dv_tvtail"] d687_617
                              return (cons l t)]
p_tvtail = foldl1 mplus [do d689_619 <- get
                            xx688_620 <- dv_alphaM
                            let a = xx688_620
                            if True
                             then return ()
                             else throwErrorPackratM "True" "not match: " ["dv_alpha"] d689_619
                            d691_621 <- get
                            xx690_622 <- dv_tvtailM
                            let t = xx690_622
                            if True
                             then return ()
                             else throwErrorPackratM "True" "not match: " ["dv_tvtail"] d691_621
                            return (cons a t),
                         do return emp]
p_integer = foldl1 mplus [do d693_623 <- get
                             xx692_624 <- dv_digitM
                             let dh = xx692_624
                             if True
                              then return ()
                              else throwErrorPackratM "True" "not match: " ["dv_digit"] d693_623
                             d695_625 <- get
                             xx694_626 <- list (foldl1 mplus [do d697_627 <- get
                                                                 xx696_628 <- dv_digitM
                                                                 let d = xx696_628
                                                                 if True
                                                                  then return ()
                                                                  else throwErrorPackratM "True" "not match: " ["dv_digit"] d697_627
                                                                 return d])
                             let ds = xx694_626
                             if True
                              then return ()
                              else throwErrorPackratM "True" "not match: " ["dv_digit"] d695_625
                             return (read (cons dh ds))]
p_alpha = foldl1 mplus [do d699_629 <- get
                           xx698_630 <- dv_upperM
                           let u = xx698_630
                           if True
                            then return ()
                            else throwErrorPackratM "True" "not match: " ["dv_upper"] d699_629
                           return u,
                        do d701_631 <- get
                           xx700_632 <- dv_lowerM
                           let l = xx700_632
                           if True
                            then return ()
                            else throwErrorPackratM "True" "not match: " ["dv_lower"] d701_631
                           return l,
                        do d703_633 <- get
                           xx702_634 <- dv_digitM
                           let d = xx702_634
                           if True
                            then return ()
                            else throwErrorPackratM "True" "not match: " ["dv_digit"] d703_633
                           return d]
p_upper = foldl1 mplus [do d705_635 <- get
                           xx704_636 <- dvCharsM
                           let u = xx704_636
                           if isUpper u
                            then return ()
                            else throwErrorPackratM "isUpper u" "not match: " ["dvChars"] d705_635
                           return u]
p_lower = foldl1 mplus [do d707_637 <- get
                           xx706_638 <- dvCharsM
                           let l = xx706_638
                           if isLowerU l
                            then return ()
                            else throwErrorPackratM "isLowerU l" "not match: " ["dvChars"] d707_637
                           return l]
p_digit = foldl1 mplus [do d709_639 <- get
                           xx708_640 <- dvCharsM
                           let d = xx708_640
                           if isDigit d
                            then return ()
                            else throwErrorPackratM "isDigit d" "not match: " ["dvChars"] d709_639
                           return d]
p_spaces = foldl1 mplus [do d711_641 <- get
                            _ <- dv_spaceM
                            if True
                             then return ()
                             else throwErrorPackratM "True" "not match: " ["dv_space"] d711_641
                            d713_642 <- get
                            _ <- dv_spacesM
                            if True
                             then return ()
                             else throwErrorPackratM "True" "not match: " ["dv_spaces"] d713_642
                            return (),
                         do return ()]
p_space = foldl1 mplus [do d715_643 <- get
                           xx714_644 <- dvCharsM
                           let s = xx714_644
                           if isSpace s
                            then return ()
                            else throwErrorPackratM "isSpace s" "not match: " ["dvChars"] d715_643
                           return (),
                        do d717_645 <- get
                           xx716_646 <- dvCharsM
                           case xx716_646 of
                               '-' -> return ()
                               _ -> throwErrorPackratM "'-'" "not match pattern: " ["dvChars"] d717_645
                           let '-' = xx716_646
                           return ()
                           if True
                            then return ()
                            else throwErrorPackratM "True" "not match: " ["dvChars"] d717_645
                           d719_647 <- get
                           xx718_648 <- dvCharsM
                           case xx718_648 of
                               '-' -> return ()
                               _ -> throwErrorPackratM "'-'" "not match pattern: " ["dvChars"] d719_647
                           let '-' = xx718_648
                           return ()
                           if True
                            then return ()
                            else throwErrorPackratM "True" "not match: " ["dvChars"] d719_647
                           d721_649 <- get
                           _ <- dv_notNLStringM
                           if True
                            then return ()
                            else throwErrorPackratM "True" "not match: " ["dv_notNLString"] d721_649
                           d723_650 <- get
                           _ <- dv_nlM
                           if True
                            then return ()
                            else throwErrorPackratM "True" "not match: " ["dv_nl"] d723_650
                           return (),
                        do d725_651 <- get
                           _ <- dv_commentM
                           if True
                            then return ()
                            else throwErrorPackratM "True" "not match: " ["dv_comment"] d725_651
                           return ()]
p_notNLString = foldl1 mplus [do ddd726_652 <- get
                                 flipMaybe "_:nl[True]" ddd726_652 ["dv_nl"] (do d728_653 <- get
                                                                                 _ <- dv_nlM
                                                                                 if True
                                                                                  then return ()
                                                                                  else throwErrorPackratM "True" "not match: " ["dv_nl"] d728_653)
                                 put ddd726_652
                                 d730_654 <- get
                                 xx729_655 <- dvCharsM
                                 let c = xx729_655
                                 if True
                                  then return ()
                                  else throwErrorPackratM "True" "not match: " ["dvChars"] d730_654
                                 d732_656 <- get
                                 xx731_657 <- dv_notNLStringM
                                 let s = xx731_657
                                 if True
                                  then return ()
                                  else throwErrorPackratM "True" "not match: " ["dv_notNLString"] d732_656
                                 return (cons c s),
                              do return emp]
p_nl = foldl1 mplus [do d734_658 <- get
                        xx733_659 <- dvCharsM
                        case xx733_659 of
                            '\n' -> return ()
                            _ -> throwErrorPackratM "'\\n'" "not match pattern: " ["dvChars"] d734_658
                        let '\n' = xx733_659
                        return ()
                        if True
                         then return ()
                         else throwErrorPackratM "True" "not match: " ["dvChars"] d734_658
                        return ()]
p_comment = foldl1 mplus [do d736_660 <- get
                             xx735_661 <- dvCharsM
                             case xx735_661 of
                                 '{' -> return ()
                                 _ -> throwErrorPackratM "'{'" "not match pattern: " ["dvChars"] d736_660
                             let '{' = xx735_661
                             return ()
                             if True
                              then return ()
                              else throwErrorPackratM "True" "not match: " ["dvChars"] d736_660
                             d738_662 <- get
                             xx737_663 <- dvCharsM
                             case xx737_663 of
                                 '-' -> return ()
                                 _ -> throwErrorPackratM "'-'" "not match pattern: " ["dvChars"] d738_662
                             let '-' = xx737_663
                             return ()
                             if True
                              then return ()
                              else throwErrorPackratM "True" "not match: " ["dvChars"] d738_662
                             ddd739_664 <- get
                             flipMaybe "'#':[True]" ddd739_664 ["dvChars"] (do d741_665 <- get
                                                                               xx740_666 <- dvCharsM
                                                                               case xx740_666 of
                                                                                   '#' -> return ()
                                                                                   _ -> throwErrorPackratM "'#'" "not match pattern: " ["dvChars"] d741_665
                                                                               let '#' = xx740_666
                                                                               return ()
                                                                               if True
                                                                                then return ()
                                                                                else throwErrorPackratM "True" "not match: " ["dvChars"] d741_665)
                             put ddd739_664
                             d743_667 <- get
                             _ <- dv_commentsM
                             if True
                              then return ()
                              else throwErrorPackratM "True" "not match: " ["dv_comments"] d743_667
                             d745_668 <- get
                             _ <- dv_comEndM
                             if True
                              then return ()
                              else throwErrorPackratM "True" "not match: " ["dv_comEnd"] d745_668
                             return ()]
p_comments = foldl1 mplus [do d747_669 <- get
                              _ <- dv_notComStrM
                              if True
                               then return ()
                               else throwErrorPackratM "True" "not match: " ["dv_notComStr"] d747_669
                              d749_670 <- get
                              _ <- dv_commentM
                              if True
                               then return ()
                               else throwErrorPackratM "True" "not match: " ["dv_comment"] d749_670
                              d751_671 <- get
                              _ <- dv_commentsM
                              if True
                               then return ()
                               else throwErrorPackratM "True" "not match: " ["dv_comments"] d751_671
                              return (),
                           do d753_672 <- get
                              _ <- dv_notComStrM
                              if True
                               then return ()
                               else throwErrorPackratM "True" "not match: " ["dv_notComStr"] d753_672
                              return ()]
p_notComStr = foldl1 mplus [do ddd754_673 <- get
                               flipMaybe "_:comment[True]" ddd754_673 ["dv_comment"] (do d756_674 <- get
                                                                                         _ <- dv_commentM
                                                                                         if True
                                                                                          then return ()
                                                                                          else throwErrorPackratM "True" "not match: " ["dv_comment"] d756_674)
                               put ddd754_673
                               ddd757_675 <- get
                               flipMaybe "_:comEnd[True]" ddd757_675 ["dv_comEnd"] (do d759_676 <- get
                                                                                       _ <- dv_comEndM
                                                                                       if True
                                                                                        then return ()
                                                                                        else throwErrorPackratM "True" "not match: " ["dv_comEnd"] d759_676)
                               put ddd757_675
                               d761_677 <- get
                               _ <- dvCharsM
                               if True
                                then return ()
                                else throwErrorPackratM "True" "not match: " ["dvChars"] d761_677
                               d763_678 <- get
                               _ <- dv_notComStrM
                               if True
                                then return ()
                                else throwErrorPackratM "True" "not match: " ["dv_notComStr"] d763_678
                               return (),
                            do return ()]
p_comEnd = foldl1 mplus [do d765_679 <- get
                            xx764_680 <- dvCharsM
                            case xx764_680 of
                                '-' -> return ()
                                _ -> throwErrorPackratM "'-'" "not match pattern: " ["dvChars"] d765_679
                            let '-' = xx764_680
                            return ()
                            if True
                             then return ()
                             else throwErrorPackratM "True" "not match: " ["dvChars"] d765_679
                            d767_681 <- get
                            xx766_682 <- dvCharsM
                            case xx766_682 of
                                '}' -> return ()
                                _ -> throwErrorPackratM "'}'" "not match pattern: " ["dvChars"] d767_681
                            let '}' = xx766_682
                            return ()
                            if True
                             then return ()
                             else throwErrorPackratM "True" "not match: " ["dvChars"] d767_681
                            return ()]

class Source sl
    where type Token sl
          data Pos sl
          getToken :: sl -> Maybe ((Token sl, sl))
          initialPos :: Pos sl
          updatePos :: Token sl -> Pos sl -> Pos sl
          showPos :: Pos sl -> String
class SourceList c
    where data ListPos c
          listToken :: [c] -> Maybe ((c, [c]))
          listInitialPos :: ListPos c
          listUpdatePos :: c -> ListPos c -> ListPos c
          listShowPos :: ListPos c -> String
instance SourceList Char
    where newtype ListPos Char = CharPos ((Int, Int)) deriving (Show)
          listToken (c : s) = Just (c, s)
          listToken _ = Nothing
          listInitialPos = CharPos (1, 1)
          listUpdatePos '\n' (CharPos (y, _)) = CharPos (y + 1, 0)
          listUpdatePos _ (CharPos (y, x)) = CharPos (y, x + 1)
          listShowPos (CharPos pos) = show pos
instance Show (ListPos a) => Show (Pos ([a]))
    where show (ListPos x) = "(" ++ (("ListPos (" ++ (show x ++ ")")) ++ ")")
instance SourceList c => Source ([c])
    where type Token ([c]) = c
          newtype Pos ([c]) = ListPos (ListPos c)
          getToken = listToken
          initialPos = ListPos listInitialPos
          updatePos c (ListPos p) = ListPos (listUpdatePos c p)
          showPos (ListPos p) = listShowPos p
list :: forall m a . (MonadPlus m, Applicative m) => m a -> m ([a])
list1 :: forall m a . (MonadPlus m, Applicative m) =>
                      m a -> m ([a])
list p = list1 p `mplus` return []
list1 p = ((:) <$> p) <*> list p
papOptional :: forall m a . (MonadPlus m, Applicative m) =>
                            m a -> m (Maybe a)
papOptional p = (Just <$> p) `mplus` return Nothing