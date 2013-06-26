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
parse :: Pos String -> String -> Derivs
parse pos___hoge s = d
          where d = Derivs pegFile pragma pragmaStr pragmaEnd moduleDec moduleDecStr whr preImpPap prePeg afterPeg importPapillon varToken typToken pap peg sourceType peg_ definition selection expressionHs expression nameLeaf_ nameLeaf nameLeafNoCom comForErr leaf patOp pat pat1 patList opConName charLit stringLit escapeC dq pats readFromLs readFrom test hsExpLam hsExpTyp hsExpOp hsOp opTail hsExp hsExp1 hsExpTpl hsTypeArr hsType hsType1 hsTypeTpl typ variable tvtail integer alpha upper lower digit spaces space notNLString nl comment comments notComStr comEnd char pos___hoge
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
                             xx424_378 <- dv_escapeCM
                             let c = xx424_378
                             if True
                              then return ()
                              else throwErrorPackratM "True" "not match: " ["dv_escapeC"] d425_377
                             return c]
p_stringLit = foldl1 mplus [do d427_379 <- get
                               xx426_380 <- dvCharsM
                               let c = xx426_380
                               if isStrLitC c
                                then return ()
                                else throwErrorPackratM "isStrLitC c" "not match: " ["dvChars"] d427_379
                               d429_381 <- get
                               xx428_382 <- dv_stringLitM
                               let s = xx428_382
                               if True
                                then return ()
                                else throwErrorPackratM "True" "not match: " ["dv_stringLit"] d429_381
                               return (cons c s),
                            do d431_383 <- get
                               xx430_384 <- dvCharsM
                               case xx430_384 of
                                   '\\' -> return ()
                                   _ -> throwErrorPackratM "'\\\\'" "not match pattern: " ["dvChars"] d431_383
                               let '\\' = xx430_384
                               return ()
                               if True
                                then return ()
                                else throwErrorPackratM "True" "not match: " ["dvChars"] d431_383
                               d433_385 <- get
                               xx432_386 <- dv_escapeCM
                               let c = xx432_386
                               if True
                                then return ()
                                else throwErrorPackratM "True" "not match: " ["dv_escapeC"] d433_385
                               d435_387 <- get
                               xx434_388 <- dv_stringLitM
                               let s = xx434_388
                               if True
                                then return ()
                                else throwErrorPackratM "True" "not match: " ["dv_stringLit"] d435_387
                               return (c : s),
                            do return emp]
p_escapeC = foldl1 mplus [do d437_389 <- get
                             xx436_390 <- dvCharsM
                             case xx436_390 of
                                 '"' -> return ()
                                 _ -> throwErrorPackratM "'\"'" "not match pattern: " ["dvChars"] d437_389
                             let '"' = xx436_390
                             return ()
                             if True
                              then return ()
                              else throwErrorPackratM "True" "not match: " ["dvChars"] d437_389
                             return '"',
                          do d439_391 <- get
                             xx438_392 <- dvCharsM
                             case xx438_392 of
                                 '\'' -> return ()
                                 _ -> throwErrorPackratM "'\\''" "not match pattern: " ["dvChars"] d439_391
                             let '\'' = xx438_392
                             return ()
                             if True
                              then return ()
                              else throwErrorPackratM "True" "not match: " ["dvChars"] d439_391
                             return '\'',
                          do d441_393 <- get
                             xx440_394 <- dvCharsM
                             case xx440_394 of
                                 '\\' -> return ()
                                 _ -> throwErrorPackratM "'\\\\'" "not match pattern: " ["dvChars"] d441_393
                             let '\\' = xx440_394
                             return ()
                             if True
                              then return ()
                              else throwErrorPackratM "True" "not match: " ["dvChars"] d441_393
                             return '\\',
                          do d443_395 <- get
                             xx442_396 <- dvCharsM
                             case xx442_396 of
                                 'n' -> return ()
                                 _ -> throwErrorPackratM "'n'" "not match pattern: " ["dvChars"] d443_395
                             let 'n' = xx442_396
                             return ()
                             if True
                              then return ()
                              else throwErrorPackratM "True" "not match: " ["dvChars"] d443_395
                             return newLine,
                          do d445_397 <- get
                             xx444_398 <- dvCharsM
                             case xx444_398 of
                                 't' -> return ()
                                 _ -> throwErrorPackratM "'t'" "not match pattern: " ["dvChars"] d445_397
                             let 't' = xx444_398
                             return ()
                             if True
                              then return ()
                              else throwErrorPackratM "True" "not match: " ["dvChars"] d445_397
                             return tab]
p_dq = foldl1 mplus [do d447_399 <- get
                        xx446_400 <- dvCharsM
                        case xx446_400 of
                            '"' -> return ()
                            _ -> throwErrorPackratM "'\"'" "not match pattern: " ["dvChars"] d447_399
                        let '"' = xx446_400
                        return ()
                        if True
                         then return ()
                         else throwErrorPackratM "True" "not match: " ["dvChars"] d447_399
                        return ()]
p_pats = foldl1 mplus [do d449_401 <- get
                          xx448_402 <- dv_patM
                          let p = xx448_402
                          if True
                           then return ()
                           else throwErrorPackratM "True" "not match: " ["dv_pat"] d449_401
                          d451_403 <- get
                          _ <- dv_spacesM
                          if True
                           then return ()
                           else throwErrorPackratM "True" "not match: " ["dv_spaces"] d451_403
                          d453_404 <- get
                          xx452_405 <- dv_patsM
                          let ps = xx452_405
                          if True
                           then return ()
                           else throwErrorPackratM "True" "not match: " ["dv_pats"] d453_404
                          return (cons p ps),
                       do return emp]
p_readFromLs = foldl1 mplus [do d455_406 <- get
                                xx454_407 <- dv_readFromM
                                let rf = xx454_407
                                if True
                                 then return ()
                                 else throwErrorPackratM "True" "not match: " ["dv_readFrom"] d455_406
                                d457_408 <- get
                                xx456_409 <- dvCharsM
                                case xx456_409 of
                                    '*' -> return ()
                                    _ -> throwErrorPackratM "'*'" "not match pattern: " ["dvChars"] d457_408
                                let '*' = xx456_409
                                return ()
                                if True
                                 then return ()
                                 else throwErrorPackratM "True" "not match: " ["dvChars"] d457_408
                                return (FromList rf),
                             do d459_410 <- get
                                xx458_411 <- dv_readFromM
                                let rf = xx458_411
                                if True
                                 then return ()
                                 else throwErrorPackratM "True" "not match: " ["dv_readFrom"] d459_410
                                d461_412 <- get
                                xx460_413 <- dvCharsM
                                case xx460_413 of
                                    '+' -> return ()
                                    _ -> throwErrorPackratM "'+'" "not match pattern: " ["dvChars"] d461_412
                                let '+' = xx460_413
                                return ()
                                if True
                                 then return ()
                                 else throwErrorPackratM "True" "not match: " ["dvChars"] d461_412
                                return (FromList1 rf),
                             do d463_414 <- get
                                xx462_415 <- dv_readFromM
                                let rf = xx462_415
                                if True
                                 then return ()
                                 else throwErrorPackratM "True" "not match: " ["dv_readFrom"] d463_414
                                d465_416 <- get
                                xx464_417 <- dvCharsM
                                case xx464_417 of
                                    '?' -> return ()
                                    _ -> throwErrorPackratM "'?'" "not match pattern: " ["dvChars"] d465_416
                                let '?' = xx464_417
                                return ()
                                if True
                                 then return ()
                                 else throwErrorPackratM "True" "not match: " ["dvChars"] d465_416
                                return (FromOptional rf),
                             do d467_418 <- get
                                xx466_419 <- dv_readFromM
                                let rf = xx466_419
                                if True
                                 then return ()
                                 else throwErrorPackratM "True" "not match: " ["dv_readFrom"] d467_418
                                return rf]
p_readFrom = foldl1 mplus [do d469_420 <- get
                              xx468_421 <- dv_variableM
                              let v = xx468_421
                              if True
                               then return ()
                               else throwErrorPackratM "True" "not match: " ["dv_variable"] d469_420
                              return (FromVariable v),
                           do d471_422 <- get
                              xx470_423 <- dvCharsM
                              case xx470_423 of
                                  '(' -> return ()
                                  _ -> throwErrorPackratM "'('" "not match pattern: " ["dvChars"] d471_422
                              let '(' = xx470_423
                              return ()
                              if True
                               then return ()
                               else throwErrorPackratM "True" "not match: " ["dvChars"] d471_422
                              d473_424 <- get
                              xx472_425 <- dv_selectionM
                              let s = xx472_425
                              if True
                               then return ()
                               else throwErrorPackratM "True" "not match: " ["dv_selection"] d473_424
                              d475_426 <- get
                              xx474_427 <- dvCharsM
                              case xx474_427 of
                                  ')' -> return ()
                                  _ -> throwErrorPackratM "')'" "not match pattern: " ["dvChars"] d475_426
                              let ')' = xx474_427
                              return ()
                              if True
                               then return ()
                               else throwErrorPackratM "True" "not match: " ["dvChars"] d475_426
                              return (FromSelection s)]
p_test = foldl1 mplus [do d477_428 <- get
                          xx476_429 <- dvCharsM
                          case xx476_429 of
                              '[' -> return ()
                              _ -> throwErrorPackratM "'['" "not match pattern: " ["dvChars"] d477_428
                          let '[' = xx476_429
                          return ()
                          if True
                           then return ()
                           else throwErrorPackratM "True" "not match: " ["dvChars"] d477_428
                          d479_430 <- get
                          xx478_431 <- dv_hsExpLamM
                          let h = xx478_431
                          if True
                           then return ()
                           else throwErrorPackratM "True" "not match: " ["dv_hsExpLam"] d479_430
                          d481_432 <- get
                          _ <- dv_spacesM
                          if True
                           then return ()
                           else throwErrorPackratM "True" "not match: " ["dv_spaces"] d481_432
                          d483_433 <- get
                          xx482_434 <- papOptional dv_comForErrM
                          let com = xx482_434
                          if True
                           then return ()
                           else throwErrorPackratM "True" "not match: " ["dv_comForErr"] d483_433
                          d485_435 <- get
                          xx484_436 <- dvCharsM
                          case xx484_436 of
                              ']' -> return ()
                              _ -> throwErrorPackratM "']'" "not match pattern: " ["dvChars"] d485_435
                          let ']' = xx484_436
                          return ()
                          if True
                           then return ()
                           else throwErrorPackratM "True" "not match: " ["dvChars"] d485_435
                          return (h, maybe "" id com)]
p_hsExpLam = foldl1 mplus [do d487_437 <- get
                              xx486_438 <- dvCharsM
                              case xx486_438 of
                                  '\\' -> return ()
                                  _ -> throwErrorPackratM "'\\\\'" "not match pattern: " ["dvChars"] d487_437
                              let '\\' = xx486_438
                              return ()
                              if True
                               then return ()
                               else throwErrorPackratM "True" "not match: " ["dvChars"] d487_437
                              d489_439 <- get
                              _ <- dv_spacesM
                              if True
                               then return ()
                               else throwErrorPackratM "True" "not match: " ["dv_spaces"] d489_439
                              d491_440 <- get
                              xx490_441 <- dv_patsM
                              let ps = xx490_441
                              if True
                               then return ()
                               else throwErrorPackratM "True" "not match: " ["dv_pats"] d491_440
                              d493_442 <- get
                              _ <- dv_spacesM
                              if True
                               then return ()
                               else throwErrorPackratM "True" "not match: " ["dv_spaces"] d493_442
                              d495_443 <- get
                              xx494_444 <- dvCharsM
                              case xx494_444 of
                                  '-' -> return ()
                                  _ -> throwErrorPackratM "'-'" "not match pattern: " ["dvChars"] d495_443
                              let '-' = xx494_444
                              return ()
                              if True
                               then return ()
                               else throwErrorPackratM "True" "not match: " ["dvChars"] d495_443
                              d497_445 <- get
                              xx496_446 <- dvCharsM
                              let c = xx496_446
                              if isGt c
                               then return ()
                               else throwErrorPackratM "isGt c" "not match: " ["dvChars"] d497_445
                              d499_447 <- get
                              _ <- dv_spacesM
                              if True
                               then return ()
                               else throwErrorPackratM "True" "not match: " ["dv_spaces"] d499_447
                              d501_448 <- get
                              xx500_449 <- dv_hsExpTypM
                              let e = xx500_449
                              if True
                               then return ()
                               else throwErrorPackratM "True" "not match: " ["dv_hsExpTyp"] d501_448
                              return (lamE ps e),
                           do d503_450 <- get
                              xx502_451 <- dv_hsExpTypM
                              let e = xx502_451
                              if True
                               then return ()
                               else throwErrorPackratM "True" "not match: " ["dv_hsExpTyp"] d503_450
                              return e]
p_hsExpTyp = foldl1 mplus [do d505_452 <- get
                              xx504_453 <- dv_hsExpOpM
                              let eo = xx504_453
                              if True
                               then return ()
                               else throwErrorPackratM "True" "not match: " ["dv_hsExpOp"] d505_452
                              d507_454 <- get
                              xx506_455 <- dvCharsM
                              case xx506_455 of
                                  ':' -> return ()
                                  _ -> throwErrorPackratM "':'" "not match pattern: " ["dvChars"] d507_454
                              let ':' = xx506_455
                              return ()
                              if True
                               then return ()
                               else throwErrorPackratM "True" "not match: " ["dvChars"] d507_454
                              d509_456 <- get
                              xx508_457 <- dvCharsM
                              case xx508_457 of
                                  ':' -> return ()
                                  _ -> throwErrorPackratM "':'" "not match pattern: " ["dvChars"] d509_456
                              let ':' = xx508_457
                              return ()
                              if True
                               then return ()
                               else throwErrorPackratM "True" "not match: " ["dvChars"] d509_456
                              d511_458 <- get
                              _ <- dv_spacesM
                              if True
                               then return ()
                               else throwErrorPackratM "True" "not match: " ["dv_spaces"] d511_458
                              d513_459 <- get
                              xx512_460 <- dv_hsTypeArrM
                              let t = xx512_460
                              if True
                               then return ()
                               else throwErrorPackratM "True" "not match: " ["dv_hsTypeArr"] d513_459
                              return (sigE eo t),
                           do d515_461 <- get
                              xx514_462 <- dv_hsExpOpM
                              let eo = xx514_462
                              if True
                               then return ()
                               else throwErrorPackratM "True" "not match: " ["dv_hsExpOp"] d515_461
                              return eo]
p_hsExpOp = foldl1 mplus [do d517_463 <- get
                             xx516_464 <- dv_hsExpM
                             let l = xx516_464
                             if True
                              then return ()
                              else throwErrorPackratM "True" "not match: " ["dv_hsExp"] d517_463
                             d519_465 <- get
                             _ <- dv_spacesM
                             if True
                              then return ()
                              else throwErrorPackratM "True" "not match: " ["dv_spaces"] d519_465
                             d521_466 <- get
                             xx520_467 <- dv_hsOpM
                             let o = xx520_467
                             if True
                              then return ()
                              else throwErrorPackratM "True" "not match: " ["dv_hsOp"] d521_466
                             d523_468 <- get
                             _ <- dv_spacesM
                             if True
                              then return ()
                              else throwErrorPackratM "True" "not match: " ["dv_spaces"] d523_468
                             d525_469 <- get
                             xx524_470 <- dv_hsExpOpM
                             let r = xx524_470
                             if True
                              then return ()
                              else throwErrorPackratM "True" "not match: " ["dv_hsExpOp"] d525_469
                             return (uInfixE (getEx l) o r),
                          do d527_471 <- get
                             xx526_472 <- dv_hsExpM
                             let e = xx526_472
                             if True
                              then return ()
                              else throwErrorPackratM "True" "not match: " ["dv_hsExp"] d527_471
                             return (getEx e)]
p_hsOp = foldl1 mplus [do d529_473 <- get
                          xx528_474 <- dvCharsM
                          let c = xx528_474
                          if isOpHeadChar c
                           then return ()
                           else throwErrorPackratM "isOpHeadChar c" "not match: " ["dvChars"] d529_473
                          d531_475 <- get
                          xx530_476 <- dv_opTailM
                          let o = xx530_476
                          if True
                           then return ()
                           else throwErrorPackratM "True" "not match: " ["dv_opTail"] d531_475
                          return (varE (mkName (cons c o))),
                       do d533_477 <- get
                          xx532_478 <- dvCharsM
                          case xx532_478 of
                              ':' -> return ()
                              _ -> throwErrorPackratM "':'" "not match pattern: " ["dvChars"] d533_477
                          let ':' = xx532_478
                          return ()
                          if True
                           then return ()
                           else throwErrorPackratM "True" "not match: " ["dvChars"] d533_477
                          ddd534_479 <- get
                          flipMaybe "':':[True]" ddd534_479 ["dvChars"] (do d536_480 <- get
                                                                            xx535_481 <- dvCharsM
                                                                            case xx535_481 of
                                                                                ':' -> return ()
                                                                                _ -> throwErrorPackratM "':'" "not match pattern: " ["dvChars"] d536_480
                                                                            let ':' = xx535_481
                                                                            return ()
                                                                            if True
                                                                             then return ()
                                                                             else throwErrorPackratM "True" "not match: " ["dvChars"] d536_480)
                          put ddd534_479
                          d538_482 <- get
                          xx537_483 <- dv_opTailM
                          let o = xx537_483
                          if True
                           then return ()
                           else throwErrorPackratM "True" "not match: " ["dv_opTail"] d538_482
                          return (conE (mkName (':' : o))),
                       do d540_484 <- get
                          xx539_485 <- dvCharsM
                          let c = xx539_485
                          if isBQ c
                           then return ()
                           else throwErrorPackratM "isBQ c" "not match: " ["dvChars"] d540_484
                          d542_486 <- get
                          xx541_487 <- dv_variableM
                          let v = xx541_487
                          if True
                           then return ()
                           else throwErrorPackratM "True" "not match: " ["dv_variable"] d542_486
                          d544_488 <- get
                          xx543_489 <- dvCharsM
                          let c_ = xx543_489
                          if isBQ c_
                           then return ()
                           else throwErrorPackratM "isBQ c_" "not match: " ["dvChars"] d544_488
                          return (varE (mkName v)),
                       do d546_490 <- get
                          xx545_491 <- dvCharsM
                          let c = xx545_491
                          if isBQ c
                           then return ()
                           else throwErrorPackratM "isBQ c" "not match: " ["dvChars"] d546_490
                          d548_492 <- get
                          xx547_493 <- dv_typM
                          let t = xx547_493
                          if True
                           then return ()
                           else throwErrorPackratM "True" "not match: " ["dv_typ"] d548_492
                          d550_494 <- get
                          xx549_495 <- dvCharsM
                          let c_ = xx549_495
                          if isBQ c_
                           then return ()
                           else throwErrorPackratM "isBQ c_" "not match: " ["dvChars"] d550_494
                          return (conE (mkName t))]
p_opTail = foldl1 mplus [do d552_496 <- get
                            xx551_497 <- dvCharsM
                            let c = xx551_497
                            if isOpTailChar c
                             then return ()
                             else throwErrorPackratM "isOpTailChar c" "not match: " ["dvChars"] d552_496
                            d554_498 <- get
                            xx553_499 <- dv_opTailM
                            let s = xx553_499
                            if True
                             then return ()
                             else throwErrorPackratM "True" "not match: " ["dv_opTail"] d554_498
                            return (cons c s),
                         do return emp]
p_hsExp = foldl1 mplus [do d556_500 <- get
                           xx555_501 <- dv_hsExp1M
                           let e = xx555_501
                           if True
                            then return ()
                            else throwErrorPackratM "True" "not match: " ["dv_hsExp1"] d556_500
                           d558_502 <- get
                           _ <- dv_spacesM
                           if True
                            then return ()
                            else throwErrorPackratM "True" "not match: " ["dv_spaces"] d558_502
                           d560_503 <- get
                           xx559_504 <- dv_hsExpM
                           let h = xx559_504
                           if True
                            then return ()
                            else throwErrorPackratM "True" "not match: " ["dv_hsExp"] d560_503
                           return (applyExR e h),
                        do d562_505 <- get
                           xx561_506 <- dv_hsExp1M
                           let e = xx561_506
                           if True
                            then return ()
                            else throwErrorPackratM "True" "not match: " ["dv_hsExp1"] d562_505
                           return (toEx e)]
p_hsExp1 = foldl1 mplus [do d564_507 <- get
                            xx563_508 <- dvCharsM
                            case xx563_508 of
                                '(' -> return ()
                                _ -> throwErrorPackratM "'('" "not match pattern: " ["dvChars"] d564_507
                            let '(' = xx563_508
                            return ()
                            if True
                             then return ()
                             else throwErrorPackratM "True" "not match: " ["dvChars"] d564_507
                            d566_509 <- get
                            xx565_510 <- papOptional (foldl1 mplus [do d568_511 <- get
                                                                       xx567_512 <- dv_hsExpTypM
                                                                       let e = xx567_512
                                                                       if True
                                                                        then return ()
                                                                        else throwErrorPackratM "True" "not match: " ["dv_hsExpTyp"] d568_511
                                                                       return e])
                            let l = xx565_510
                            if True
                             then return ()
                             else throwErrorPackratM "True" "not match: " ["dv_hsExpTyp"] d566_509
                            d570_513 <- get
                            _ <- dv_spacesM
                            if True
                             then return ()
                             else throwErrorPackratM "True" "not match: " ["dv_spaces"] d570_513
                            d572_514 <- get
                            xx571_515 <- dv_hsOpM
                            let o = xx571_515
                            if True
                             then return ()
                             else throwErrorPackratM "True" "not match: " ["dv_hsOp"] d572_514
                            d574_516 <- get
                            _ <- dv_spacesM
                            if True
                             then return ()
                             else throwErrorPackratM "True" "not match: " ["dv_spaces"] d574_516
                            d576_517 <- get
                            xx575_518 <- papOptional (foldl1 mplus [do d578_519 <- get
                                                                       xx577_520 <- dv_hsExpTypM
                                                                       let e = xx577_520
                                                                       if True
                                                                        then return ()
                                                                        else throwErrorPackratM "True" "not match: " ["dv_hsExpTyp"] d578_519
                                                                       return e])
                            let r = xx575_518
                            if True
                             then return ()
                             else throwErrorPackratM "True" "not match: " ["dv_hsExpTyp"] d576_517
                            d580_521 <- get
                            xx579_522 <- dvCharsM
                            case xx579_522 of
                                ')' -> return ()
                                _ -> throwErrorPackratM "')'" "not match pattern: " ["dvChars"] d580_521
                            let ')' = xx579_522
                            return ()
                            if True
                             then return ()
                             else throwErrorPackratM "True" "not match: " ["dvChars"] d580_521
                            return (infixE l o r),
                         do d582_523 <- get
                            xx581_524 <- dvCharsM
                            case xx581_524 of
                                '(' -> return ()
                                _ -> throwErrorPackratM "'('" "not match pattern: " ["dvChars"] d582_523
                            let '(' = xx581_524
                            return ()
                            if True
                             then return ()
                             else throwErrorPackratM "True" "not match: " ["dvChars"] d582_523
                            d584_525 <- get
                            xx583_526 <- dv_hsExpTplM
                            let et = xx583_526
                            if True
                             then return ()
                             else throwErrorPackratM "True" "not match: " ["dv_hsExpTpl"] d584_525
                            d586_527 <- get
                            xx585_528 <- dvCharsM
                            case xx585_528 of
                                ')' -> return ()
                                _ -> throwErrorPackratM "')'" "not match pattern: " ["dvChars"] d586_527
                            let ')' = xx585_528
                            return ()
                            if True
                             then return ()
                             else throwErrorPackratM "True" "not match: " ["dvChars"] d586_527
                            return (tupE et),
                         do d588_529 <- get
                            xx587_530 <- dvCharsM
                            case xx587_530 of
                                '[' -> return ()
                                _ -> throwErrorPackratM "'['" "not match pattern: " ["dvChars"] d588_529
                            let '[' = xx587_530
                            return ()
                            if True
                             then return ()
                             else throwErrorPackratM "True" "not match: " ["dvChars"] d588_529
                            d590_531 <- get
                            xx589_532 <- dv_hsExpTplM
                            let et = xx589_532
                            if True
                             then return ()
                             else throwErrorPackratM "True" "not match: " ["dv_hsExpTpl"] d590_531
                            d592_533 <- get
                            xx591_534 <- dvCharsM
                            case xx591_534 of
                                ']' -> return ()
                                _ -> throwErrorPackratM "']'" "not match pattern: " ["dvChars"] d592_533
                            let ']' = xx591_534
                            return ()
                            if True
                             then return ()
                             else throwErrorPackratM "True" "not match: " ["dvChars"] d592_533
                            return (listE et),
                         do d594_535 <- get
                            xx593_536 <- dv_variableM
                            let v = xx593_536
                            if True
                             then return ()
                             else throwErrorPackratM "True" "not match: " ["dv_variable"] d594_535
                            return (varE (mkName v)),
                         do d596_537 <- get
                            xx595_538 <- dv_typM
                            let t = xx595_538
                            if True
                             then return ()
                             else throwErrorPackratM "True" "not match: " ["dv_typ"] d596_537
                            return (conE (mkName t)),
                         do d598_539 <- get
                            xx597_540 <- dv_integerM
                            let i = xx597_540
                            if True
                             then return ()
                             else throwErrorPackratM "True" "not match: " ["dv_integer"] d598_539
                            d600_541 <- get
                            _ <- dv_spacesM
                            if True
                             then return ()
                             else throwErrorPackratM "True" "not match: " ["dv_spaces"] d600_541
                            return (litE (integerL i)),
                         do d602_542 <- get
                            xx601_543 <- dvCharsM
                            case xx601_543 of
                                '\'' -> return ()
                                _ -> throwErrorPackratM "'\\''" "not match pattern: " ["dvChars"] d602_542
                            let '\'' = xx601_543
                            return ()
                            if True
                             then return ()
                             else throwErrorPackratM "True" "not match: " ["dvChars"] d602_542
                            d604_544 <- get
                            xx603_545 <- dv_charLitM
                            let c = xx603_545
                            if True
                             then return ()
                             else throwErrorPackratM "True" "not match: " ["dv_charLit"] d604_544
                            d606_546 <- get
                            xx605_547 <- dvCharsM
                            case xx605_547 of
                                '\'' -> return ()
                                _ -> throwErrorPackratM "'\\''" "not match pattern: " ["dvChars"] d606_546
                            let '\'' = xx605_547
                            return ()
                            if True
                             then return ()
                             else throwErrorPackratM "True" "not match: " ["dvChars"] d606_546
                            return (litE (charL c)),
                         do d608_548 <- get
                            xx607_549 <- dvCharsM
                            case xx607_549 of
                                '"' -> return ()
                                _ -> throwErrorPackratM "'\"'" "not match pattern: " ["dvChars"] d608_548
                            let '"' = xx607_549
                            return ()
                            if True
                             then return ()
                             else throwErrorPackratM "True" "not match: " ["dvChars"] d608_548
                            d610_550 <- get
                            xx609_551 <- dv_stringLitM
                            let s = xx609_551
                            if True
                             then return ()
                             else throwErrorPackratM "True" "not match: " ["dv_stringLit"] d610_550
                            d612_552 <- get
                            xx611_553 <- dvCharsM
                            case xx611_553 of
                                '"' -> return ()
                                _ -> throwErrorPackratM "'\"'" "not match pattern: " ["dvChars"] d612_552
                            let '"' = xx611_553
                            return ()
                            if True
                             then return ()
                             else throwErrorPackratM "True" "not match: " ["dvChars"] d612_552
                            return (litE (stringL s)),
                         do d614_554 <- get
                            xx613_555 <- dvCharsM
                            case xx613_555 of
                                '-' -> return ()
                                _ -> throwErrorPackratM "'-'" "not match pattern: " ["dvChars"] d614_554
                            let '-' = xx613_555
                            return ()
                            if True
                             then return ()
                             else throwErrorPackratM "True" "not match: " ["dvChars"] d614_554
                            d616_556 <- get
                            _ <- dv_spacesM
                            if True
                             then return ()
                             else throwErrorPackratM "True" "not match: " ["dv_spaces"] d616_556
                            d618_557 <- get
                            xx617_558 <- dv_hsExp1M
                            let e = xx617_558
                            if True
                             then return ()
                             else throwErrorPackratM "True" "not match: " ["dv_hsExp1"] d618_557
                            return (appE (varE $ mkName "negate") e)]
p_hsExpTpl = foldl1 mplus [do d620_559 <- get
                              xx619_560 <- dv_hsExpLamM
                              let e = xx619_560
                              if True
                               then return ()
                               else throwErrorPackratM "True" "not match: " ["dv_hsExpLam"] d620_559
                              d622_561 <- get
                              _ <- dv_spacesM
                              if True
                               then return ()
                               else throwErrorPackratM "True" "not match: " ["dv_spaces"] d622_561
                              d624_562 <- get
                              xx623_563 <- dvCharsM
                              let c = xx623_563
                              if isComma c
                               then return ()
                               else throwErrorPackratM "isComma c" "not match: " ["dvChars"] d624_562
                              d626_564 <- get
                              _ <- dv_spacesM
                              if True
                               then return ()
                               else throwErrorPackratM "True" "not match: " ["dv_spaces"] d626_564
                              d628_565 <- get
                              xx627_566 <- dv_hsExpTplM
                              let et = xx627_566
                              if True
                               then return ()
                               else throwErrorPackratM "True" "not match: " ["dv_hsExpTpl"] d628_565
                              return (cons e et),
                           do d630_567 <- get
                              xx629_568 <- dv_hsExpLamM
                              let e = xx629_568
                              if True
                               then return ()
                               else throwErrorPackratM "True" "not match: " ["dv_hsExpLam"] d630_567
                              return (cons e emp),
                           do return emp]
p_hsTypeArr = foldl1 mplus [do d632_569 <- get
                               xx631_570 <- dv_hsTypeM
                               let l = xx631_570
                               if True
                                then return ()
                                else throwErrorPackratM "True" "not match: " ["dv_hsType"] d632_569
                               d634_571 <- get
                               xx633_572 <- dvCharsM
                               case xx633_572 of
                                   '-' -> return ()
                                   _ -> throwErrorPackratM "'-'" "not match pattern: " ["dvChars"] d634_571
                               let '-' = xx633_572
                               return ()
                               if True
                                then return ()
                                else throwErrorPackratM "True" "not match: " ["dvChars"] d634_571
                               d636_573 <- get
                               xx635_574 <- dvCharsM
                               let c = xx635_574
                               if isGt c
                                then return ()
                                else throwErrorPackratM "isGt c" "not match: " ["dvChars"] d636_573
                               d638_575 <- get
                               _ <- dv_spacesM
                               if True
                                then return ()
                                else throwErrorPackratM "True" "not match: " ["dv_spaces"] d638_575
                               d640_576 <- get
                               xx639_577 <- dv_hsTypeArrM
                               let r = xx639_577
                               if True
                                then return ()
                                else throwErrorPackratM "True" "not match: " ["dv_hsTypeArr"] d640_576
                               return (appT (appT arrowT (getTyp l)) r),
                            do d642_578 <- get
                               xx641_579 <- dv_hsTypeM
                               let t = xx641_579
                               if True
                                then return ()
                                else throwErrorPackratM "True" "not match: " ["dv_hsType"] d642_578
                               return (getTyp t)]
p_hsType = foldl1 mplus [do d644_580 <- get
                            xx643_581 <- dv_hsType1M
                            let t = xx643_581
                            if True
                             then return ()
                             else throwErrorPackratM "True" "not match: " ["dv_hsType1"] d644_580
                            d646_582 <- get
                            xx645_583 <- dv_hsTypeM
                            let ts = xx645_583
                            if True
                             then return ()
                             else throwErrorPackratM "True" "not match: " ["dv_hsType"] d646_582
                            return (applyTyp (toTyp t) ts),
                         do d648_584 <- get
                            xx647_585 <- dv_hsType1M
                            let t = xx647_585
                            if True
                             then return ()
                             else throwErrorPackratM "True" "not match: " ["dv_hsType1"] d648_584
                            return (toTyp t)]
p_hsType1 = foldl1 mplus [do d650_586 <- get
                             xx649_587 <- dvCharsM
                             case xx649_587 of
                                 '[' -> return ()
                                 _ -> throwErrorPackratM "'['" "not match pattern: " ["dvChars"] d650_586
                             let '[' = xx649_587
                             return ()
                             if True
                              then return ()
                              else throwErrorPackratM "True" "not match: " ["dvChars"] d650_586
                             d652_588 <- get
                             xx651_589 <- dvCharsM
                             case xx651_589 of
                                 ']' -> return ()
                                 _ -> throwErrorPackratM "']'" "not match pattern: " ["dvChars"] d652_588
                             let ']' = xx651_589
                             return ()
                             if True
                              then return ()
                              else throwErrorPackratM "True" "not match: " ["dvChars"] d652_588
                             d654_590 <- get
                             _ <- dv_spacesM
                             if True
                              then return ()
                              else throwErrorPackratM "True" "not match: " ["dv_spaces"] d654_590
                             return listT,
                          do d656_591 <- get
                             xx655_592 <- dvCharsM
                             case xx655_592 of
                                 '[' -> return ()
                                 _ -> throwErrorPackratM "'['" "not match pattern: " ["dvChars"] d656_591
                             let '[' = xx655_592
                             return ()
                             if True
                              then return ()
                              else throwErrorPackratM "True" "not match: " ["dvChars"] d656_591
                             d658_593 <- get
                             xx657_594 <- dv_hsTypeArrM
                             let t = xx657_594
                             if True
                              then return ()
                              else throwErrorPackratM "True" "not match: " ["dv_hsTypeArr"] d658_593
                             d660_595 <- get
                             xx659_596 <- dvCharsM
                             case xx659_596 of
                                 ']' -> return ()
                                 _ -> throwErrorPackratM "']'" "not match pattern: " ["dvChars"] d660_595
                             let ']' = xx659_596
                             return ()
                             if True
                              then return ()
                              else throwErrorPackratM "True" "not match: " ["dvChars"] d660_595
                             d662_597 <- get
                             _ <- dv_spacesM
                             if True
                              then return ()
                              else throwErrorPackratM "True" "not match: " ["dv_spaces"] d662_597
                             return (appT listT t),
                          do d664_598 <- get
                             xx663_599 <- dvCharsM
                             case xx663_599 of
                                 '(' -> return ()
                                 _ -> throwErrorPackratM "'('" "not match pattern: " ["dvChars"] d664_598
                             let '(' = xx663_599
                             return ()
                             if True
                              then return ()
                              else throwErrorPackratM "True" "not match: " ["dvChars"] d664_598
                             d666_600 <- get
                             _ <- dv_spacesM
                             if True
                              then return ()
                              else throwErrorPackratM "True" "not match: " ["dv_spaces"] d666_600
                             d668_601 <- get
                             xx667_602 <- dv_hsTypeTplM
                             let tt = xx667_602
                             if True
                              then return ()
                              else throwErrorPackratM "True" "not match: " ["dv_hsTypeTpl"] d668_601
                             d670_603 <- get
                             xx669_604 <- dvCharsM
                             case xx669_604 of
                                 ')' -> return ()
                                 _ -> throwErrorPackratM "')'" "not match pattern: " ["dvChars"] d670_603
                             let ')' = xx669_604
                             return ()
                             if True
                              then return ()
                              else throwErrorPackratM "True" "not match: " ["dvChars"] d670_603
                             return (tupT tt),
                          do d672_605 <- get
                             xx671_606 <- dv_typTokenM
                             let t = xx671_606
                             if True
                              then return ()
                              else throwErrorPackratM "True" "not match: " ["dv_typToken"] d672_605
                             return (conT (mkName t)),
                          do d674_607 <- get
                             xx673_608 <- dvCharsM
                             case xx673_608 of
                                 '(' -> return ()
                                 _ -> throwErrorPackratM "'('" "not match pattern: " ["dvChars"] d674_607
                             let '(' = xx673_608
                             return ()
                             if True
                              then return ()
                              else throwErrorPackratM "True" "not match: " ["dvChars"] d674_607
                             d676_609 <- get
                             xx675_610 <- dvCharsM
                             case xx675_610 of
                                 '-' -> return ()
                                 _ -> throwErrorPackratM "'-'" "not match pattern: " ["dvChars"] d676_609
                             let '-' = xx675_610
                             return ()
                             if True
                              then return ()
                              else throwErrorPackratM "True" "not match: " ["dvChars"] d676_609
                             d678_611 <- get
                             xx677_612 <- dvCharsM
                             let c = xx677_612
                             if isGt c
                              then return ()
                              else throwErrorPackratM "isGt c" "not match: " ["dvChars"] d678_611
                             d680_613 <- get
                             xx679_614 <- dvCharsM
                             case xx679_614 of
                                 ')' -> return ()
                                 _ -> throwErrorPackratM "')'" "not match pattern: " ["dvChars"] d680_613
                             let ')' = xx679_614
                             return ()
                             if True
                              then return ()
                              else throwErrorPackratM "True" "not match: " ["dvChars"] d680_613
                             d682_615 <- get
                             _ <- dv_spacesM
                             if True
                              then return ()
                              else throwErrorPackratM "True" "not match: " ["dv_spaces"] d682_615
                             return arrowT]
p_hsTypeTpl = foldl1 mplus [do d684_616 <- get
                               xx683_617 <- dv_hsTypeArrM
                               let t = xx683_617
                               if True
                                then return ()
                                else throwErrorPackratM "True" "not match: " ["dv_hsTypeArr"] d684_616
                               d686_618 <- get
                               xx685_619 <- dvCharsM
                               let c = xx685_619
                               if isComma c
                                then return ()
                                else throwErrorPackratM "isComma c" "not match: " ["dvChars"] d686_618
                               d688_620 <- get
                               _ <- dv_spacesM
                               if True
                                then return ()
                                else throwErrorPackratM "True" "not match: " ["dv_spaces"] d688_620
                               d690_621 <- get
                               xx689_622 <- dv_hsTypeTplM
                               let tt = xx689_622
                               if True
                                then return ()
                                else throwErrorPackratM "True" "not match: " ["dv_hsTypeTpl"] d690_621
                               return (cons t tt),
                            do d692_623 <- get
                               xx691_624 <- dv_hsTypeArrM
                               let t = xx691_624
                               if True
                                then return ()
                                else throwErrorPackratM "True" "not match: " ["dv_hsTypeArr"] d692_623
                               return (cons t emp),
                            do return emp]
p_typ = foldl1 mplus [do d694_625 <- get
                         xx693_626 <- dv_upperM
                         let u = xx693_626
                         if True
                          then return ()
                          else throwErrorPackratM "True" "not match: " ["dv_upper"] d694_625
                         d696_627 <- get
                         xx695_628 <- dv_tvtailM
                         let t = xx695_628
                         if True
                          then return ()
                          else throwErrorPackratM "True" "not match: " ["dv_tvtail"] d696_627
                         return (cons u t)]
p_variable = foldl1 mplus [do d698_629 <- get
                              xx697_630 <- dv_lowerM
                              let l = xx697_630
                              if True
                               then return ()
                               else throwErrorPackratM "True" "not match: " ["dv_lower"] d698_629
                              d700_631 <- get
                              xx699_632 <- dv_tvtailM
                              let t = xx699_632
                              if True
                               then return ()
                               else throwErrorPackratM "True" "not match: " ["dv_tvtail"] d700_631
                              return (cons l t)]
p_tvtail = foldl1 mplus [do d702_633 <- get
                            xx701_634 <- dv_alphaM
                            let a = xx701_634
                            if True
                             then return ()
                             else throwErrorPackratM "True" "not match: " ["dv_alpha"] d702_633
                            d704_635 <- get
                            xx703_636 <- dv_tvtailM
                            let t = xx703_636
                            if True
                             then return ()
                             else throwErrorPackratM "True" "not match: " ["dv_tvtail"] d704_635
                            return (cons a t),
                         do return emp]
p_integer = foldl1 mplus [do d706_637 <- get
                             xx705_638 <- dv_digitM
                             let dh = xx705_638
                             if True
                              then return ()
                              else throwErrorPackratM "True" "not match: " ["dv_digit"] d706_637
                             d708_639 <- get
                             xx707_640 <- list (foldl1 mplus [do d710_641 <- get
                                                                 xx709_642 <- dv_digitM
                                                                 let d = xx709_642
                                                                 if True
                                                                  then return ()
                                                                  else throwErrorPackratM "True" "not match: " ["dv_digit"] d710_641
                                                                 return d])
                             let ds = xx707_640
                             if True
                              then return ()
                              else throwErrorPackratM "True" "not match: " ["dv_digit"] d708_639
                             return (read (cons dh ds))]
p_alpha = foldl1 mplus [do d712_643 <- get
                           xx711_644 <- dv_upperM
                           let u = xx711_644
                           if True
                            then return ()
                            else throwErrorPackratM "True" "not match: " ["dv_upper"] d712_643
                           return u,
                        do d714_645 <- get
                           xx713_646 <- dv_lowerM
                           let l = xx713_646
                           if True
                            then return ()
                            else throwErrorPackratM "True" "not match: " ["dv_lower"] d714_645
                           return l,
                        do d716_647 <- get
                           xx715_648 <- dv_digitM
                           let d = xx715_648
                           if True
                            then return ()
                            else throwErrorPackratM "True" "not match: " ["dv_digit"] d716_647
                           return d,
                        do d718_649 <- get
                           xx717_650 <- dvCharsM
                           case xx717_650 of
                               '\'' -> return ()
                               _ -> throwErrorPackratM "'\\''" "not match pattern: " ["dvChars"] d718_649
                           let '\'' = xx717_650
                           return ()
                           if True
                            then return ()
                            else throwErrorPackratM "True" "not match: " ["dvChars"] d718_649
                           return '\'']
p_upper = foldl1 mplus [do d720_651 <- get
                           xx719_652 <- dvCharsM
                           let u = xx719_652
                           if isUpper u
                            then return ()
                            else throwErrorPackratM "isUpper u" "not match: " ["dvChars"] d720_651
                           return u]
p_lower = foldl1 mplus [do d722_653 <- get
                           xx721_654 <- dvCharsM
                           let l = xx721_654
                           if isLowerU l
                            then return ()
                            else throwErrorPackratM "isLowerU l" "not match: " ["dvChars"] d722_653
                           return l]
p_digit = foldl1 mplus [do d724_655 <- get
                           xx723_656 <- dvCharsM
                           let d = xx723_656
                           if isDigit d
                            then return ()
                            else throwErrorPackratM "isDigit d" "not match: " ["dvChars"] d724_655
                           return d]
p_spaces = foldl1 mplus [do d726_657 <- get
                            _ <- dv_spaceM
                            if True
                             then return ()
                             else throwErrorPackratM "True" "not match: " ["dv_space"] d726_657
                            d728_658 <- get
                            _ <- dv_spacesM
                            if True
                             then return ()
                             else throwErrorPackratM "True" "not match: " ["dv_spaces"] d728_658
                            return (),
                         do return ()]
p_space = foldl1 mplus [do d730_659 <- get
                           xx729_660 <- dvCharsM
                           let s = xx729_660
                           if isSpace s
                            then return ()
                            else throwErrorPackratM "isSpace s" "not match: " ["dvChars"] d730_659
                           return (),
                        do d732_661 <- get
                           xx731_662 <- dvCharsM
                           case xx731_662 of
                               '-' -> return ()
                               _ -> throwErrorPackratM "'-'" "not match pattern: " ["dvChars"] d732_661
                           let '-' = xx731_662
                           return ()
                           if True
                            then return ()
                            else throwErrorPackratM "True" "not match: " ["dvChars"] d732_661
                           d734_663 <- get
                           xx733_664 <- dvCharsM
                           case xx733_664 of
                               '-' -> return ()
                               _ -> throwErrorPackratM "'-'" "not match pattern: " ["dvChars"] d734_663
                           let '-' = xx733_664
                           return ()
                           if True
                            then return ()
                            else throwErrorPackratM "True" "not match: " ["dvChars"] d734_663
                           d736_665 <- get
                           _ <- dv_notNLStringM
                           if True
                            then return ()
                            else throwErrorPackratM "True" "not match: " ["dv_notNLString"] d736_665
                           d738_666 <- get
                           _ <- dv_nlM
                           if True
                            then return ()
                            else throwErrorPackratM "True" "not match: " ["dv_nl"] d738_666
                           return (),
                        do d740_667 <- get
                           _ <- dv_commentM
                           if True
                            then return ()
                            else throwErrorPackratM "True" "not match: " ["dv_comment"] d740_667
                           return ()]
p_notNLString = foldl1 mplus [do ddd741_668 <- get
                                 flipMaybe "_:nl[True]" ddd741_668 ["dv_nl"] (do d743_669 <- get
                                                                                 _ <- dv_nlM
                                                                                 if True
                                                                                  then return ()
                                                                                  else throwErrorPackratM "True" "not match: " ["dv_nl"] d743_669)
                                 put ddd741_668
                                 d745_670 <- get
                                 xx744_671 <- dvCharsM
                                 let c = xx744_671
                                 if True
                                  then return ()
                                  else throwErrorPackratM "True" "not match: " ["dvChars"] d745_670
                                 d747_672 <- get
                                 xx746_673 <- dv_notNLStringM
                                 let s = xx746_673
                                 if True
                                  then return ()
                                  else throwErrorPackratM "True" "not match: " ["dv_notNLString"] d747_672
                                 return (cons c s),
                              do return emp]
p_nl = foldl1 mplus [do d749_674 <- get
                        xx748_675 <- dvCharsM
                        case xx748_675 of
                            '\n' -> return ()
                            _ -> throwErrorPackratM "'\\n'" "not match pattern: " ["dvChars"] d749_674
                        let '\n' = xx748_675
                        return ()
                        if True
                         then return ()
                         else throwErrorPackratM "True" "not match: " ["dvChars"] d749_674
                        return ()]
p_comment = foldl1 mplus [do d751_676 <- get
                             xx750_677 <- dvCharsM
                             case xx750_677 of
                                 '{' -> return ()
                                 _ -> throwErrorPackratM "'{'" "not match pattern: " ["dvChars"] d751_676
                             let '{' = xx750_677
                             return ()
                             if True
                              then return ()
                              else throwErrorPackratM "True" "not match: " ["dvChars"] d751_676
                             d753_678 <- get
                             xx752_679 <- dvCharsM
                             case xx752_679 of
                                 '-' -> return ()
                                 _ -> throwErrorPackratM "'-'" "not match pattern: " ["dvChars"] d753_678
                             let '-' = xx752_679
                             return ()
                             if True
                              then return ()
                              else throwErrorPackratM "True" "not match: " ["dvChars"] d753_678
                             ddd754_680 <- get
                             flipMaybe "'#':[True]" ddd754_680 ["dvChars"] (do d756_681 <- get
                                                                               xx755_682 <- dvCharsM
                                                                               case xx755_682 of
                                                                                   '#' -> return ()
                                                                                   _ -> throwErrorPackratM "'#'" "not match pattern: " ["dvChars"] d756_681
                                                                               let '#' = xx755_682
                                                                               return ()
                                                                               if True
                                                                                then return ()
                                                                                else throwErrorPackratM "True" "not match: " ["dvChars"] d756_681)
                             put ddd754_680
                             d758_683 <- get
                             _ <- dv_commentsM
                             if True
                              then return ()
                              else throwErrorPackratM "True" "not match: " ["dv_comments"] d758_683
                             d760_684 <- get
                             _ <- dv_comEndM
                             if True
                              then return ()
                              else throwErrorPackratM "True" "not match: " ["dv_comEnd"] d760_684
                             return ()]
p_comments = foldl1 mplus [do d762_685 <- get
                              _ <- dv_notComStrM
                              if True
                               then return ()
                               else throwErrorPackratM "True" "not match: " ["dv_notComStr"] d762_685
                              d764_686 <- get
                              _ <- dv_commentM
                              if True
                               then return ()
                               else throwErrorPackratM "True" "not match: " ["dv_comment"] d764_686
                              d766_687 <- get
                              _ <- dv_commentsM
                              if True
                               then return ()
                               else throwErrorPackratM "True" "not match: " ["dv_comments"] d766_687
                              return (),
                           do d768_688 <- get
                              _ <- dv_notComStrM
                              if True
                               then return ()
                               else throwErrorPackratM "True" "not match: " ["dv_notComStr"] d768_688
                              return ()]
p_notComStr = foldl1 mplus [do ddd769_689 <- get
                               flipMaybe "_:comment[True]" ddd769_689 ["dv_comment"] (do d771_690 <- get
                                                                                         _ <- dv_commentM
                                                                                         if True
                                                                                          then return ()
                                                                                          else throwErrorPackratM "True" "not match: " ["dv_comment"] d771_690)
                               put ddd769_689
                               ddd772_691 <- get
                               flipMaybe "_:comEnd[True]" ddd772_691 ["dv_comEnd"] (do d774_692 <- get
                                                                                       _ <- dv_comEndM
                                                                                       if True
                                                                                        then return ()
                                                                                        else throwErrorPackratM "True" "not match: " ["dv_comEnd"] d774_692)
                               put ddd772_691
                               d776_693 <- get
                               _ <- dvCharsM
                               if True
                                then return ()
                                else throwErrorPackratM "True" "not match: " ["dvChars"] d776_693
                               d778_694 <- get
                               _ <- dv_notComStrM
                               if True
                                then return ()
                                else throwErrorPackratM "True" "not match: " ["dv_notComStr"] d778_694
                               return (),
                            do return ()]
p_comEnd = foldl1 mplus [do d780_695 <- get
                            xx779_696 <- dvCharsM
                            case xx779_696 of
                                '-' -> return ()
                                _ -> throwErrorPackratM "'-'" "not match pattern: " ["dvChars"] d780_695
                            let '-' = xx779_696
                            return ()
                            if True
                             then return ()
                             else throwErrorPackratM "True" "not match: " ["dvChars"] d780_695
                            d782_697 <- get
                            xx781_698 <- dvCharsM
                            case xx781_698 of
                                '}' -> return ()
                                _ -> throwErrorPackratM "'}'" "not match pattern: " ["dvChars"] d782_697
                            let '}' = xx781_698
                            return ()
                            if True
                             then return ()
                             else throwErrorPackratM "True" "not match: " ["dvChars"] d782_697
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