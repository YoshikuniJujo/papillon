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
          where d = Derivs pegFile pragma pragmaStr pragmaEnd moduleDec moduleDecStr whr preImpPap prePeg afterPeg importPapillon varToken typToken pap peg sourceType peg_ definition selection expressionHs expression nameLeaf_ nameLeaf comForErr leaf patOp pat pat1 patList opConName charLit stringLit dq pats readFromLs readFrom test hsExpLam hsExpTyp hsExpOp hsOp opTail hsExp hsExp1 hsExpTpl hsTypeArr hsType hsType1 hsTypeTpl typ variable tvtail integer alpha upper lower digit spaces space notNLString nl comment comments notComStr comEnd char pos___hoge
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
                               xx252_221 <- dv_nameLeafM
                               let nl = xx252_221
                               if True
                                then return ()
                                else throwErrorPackratM "True" "not match: " ["dv_nameLeaf"] d253_220
                               return (notAfter nl),
                            do d255_222 <- get
                               xx254_223 <- dvCharsM
                               let c = xx254_223
                               if isAmp c
                                then return ()
                                else throwErrorPackratM "isAmp c" "not match: " ["dvChars"] d255_222
                               d257_224 <- get
                               xx256_225 <- dv_nameLeafM
                               let nl = xx256_225
                               if True
                                then return ()
                                else throwErrorPackratM "True" "not match: " ["dv_nameLeaf"] d257_224
                               return (After nl),
                            do d259_226 <- get
                               xx258_227 <- dv_nameLeafM
                               let nl = xx258_227
                               if True
                                then return ()
                                else throwErrorPackratM "True" "not match: " ["dv_nameLeaf"] d259_226
                               return (here nl)]
p_nameLeaf = foldl1 mplus [do d261_228 <- get
                              xx260_229 <- dv_pat1M
                              let n = xx260_229
                              if True
                               then return ()
                               else throwErrorPackratM "True" "not match: " ["dv_pat1"] d261_228
                              d263_230 <- get
                              _ <- dv_spacesM
                              if True
                               then return ()
                               else throwErrorPackratM "True" "not match: " ["dv_spaces"] d263_230
                              d265_231 <- get
                              xx264_232 <- papOptional dv_comForErrM
                              let com = xx264_232
                              if True
                               then return ()
                               else throwErrorPackratM "True" "not match: " ["dv_comForErr"] d265_231
                              d267_233 <- get
                              xx266_234 <- dvCharsM
                              case xx266_234 of
                                  ':' -> return ()
                                  _ -> throwErrorPackratM "':'" "not match pattern: " ["dvChars"] d267_233
                              let ':' = xx266_234
                              return ()
                              if True
                               then return ()
                               else throwErrorPackratM "True" "not match: " ["dvChars"] d267_233
                              d269_235 <- get
                              xx268_236 <- dv_leafM
                              let (rf, p) = xx268_236
                              if True
                               then return ()
                               else throwErrorPackratM "True" "not match: " ["dv_leaf"] d269_235
                              return (NameLeaf (n, maybe "" id com) rf p),
                           do d271_237 <- get
                              xx270_238 <- dv_pat1M
                              let n = xx270_238
                              if True
                               then return ()
                               else throwErrorPackratM "True" "not match: " ["dv_pat1"] d271_237
                              d273_239 <- get
                              _ <- dv_spacesM
                              if True
                               then return ()
                               else throwErrorPackratM "True" "not match: " ["dv_spaces"] d273_239
                              d275_240 <- get
                              xx274_241 <- papOptional dv_comForErrM
                              let com = xx274_241
                              if True
                               then return ()
                               else throwErrorPackratM "True" "not match: " ["dv_comForErr"] d275_240
                              return (NameLeaf (n,
                                                maybe "" id com) FromToken (conE $ mkName "True",
                                                                            ""))]
p_comForErr = foldl1 mplus [do d277_242 <- get
                               xx276_243 <- dvCharsM
                               case xx276_243 of
                                   '{' -> return ()
                                   _ -> throwErrorPackratM "'{'" "not match pattern: " ["dvChars"] d277_242
                               let '{' = xx276_243
                               return ()
                               if True
                                then return ()
                                else throwErrorPackratM "True" "not match: " ["dvChars"] d277_242
                               d279_244 <- get
                               xx278_245 <- dvCharsM
                               case xx278_245 of
                                   '-' -> return ()
                                   _ -> throwErrorPackratM "'-'" "not match pattern: " ["dvChars"] d279_244
                               let '-' = xx278_245
                               return ()
                               if True
                                then return ()
                                else throwErrorPackratM "True" "not match: " ["dvChars"] d279_244
                               d281_246 <- get
                               xx280_247 <- dvCharsM
                               case xx280_247 of
                                   '#' -> return ()
                                   _ -> throwErrorPackratM "'#'" "not match pattern: " ["dvChars"] d281_246
                               let '#' = xx280_247
                               return ()
                               if True
                                then return ()
                                else throwErrorPackratM "True" "not match: " ["dvChars"] d281_246
                               d283_248 <- get
                               _ <- dv_spacesM
                               if True
                                then return ()
                                else throwErrorPackratM "True" "not match: " ["dv_spaces"] d283_248
                               d285_249 <- get
                               xx284_250 <- dvCharsM
                               case xx284_250 of
                                   '"' -> return ()
                                   _ -> throwErrorPackratM "'\"'" "not match pattern: " ["dvChars"] d285_249
                               let '"' = xx284_250
                               return ()
                               if True
                                then return ()
                                else throwErrorPackratM "True" "not match: " ["dvChars"] d285_249
                               d287_251 <- get
                               xx286_252 <- dv_stringLitM
                               let s = xx286_252
                               if True
                                then return ()
                                else throwErrorPackratM "True" "not match: " ["dv_stringLit"] d287_251
                               d289_253 <- get
                               xx288_254 <- dvCharsM
                               case xx288_254 of
                                   '"' -> return ()
                                   _ -> throwErrorPackratM "'\"'" "not match pattern: " ["dvChars"] d289_253
                               let '"' = xx288_254
                               return ()
                               if True
                                then return ()
                                else throwErrorPackratM "True" "not match: " ["dvChars"] d289_253
                               d291_255 <- get
                               _ <- dv_spacesM
                               if True
                                then return ()
                                else throwErrorPackratM "True" "not match: " ["dv_spaces"] d291_255
                               d293_256 <- get
                               xx292_257 <- dvCharsM
                               case xx292_257 of
                                   '#' -> return ()
                                   _ -> throwErrorPackratM "'#'" "not match pattern: " ["dvChars"] d293_256
                               let '#' = xx292_257
                               return ()
                               if True
                                then return ()
                                else throwErrorPackratM "True" "not match: " ["dvChars"] d293_256
                               d295_258 <- get
                               xx294_259 <- dvCharsM
                               case xx294_259 of
                                   '-' -> return ()
                                   _ -> throwErrorPackratM "'-'" "not match pattern: " ["dvChars"] d295_258
                               let '-' = xx294_259
                               return ()
                               if True
                                then return ()
                                else throwErrorPackratM "True" "not match: " ["dvChars"] d295_258
                               d297_260 <- get
                               xx296_261 <- dvCharsM
                               case xx296_261 of
                                   '}' -> return ()
                                   _ -> throwErrorPackratM "'}'" "not match pattern: " ["dvChars"] d297_260
                               let '}' = xx296_261
                               return ()
                               if True
                                then return ()
                                else throwErrorPackratM "True" "not match: " ["dvChars"] d297_260
                               d299_262 <- get
                               _ <- dv_spacesM
                               if True
                                then return ()
                                else throwErrorPackratM "True" "not match: " ["dv_spaces"] d299_262
                               return s]
p_leaf = foldl1 mplus [do d301_263 <- get
                          xx300_264 <- dv_readFromLsM
                          let rf = xx300_264
                          if True
                           then return ()
                           else throwErrorPackratM "True" "not match: " ["dv_readFromLs"] d301_263
                          d303_265 <- get
                          xx302_266 <- dv_testM
                          let t = xx302_266
                          if True
                           then return ()
                           else throwErrorPackratM "True" "not match: " ["dv_test"] d303_265
                          return (rf, t),
                       do d305_267 <- get
                          xx304_268 <- dv_readFromLsM
                          let rf = xx304_268
                          if True
                           then return ()
                           else throwErrorPackratM "True" "not match: " ["dv_readFromLs"] d305_267
                          return (rf, (true, "")),
                       do d307_269 <- get
                          xx306_270 <- dv_testM
                          let t = xx306_270
                          if True
                           then return ()
                           else throwErrorPackratM "True" "not match: " ["dv_test"] d307_269
                          return (FromToken, t)]
p_patOp = foldl1 mplus [do d309_271 <- get
                           xx308_272 <- dv_patM
                           let p = xx308_272
                           if True
                            then return ()
                            else throwErrorPackratM "True" "not match: " ["dv_pat"] d309_271
                           d311_273 <- get
                           xx310_274 <- dv_opConNameM
                           let o = xx310_274
                           if True
                            then return ()
                            else throwErrorPackratM "True" "not match: " ["dv_opConName"] d311_273
                           d313_275 <- get
                           xx312_276 <- dv_patOpM
                           let po = xx312_276
                           if True
                            then return ()
                            else throwErrorPackratM "True" "not match: " ["dv_patOp"] d313_275
                           return (uInfixP p o po),
                        do d315_277 <- get
                           xx314_278 <- dv_patM
                           let p = xx314_278
                           if True
                            then return ()
                            else throwErrorPackratM "True" "not match: " ["dv_pat"] d315_277
                           d317_279 <- get
                           _ <- dv_spacesM
                           if True
                            then return ()
                            else throwErrorPackratM "True" "not match: " ["dv_spaces"] d317_279
                           d319_280 <- get
                           xx318_281 <- dvCharsM
                           let q = xx318_281
                           if isBQ q
                            then return ()
                            else throwErrorPackratM "isBQ q" "not match: " ["dvChars"] d319_280
                           d321_282 <- get
                           xx320_283 <- dv_typM
                           let t = xx320_283
                           if True
                            then return ()
                            else throwErrorPackratM "True" "not match: " ["dv_typ"] d321_282
                           d323_284 <- get
                           xx322_285 <- dvCharsM
                           let q_ = xx322_285
                           if isBQ q_
                            then return ()
                            else throwErrorPackratM "isBQ q_" "not match: " ["dvChars"] d323_284
                           d325_286 <- get
                           _ <- dv_spacesM
                           if True
                            then return ()
                            else throwErrorPackratM "True" "not match: " ["dv_spaces"] d325_286
                           d327_287 <- get
                           xx326_288 <- dv_patOpM
                           let po = xx326_288
                           if True
                            then return ()
                            else throwErrorPackratM "True" "not match: " ["dv_patOp"] d327_287
                           return (uInfixP p (mkName t) po),
                        do d329_289 <- get
                           xx328_290 <- dv_patM
                           let p = xx328_290
                           if True
                            then return ()
                            else throwErrorPackratM "True" "not match: " ["dv_pat"] d329_289
                           return p]
p_pat = foldl1 mplus [do d331_291 <- get
                         xx330_292 <- dv_typM
                         let t = xx330_292
                         if True
                          then return ()
                          else throwErrorPackratM "True" "not match: " ["dv_typ"] d331_291
                         d333_293 <- get
                         _ <- dv_spacesM
                         if True
                          then return ()
                          else throwErrorPackratM "True" "not match: " ["dv_spaces"] d333_293
                         d335_294 <- get
                         xx334_295 <- dv_patsM
                         let ps = xx334_295
                         if True
                          then return ()
                          else throwErrorPackratM "True" "not match: " ["dv_pats"] d335_294
                         return (conToPatQ t ps),
                      do d337_296 <- get
                         xx336_297 <- dvCharsM
                         case xx336_297 of
                             '(' -> return ()
                             _ -> throwErrorPackratM "'('" "not match pattern: " ["dvChars"] d337_296
                         let '(' = xx336_297
                         return ()
                         if True
                          then return ()
                          else throwErrorPackratM "True" "not match: " ["dvChars"] d337_296
                         d339_298 <- get
                         xx338_299 <- dv_opConNameM
                         let o = xx338_299
                         if True
                          then return ()
                          else throwErrorPackratM "True" "not match: " ["dv_opConName"] d339_298
                         d341_300 <- get
                         xx340_301 <- dvCharsM
                         case xx340_301 of
                             ')' -> return ()
                             _ -> throwErrorPackratM "')'" "not match pattern: " ["dvChars"] d341_300
                         let ')' = xx340_301
                         return ()
                         if True
                          then return ()
                          else throwErrorPackratM "True" "not match: " ["dvChars"] d341_300
                         d343_302 <- get
                         _ <- dv_spacesM
                         if True
                          then return ()
                          else throwErrorPackratM "True" "not match: " ["dv_spaces"] d343_302
                         d345_303 <- get
                         xx344_304 <- dv_patsM
                         let ps = xx344_304
                         if True
                          then return ()
                          else throwErrorPackratM "True" "not match: " ["dv_pats"] d345_303
                         return (conP o ps),
                      do d347_305 <- get
                         xx346_306 <- dv_pat1M
                         let p = xx346_306
                         if True
                          then return ()
                          else throwErrorPackratM "True" "not match: " ["dv_pat1"] d347_305
                         return p]
p_pat1 = foldl1 mplus [do d349_307 <- get
                          xx348_308 <- dv_typM
                          let t = xx348_308
                          if True
                           then return ()
                           else throwErrorPackratM "True" "not match: " ["dv_typ"] d349_307
                          return (conToPatQ t emp),
                       do d351_309 <- get
                          xx350_310 <- dv_variableM
                          case xx350_310 of
                              "_" -> return ()
                              _ -> throwErrorPackratM "\"_\"" "not match pattern: " ["dv_variable"] d351_309
                          let "_" = xx350_310
                          return ()
                          if True
                           then return ()
                           else throwErrorPackratM "True" "not match: " ["dv_variable"] d351_309
                          return wildP,
                       do d353_311 <- get
                          xx352_312 <- dv_variableM
                          let n = xx352_312
                          if True
                           then return ()
                           else throwErrorPackratM "True" "not match: " ["dv_variable"] d353_311
                          return (strToPatQ n),
                       do d355_313 <- get
                          xx354_314 <- dv_integerM
                          let i = xx354_314
                          if True
                           then return ()
                           else throwErrorPackratM "True" "not match: " ["dv_integer"] d355_313
                          return (litP (integerL i)),
                       do d357_315 <- get
                          xx356_316 <- dvCharsM
                          case xx356_316 of
                              '-' -> return ()
                              _ -> throwErrorPackratM "'-'" "not match pattern: " ["dvChars"] d357_315
                          let '-' = xx356_316
                          return ()
                          if True
                           then return ()
                           else throwErrorPackratM "True" "not match: " ["dvChars"] d357_315
                          d359_317 <- get
                          _ <- dv_spacesM
                          if True
                           then return ()
                           else throwErrorPackratM "True" "not match: " ["dv_spaces"] d359_317
                          d361_318 <- get
                          xx360_319 <- dv_integerM
                          let i = xx360_319
                          if True
                           then return ()
                           else throwErrorPackratM "True" "not match: " ["dv_integer"] d361_318
                          return (litP (integerL $ negate i)),
                       do d363_320 <- get
                          xx362_321 <- dvCharsM
                          case xx362_321 of
                              '\'' -> return ()
                              _ -> throwErrorPackratM "'\\''" "not match pattern: " ["dvChars"] d363_320
                          let '\'' = xx362_321
                          return ()
                          if True
                           then return ()
                           else throwErrorPackratM "True" "not match: " ["dvChars"] d363_320
                          d365_322 <- get
                          xx364_323 <- dv_charLitM
                          let c = xx364_323
                          if True
                           then return ()
                           else throwErrorPackratM "True" "not match: " ["dv_charLit"] d365_322
                          d367_324 <- get
                          xx366_325 <- dvCharsM
                          case xx366_325 of
                              '\'' -> return ()
                              _ -> throwErrorPackratM "'\\''" "not match pattern: " ["dvChars"] d367_324
                          let '\'' = xx366_325
                          return ()
                          if True
                           then return ()
                           else throwErrorPackratM "True" "not match: " ["dvChars"] d367_324
                          return (charP c),
                       do d369_326 <- get
                          xx368_327 <- dvCharsM
                          case xx368_327 of
                              '"' -> return ()
                              _ -> throwErrorPackratM "'\"'" "not match pattern: " ["dvChars"] d369_326
                          let '"' = xx368_327
                          return ()
                          if True
                           then return ()
                           else throwErrorPackratM "True" "not match: " ["dvChars"] d369_326
                          d371_328 <- get
                          xx370_329 <- dv_stringLitM
                          let s = xx370_329
                          if True
                           then return ()
                           else throwErrorPackratM "True" "not match: " ["dv_stringLit"] d371_328
                          d373_330 <- get
                          xx372_331 <- dvCharsM
                          case xx372_331 of
                              '"' -> return ()
                              _ -> throwErrorPackratM "'\"'" "not match pattern: " ["dvChars"] d373_330
                          let '"' = xx372_331
                          return ()
                          if True
                           then return ()
                           else throwErrorPackratM "True" "not match: " ["dvChars"] d373_330
                          return (stringP s),
                       do d375_332 <- get
                          xx374_333 <- dvCharsM
                          case xx374_333 of
                              '(' -> return ()
                              _ -> throwErrorPackratM "'('" "not match pattern: " ["dvChars"] d375_332
                          let '(' = xx374_333
                          return ()
                          if True
                           then return ()
                           else throwErrorPackratM "True" "not match: " ["dvChars"] d375_332
                          d377_334 <- get
                          xx376_335 <- dv_patListM
                          let p = xx376_335
                          if True
                           then return ()
                           else throwErrorPackratM "True" "not match: " ["dv_patList"] d377_334
                          d379_336 <- get
                          xx378_337 <- dvCharsM
                          case xx378_337 of
                              ')' -> return ()
                              _ -> throwErrorPackratM "')'" "not match pattern: " ["dvChars"] d379_336
                          let ')' = xx378_337
                          return ()
                          if True
                           then return ()
                           else throwErrorPackratM "True" "not match: " ["dvChars"] d379_336
                          return (tupP p),
                       do d381_338 <- get
                          xx380_339 <- dvCharsM
                          case xx380_339 of
                              '[' -> return ()
                              _ -> throwErrorPackratM "'['" "not match pattern: " ["dvChars"] d381_338
                          let '[' = xx380_339
                          return ()
                          if True
                           then return ()
                           else throwErrorPackratM "True" "not match: " ["dvChars"] d381_338
                          d383_340 <- get
                          xx382_341 <- dv_patListM
                          let p = xx382_341
                          if True
                           then return ()
                           else throwErrorPackratM "True" "not match: " ["dv_patList"] d383_340
                          d385_342 <- get
                          xx384_343 <- dvCharsM
                          case xx384_343 of
                              ']' -> return ()
                              _ -> throwErrorPackratM "']'" "not match pattern: " ["dvChars"] d385_342
                          let ']' = xx384_343
                          return ()
                          if True
                           then return ()
                           else throwErrorPackratM "True" "not match: " ["dvChars"] d385_342
                          return (listP p)]
p_patList = foldl1 mplus [do d387_344 <- get
                             xx386_345 <- dv_patOpM
                             let p = xx386_345
                             if True
                              then return ()
                              else throwErrorPackratM "True" "not match: " ["dv_patOp"] d387_344
                             d389_346 <- get
                             _ <- dv_spacesM
                             if True
                              then return ()
                              else throwErrorPackratM "True" "not match: " ["dv_spaces"] d389_346
                             d391_347 <- get
                             xx390_348 <- dvCharsM
                             case xx390_348 of
                                 ',' -> return ()
                                 _ -> throwErrorPackratM "','" "not match pattern: " ["dvChars"] d391_347
                             let ',' = xx390_348
                             return ()
                             if True
                              then return ()
                              else throwErrorPackratM "True" "not match: " ["dvChars"] d391_347
                             d393_349 <- get
                             _ <- dv_spacesM
                             if True
                              then return ()
                              else throwErrorPackratM "True" "not match: " ["dv_spaces"] d393_349
                             d395_350 <- get
                             xx394_351 <- dv_patListM
                             let ps = xx394_351
                             if True
                              then return ()
                              else throwErrorPackratM "True" "not match: " ["dv_patList"] d395_350
                             return (p : ps),
                          do d397_352 <- get
                             xx396_353 <- dv_patOpM
                             let p = xx396_353
                             if True
                              then return ()
                              else throwErrorPackratM "True" "not match: " ["dv_patOp"] d397_352
                             return [p],
                          do return []]
p_opConName = foldl1 mplus [do d399_354 <- get
                               xx398_355 <- dvCharsM
                               case xx398_355 of
                                   ':' -> return ()
                                   _ -> throwErrorPackratM "':'" "not match pattern: " ["dvChars"] d399_354
                               let ':' = xx398_355
                               return ()
                               if True
                                then return ()
                                else throwErrorPackratM "True" "not match: " ["dvChars"] d399_354
                               d401_356 <- get
                               xx400_357 <- dv_opTailM
                               let ot = xx400_357
                               if True
                                then return ()
                                else throwErrorPackratM "True" "not match: " ["dv_opTail"] d401_356
                               return (mkName $ colon : ot)]
p_charLit = foldl1 mplus [do d403_358 <- get
                             xx402_359 <- dvCharsM
                             let c = xx402_359
                             if isAlphaNumOt c
                              then return ()
                              else throwErrorPackratM "isAlphaNumOt c" "not match: " ["dvChars"] d403_358
                             return c,
                          do d405_360 <- get
                             xx404_361 <- dvCharsM
                             case xx404_361 of
                                 '\\' -> return ()
                                 _ -> throwErrorPackratM "'\\\\'" "not match pattern: " ["dvChars"] d405_360
                             let '\\' = xx404_361
                             return ()
                             if True
                              then return ()
                              else throwErrorPackratM "True" "not match: " ["dvChars"] d405_360
                             d407_362 <- get
                             xx406_363 <- dvCharsM
                             let c = xx406_363
                             if elemNTs c
                              then return ()
                              else throwErrorPackratM "elemNTs c" "not match: " ["dvChars"] d407_362
                             return (getNTs c)]
p_stringLit = foldl1 mplus [do ddd408_364 <- get
                               flipMaybe "_:dq[True]" ddd408_364 ["dv_dq"] (do d410_365 <- get
                                                                               _ <- dv_dqM
                                                                               if True
                                                                                then return ()
                                                                                else throwErrorPackratM "True" "not match: " ["dv_dq"] d410_365)
                               put ddd408_364
                               d412_366 <- get
                               xx411_367 <- dvCharsM
                               let c = xx411_367
                               if True
                                then return ()
                                else throwErrorPackratM "True" "not match: " ["dvChars"] d412_366
                               d414_368 <- get
                               xx413_369 <- dv_stringLitM
                               let s = xx413_369
                               if True
                                then return ()
                                else throwErrorPackratM "True" "not match: " ["dv_stringLit"] d414_368
                               return (cons c s),
                            do return emp]
p_dq = foldl1 mplus [do d416_370 <- get
                        xx415_371 <- dvCharsM
                        case xx415_371 of
                            '"' -> return ()
                            _ -> throwErrorPackratM "'\"'" "not match pattern: " ["dvChars"] d416_370
                        let '"' = xx415_371
                        return ()
                        if True
                         then return ()
                         else throwErrorPackratM "True" "not match: " ["dvChars"] d416_370
                        return ()]
p_pats = foldl1 mplus [do d418_372 <- get
                          xx417_373 <- dv_patM
                          let p = xx417_373
                          if True
                           then return ()
                           else throwErrorPackratM "True" "not match: " ["dv_pat"] d418_372
                          d420_374 <- get
                          _ <- dv_spacesM
                          if True
                           then return ()
                           else throwErrorPackratM "True" "not match: " ["dv_spaces"] d420_374
                          d422_375 <- get
                          xx421_376 <- dv_patsM
                          let ps = xx421_376
                          if True
                           then return ()
                           else throwErrorPackratM "True" "not match: " ["dv_pats"] d422_375
                          return (cons p ps),
                       do return emp]
p_readFromLs = foldl1 mplus [do d424_377 <- get
                                xx423_378 <- dv_readFromM
                                let rf = xx423_378
                                if True
                                 then return ()
                                 else throwErrorPackratM "True" "not match: " ["dv_readFrom"] d424_377
                                d426_379 <- get
                                xx425_380 <- dvCharsM
                                case xx425_380 of
                                    '*' -> return ()
                                    _ -> throwErrorPackratM "'*'" "not match pattern: " ["dvChars"] d426_379
                                let '*' = xx425_380
                                return ()
                                if True
                                 then return ()
                                 else throwErrorPackratM "True" "not match: " ["dvChars"] d426_379
                                return (FromList rf),
                             do d428_381 <- get
                                xx427_382 <- dv_readFromM
                                let rf = xx427_382
                                if True
                                 then return ()
                                 else throwErrorPackratM "True" "not match: " ["dv_readFrom"] d428_381
                                d430_383 <- get
                                xx429_384 <- dvCharsM
                                case xx429_384 of
                                    '+' -> return ()
                                    _ -> throwErrorPackratM "'+'" "not match pattern: " ["dvChars"] d430_383
                                let '+' = xx429_384
                                return ()
                                if True
                                 then return ()
                                 else throwErrorPackratM "True" "not match: " ["dvChars"] d430_383
                                return (FromList1 rf),
                             do d432_385 <- get
                                xx431_386 <- dv_readFromM
                                let rf = xx431_386
                                if True
                                 then return ()
                                 else throwErrorPackratM "True" "not match: " ["dv_readFrom"] d432_385
                                d434_387 <- get
                                xx433_388 <- dvCharsM
                                case xx433_388 of
                                    '?' -> return ()
                                    _ -> throwErrorPackratM "'?'" "not match pattern: " ["dvChars"] d434_387
                                let '?' = xx433_388
                                return ()
                                if True
                                 then return ()
                                 else throwErrorPackratM "True" "not match: " ["dvChars"] d434_387
                                return (FromOptional rf),
                             do d436_389 <- get
                                xx435_390 <- dv_readFromM
                                let rf = xx435_390
                                if True
                                 then return ()
                                 else throwErrorPackratM "True" "not match: " ["dv_readFrom"] d436_389
                                return rf]
p_readFrom = foldl1 mplus [do d438_391 <- get
                              xx437_392 <- dv_variableM
                              let v = xx437_392
                              if True
                               then return ()
                               else throwErrorPackratM "True" "not match: " ["dv_variable"] d438_391
                              return (FromVariable v),
                           do d440_393 <- get
                              xx439_394 <- dvCharsM
                              case xx439_394 of
                                  '(' -> return ()
                                  _ -> throwErrorPackratM "'('" "not match pattern: " ["dvChars"] d440_393
                              let '(' = xx439_394
                              return ()
                              if True
                               then return ()
                               else throwErrorPackratM "True" "not match: " ["dvChars"] d440_393
                              d442_395 <- get
                              xx441_396 <- dv_selectionM
                              let s = xx441_396
                              if True
                               then return ()
                               else throwErrorPackratM "True" "not match: " ["dv_selection"] d442_395
                              d444_397 <- get
                              xx443_398 <- dvCharsM
                              case xx443_398 of
                                  ')' -> return ()
                                  _ -> throwErrorPackratM "')'" "not match pattern: " ["dvChars"] d444_397
                              let ')' = xx443_398
                              return ()
                              if True
                               then return ()
                               else throwErrorPackratM "True" "not match: " ["dvChars"] d444_397
                              return (FromSelection s)]
p_test = foldl1 mplus [do d446_399 <- get
                          xx445_400 <- dvCharsM
                          case xx445_400 of
                              '[' -> return ()
                              _ -> throwErrorPackratM "'['" "not match pattern: " ["dvChars"] d446_399
                          let '[' = xx445_400
                          return ()
                          if True
                           then return ()
                           else throwErrorPackratM "True" "not match: " ["dvChars"] d446_399
                          d448_401 <- get
                          xx447_402 <- dv_hsExpLamM
                          let h = xx447_402
                          if True
                           then return ()
                           else throwErrorPackratM "True" "not match: " ["dv_hsExpLam"] d448_401
                          d450_403 <- get
                          _ <- dv_spacesM
                          if True
                           then return ()
                           else throwErrorPackratM "True" "not match: " ["dv_spaces"] d450_403
                          d452_404 <- get
                          xx451_405 <- papOptional dv_comForErrM
                          let com = xx451_405
                          if True
                           then return ()
                           else throwErrorPackratM "True" "not match: " ["dv_comForErr"] d452_404
                          d454_406 <- get
                          xx453_407 <- dvCharsM
                          case xx453_407 of
                              ']' -> return ()
                              _ -> throwErrorPackratM "']'" "not match pattern: " ["dvChars"] d454_406
                          let ']' = xx453_407
                          return ()
                          if True
                           then return ()
                           else throwErrorPackratM "True" "not match: " ["dvChars"] d454_406
                          return (h, maybe "" id com)]
p_hsExpLam = foldl1 mplus [do d456_408 <- get
                              xx455_409 <- dvCharsM
                              case xx455_409 of
                                  '\\' -> return ()
                                  _ -> throwErrorPackratM "'\\\\'" "not match pattern: " ["dvChars"] d456_408
                              let '\\' = xx455_409
                              return ()
                              if True
                               then return ()
                               else throwErrorPackratM "True" "not match: " ["dvChars"] d456_408
                              d458_410 <- get
                              _ <- dv_spacesM
                              if True
                               then return ()
                               else throwErrorPackratM "True" "not match: " ["dv_spaces"] d458_410
                              d460_411 <- get
                              xx459_412 <- dv_patsM
                              let ps = xx459_412
                              if True
                               then return ()
                               else throwErrorPackratM "True" "not match: " ["dv_pats"] d460_411
                              d462_413 <- get
                              _ <- dv_spacesM
                              if True
                               then return ()
                               else throwErrorPackratM "True" "not match: " ["dv_spaces"] d462_413
                              d464_414 <- get
                              xx463_415 <- dvCharsM
                              case xx463_415 of
                                  '-' -> return ()
                                  _ -> throwErrorPackratM "'-'" "not match pattern: " ["dvChars"] d464_414
                              let '-' = xx463_415
                              return ()
                              if True
                               then return ()
                               else throwErrorPackratM "True" "not match: " ["dvChars"] d464_414
                              d466_416 <- get
                              xx465_417 <- dvCharsM
                              let c = xx465_417
                              if isGt c
                               then return ()
                               else throwErrorPackratM "isGt c" "not match: " ["dvChars"] d466_416
                              d468_418 <- get
                              _ <- dv_spacesM
                              if True
                               then return ()
                               else throwErrorPackratM "True" "not match: " ["dv_spaces"] d468_418
                              d470_419 <- get
                              xx469_420 <- dv_hsExpTypM
                              let e = xx469_420
                              if True
                               then return ()
                               else throwErrorPackratM "True" "not match: " ["dv_hsExpTyp"] d470_419
                              return (lamE ps e),
                           do d472_421 <- get
                              xx471_422 <- dv_hsExpTypM
                              let e = xx471_422
                              if True
                               then return ()
                               else throwErrorPackratM "True" "not match: " ["dv_hsExpTyp"] d472_421
                              return e]
p_hsExpTyp = foldl1 mplus [do d474_423 <- get
                              xx473_424 <- dv_hsExpOpM
                              let eo = xx473_424
                              if True
                               then return ()
                               else throwErrorPackratM "True" "not match: " ["dv_hsExpOp"] d474_423
                              d476_425 <- get
                              xx475_426 <- dvCharsM
                              case xx475_426 of
                                  ':' -> return ()
                                  _ -> throwErrorPackratM "':'" "not match pattern: " ["dvChars"] d476_425
                              let ':' = xx475_426
                              return ()
                              if True
                               then return ()
                               else throwErrorPackratM "True" "not match: " ["dvChars"] d476_425
                              d478_427 <- get
                              xx477_428 <- dvCharsM
                              case xx477_428 of
                                  ':' -> return ()
                                  _ -> throwErrorPackratM "':'" "not match pattern: " ["dvChars"] d478_427
                              let ':' = xx477_428
                              return ()
                              if True
                               then return ()
                               else throwErrorPackratM "True" "not match: " ["dvChars"] d478_427
                              d480_429 <- get
                              _ <- dv_spacesM
                              if True
                               then return ()
                               else throwErrorPackratM "True" "not match: " ["dv_spaces"] d480_429
                              d482_430 <- get
                              xx481_431 <- dv_hsTypeArrM
                              let t = xx481_431
                              if True
                               then return ()
                               else throwErrorPackratM "True" "not match: " ["dv_hsTypeArr"] d482_430
                              return (sigE eo t),
                           do d484_432 <- get
                              xx483_433 <- dv_hsExpOpM
                              let eo = xx483_433
                              if True
                               then return ()
                               else throwErrorPackratM "True" "not match: " ["dv_hsExpOp"] d484_432
                              return eo]
p_hsExpOp = foldl1 mplus [do d486_434 <- get
                             xx485_435 <- dv_hsExpM
                             let l = xx485_435
                             if True
                              then return ()
                              else throwErrorPackratM "True" "not match: " ["dv_hsExp"] d486_434
                             d488_436 <- get
                             _ <- dv_spacesM
                             if True
                              then return ()
                              else throwErrorPackratM "True" "not match: " ["dv_spaces"] d488_436
                             d490_437 <- get
                             xx489_438 <- dv_hsOpM
                             let o = xx489_438
                             if True
                              then return ()
                              else throwErrorPackratM "True" "not match: " ["dv_hsOp"] d490_437
                             d492_439 <- get
                             _ <- dv_spacesM
                             if True
                              then return ()
                              else throwErrorPackratM "True" "not match: " ["dv_spaces"] d492_439
                             d494_440 <- get
                             xx493_441 <- dv_hsExpOpM
                             let r = xx493_441
                             if True
                              then return ()
                              else throwErrorPackratM "True" "not match: " ["dv_hsExpOp"] d494_440
                             return (uInfixE (getEx l) o r),
                          do d496_442 <- get
                             xx495_443 <- dv_hsExpM
                             let e = xx495_443
                             if True
                              then return ()
                              else throwErrorPackratM "True" "not match: " ["dv_hsExp"] d496_442
                             return (getEx e)]
p_hsOp = foldl1 mplus [do d498_444 <- get
                          xx497_445 <- dvCharsM
                          let c = xx497_445
                          if isOpHeadChar c
                           then return ()
                           else throwErrorPackratM "isOpHeadChar c" "not match: " ["dvChars"] d498_444
                          d500_446 <- get
                          xx499_447 <- dv_opTailM
                          let o = xx499_447
                          if True
                           then return ()
                           else throwErrorPackratM "True" "not match: " ["dv_opTail"] d500_446
                          return (varE (mkName (cons c o))),
                       do d502_448 <- get
                          xx501_449 <- dvCharsM
                          case xx501_449 of
                              ':' -> return ()
                              _ -> throwErrorPackratM "':'" "not match pattern: " ["dvChars"] d502_448
                          let ':' = xx501_449
                          return ()
                          if True
                           then return ()
                           else throwErrorPackratM "True" "not match: " ["dvChars"] d502_448
                          ddd503_450 <- get
                          flipMaybe "':':[True]" ddd503_450 ["dvChars"] (do d505_451 <- get
                                                                            xx504_452 <- dvCharsM
                                                                            case xx504_452 of
                                                                                ':' -> return ()
                                                                                _ -> throwErrorPackratM "':'" "not match pattern: " ["dvChars"] d505_451
                                                                            let ':' = xx504_452
                                                                            return ()
                                                                            if True
                                                                             then return ()
                                                                             else throwErrorPackratM "True" "not match: " ["dvChars"] d505_451)
                          put ddd503_450
                          d507_453 <- get
                          xx506_454 <- dv_opTailM
                          let o = xx506_454
                          if True
                           then return ()
                           else throwErrorPackratM "True" "not match: " ["dv_opTail"] d507_453
                          return (conE (mkName (':' : o))),
                       do d509_455 <- get
                          xx508_456 <- dvCharsM
                          let c = xx508_456
                          if isBQ c
                           then return ()
                           else throwErrorPackratM "isBQ c" "not match: " ["dvChars"] d509_455
                          d511_457 <- get
                          xx510_458 <- dv_variableM
                          let v = xx510_458
                          if True
                           then return ()
                           else throwErrorPackratM "True" "not match: " ["dv_variable"] d511_457
                          d513_459 <- get
                          xx512_460 <- dvCharsM
                          let c_ = xx512_460
                          if isBQ c_
                           then return ()
                           else throwErrorPackratM "isBQ c_" "not match: " ["dvChars"] d513_459
                          return (varE (mkName v)),
                       do d515_461 <- get
                          xx514_462 <- dvCharsM
                          let c = xx514_462
                          if isBQ c
                           then return ()
                           else throwErrorPackratM "isBQ c" "not match: " ["dvChars"] d515_461
                          d517_463 <- get
                          xx516_464 <- dv_typM
                          let t = xx516_464
                          if True
                           then return ()
                           else throwErrorPackratM "True" "not match: " ["dv_typ"] d517_463
                          d519_465 <- get
                          xx518_466 <- dvCharsM
                          let c_ = xx518_466
                          if isBQ c_
                           then return ()
                           else throwErrorPackratM "isBQ c_" "not match: " ["dvChars"] d519_465
                          return (conE (mkName t))]
p_opTail = foldl1 mplus [do d521_467 <- get
                            xx520_468 <- dvCharsM
                            let c = xx520_468
                            if isOpTailChar c
                             then return ()
                             else throwErrorPackratM "isOpTailChar c" "not match: " ["dvChars"] d521_467
                            d523_469 <- get
                            xx522_470 <- dv_opTailM
                            let s = xx522_470
                            if True
                             then return ()
                             else throwErrorPackratM "True" "not match: " ["dv_opTail"] d523_469
                            return (cons c s),
                         do return emp]
p_hsExp = foldl1 mplus [do d525_471 <- get
                           xx524_472 <- dv_hsExp1M
                           let e = xx524_472
                           if True
                            then return ()
                            else throwErrorPackratM "True" "not match: " ["dv_hsExp1"] d525_471
                           d527_473 <- get
                           _ <- dv_spacesM
                           if True
                            then return ()
                            else throwErrorPackratM "True" "not match: " ["dv_spaces"] d527_473
                           d529_474 <- get
                           xx528_475 <- dv_hsExpM
                           let h = xx528_475
                           if True
                            then return ()
                            else throwErrorPackratM "True" "not match: " ["dv_hsExp"] d529_474
                           return (applyExR e h),
                        do d531_476 <- get
                           xx530_477 <- dv_hsExp1M
                           let e = xx530_477
                           if True
                            then return ()
                            else throwErrorPackratM "True" "not match: " ["dv_hsExp1"] d531_476
                           return (toEx e)]
p_hsExp1 = foldl1 mplus [do d533_478 <- get
                            xx532_479 <- dvCharsM
                            case xx532_479 of
                                '(' -> return ()
                                _ -> throwErrorPackratM "'('" "not match pattern: " ["dvChars"] d533_478
                            let '(' = xx532_479
                            return ()
                            if True
                             then return ()
                             else throwErrorPackratM "True" "not match: " ["dvChars"] d533_478
                            d535_480 <- get
                            xx534_481 <- papOptional (foldl1 mplus [do d537_482 <- get
                                                                       xx536_483 <- dv_hsExpTypM
                                                                       let e = xx536_483
                                                                       if True
                                                                        then return ()
                                                                        else throwErrorPackratM "True" "not match: " ["dv_hsExpTyp"] d537_482
                                                                       return e])
                            let l = xx534_481
                            if True
                             then return ()
                             else throwErrorPackratM "True" "not match: " ["dv_hsExpTyp"] d535_480
                            d539_484 <- get
                            _ <- dv_spacesM
                            if True
                             then return ()
                             else throwErrorPackratM "True" "not match: " ["dv_spaces"] d539_484
                            d541_485 <- get
                            xx540_486 <- dv_hsOpM
                            let o = xx540_486
                            if True
                             then return ()
                             else throwErrorPackratM "True" "not match: " ["dv_hsOp"] d541_485
                            d543_487 <- get
                            _ <- dv_spacesM
                            if True
                             then return ()
                             else throwErrorPackratM "True" "not match: " ["dv_spaces"] d543_487
                            d545_488 <- get
                            xx544_489 <- papOptional (foldl1 mplus [do d547_490 <- get
                                                                       xx546_491 <- dv_hsExpTypM
                                                                       let e = xx546_491
                                                                       if True
                                                                        then return ()
                                                                        else throwErrorPackratM "True" "not match: " ["dv_hsExpTyp"] d547_490
                                                                       return e])
                            let r = xx544_489
                            if True
                             then return ()
                             else throwErrorPackratM "True" "not match: " ["dv_hsExpTyp"] d545_488
                            d549_492 <- get
                            xx548_493 <- dvCharsM
                            case xx548_493 of
                                ')' -> return ()
                                _ -> throwErrorPackratM "')'" "not match pattern: " ["dvChars"] d549_492
                            let ')' = xx548_493
                            return ()
                            if True
                             then return ()
                             else throwErrorPackratM "True" "not match: " ["dvChars"] d549_492
                            return (infixE l o r),
                         do d551_494 <- get
                            xx550_495 <- dvCharsM
                            case xx550_495 of
                                '(' -> return ()
                                _ -> throwErrorPackratM "'('" "not match pattern: " ["dvChars"] d551_494
                            let '(' = xx550_495
                            return ()
                            if True
                             then return ()
                             else throwErrorPackratM "True" "not match: " ["dvChars"] d551_494
                            d553_496 <- get
                            xx552_497 <- dv_hsExpTplM
                            let et = xx552_497
                            if True
                             then return ()
                             else throwErrorPackratM "True" "not match: " ["dv_hsExpTpl"] d553_496
                            d555_498 <- get
                            xx554_499 <- dvCharsM
                            case xx554_499 of
                                ')' -> return ()
                                _ -> throwErrorPackratM "')'" "not match pattern: " ["dvChars"] d555_498
                            let ')' = xx554_499
                            return ()
                            if True
                             then return ()
                             else throwErrorPackratM "True" "not match: " ["dvChars"] d555_498
                            return (tupE et),
                         do d557_500 <- get
                            xx556_501 <- dvCharsM
                            case xx556_501 of
                                '[' -> return ()
                                _ -> throwErrorPackratM "'['" "not match pattern: " ["dvChars"] d557_500
                            let '[' = xx556_501
                            return ()
                            if True
                             then return ()
                             else throwErrorPackratM "True" "not match: " ["dvChars"] d557_500
                            d559_502 <- get
                            xx558_503 <- dv_hsExpTplM
                            let et = xx558_503
                            if True
                             then return ()
                             else throwErrorPackratM "True" "not match: " ["dv_hsExpTpl"] d559_502
                            d561_504 <- get
                            xx560_505 <- dvCharsM
                            case xx560_505 of
                                ']' -> return ()
                                _ -> throwErrorPackratM "']'" "not match pattern: " ["dvChars"] d561_504
                            let ']' = xx560_505
                            return ()
                            if True
                             then return ()
                             else throwErrorPackratM "True" "not match: " ["dvChars"] d561_504
                            return (listE et),
                         do d563_506 <- get
                            xx562_507 <- dv_variableM
                            let v = xx562_507
                            if True
                             then return ()
                             else throwErrorPackratM "True" "not match: " ["dv_variable"] d563_506
                            return (varE (mkName v)),
                         do d565_508 <- get
                            xx564_509 <- dv_typM
                            let t = xx564_509
                            if True
                             then return ()
                             else throwErrorPackratM "True" "not match: " ["dv_typ"] d565_508
                            return (conE (mkName t)),
                         do d567_510 <- get
                            xx566_511 <- dv_integerM
                            let i = xx566_511
                            if True
                             then return ()
                             else throwErrorPackratM "True" "not match: " ["dv_integer"] d567_510
                            d569_512 <- get
                            _ <- dv_spacesM
                            if True
                             then return ()
                             else throwErrorPackratM "True" "not match: " ["dv_spaces"] d569_512
                            return (litE (integerL i)),
                         do d571_513 <- get
                            xx570_514 <- dvCharsM
                            case xx570_514 of
                                '\'' -> return ()
                                _ -> throwErrorPackratM "'\\''" "not match pattern: " ["dvChars"] d571_513
                            let '\'' = xx570_514
                            return ()
                            if True
                             then return ()
                             else throwErrorPackratM "True" "not match: " ["dvChars"] d571_513
                            d573_515 <- get
                            xx572_516 <- dv_charLitM
                            let c = xx572_516
                            if True
                             then return ()
                             else throwErrorPackratM "True" "not match: " ["dv_charLit"] d573_515
                            d575_517 <- get
                            xx574_518 <- dvCharsM
                            case xx574_518 of
                                '\'' -> return ()
                                _ -> throwErrorPackratM "'\\''" "not match pattern: " ["dvChars"] d575_517
                            let '\'' = xx574_518
                            return ()
                            if True
                             then return ()
                             else throwErrorPackratM "True" "not match: " ["dvChars"] d575_517
                            return (litE (charL c)),
                         do d577_519 <- get
                            xx576_520 <- dvCharsM
                            case xx576_520 of
                                '"' -> return ()
                                _ -> throwErrorPackratM "'\"'" "not match pattern: " ["dvChars"] d577_519
                            let '"' = xx576_520
                            return ()
                            if True
                             then return ()
                             else throwErrorPackratM "True" "not match: " ["dvChars"] d577_519
                            d579_521 <- get
                            xx578_522 <- dv_stringLitM
                            let s = xx578_522
                            if True
                             then return ()
                             else throwErrorPackratM "True" "not match: " ["dv_stringLit"] d579_521
                            d581_523 <- get
                            xx580_524 <- dvCharsM
                            case xx580_524 of
                                '"' -> return ()
                                _ -> throwErrorPackratM "'\"'" "not match pattern: " ["dvChars"] d581_523
                            let '"' = xx580_524
                            return ()
                            if True
                             then return ()
                             else throwErrorPackratM "True" "not match: " ["dvChars"] d581_523
                            return (litE (stringL s)),
                         do d583_525 <- get
                            xx582_526 <- dvCharsM
                            case xx582_526 of
                                '-' -> return ()
                                _ -> throwErrorPackratM "'-'" "not match pattern: " ["dvChars"] d583_525
                            let '-' = xx582_526
                            return ()
                            if True
                             then return ()
                             else throwErrorPackratM "True" "not match: " ["dvChars"] d583_525
                            d585_527 <- get
                            _ <- dv_spacesM
                            if True
                             then return ()
                             else throwErrorPackratM "True" "not match: " ["dv_spaces"] d585_527
                            d587_528 <- get
                            xx586_529 <- dv_hsExp1M
                            let e = xx586_529
                            if True
                             then return ()
                             else throwErrorPackratM "True" "not match: " ["dv_hsExp1"] d587_528
                            return (appE (varE $ mkName "negate") e)]
p_hsExpTpl = foldl1 mplus [do d589_530 <- get
                              xx588_531 <- dv_hsExpLamM
                              let e = xx588_531
                              if True
                               then return ()
                               else throwErrorPackratM "True" "not match: " ["dv_hsExpLam"] d589_530
                              d591_532 <- get
                              _ <- dv_spacesM
                              if True
                               then return ()
                               else throwErrorPackratM "True" "not match: " ["dv_spaces"] d591_532
                              d593_533 <- get
                              xx592_534 <- dvCharsM
                              let c = xx592_534
                              if isComma c
                               then return ()
                               else throwErrorPackratM "isComma c" "not match: " ["dvChars"] d593_533
                              d595_535 <- get
                              _ <- dv_spacesM
                              if True
                               then return ()
                               else throwErrorPackratM "True" "not match: " ["dv_spaces"] d595_535
                              d597_536 <- get
                              xx596_537 <- dv_hsExpTplM
                              let et = xx596_537
                              if True
                               then return ()
                               else throwErrorPackratM "True" "not match: " ["dv_hsExpTpl"] d597_536
                              return (cons e et),
                           do d599_538 <- get
                              xx598_539 <- dv_hsExpLamM
                              let e = xx598_539
                              if True
                               then return ()
                               else throwErrorPackratM "True" "not match: " ["dv_hsExpLam"] d599_538
                              return (cons e emp),
                           do return emp]
p_hsTypeArr = foldl1 mplus [do d601_540 <- get
                               xx600_541 <- dv_hsTypeM
                               let l = xx600_541
                               if True
                                then return ()
                                else throwErrorPackratM "True" "not match: " ["dv_hsType"] d601_540
                               d603_542 <- get
                               xx602_543 <- dvCharsM
                               case xx602_543 of
                                   '-' -> return ()
                                   _ -> throwErrorPackratM "'-'" "not match pattern: " ["dvChars"] d603_542
                               let '-' = xx602_543
                               return ()
                               if True
                                then return ()
                                else throwErrorPackratM "True" "not match: " ["dvChars"] d603_542
                               d605_544 <- get
                               xx604_545 <- dvCharsM
                               let c = xx604_545
                               if isGt c
                                then return ()
                                else throwErrorPackratM "isGt c" "not match: " ["dvChars"] d605_544
                               d607_546 <- get
                               _ <- dv_spacesM
                               if True
                                then return ()
                                else throwErrorPackratM "True" "not match: " ["dv_spaces"] d607_546
                               d609_547 <- get
                               xx608_548 <- dv_hsTypeArrM
                               let r = xx608_548
                               if True
                                then return ()
                                else throwErrorPackratM "True" "not match: " ["dv_hsTypeArr"] d609_547
                               return (appT (appT arrowT (getTyp l)) r),
                            do d611_549 <- get
                               xx610_550 <- dv_hsTypeM
                               let t = xx610_550
                               if True
                                then return ()
                                else throwErrorPackratM "True" "not match: " ["dv_hsType"] d611_549
                               return (getTyp t)]
p_hsType = foldl1 mplus [do d613_551 <- get
                            xx612_552 <- dv_hsType1M
                            let t = xx612_552
                            if True
                             then return ()
                             else throwErrorPackratM "True" "not match: " ["dv_hsType1"] d613_551
                            d615_553 <- get
                            xx614_554 <- dv_hsTypeM
                            let ts = xx614_554
                            if True
                             then return ()
                             else throwErrorPackratM "True" "not match: " ["dv_hsType"] d615_553
                            return (applyTyp (toTyp t) ts),
                         do d617_555 <- get
                            xx616_556 <- dv_hsType1M
                            let t = xx616_556
                            if True
                             then return ()
                             else throwErrorPackratM "True" "not match: " ["dv_hsType1"] d617_555
                            return (toTyp t)]
p_hsType1 = foldl1 mplus [do d619_557 <- get
                             xx618_558 <- dvCharsM
                             case xx618_558 of
                                 '[' -> return ()
                                 _ -> throwErrorPackratM "'['" "not match pattern: " ["dvChars"] d619_557
                             let '[' = xx618_558
                             return ()
                             if True
                              then return ()
                              else throwErrorPackratM "True" "not match: " ["dvChars"] d619_557
                             d621_559 <- get
                             xx620_560 <- dvCharsM
                             case xx620_560 of
                                 ']' -> return ()
                                 _ -> throwErrorPackratM "']'" "not match pattern: " ["dvChars"] d621_559
                             let ']' = xx620_560
                             return ()
                             if True
                              then return ()
                              else throwErrorPackratM "True" "not match: " ["dvChars"] d621_559
                             d623_561 <- get
                             _ <- dv_spacesM
                             if True
                              then return ()
                              else throwErrorPackratM "True" "not match: " ["dv_spaces"] d623_561
                             return listT,
                          do d625_562 <- get
                             xx624_563 <- dvCharsM
                             case xx624_563 of
                                 '[' -> return ()
                                 _ -> throwErrorPackratM "'['" "not match pattern: " ["dvChars"] d625_562
                             let '[' = xx624_563
                             return ()
                             if True
                              then return ()
                              else throwErrorPackratM "True" "not match: " ["dvChars"] d625_562
                             d627_564 <- get
                             xx626_565 <- dv_hsTypeArrM
                             let t = xx626_565
                             if True
                              then return ()
                              else throwErrorPackratM "True" "not match: " ["dv_hsTypeArr"] d627_564
                             d629_566 <- get
                             xx628_567 <- dvCharsM
                             case xx628_567 of
                                 ']' -> return ()
                                 _ -> throwErrorPackratM "']'" "not match pattern: " ["dvChars"] d629_566
                             let ']' = xx628_567
                             return ()
                             if True
                              then return ()
                              else throwErrorPackratM "True" "not match: " ["dvChars"] d629_566
                             d631_568 <- get
                             _ <- dv_spacesM
                             if True
                              then return ()
                              else throwErrorPackratM "True" "not match: " ["dv_spaces"] d631_568
                             return (appT listT t),
                          do d633_569 <- get
                             xx632_570 <- dvCharsM
                             case xx632_570 of
                                 '(' -> return ()
                                 _ -> throwErrorPackratM "'('" "not match pattern: " ["dvChars"] d633_569
                             let '(' = xx632_570
                             return ()
                             if True
                              then return ()
                              else throwErrorPackratM "True" "not match: " ["dvChars"] d633_569
                             d635_571 <- get
                             _ <- dv_spacesM
                             if True
                              then return ()
                              else throwErrorPackratM "True" "not match: " ["dv_spaces"] d635_571
                             d637_572 <- get
                             xx636_573 <- dv_hsTypeTplM
                             let tt = xx636_573
                             if True
                              then return ()
                              else throwErrorPackratM "True" "not match: " ["dv_hsTypeTpl"] d637_572
                             d639_574 <- get
                             xx638_575 <- dvCharsM
                             case xx638_575 of
                                 ')' -> return ()
                                 _ -> throwErrorPackratM "')'" "not match pattern: " ["dvChars"] d639_574
                             let ')' = xx638_575
                             return ()
                             if True
                              then return ()
                              else throwErrorPackratM "True" "not match: " ["dvChars"] d639_574
                             return (tupT tt),
                          do d641_576 <- get
                             xx640_577 <- dv_typTokenM
                             let t = xx640_577
                             if True
                              then return ()
                              else throwErrorPackratM "True" "not match: " ["dv_typToken"] d641_576
                             return (conT (mkName t)),
                          do d643_578 <- get
                             xx642_579 <- dvCharsM
                             case xx642_579 of
                                 '(' -> return ()
                                 _ -> throwErrorPackratM "'('" "not match pattern: " ["dvChars"] d643_578
                             let '(' = xx642_579
                             return ()
                             if True
                              then return ()
                              else throwErrorPackratM "True" "not match: " ["dvChars"] d643_578
                             d645_580 <- get
                             xx644_581 <- dvCharsM
                             case xx644_581 of
                                 '-' -> return ()
                                 _ -> throwErrorPackratM "'-'" "not match pattern: " ["dvChars"] d645_580
                             let '-' = xx644_581
                             return ()
                             if True
                              then return ()
                              else throwErrorPackratM "True" "not match: " ["dvChars"] d645_580
                             d647_582 <- get
                             xx646_583 <- dvCharsM
                             let c = xx646_583
                             if isGt c
                              then return ()
                              else throwErrorPackratM "isGt c" "not match: " ["dvChars"] d647_582
                             d649_584 <- get
                             xx648_585 <- dvCharsM
                             case xx648_585 of
                                 ')' -> return ()
                                 _ -> throwErrorPackratM "')'" "not match pattern: " ["dvChars"] d649_584
                             let ')' = xx648_585
                             return ()
                             if True
                              then return ()
                              else throwErrorPackratM "True" "not match: " ["dvChars"] d649_584
                             d651_586 <- get
                             _ <- dv_spacesM
                             if True
                              then return ()
                              else throwErrorPackratM "True" "not match: " ["dv_spaces"] d651_586
                             return arrowT]
p_hsTypeTpl = foldl1 mplus [do d653_587 <- get
                               xx652_588 <- dv_hsTypeArrM
                               let t = xx652_588
                               if True
                                then return ()
                                else throwErrorPackratM "True" "not match: " ["dv_hsTypeArr"] d653_587
                               d655_589 <- get
                               xx654_590 <- dvCharsM
                               let c = xx654_590
                               if isComma c
                                then return ()
                                else throwErrorPackratM "isComma c" "not match: " ["dvChars"] d655_589
                               d657_591 <- get
                               _ <- dv_spacesM
                               if True
                                then return ()
                                else throwErrorPackratM "True" "not match: " ["dv_spaces"] d657_591
                               d659_592 <- get
                               xx658_593 <- dv_hsTypeTplM
                               let tt = xx658_593
                               if True
                                then return ()
                                else throwErrorPackratM "True" "not match: " ["dv_hsTypeTpl"] d659_592
                               return (cons t tt),
                            do d661_594 <- get
                               xx660_595 <- dv_hsTypeArrM
                               let t = xx660_595
                               if True
                                then return ()
                                else throwErrorPackratM "True" "not match: " ["dv_hsTypeArr"] d661_594
                               return (cons t emp),
                            do return emp]
p_typ = foldl1 mplus [do d663_596 <- get
                         xx662_597 <- dv_upperM
                         let u = xx662_597
                         if True
                          then return ()
                          else throwErrorPackratM "True" "not match: " ["dv_upper"] d663_596
                         d665_598 <- get
                         xx664_599 <- dv_tvtailM
                         let t = xx664_599
                         if True
                          then return ()
                          else throwErrorPackratM "True" "not match: " ["dv_tvtail"] d665_598
                         return (cons u t)]
p_variable = foldl1 mplus [do d667_600 <- get
                              xx666_601 <- dv_lowerM
                              let l = xx666_601
                              if True
                               then return ()
                               else throwErrorPackratM "True" "not match: " ["dv_lower"] d667_600
                              d669_602 <- get
                              xx668_603 <- dv_tvtailM
                              let t = xx668_603
                              if True
                               then return ()
                               else throwErrorPackratM "True" "not match: " ["dv_tvtail"] d669_602
                              return (cons l t)]
p_tvtail = foldl1 mplus [do d671_604 <- get
                            xx670_605 <- dv_alphaM
                            let a = xx670_605
                            if True
                             then return ()
                             else throwErrorPackratM "True" "not match: " ["dv_alpha"] d671_604
                            d673_606 <- get
                            xx672_607 <- dv_tvtailM
                            let t = xx672_607
                            if True
                             then return ()
                             else throwErrorPackratM "True" "not match: " ["dv_tvtail"] d673_606
                            return (cons a t),
                         do return emp]
p_integer = foldl1 mplus [do d675_608 <- get
                             xx674_609 <- dv_digitM
                             let dh = xx674_609
                             if True
                              then return ()
                              else throwErrorPackratM "True" "not match: " ["dv_digit"] d675_608
                             d677_610 <- get
                             xx676_611 <- list (foldl1 mplus [do d679_612 <- get
                                                                 xx678_613 <- dv_digitM
                                                                 let d = xx678_613
                                                                 if True
                                                                  then return ()
                                                                  else throwErrorPackratM "True" "not match: " ["dv_digit"] d679_612
                                                                 return d])
                             let ds = xx676_611
                             if True
                              then return ()
                              else throwErrorPackratM "True" "not match: " ["dv_digit"] d677_610
                             return (read (cons dh ds))]
p_alpha = foldl1 mplus [do d681_614 <- get
                           xx680_615 <- dv_upperM
                           let u = xx680_615
                           if True
                            then return ()
                            else throwErrorPackratM "True" "not match: " ["dv_upper"] d681_614
                           return u,
                        do d683_616 <- get
                           xx682_617 <- dv_lowerM
                           let l = xx682_617
                           if True
                            then return ()
                            else throwErrorPackratM "True" "not match: " ["dv_lower"] d683_616
                           return l,
                        do d685_618 <- get
                           xx684_619 <- dv_digitM
                           let d = xx684_619
                           if True
                            then return ()
                            else throwErrorPackratM "True" "not match: " ["dv_digit"] d685_618
                           return d]
p_upper = foldl1 mplus [do d687_620 <- get
                           xx686_621 <- dvCharsM
                           let u = xx686_621
                           if isUpper u
                            then return ()
                            else throwErrorPackratM "isUpper u" "not match: " ["dvChars"] d687_620
                           return u]
p_lower = foldl1 mplus [do d689_622 <- get
                           xx688_623 <- dvCharsM
                           let l = xx688_623
                           if isLowerU l
                            then return ()
                            else throwErrorPackratM "isLowerU l" "not match: " ["dvChars"] d689_622
                           return l]
p_digit = foldl1 mplus [do d691_624 <- get
                           xx690_625 <- dvCharsM
                           let d = xx690_625
                           if isDigit d
                            then return ()
                            else throwErrorPackratM "isDigit d" "not match: " ["dvChars"] d691_624
                           return d]
p_spaces = foldl1 mplus [do d693_626 <- get
                            _ <- dv_spaceM
                            if True
                             then return ()
                             else throwErrorPackratM "True" "not match: " ["dv_space"] d693_626
                            d695_627 <- get
                            _ <- dv_spacesM
                            if True
                             then return ()
                             else throwErrorPackratM "True" "not match: " ["dv_spaces"] d695_627
                            return (),
                         do return ()]
p_space = foldl1 mplus [do d697_628 <- get
                           xx696_629 <- dvCharsM
                           let s = xx696_629
                           if isSpace s
                            then return ()
                            else throwErrorPackratM "isSpace s" "not match: " ["dvChars"] d697_628
                           return (),
                        do d699_630 <- get
                           xx698_631 <- dvCharsM
                           case xx698_631 of
                               '-' -> return ()
                               _ -> throwErrorPackratM "'-'" "not match pattern: " ["dvChars"] d699_630
                           let '-' = xx698_631
                           return ()
                           if True
                            then return ()
                            else throwErrorPackratM "True" "not match: " ["dvChars"] d699_630
                           d701_632 <- get
                           xx700_633 <- dvCharsM
                           case xx700_633 of
                               '-' -> return ()
                               _ -> throwErrorPackratM "'-'" "not match pattern: " ["dvChars"] d701_632
                           let '-' = xx700_633
                           return ()
                           if True
                            then return ()
                            else throwErrorPackratM "True" "not match: " ["dvChars"] d701_632
                           d703_634 <- get
                           _ <- dv_notNLStringM
                           if True
                            then return ()
                            else throwErrorPackratM "True" "not match: " ["dv_notNLString"] d703_634
                           d705_635 <- get
                           _ <- dv_nlM
                           if True
                            then return ()
                            else throwErrorPackratM "True" "not match: " ["dv_nl"] d705_635
                           return (),
                        do d707_636 <- get
                           _ <- dv_commentM
                           if True
                            then return ()
                            else throwErrorPackratM "True" "not match: " ["dv_comment"] d707_636
                           return ()]
p_notNLString = foldl1 mplus [do ddd708_637 <- get
                                 flipMaybe "_:nl[True]" ddd708_637 ["dv_nl"] (do d710_638 <- get
                                                                                 _ <- dv_nlM
                                                                                 if True
                                                                                  then return ()
                                                                                  else throwErrorPackratM "True" "not match: " ["dv_nl"] d710_638)
                                 put ddd708_637
                                 d712_639 <- get
                                 xx711_640 <- dvCharsM
                                 let c = xx711_640
                                 if True
                                  then return ()
                                  else throwErrorPackratM "True" "not match: " ["dvChars"] d712_639
                                 d714_641 <- get
                                 xx713_642 <- dv_notNLStringM
                                 let s = xx713_642
                                 if True
                                  then return ()
                                  else throwErrorPackratM "True" "not match: " ["dv_notNLString"] d714_641
                                 return (cons c s),
                              do return emp]
p_nl = foldl1 mplus [do d716_643 <- get
                        xx715_644 <- dvCharsM
                        case xx715_644 of
                            '\n' -> return ()
                            _ -> throwErrorPackratM "'\\n'" "not match pattern: " ["dvChars"] d716_643
                        let '\n' = xx715_644
                        return ()
                        if True
                         then return ()
                         else throwErrorPackratM "True" "not match: " ["dvChars"] d716_643
                        return ()]
p_comment = foldl1 mplus [do d718_645 <- get
                             xx717_646 <- dvCharsM
                             case xx717_646 of
                                 '{' -> return ()
                                 _ -> throwErrorPackratM "'{'" "not match pattern: " ["dvChars"] d718_645
                             let '{' = xx717_646
                             return ()
                             if True
                              then return ()
                              else throwErrorPackratM "True" "not match: " ["dvChars"] d718_645
                             d720_647 <- get
                             xx719_648 <- dvCharsM
                             case xx719_648 of
                                 '-' -> return ()
                                 _ -> throwErrorPackratM "'-'" "not match pattern: " ["dvChars"] d720_647
                             let '-' = xx719_648
                             return ()
                             if True
                              then return ()
                              else throwErrorPackratM "True" "not match: " ["dvChars"] d720_647
                             ddd721_649 <- get
                             flipMaybe "'#':[True]" ddd721_649 ["dvChars"] (do d723_650 <- get
                                                                               xx722_651 <- dvCharsM
                                                                               case xx722_651 of
                                                                                   '#' -> return ()
                                                                                   _ -> throwErrorPackratM "'#'" "not match pattern: " ["dvChars"] d723_650
                                                                               let '#' = xx722_651
                                                                               return ()
                                                                               if True
                                                                                then return ()
                                                                                else throwErrorPackratM "True" "not match: " ["dvChars"] d723_650)
                             put ddd721_649
                             d725_652 <- get
                             _ <- dv_commentsM
                             if True
                              then return ()
                              else throwErrorPackratM "True" "not match: " ["dv_comments"] d725_652
                             d727_653 <- get
                             _ <- dv_comEndM
                             if True
                              then return ()
                              else throwErrorPackratM "True" "not match: " ["dv_comEnd"] d727_653
                             return ()]
p_comments = foldl1 mplus [do d729_654 <- get
                              _ <- dv_notComStrM
                              if True
                               then return ()
                               else throwErrorPackratM "True" "not match: " ["dv_notComStr"] d729_654
                              d731_655 <- get
                              _ <- dv_commentM
                              if True
                               then return ()
                               else throwErrorPackratM "True" "not match: " ["dv_comment"] d731_655
                              d733_656 <- get
                              _ <- dv_commentsM
                              if True
                               then return ()
                               else throwErrorPackratM "True" "not match: " ["dv_comments"] d733_656
                              return (),
                           do d735_657 <- get
                              _ <- dv_notComStrM
                              if True
                               then return ()
                               else throwErrorPackratM "True" "not match: " ["dv_notComStr"] d735_657
                              return ()]
p_notComStr = foldl1 mplus [do ddd736_658 <- get
                               flipMaybe "_:comment[True]" ddd736_658 ["dv_comment"] (do d738_659 <- get
                                                                                         _ <- dv_commentM
                                                                                         if True
                                                                                          then return ()
                                                                                          else throwErrorPackratM "True" "not match: " ["dv_comment"] d738_659)
                               put ddd736_658
                               ddd739_660 <- get
                               flipMaybe "_:comEnd[True]" ddd739_660 ["dv_comEnd"] (do d741_661 <- get
                                                                                       _ <- dv_comEndM
                                                                                       if True
                                                                                        then return ()
                                                                                        else throwErrorPackratM "True" "not match: " ["dv_comEnd"] d741_661)
                               put ddd739_660
                               d743_662 <- get
                               _ <- dvCharsM
                               if True
                                then return ()
                                else throwErrorPackratM "True" "not match: " ["dvChars"] d743_662
                               d745_663 <- get
                               _ <- dv_notComStrM
                               if True
                                then return ()
                                else throwErrorPackratM "True" "not match: " ["dv_notComStr"] d745_663
                               return (),
                            do return ()]
p_comEnd = foldl1 mplus [do d747_664 <- get
                            xx746_665 <- dvCharsM
                            case xx746_665 of
                                '-' -> return ()
                                _ -> throwErrorPackratM "'-'" "not match pattern: " ["dvChars"] d747_664
                            let '-' = xx746_665
                            return ()
                            if True
                             then return ()
                             else throwErrorPackratM "True" "not match: " ["dvChars"] d747_664
                            d749_666 <- get
                            xx748_667 <- dvCharsM
                            case xx748_667 of
                                '}' -> return ()
                                _ -> throwErrorPackratM "'}'" "not match pattern: " ["dvChars"] d749_666
                            let '}' = xx748_667
                            return ()
                            if True
                             then return ()
                             else throwErrorPackratM "True" "not match: " ["dvChars"] d749_666
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