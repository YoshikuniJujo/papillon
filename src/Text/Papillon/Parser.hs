{-# LANGUAGE FlexibleContexts, TemplateHaskell, UndecidableInstances , FlexibleContexts, PackageImports, TypeFamilies, RankNTypes, FlexibleInstances #-}
module  Text.Papillon.Parser (
	Peg,
	Definition,
	Selection,
	ExpressionHs,
	NameLeaf(..),
	NameLeaf_(..),
	Leaf,
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
              dv_patOp :: (Result PatQ),
              dv_pat :: (Result PatQ),
              dv_pat1 :: (Result PatQ),
              dv_patList :: (Result ([PatQ])),
              dv_opConName :: (Result Name),
              dv_charLit :: (Result Char),
              dv_stringLit :: (Result String),
              dv_dq :: (Result ()),
              dv_pats :: (Result PatQs),
              dv_leaf :: (Result Leaf),
              dv_readFromLs :: (Result ReadFrom),
              dv_readFrom :: (Result ReadFrom),
              dv_test :: (Result ExR),
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
          where d = Derivs pegFile pragma pragmaStr pragmaEnd moduleDec moduleDecStr whr preImpPap prePeg afterPeg importPapillon varToken typToken pap peg sourceType peg_ definition selection expressionHs expression nameLeaf_ nameLeaf patOp pat pat1 patList opConName charLit stringLit dq pats leaf readFromLs readFrom test hsExpLam hsExpTyp hsExpOp hsOp opTail hsExp hsExp1 hsExpTpl hsTypeArr hsType hsType1 hsTypeTpl typ variable tvtail integer alpha upper lower digit spaces space notNLString nl comment comments notComStr comEnd char pos___hoge
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
                patOp = runStateT p_patOp d
                pat = runStateT p_pat d
                pat1 = runStateT p_pat1 d
                patList = runStateT p_patList d
                opConName = runStateT p_opConName d
                charLit = runStateT p_charLit d
                stringLit = runStateT p_stringLit d
                dq = runStateT p_dq d
                pats = runStateT p_pats d
                leaf = runStateT p_leaf d
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
dv_patOpM :: PackratM PatQ
dv_patM :: PackratM PatQ
dv_pat1M :: PackratM PatQ
dv_patListM :: PackratM ([PatQ])
dv_opConNameM :: PackratM Name
dv_charLitM :: PackratM Char
dv_stringLitM :: PackratM String
dv_dqM :: PackratM ()
dv_patsM :: PackratM PatQs
dv_leafM :: PackratM Leaf
dv_readFromLsM :: PackratM ReadFrom
dv_readFromM :: PackratM ReadFrom
dv_testM :: PackratM ExR
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
dv_patOpM = StateT dv_patOp
dv_patM = StateT dv_pat
dv_pat1M = StateT dv_pat1
dv_patListM = StateT dv_patList
dv_opConNameM = StateT dv_opConName
dv_charLitM = StateT dv_charLit
dv_stringLitM = StateT dv_stringLit
dv_dqM = StateT dv_dq
dv_patsM = StateT dv_pats
dv_leafM = StateT dv_leaf
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
p_patOp :: PackratM PatQ
p_pat :: PackratM PatQ
p_pat1 :: PackratM PatQ
p_patList :: PackratM ([PatQ])
p_opConName :: PackratM Name
p_charLit :: PackratM Char
p_stringLit :: PackratM String
p_dq :: PackratM ()
p_pats :: PackratM PatQs
p_leaf :: PackratM Leaf
p_readFromLs :: PackratM ReadFrom
p_readFrom :: PackratM ReadFrom
p_test :: PackratM ExR
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
                              xx262_231 <- dvCharsM
                              case xx262_231 of
                                  ':' -> return ()
                                  _ -> throwErrorPackratM "':'" "not match pattern: " ["dvChars"] d263_230
                              let ':' = xx262_231
                              return ()
                              if True
                               then return ()
                               else throwErrorPackratM "True" "not match: " ["dvChars"] d263_230
                              d265_232 <- get
                              xx264_233 <- dv_leafM
                              let l = xx264_233
                              if True
                               then return ()
                               else throwErrorPackratM "True" "not match: " ["dv_leaf"] d265_232
                              return (mkNameLeaf n l),
                           do d267_234 <- get
                              xx266_235 <- dv_pat1M
                              let n = xx266_235
                              if True
                               then return ()
                               else throwErrorPackratM "True" "not match: " ["dv_pat1"] d267_234
                              return (mkNameLeaf n ctLeaf)]
p_patOp = foldl1 mplus [do d269_236 <- get
                           xx268_237 <- dv_patM
                           let p = xx268_237
                           if True
                            then return ()
                            else throwErrorPackratM "True" "not match: " ["dv_pat"] d269_236
                           d271_238 <- get
                           xx270_239 <- dv_opConNameM
                           let o = xx270_239
                           if True
                            then return ()
                            else throwErrorPackratM "True" "not match: " ["dv_opConName"] d271_238
                           d273_240 <- get
                           xx272_241 <- dv_patOpM
                           let po = xx272_241
                           if True
                            then return ()
                            else throwErrorPackratM "True" "not match: " ["dv_patOp"] d273_240
                           return (uInfixP p o po),
                        do d275_242 <- get
                           xx274_243 <- dv_patM
                           let p = xx274_243
                           if True
                            then return ()
                            else throwErrorPackratM "True" "not match: " ["dv_pat"] d275_242
                           d277_244 <- get
                           _ <- dv_spacesM
                           if True
                            then return ()
                            else throwErrorPackratM "True" "not match: " ["dv_spaces"] d277_244
                           d279_245 <- get
                           xx278_246 <- dvCharsM
                           let q = xx278_246
                           if isBQ q
                            then return ()
                            else throwErrorPackratM "isBQ q" "not match: " ["dvChars"] d279_245
                           d281_247 <- get
                           xx280_248 <- dv_typM
                           let t = xx280_248
                           if True
                            then return ()
                            else throwErrorPackratM "True" "not match: " ["dv_typ"] d281_247
                           d283_249 <- get
                           xx282_250 <- dvCharsM
                           let q_ = xx282_250
                           if isBQ q_
                            then return ()
                            else throwErrorPackratM "isBQ q_" "not match: " ["dvChars"] d283_249
                           d285_251 <- get
                           _ <- dv_spacesM
                           if True
                            then return ()
                            else throwErrorPackratM "True" "not match: " ["dv_spaces"] d285_251
                           d287_252 <- get
                           xx286_253 <- dv_patOpM
                           let po = xx286_253
                           if True
                            then return ()
                            else throwErrorPackratM "True" "not match: " ["dv_patOp"] d287_252
                           return (uInfixP p (mkName t) po),
                        do d289_254 <- get
                           xx288_255 <- dv_patM
                           let p = xx288_255
                           if True
                            then return ()
                            else throwErrorPackratM "True" "not match: " ["dv_pat"] d289_254
                           return p]
p_pat = foldl1 mplus [do d291_256 <- get
                         xx290_257 <- dv_typM
                         let t = xx290_257
                         if True
                          then return ()
                          else throwErrorPackratM "True" "not match: " ["dv_typ"] d291_256
                         d293_258 <- get
                         _ <- dv_spacesM
                         if True
                          then return ()
                          else throwErrorPackratM "True" "not match: " ["dv_spaces"] d293_258
                         d295_259 <- get
                         xx294_260 <- dv_patsM
                         let ps = xx294_260
                         if True
                          then return ()
                          else throwErrorPackratM "True" "not match: " ["dv_pats"] d295_259
                         return (conToPatQ t ps),
                      do d297_261 <- get
                         xx296_262 <- dvCharsM
                         case xx296_262 of
                             '(' -> return ()
                             _ -> throwErrorPackratM "'('" "not match pattern: " ["dvChars"] d297_261
                         let '(' = xx296_262
                         return ()
                         if True
                          then return ()
                          else throwErrorPackratM "True" "not match: " ["dvChars"] d297_261
                         d299_263 <- get
                         xx298_264 <- dv_opConNameM
                         let o = xx298_264
                         if True
                          then return ()
                          else throwErrorPackratM "True" "not match: " ["dv_opConName"] d299_263
                         d301_265 <- get
                         xx300_266 <- dvCharsM
                         case xx300_266 of
                             ')' -> return ()
                             _ -> throwErrorPackratM "')'" "not match pattern: " ["dvChars"] d301_265
                         let ')' = xx300_266
                         return ()
                         if True
                          then return ()
                          else throwErrorPackratM "True" "not match: " ["dvChars"] d301_265
                         d303_267 <- get
                         _ <- dv_spacesM
                         if True
                          then return ()
                          else throwErrorPackratM "True" "not match: " ["dv_spaces"] d303_267
                         d305_268 <- get
                         xx304_269 <- dv_patsM
                         let ps = xx304_269
                         if True
                          then return ()
                          else throwErrorPackratM "True" "not match: " ["dv_pats"] d305_268
                         return (conP o ps),
                      do d307_270 <- get
                         xx306_271 <- dv_pat1M
                         let p = xx306_271
                         if True
                          then return ()
                          else throwErrorPackratM "True" "not match: " ["dv_pat1"] d307_270
                         return p]
p_pat1 = foldl1 mplus [do d309_272 <- get
                          xx308_273 <- dv_typM
                          let t = xx308_273
                          if True
                           then return ()
                           else throwErrorPackratM "True" "not match: " ["dv_typ"] d309_272
                          return (conToPatQ t emp),
                       do d311_274 <- get
                          xx310_275 <- dv_variableM
                          case xx310_275 of
                              "_" -> return ()
                              _ -> throwErrorPackratM "\"_\"" "not match pattern: " ["dv_variable"] d311_274
                          let "_" = xx310_275
                          return ()
                          if True
                           then return ()
                           else throwErrorPackratM "True" "not match: " ["dv_variable"] d311_274
                          return wildP,
                       do d313_276 <- get
                          xx312_277 <- dv_variableM
                          let n = xx312_277
                          if True
                           then return ()
                           else throwErrorPackratM "True" "not match: " ["dv_variable"] d313_276
                          return (strToPatQ n),
                       do d315_278 <- get
                          xx314_279 <- dv_integerM
                          let i = xx314_279
                          if True
                           then return ()
                           else throwErrorPackratM "True" "not match: " ["dv_integer"] d315_278
                          return (litP (integerL i)),
                       do d317_280 <- get
                          xx316_281 <- dvCharsM
                          case xx316_281 of
                              '-' -> return ()
                              _ -> throwErrorPackratM "'-'" "not match pattern: " ["dvChars"] d317_280
                          let '-' = xx316_281
                          return ()
                          if True
                           then return ()
                           else throwErrorPackratM "True" "not match: " ["dvChars"] d317_280
                          d319_282 <- get
                          _ <- dv_spacesM
                          if True
                           then return ()
                           else throwErrorPackratM "True" "not match: " ["dv_spaces"] d319_282
                          d321_283 <- get
                          xx320_284 <- dv_integerM
                          let i = xx320_284
                          if True
                           then return ()
                           else throwErrorPackratM "True" "not match: " ["dv_integer"] d321_283
                          return (litP (integerL $ negate i)),
                       do d323_285 <- get
                          xx322_286 <- dvCharsM
                          case xx322_286 of
                              '\'' -> return ()
                              _ -> throwErrorPackratM "'\\''" "not match pattern: " ["dvChars"] d323_285
                          let '\'' = xx322_286
                          return ()
                          if True
                           then return ()
                           else throwErrorPackratM "True" "not match: " ["dvChars"] d323_285
                          d325_287 <- get
                          xx324_288 <- dv_charLitM
                          let c = xx324_288
                          if True
                           then return ()
                           else throwErrorPackratM "True" "not match: " ["dv_charLit"] d325_287
                          d327_289 <- get
                          xx326_290 <- dvCharsM
                          case xx326_290 of
                              '\'' -> return ()
                              _ -> throwErrorPackratM "'\\''" "not match pattern: " ["dvChars"] d327_289
                          let '\'' = xx326_290
                          return ()
                          if True
                           then return ()
                           else throwErrorPackratM "True" "not match: " ["dvChars"] d327_289
                          return (charP c),
                       do d329_291 <- get
                          xx328_292 <- dvCharsM
                          case xx328_292 of
                              '"' -> return ()
                              _ -> throwErrorPackratM "'\"'" "not match pattern: " ["dvChars"] d329_291
                          let '"' = xx328_292
                          return ()
                          if True
                           then return ()
                           else throwErrorPackratM "True" "not match: " ["dvChars"] d329_291
                          d331_293 <- get
                          xx330_294 <- dv_stringLitM
                          let s = xx330_294
                          if True
                           then return ()
                           else throwErrorPackratM "True" "not match: " ["dv_stringLit"] d331_293
                          d333_295 <- get
                          xx332_296 <- dvCharsM
                          case xx332_296 of
                              '"' -> return ()
                              _ -> throwErrorPackratM "'\"'" "not match pattern: " ["dvChars"] d333_295
                          let '"' = xx332_296
                          return ()
                          if True
                           then return ()
                           else throwErrorPackratM "True" "not match: " ["dvChars"] d333_295
                          return (stringP s),
                       do d335_297 <- get
                          xx334_298 <- dvCharsM
                          case xx334_298 of
                              '(' -> return ()
                              _ -> throwErrorPackratM "'('" "not match pattern: " ["dvChars"] d335_297
                          let '(' = xx334_298
                          return ()
                          if True
                           then return ()
                           else throwErrorPackratM "True" "not match: " ["dvChars"] d335_297
                          d337_299 <- get
                          xx336_300 <- dv_patListM
                          let p = xx336_300
                          if True
                           then return ()
                           else throwErrorPackratM "True" "not match: " ["dv_patList"] d337_299
                          d339_301 <- get
                          xx338_302 <- dvCharsM
                          case xx338_302 of
                              ')' -> return ()
                              _ -> throwErrorPackratM "')'" "not match pattern: " ["dvChars"] d339_301
                          let ')' = xx338_302
                          return ()
                          if True
                           then return ()
                           else throwErrorPackratM "True" "not match: " ["dvChars"] d339_301
                          return (tupP p),
                       do d341_303 <- get
                          xx340_304 <- dvCharsM
                          case xx340_304 of
                              '[' -> return ()
                              _ -> throwErrorPackratM "'['" "not match pattern: " ["dvChars"] d341_303
                          let '[' = xx340_304
                          return ()
                          if True
                           then return ()
                           else throwErrorPackratM "True" "not match: " ["dvChars"] d341_303
                          d343_305 <- get
                          xx342_306 <- dv_patListM
                          let p = xx342_306
                          if True
                           then return ()
                           else throwErrorPackratM "True" "not match: " ["dv_patList"] d343_305
                          d345_307 <- get
                          xx344_308 <- dvCharsM
                          case xx344_308 of
                              ']' -> return ()
                              _ -> throwErrorPackratM "']'" "not match pattern: " ["dvChars"] d345_307
                          let ']' = xx344_308
                          return ()
                          if True
                           then return ()
                           else throwErrorPackratM "True" "not match: " ["dvChars"] d345_307
                          return (listP p)]
p_patList = foldl1 mplus [do d347_309 <- get
                             xx346_310 <- dv_patOpM
                             let p = xx346_310
                             if True
                              then return ()
                              else throwErrorPackratM "True" "not match: " ["dv_patOp"] d347_309
                             d349_311 <- get
                             _ <- dv_spacesM
                             if True
                              then return ()
                              else throwErrorPackratM "True" "not match: " ["dv_spaces"] d349_311
                             d351_312 <- get
                             xx350_313 <- dvCharsM
                             case xx350_313 of
                                 ',' -> return ()
                                 _ -> throwErrorPackratM "','" "not match pattern: " ["dvChars"] d351_312
                             let ',' = xx350_313
                             return ()
                             if True
                              then return ()
                              else throwErrorPackratM "True" "not match: " ["dvChars"] d351_312
                             d353_314 <- get
                             _ <- dv_spacesM
                             if True
                              then return ()
                              else throwErrorPackratM "True" "not match: " ["dv_spaces"] d353_314
                             d355_315 <- get
                             xx354_316 <- dv_patListM
                             let ps = xx354_316
                             if True
                              then return ()
                              else throwErrorPackratM "True" "not match: " ["dv_patList"] d355_315
                             return (p : ps),
                          do d357_317 <- get
                             xx356_318 <- dv_patOpM
                             let p = xx356_318
                             if True
                              then return ()
                              else throwErrorPackratM "True" "not match: " ["dv_patOp"] d357_317
                             return [p],
                          do return []]
p_opConName = foldl1 mplus [do d359_319 <- get
                               xx358_320 <- dvCharsM
                               case xx358_320 of
                                   ':' -> return ()
                                   _ -> throwErrorPackratM "':'" "not match pattern: " ["dvChars"] d359_319
                               let ':' = xx358_320
                               return ()
                               if True
                                then return ()
                                else throwErrorPackratM "True" "not match: " ["dvChars"] d359_319
                               d361_321 <- get
                               xx360_322 <- dv_opTailM
                               let ot = xx360_322
                               if True
                                then return ()
                                else throwErrorPackratM "True" "not match: " ["dv_opTail"] d361_321
                               return (mkName $ colon : ot)]
p_charLit = foldl1 mplus [do d363_323 <- get
                             xx362_324 <- dvCharsM
                             let c = xx362_324
                             if isAlphaNumOt c
                              then return ()
                              else throwErrorPackratM "isAlphaNumOt c" "not match: " ["dvChars"] d363_323
                             return c,
                          do d365_325 <- get
                             xx364_326 <- dvCharsM
                             case xx364_326 of
                                 '\\' -> return ()
                                 _ -> throwErrorPackratM "'\\\\'" "not match pattern: " ["dvChars"] d365_325
                             let '\\' = xx364_326
                             return ()
                             if True
                              then return ()
                              else throwErrorPackratM "True" "not match: " ["dvChars"] d365_325
                             d367_327 <- get
                             xx366_328 <- dvCharsM
                             let c = xx366_328
                             if elemNTs c
                              then return ()
                              else throwErrorPackratM "elemNTs c" "not match: " ["dvChars"] d367_327
                             return (getNTs c)]
p_stringLit = foldl1 mplus [do ddd368_329 <- get
                               flipMaybe "_:dq[True]" ddd368_329 ["dv_dq"] (do d370_330 <- get
                                                                               _ <- dv_dqM
                                                                               if True
                                                                                then return ()
                                                                                else throwErrorPackratM "True" "not match: " ["dv_dq"] d370_330)
                               put ddd368_329
                               d372_331 <- get
                               xx371_332 <- dvCharsM
                               let c = xx371_332
                               if True
                                then return ()
                                else throwErrorPackratM "True" "not match: " ["dvChars"] d372_331
                               d374_333 <- get
                               xx373_334 <- dv_stringLitM
                               let s = xx373_334
                               if True
                                then return ()
                                else throwErrorPackratM "True" "not match: " ["dv_stringLit"] d374_333
                               return (cons c s),
                            do return emp]
p_dq = foldl1 mplus [do d376_335 <- get
                        xx375_336 <- dvCharsM
                        case xx375_336 of
                            '"' -> return ()
                            _ -> throwErrorPackratM "'\"'" "not match pattern: " ["dvChars"] d376_335
                        let '"' = xx375_336
                        return ()
                        if True
                         then return ()
                         else throwErrorPackratM "True" "not match: " ["dvChars"] d376_335
                        return ()]
p_pats = foldl1 mplus [do d378_337 <- get
                          xx377_338 <- dv_patM
                          let p = xx377_338
                          if True
                           then return ()
                           else throwErrorPackratM "True" "not match: " ["dv_pat"] d378_337
                          d380_339 <- get
                          _ <- dv_spacesM
                          if True
                           then return ()
                           else throwErrorPackratM "True" "not match: " ["dv_spaces"] d380_339
                          d382_340 <- get
                          xx381_341 <- dv_patsM
                          let ps = xx381_341
                          if True
                           then return ()
                           else throwErrorPackratM "True" "not match: " ["dv_pats"] d382_340
                          return (cons p ps),
                       do return emp]
p_leaf = foldl1 mplus [do d384_342 <- get
                          xx383_343 <- dv_testM
                          let t = xx383_343
                          if True
                           then return ()
                           else throwErrorPackratM "True" "not match: " ["dv_test"] d384_342
                          return (boolLeaf t),
                       do d386_344 <- get
                          xx385_345 <- dv_readFromLsM
                          let rf = xx385_345
                          if True
                           then return ()
                           else throwErrorPackratM "True" "not match: " ["dv_readFromLs"] d386_344
                          d388_346 <- get
                          xx387_347 <- dv_testM
                          let t = xx387_347
                          if True
                           then return ()
                           else throwErrorPackratM "True" "not match: " ["dv_test"] d388_346
                          return (rf, t),
                       do d390_348 <- get
                          xx389_349 <- dv_readFromLsM
                          let rf = xx389_349
                          if True
                           then return ()
                           else throwErrorPackratM "True" "not match: " ["dv_readFromLs"] d390_348
                          return (rf, true)]
p_readFromLs = foldl1 mplus [do d392_350 <- get
                                xx391_351 <- dv_readFromM
                                let rf = xx391_351
                                if True
                                 then return ()
                                 else throwErrorPackratM "True" "not match: " ["dv_readFrom"] d392_350
                                d394_352 <- get
                                xx393_353 <- dvCharsM
                                case xx393_353 of
                                    '*' -> return ()
                                    _ -> throwErrorPackratM "'*'" "not match pattern: " ["dvChars"] d394_352
                                let '*' = xx393_353
                                return ()
                                if True
                                 then return ()
                                 else throwErrorPackratM "True" "not match: " ["dvChars"] d394_352
                                return (FromList rf),
                             do d396_354 <- get
                                xx395_355 <- dv_readFromM
                                let rf = xx395_355
                                if True
                                 then return ()
                                 else throwErrorPackratM "True" "not match: " ["dv_readFrom"] d396_354
                                d398_356 <- get
                                xx397_357 <- dvCharsM
                                case xx397_357 of
                                    '+' -> return ()
                                    _ -> throwErrorPackratM "'+'" "not match pattern: " ["dvChars"] d398_356
                                let '+' = xx397_357
                                return ()
                                if True
                                 then return ()
                                 else throwErrorPackratM "True" "not match: " ["dvChars"] d398_356
                                return (FromList1 rf),
                             do d400_358 <- get
                                xx399_359 <- dv_readFromM
                                let rf = xx399_359
                                if True
                                 then return ()
                                 else throwErrorPackratM "True" "not match: " ["dv_readFrom"] d400_358
                                d402_360 <- get
                                xx401_361 <- dvCharsM
                                case xx401_361 of
                                    '?' -> return ()
                                    _ -> throwErrorPackratM "'?'" "not match pattern: " ["dvChars"] d402_360
                                let '?' = xx401_361
                                return ()
                                if True
                                 then return ()
                                 else throwErrorPackratM "True" "not match: " ["dvChars"] d402_360
                                return (FromOptional rf),
                             do d404_362 <- get
                                xx403_363 <- dv_readFromM
                                let rf = xx403_363
                                if True
                                 then return ()
                                 else throwErrorPackratM "True" "not match: " ["dv_readFrom"] d404_362
                                return rf]
p_readFrom = foldl1 mplus [do d406_364 <- get
                              xx405_365 <- dv_variableM
                              let v = xx405_365
                              if True
                               then return ()
                               else throwErrorPackratM "True" "not match: " ["dv_variable"] d406_364
                              return (FromVariable v),
                           do d408_366 <- get
                              xx407_367 <- dvCharsM
                              case xx407_367 of
                                  '(' -> return ()
                                  _ -> throwErrorPackratM "'('" "not match pattern: " ["dvChars"] d408_366
                              let '(' = xx407_367
                              return ()
                              if True
                               then return ()
                               else throwErrorPackratM "True" "not match: " ["dvChars"] d408_366
                              d410_368 <- get
                              xx409_369 <- dv_selectionM
                              let s = xx409_369
                              if True
                               then return ()
                               else throwErrorPackratM "True" "not match: " ["dv_selection"] d410_368
                              d412_370 <- get
                              xx411_371 <- dvCharsM
                              case xx411_371 of
                                  ')' -> return ()
                                  _ -> throwErrorPackratM "')'" "not match pattern: " ["dvChars"] d412_370
                              let ')' = xx411_371
                              return ()
                              if True
                               then return ()
                               else throwErrorPackratM "True" "not match: " ["dvChars"] d412_370
                              return (FromSelection s)]
p_test = foldl1 mplus [do d414_372 <- get
                          xx413_373 <- dvCharsM
                          case xx413_373 of
                              '[' -> return ()
                              _ -> throwErrorPackratM "'['" "not match pattern: " ["dvChars"] d414_372
                          let '[' = xx413_373
                          return ()
                          if True
                           then return ()
                           else throwErrorPackratM "True" "not match: " ["dvChars"] d414_372
                          d416_374 <- get
                          xx415_375 <- dv_hsExpLamM
                          let h = xx415_375
                          if True
                           then return ()
                           else throwErrorPackratM "True" "not match: " ["dv_hsExpLam"] d416_374
                          d418_376 <- get
                          xx417_377 <- dvCharsM
                          case xx417_377 of
                              ']' -> return ()
                              _ -> throwErrorPackratM "']'" "not match pattern: " ["dvChars"] d418_376
                          let ']' = xx417_377
                          return ()
                          if True
                           then return ()
                           else throwErrorPackratM "True" "not match: " ["dvChars"] d418_376
                          return h]
p_hsExpLam = foldl1 mplus [do d420_378 <- get
                              xx419_379 <- dvCharsM
                              case xx419_379 of
                                  '\\' -> return ()
                                  _ -> throwErrorPackratM "'\\\\'" "not match pattern: " ["dvChars"] d420_378
                              let '\\' = xx419_379
                              return ()
                              if True
                               then return ()
                               else throwErrorPackratM "True" "not match: " ["dvChars"] d420_378
                              d422_380 <- get
                              _ <- dv_spacesM
                              if True
                               then return ()
                               else throwErrorPackratM "True" "not match: " ["dv_spaces"] d422_380
                              d424_381 <- get
                              xx423_382 <- dv_patsM
                              let ps = xx423_382
                              if True
                               then return ()
                               else throwErrorPackratM "True" "not match: " ["dv_pats"] d424_381
                              d426_383 <- get
                              _ <- dv_spacesM
                              if True
                               then return ()
                               else throwErrorPackratM "True" "not match: " ["dv_spaces"] d426_383
                              d428_384 <- get
                              xx427_385 <- dvCharsM
                              case xx427_385 of
                                  '-' -> return ()
                                  _ -> throwErrorPackratM "'-'" "not match pattern: " ["dvChars"] d428_384
                              let '-' = xx427_385
                              return ()
                              if True
                               then return ()
                               else throwErrorPackratM "True" "not match: " ["dvChars"] d428_384
                              d430_386 <- get
                              xx429_387 <- dvCharsM
                              let c = xx429_387
                              if isGt c
                               then return ()
                               else throwErrorPackratM "isGt c" "not match: " ["dvChars"] d430_386
                              d432_388 <- get
                              _ <- dv_spacesM
                              if True
                               then return ()
                               else throwErrorPackratM "True" "not match: " ["dv_spaces"] d432_388
                              d434_389 <- get
                              xx433_390 <- dv_hsExpTypM
                              let e = xx433_390
                              if True
                               then return ()
                               else throwErrorPackratM "True" "not match: " ["dv_hsExpTyp"] d434_389
                              return (lamE ps e),
                           do d436_391 <- get
                              xx435_392 <- dv_hsExpTypM
                              let e = xx435_392
                              if True
                               then return ()
                               else throwErrorPackratM "True" "not match: " ["dv_hsExpTyp"] d436_391
                              return e]
p_hsExpTyp = foldl1 mplus [do d438_393 <- get
                              xx437_394 <- dv_hsExpOpM
                              let eo = xx437_394
                              if True
                               then return ()
                               else throwErrorPackratM "True" "not match: " ["dv_hsExpOp"] d438_393
                              d440_395 <- get
                              xx439_396 <- dvCharsM
                              case xx439_396 of
                                  ':' -> return ()
                                  _ -> throwErrorPackratM "':'" "not match pattern: " ["dvChars"] d440_395
                              let ':' = xx439_396
                              return ()
                              if True
                               then return ()
                               else throwErrorPackratM "True" "not match: " ["dvChars"] d440_395
                              d442_397 <- get
                              xx441_398 <- dvCharsM
                              case xx441_398 of
                                  ':' -> return ()
                                  _ -> throwErrorPackratM "':'" "not match pattern: " ["dvChars"] d442_397
                              let ':' = xx441_398
                              return ()
                              if True
                               then return ()
                               else throwErrorPackratM "True" "not match: " ["dvChars"] d442_397
                              d444_399 <- get
                              _ <- dv_spacesM
                              if True
                               then return ()
                               else throwErrorPackratM "True" "not match: " ["dv_spaces"] d444_399
                              d446_400 <- get
                              xx445_401 <- dv_hsTypeArrM
                              let t = xx445_401
                              if True
                               then return ()
                               else throwErrorPackratM "True" "not match: " ["dv_hsTypeArr"] d446_400
                              return (sigE eo t),
                           do d448_402 <- get
                              xx447_403 <- dv_hsExpOpM
                              let eo = xx447_403
                              if True
                               then return ()
                               else throwErrorPackratM "True" "not match: " ["dv_hsExpOp"] d448_402
                              return eo]
p_hsExpOp = foldl1 mplus [do d450_404 <- get
                             xx449_405 <- dv_hsExpM
                             let l = xx449_405
                             if True
                              then return ()
                              else throwErrorPackratM "True" "not match: " ["dv_hsExp"] d450_404
                             d452_406 <- get
                             _ <- dv_spacesM
                             if True
                              then return ()
                              else throwErrorPackratM "True" "not match: " ["dv_spaces"] d452_406
                             d454_407 <- get
                             xx453_408 <- dv_hsOpM
                             let o = xx453_408
                             if True
                              then return ()
                              else throwErrorPackratM "True" "not match: " ["dv_hsOp"] d454_407
                             d456_409 <- get
                             _ <- dv_spacesM
                             if True
                              then return ()
                              else throwErrorPackratM "True" "not match: " ["dv_spaces"] d456_409
                             d458_410 <- get
                             xx457_411 <- dv_hsExpOpM
                             let r = xx457_411
                             if True
                              then return ()
                              else throwErrorPackratM "True" "not match: " ["dv_hsExpOp"] d458_410
                             return (uInfixE (getEx l) o r),
                          do d460_412 <- get
                             xx459_413 <- dv_hsExpM
                             let e = xx459_413
                             if True
                              then return ()
                              else throwErrorPackratM "True" "not match: " ["dv_hsExp"] d460_412
                             return (getEx e)]
p_hsOp = foldl1 mplus [do d462_414 <- get
                          xx461_415 <- dvCharsM
                          let c = xx461_415
                          if isOpHeadChar c
                           then return ()
                           else throwErrorPackratM "isOpHeadChar c" "not match: " ["dvChars"] d462_414
                          d464_416 <- get
                          xx463_417 <- dv_opTailM
                          let o = xx463_417
                          if True
                           then return ()
                           else throwErrorPackratM "True" "not match: " ["dv_opTail"] d464_416
                          return (varE (mkName (cons c o))),
                       do d466_418 <- get
                          xx465_419 <- dvCharsM
                          case xx465_419 of
                              ':' -> return ()
                              _ -> throwErrorPackratM "':'" "not match pattern: " ["dvChars"] d466_418
                          let ':' = xx465_419
                          return ()
                          if True
                           then return ()
                           else throwErrorPackratM "True" "not match: " ["dvChars"] d466_418
                          ddd467_420 <- get
                          flipMaybe "':':[True]" ddd467_420 ["dvChars"] (do d469_421 <- get
                                                                            xx468_422 <- dvCharsM
                                                                            case xx468_422 of
                                                                                ':' -> return ()
                                                                                _ -> throwErrorPackratM "':'" "not match pattern: " ["dvChars"] d469_421
                                                                            let ':' = xx468_422
                                                                            return ()
                                                                            if True
                                                                             then return ()
                                                                             else throwErrorPackratM "True" "not match: " ["dvChars"] d469_421)
                          put ddd467_420
                          d471_423 <- get
                          xx470_424 <- dv_opTailM
                          let o = xx470_424
                          if True
                           then return ()
                           else throwErrorPackratM "True" "not match: " ["dv_opTail"] d471_423
                          return (conE (mkName (':' : o))),
                       do d473_425 <- get
                          xx472_426 <- dvCharsM
                          let c = xx472_426
                          if isBQ c
                           then return ()
                           else throwErrorPackratM "isBQ c" "not match: " ["dvChars"] d473_425
                          d475_427 <- get
                          xx474_428 <- dv_variableM
                          let v = xx474_428
                          if True
                           then return ()
                           else throwErrorPackratM "True" "not match: " ["dv_variable"] d475_427
                          d477_429 <- get
                          xx476_430 <- dvCharsM
                          let c_ = xx476_430
                          if isBQ c_
                           then return ()
                           else throwErrorPackratM "isBQ c_" "not match: " ["dvChars"] d477_429
                          return (varE (mkName v)),
                       do d479_431 <- get
                          xx478_432 <- dvCharsM
                          let c = xx478_432
                          if isBQ c
                           then return ()
                           else throwErrorPackratM "isBQ c" "not match: " ["dvChars"] d479_431
                          d481_433 <- get
                          xx480_434 <- dv_typM
                          let t = xx480_434
                          if True
                           then return ()
                           else throwErrorPackratM "True" "not match: " ["dv_typ"] d481_433
                          d483_435 <- get
                          xx482_436 <- dvCharsM
                          let c_ = xx482_436
                          if isBQ c_
                           then return ()
                           else throwErrorPackratM "isBQ c_" "not match: " ["dvChars"] d483_435
                          return (conE (mkName t))]
p_opTail = foldl1 mplus [do d485_437 <- get
                            xx484_438 <- dvCharsM
                            let c = xx484_438
                            if isOpTailChar c
                             then return ()
                             else throwErrorPackratM "isOpTailChar c" "not match: " ["dvChars"] d485_437
                            d487_439 <- get
                            xx486_440 <- dv_opTailM
                            let s = xx486_440
                            if True
                             then return ()
                             else throwErrorPackratM "True" "not match: " ["dv_opTail"] d487_439
                            return (cons c s),
                         do return emp]
p_hsExp = foldl1 mplus [do d489_441 <- get
                           xx488_442 <- dv_hsExp1M
                           let e = xx488_442
                           if True
                            then return ()
                            else throwErrorPackratM "True" "not match: " ["dv_hsExp1"] d489_441
                           d491_443 <- get
                           _ <- dv_spacesM
                           if True
                            then return ()
                            else throwErrorPackratM "True" "not match: " ["dv_spaces"] d491_443
                           d493_444 <- get
                           xx492_445 <- dv_hsExpM
                           let h = xx492_445
                           if True
                            then return ()
                            else throwErrorPackratM "True" "not match: " ["dv_hsExp"] d493_444
                           return (applyExR e h),
                        do d495_446 <- get
                           xx494_447 <- dv_hsExp1M
                           let e = xx494_447
                           if True
                            then return ()
                            else throwErrorPackratM "True" "not match: " ["dv_hsExp1"] d495_446
                           return (toEx e)]
p_hsExp1 = foldl1 mplus [do d497_448 <- get
                            xx496_449 <- dvCharsM
                            case xx496_449 of
                                '(' -> return ()
                                _ -> throwErrorPackratM "'('" "not match pattern: " ["dvChars"] d497_448
                            let '(' = xx496_449
                            return ()
                            if True
                             then return ()
                             else throwErrorPackratM "True" "not match: " ["dvChars"] d497_448
                            d499_450 <- get
                            xx498_451 <- papOptional (foldl1 mplus [do d501_452 <- get
                                                                       xx500_453 <- dv_hsExpTypM
                                                                       let e = xx500_453
                                                                       if True
                                                                        then return ()
                                                                        else throwErrorPackratM "True" "not match: " ["dv_hsExpTyp"] d501_452
                                                                       return e])
                            let l = xx498_451
                            if True
                             then return ()
                             else throwErrorPackratM "True" "not match: " ["dv_hsExpTyp"] d499_450
                            d503_454 <- get
                            _ <- dv_spacesM
                            if True
                             then return ()
                             else throwErrorPackratM "True" "not match: " ["dv_spaces"] d503_454
                            d505_455 <- get
                            xx504_456 <- dv_hsOpM
                            let o = xx504_456
                            if True
                             then return ()
                             else throwErrorPackratM "True" "not match: " ["dv_hsOp"] d505_455
                            d507_457 <- get
                            _ <- dv_spacesM
                            if True
                             then return ()
                             else throwErrorPackratM "True" "not match: " ["dv_spaces"] d507_457
                            d509_458 <- get
                            xx508_459 <- papOptional (foldl1 mplus [do d511_460 <- get
                                                                       xx510_461 <- dv_hsExpTypM
                                                                       let e = xx510_461
                                                                       if True
                                                                        then return ()
                                                                        else throwErrorPackratM "True" "not match: " ["dv_hsExpTyp"] d511_460
                                                                       return e])
                            let r = xx508_459
                            if True
                             then return ()
                             else throwErrorPackratM "True" "not match: " ["dv_hsExpTyp"] d509_458
                            d513_462 <- get
                            xx512_463 <- dvCharsM
                            case xx512_463 of
                                ')' -> return ()
                                _ -> throwErrorPackratM "')'" "not match pattern: " ["dvChars"] d513_462
                            let ')' = xx512_463
                            return ()
                            if True
                             then return ()
                             else throwErrorPackratM "True" "not match: " ["dvChars"] d513_462
                            return (infixE l o r),
                         do d515_464 <- get
                            xx514_465 <- dvCharsM
                            case xx514_465 of
                                '(' -> return ()
                                _ -> throwErrorPackratM "'('" "not match pattern: " ["dvChars"] d515_464
                            let '(' = xx514_465
                            return ()
                            if True
                             then return ()
                             else throwErrorPackratM "True" "not match: " ["dvChars"] d515_464
                            d517_466 <- get
                            xx516_467 <- dv_hsExpTplM
                            let et = xx516_467
                            if True
                             then return ()
                             else throwErrorPackratM "True" "not match: " ["dv_hsExpTpl"] d517_466
                            d519_468 <- get
                            xx518_469 <- dvCharsM
                            case xx518_469 of
                                ')' -> return ()
                                _ -> throwErrorPackratM "')'" "not match pattern: " ["dvChars"] d519_468
                            let ')' = xx518_469
                            return ()
                            if True
                             then return ()
                             else throwErrorPackratM "True" "not match: " ["dvChars"] d519_468
                            return (tupE et),
                         do d521_470 <- get
                            xx520_471 <- dvCharsM
                            case xx520_471 of
                                '[' -> return ()
                                _ -> throwErrorPackratM "'['" "not match pattern: " ["dvChars"] d521_470
                            let '[' = xx520_471
                            return ()
                            if True
                             then return ()
                             else throwErrorPackratM "True" "not match: " ["dvChars"] d521_470
                            d523_472 <- get
                            xx522_473 <- dv_hsExpTplM
                            let et = xx522_473
                            if True
                             then return ()
                             else throwErrorPackratM "True" "not match: " ["dv_hsExpTpl"] d523_472
                            d525_474 <- get
                            xx524_475 <- dvCharsM
                            case xx524_475 of
                                ']' -> return ()
                                _ -> throwErrorPackratM "']'" "not match pattern: " ["dvChars"] d525_474
                            let ']' = xx524_475
                            return ()
                            if True
                             then return ()
                             else throwErrorPackratM "True" "not match: " ["dvChars"] d525_474
                            return (listE et),
                         do d527_476 <- get
                            xx526_477 <- dv_variableM
                            let v = xx526_477
                            if True
                             then return ()
                             else throwErrorPackratM "True" "not match: " ["dv_variable"] d527_476
                            return (varE (mkName v)),
                         do d529_478 <- get
                            xx528_479 <- dv_typM
                            let t = xx528_479
                            if True
                             then return ()
                             else throwErrorPackratM "True" "not match: " ["dv_typ"] d529_478
                            return (conE (mkName t)),
                         do d531_480 <- get
                            xx530_481 <- dv_integerM
                            let i = xx530_481
                            if True
                             then return ()
                             else throwErrorPackratM "True" "not match: " ["dv_integer"] d531_480
                            d533_482 <- get
                            _ <- dv_spacesM
                            if True
                             then return ()
                             else throwErrorPackratM "True" "not match: " ["dv_spaces"] d533_482
                            return (litE (integerL i)),
                         do d535_483 <- get
                            xx534_484 <- dvCharsM
                            case xx534_484 of
                                '\'' -> return ()
                                _ -> throwErrorPackratM "'\\''" "not match pattern: " ["dvChars"] d535_483
                            let '\'' = xx534_484
                            return ()
                            if True
                             then return ()
                             else throwErrorPackratM "True" "not match: " ["dvChars"] d535_483
                            d537_485 <- get
                            xx536_486 <- dv_charLitM
                            let c = xx536_486
                            if True
                             then return ()
                             else throwErrorPackratM "True" "not match: " ["dv_charLit"] d537_485
                            d539_487 <- get
                            xx538_488 <- dvCharsM
                            case xx538_488 of
                                '\'' -> return ()
                                _ -> throwErrorPackratM "'\\''" "not match pattern: " ["dvChars"] d539_487
                            let '\'' = xx538_488
                            return ()
                            if True
                             then return ()
                             else throwErrorPackratM "True" "not match: " ["dvChars"] d539_487
                            return (litE (charL c)),
                         do d541_489 <- get
                            xx540_490 <- dvCharsM
                            case xx540_490 of
                                '"' -> return ()
                                _ -> throwErrorPackratM "'\"'" "not match pattern: " ["dvChars"] d541_489
                            let '"' = xx540_490
                            return ()
                            if True
                             then return ()
                             else throwErrorPackratM "True" "not match: " ["dvChars"] d541_489
                            d543_491 <- get
                            xx542_492 <- dv_stringLitM
                            let s = xx542_492
                            if True
                             then return ()
                             else throwErrorPackratM "True" "not match: " ["dv_stringLit"] d543_491
                            d545_493 <- get
                            xx544_494 <- dvCharsM
                            case xx544_494 of
                                '"' -> return ()
                                _ -> throwErrorPackratM "'\"'" "not match pattern: " ["dvChars"] d545_493
                            let '"' = xx544_494
                            return ()
                            if True
                             then return ()
                             else throwErrorPackratM "True" "not match: " ["dvChars"] d545_493
                            return (litE (stringL s)),
                         do d547_495 <- get
                            xx546_496 <- dvCharsM
                            case xx546_496 of
                                '-' -> return ()
                                _ -> throwErrorPackratM "'-'" "not match pattern: " ["dvChars"] d547_495
                            let '-' = xx546_496
                            return ()
                            if True
                             then return ()
                             else throwErrorPackratM "True" "not match: " ["dvChars"] d547_495
                            d549_497 <- get
                            _ <- dv_spacesM
                            if True
                             then return ()
                             else throwErrorPackratM "True" "not match: " ["dv_spaces"] d549_497
                            d551_498 <- get
                            xx550_499 <- dv_hsExp1M
                            let e = xx550_499
                            if True
                             then return ()
                             else throwErrorPackratM "True" "not match: " ["dv_hsExp1"] d551_498
                            return (appE (varE $ mkName "negate") e)]
p_hsExpTpl = foldl1 mplus [do d553_500 <- get
                              xx552_501 <- dv_hsExpLamM
                              let e = xx552_501
                              if True
                               then return ()
                               else throwErrorPackratM "True" "not match: " ["dv_hsExpLam"] d553_500
                              d555_502 <- get
                              _ <- dv_spacesM
                              if True
                               then return ()
                               else throwErrorPackratM "True" "not match: " ["dv_spaces"] d555_502
                              d557_503 <- get
                              xx556_504 <- dvCharsM
                              let c = xx556_504
                              if isComma c
                               then return ()
                               else throwErrorPackratM "isComma c" "not match: " ["dvChars"] d557_503
                              d559_505 <- get
                              _ <- dv_spacesM
                              if True
                               then return ()
                               else throwErrorPackratM "True" "not match: " ["dv_spaces"] d559_505
                              d561_506 <- get
                              xx560_507 <- dv_hsExpTplM
                              let et = xx560_507
                              if True
                               then return ()
                               else throwErrorPackratM "True" "not match: " ["dv_hsExpTpl"] d561_506
                              return (cons e et),
                           do d563_508 <- get
                              xx562_509 <- dv_hsExpLamM
                              let e = xx562_509
                              if True
                               then return ()
                               else throwErrorPackratM "True" "not match: " ["dv_hsExpLam"] d563_508
                              return (cons e emp),
                           do return emp]
p_hsTypeArr = foldl1 mplus [do d565_510 <- get
                               xx564_511 <- dv_hsTypeM
                               let l = xx564_511
                               if True
                                then return ()
                                else throwErrorPackratM "True" "not match: " ["dv_hsType"] d565_510
                               d567_512 <- get
                               xx566_513 <- dvCharsM
                               case xx566_513 of
                                   '-' -> return ()
                                   _ -> throwErrorPackratM "'-'" "not match pattern: " ["dvChars"] d567_512
                               let '-' = xx566_513
                               return ()
                               if True
                                then return ()
                                else throwErrorPackratM "True" "not match: " ["dvChars"] d567_512
                               d569_514 <- get
                               xx568_515 <- dvCharsM
                               let c = xx568_515
                               if isGt c
                                then return ()
                                else throwErrorPackratM "isGt c" "not match: " ["dvChars"] d569_514
                               d571_516 <- get
                               _ <- dv_spacesM
                               if True
                                then return ()
                                else throwErrorPackratM "True" "not match: " ["dv_spaces"] d571_516
                               d573_517 <- get
                               xx572_518 <- dv_hsTypeArrM
                               let r = xx572_518
                               if True
                                then return ()
                                else throwErrorPackratM "True" "not match: " ["dv_hsTypeArr"] d573_517
                               return (appT (appT arrowT (getTyp l)) r),
                            do d575_519 <- get
                               xx574_520 <- dv_hsTypeM
                               let t = xx574_520
                               if True
                                then return ()
                                else throwErrorPackratM "True" "not match: " ["dv_hsType"] d575_519
                               return (getTyp t)]
p_hsType = foldl1 mplus [do d577_521 <- get
                            xx576_522 <- dv_hsType1M
                            let t = xx576_522
                            if True
                             then return ()
                             else throwErrorPackratM "True" "not match: " ["dv_hsType1"] d577_521
                            d579_523 <- get
                            xx578_524 <- dv_hsTypeM
                            let ts = xx578_524
                            if True
                             then return ()
                             else throwErrorPackratM "True" "not match: " ["dv_hsType"] d579_523
                            return (applyTyp (toTyp t) ts),
                         do d581_525 <- get
                            xx580_526 <- dv_hsType1M
                            let t = xx580_526
                            if True
                             then return ()
                             else throwErrorPackratM "True" "not match: " ["dv_hsType1"] d581_525
                            return (toTyp t)]
p_hsType1 = foldl1 mplus [do d583_527 <- get
                             xx582_528 <- dvCharsM
                             case xx582_528 of
                                 '[' -> return ()
                                 _ -> throwErrorPackratM "'['" "not match pattern: " ["dvChars"] d583_527
                             let '[' = xx582_528
                             return ()
                             if True
                              then return ()
                              else throwErrorPackratM "True" "not match: " ["dvChars"] d583_527
                             d585_529 <- get
                             xx584_530 <- dvCharsM
                             case xx584_530 of
                                 ']' -> return ()
                                 _ -> throwErrorPackratM "']'" "not match pattern: " ["dvChars"] d585_529
                             let ']' = xx584_530
                             return ()
                             if True
                              then return ()
                              else throwErrorPackratM "True" "not match: " ["dvChars"] d585_529
                             d587_531 <- get
                             _ <- dv_spacesM
                             if True
                              then return ()
                              else throwErrorPackratM "True" "not match: " ["dv_spaces"] d587_531
                             return listT,
                          do d589_532 <- get
                             xx588_533 <- dvCharsM
                             case xx588_533 of
                                 '[' -> return ()
                                 _ -> throwErrorPackratM "'['" "not match pattern: " ["dvChars"] d589_532
                             let '[' = xx588_533
                             return ()
                             if True
                              then return ()
                              else throwErrorPackratM "True" "not match: " ["dvChars"] d589_532
                             d591_534 <- get
                             xx590_535 <- dv_hsTypeArrM
                             let t = xx590_535
                             if True
                              then return ()
                              else throwErrorPackratM "True" "not match: " ["dv_hsTypeArr"] d591_534
                             d593_536 <- get
                             xx592_537 <- dvCharsM
                             case xx592_537 of
                                 ']' -> return ()
                                 _ -> throwErrorPackratM "']'" "not match pattern: " ["dvChars"] d593_536
                             let ']' = xx592_537
                             return ()
                             if True
                              then return ()
                              else throwErrorPackratM "True" "not match: " ["dvChars"] d593_536
                             d595_538 <- get
                             _ <- dv_spacesM
                             if True
                              then return ()
                              else throwErrorPackratM "True" "not match: " ["dv_spaces"] d595_538
                             return (appT listT t),
                          do d597_539 <- get
                             xx596_540 <- dvCharsM
                             case xx596_540 of
                                 '(' -> return ()
                                 _ -> throwErrorPackratM "'('" "not match pattern: " ["dvChars"] d597_539
                             let '(' = xx596_540
                             return ()
                             if True
                              then return ()
                              else throwErrorPackratM "True" "not match: " ["dvChars"] d597_539
                             d599_541 <- get
                             _ <- dv_spacesM
                             if True
                              then return ()
                              else throwErrorPackratM "True" "not match: " ["dv_spaces"] d599_541
                             d601_542 <- get
                             xx600_543 <- dv_hsTypeTplM
                             let tt = xx600_543
                             if True
                              then return ()
                              else throwErrorPackratM "True" "not match: " ["dv_hsTypeTpl"] d601_542
                             d603_544 <- get
                             xx602_545 <- dvCharsM
                             case xx602_545 of
                                 ')' -> return ()
                                 _ -> throwErrorPackratM "')'" "not match pattern: " ["dvChars"] d603_544
                             let ')' = xx602_545
                             return ()
                             if True
                              then return ()
                              else throwErrorPackratM "True" "not match: " ["dvChars"] d603_544
                             return (tupT tt),
                          do d605_546 <- get
                             xx604_547 <- dv_typTokenM
                             let t = xx604_547
                             if True
                              then return ()
                              else throwErrorPackratM "True" "not match: " ["dv_typToken"] d605_546
                             return (conT (mkName t)),
                          do d607_548 <- get
                             xx606_549 <- dvCharsM
                             case xx606_549 of
                                 '(' -> return ()
                                 _ -> throwErrorPackratM "'('" "not match pattern: " ["dvChars"] d607_548
                             let '(' = xx606_549
                             return ()
                             if True
                              then return ()
                              else throwErrorPackratM "True" "not match: " ["dvChars"] d607_548
                             d609_550 <- get
                             xx608_551 <- dvCharsM
                             case xx608_551 of
                                 '-' -> return ()
                                 _ -> throwErrorPackratM "'-'" "not match pattern: " ["dvChars"] d609_550
                             let '-' = xx608_551
                             return ()
                             if True
                              then return ()
                              else throwErrorPackratM "True" "not match: " ["dvChars"] d609_550
                             d611_552 <- get
                             xx610_553 <- dvCharsM
                             let c = xx610_553
                             if isGt c
                              then return ()
                              else throwErrorPackratM "isGt c" "not match: " ["dvChars"] d611_552
                             d613_554 <- get
                             xx612_555 <- dvCharsM
                             case xx612_555 of
                                 ')' -> return ()
                                 _ -> throwErrorPackratM "')'" "not match pattern: " ["dvChars"] d613_554
                             let ')' = xx612_555
                             return ()
                             if True
                              then return ()
                              else throwErrorPackratM "True" "not match: " ["dvChars"] d613_554
                             d615_556 <- get
                             _ <- dv_spacesM
                             if True
                              then return ()
                              else throwErrorPackratM "True" "not match: " ["dv_spaces"] d615_556
                             return arrowT]
p_hsTypeTpl = foldl1 mplus [do d617_557 <- get
                               xx616_558 <- dv_hsTypeArrM
                               let t = xx616_558
                               if True
                                then return ()
                                else throwErrorPackratM "True" "not match: " ["dv_hsTypeArr"] d617_557
                               d619_559 <- get
                               xx618_560 <- dvCharsM
                               let c = xx618_560
                               if isComma c
                                then return ()
                                else throwErrorPackratM "isComma c" "not match: " ["dvChars"] d619_559
                               d621_561 <- get
                               _ <- dv_spacesM
                               if True
                                then return ()
                                else throwErrorPackratM "True" "not match: " ["dv_spaces"] d621_561
                               d623_562 <- get
                               xx622_563 <- dv_hsTypeTplM
                               let tt = xx622_563
                               if True
                                then return ()
                                else throwErrorPackratM "True" "not match: " ["dv_hsTypeTpl"] d623_562
                               return (cons t tt),
                            do d625_564 <- get
                               xx624_565 <- dv_hsTypeArrM
                               let t = xx624_565
                               if True
                                then return ()
                                else throwErrorPackratM "True" "not match: " ["dv_hsTypeArr"] d625_564
                               return (cons t emp),
                            do return emp]
p_typ = foldl1 mplus [do d627_566 <- get
                         xx626_567 <- dv_upperM
                         let u = xx626_567
                         if True
                          then return ()
                          else throwErrorPackratM "True" "not match: " ["dv_upper"] d627_566
                         d629_568 <- get
                         xx628_569 <- dv_tvtailM
                         let t = xx628_569
                         if True
                          then return ()
                          else throwErrorPackratM "True" "not match: " ["dv_tvtail"] d629_568
                         return (cons u t)]
p_variable = foldl1 mplus [do d631_570 <- get
                              xx630_571 <- dv_lowerM
                              let l = xx630_571
                              if True
                               then return ()
                               else throwErrorPackratM "True" "not match: " ["dv_lower"] d631_570
                              d633_572 <- get
                              xx632_573 <- dv_tvtailM
                              let t = xx632_573
                              if True
                               then return ()
                               else throwErrorPackratM "True" "not match: " ["dv_tvtail"] d633_572
                              return (cons l t)]
p_tvtail = foldl1 mplus [do d635_574 <- get
                            xx634_575 <- dv_alphaM
                            let a = xx634_575
                            if True
                             then return ()
                             else throwErrorPackratM "True" "not match: " ["dv_alpha"] d635_574
                            d637_576 <- get
                            xx636_577 <- dv_tvtailM
                            let t = xx636_577
                            if True
                             then return ()
                             else throwErrorPackratM "True" "not match: " ["dv_tvtail"] d637_576
                            return (cons a t),
                         do return emp]
p_integer = foldl1 mplus [do d639_578 <- get
                             xx638_579 <- dv_digitM
                             let dh = xx638_579
                             if True
                              then return ()
                              else throwErrorPackratM "True" "not match: " ["dv_digit"] d639_578
                             d641_580 <- get
                             xx640_581 <- list (foldl1 mplus [do d643_582 <- get
                                                                 xx642_583 <- dv_digitM
                                                                 let d = xx642_583
                                                                 if True
                                                                  then return ()
                                                                  else throwErrorPackratM "True" "not match: " ["dv_digit"] d643_582
                                                                 return d])
                             let ds = xx640_581
                             if True
                              then return ()
                              else throwErrorPackratM "True" "not match: " ["dv_digit"] d641_580
                             return (read (cons dh ds))]
p_alpha = foldl1 mplus [do d645_584 <- get
                           xx644_585 <- dv_upperM
                           let u = xx644_585
                           if True
                            then return ()
                            else throwErrorPackratM "True" "not match: " ["dv_upper"] d645_584
                           return u,
                        do d647_586 <- get
                           xx646_587 <- dv_lowerM
                           let l = xx646_587
                           if True
                            then return ()
                            else throwErrorPackratM "True" "not match: " ["dv_lower"] d647_586
                           return l,
                        do d649_588 <- get
                           xx648_589 <- dv_digitM
                           let d = xx648_589
                           if True
                            then return ()
                            else throwErrorPackratM "True" "not match: " ["dv_digit"] d649_588
                           return d]
p_upper = foldl1 mplus [do d651_590 <- get
                           xx650_591 <- dvCharsM
                           let u = xx650_591
                           if isUpper u
                            then return ()
                            else throwErrorPackratM "isUpper u" "not match: " ["dvChars"] d651_590
                           return u]
p_lower = foldl1 mplus [do d653_592 <- get
                           xx652_593 <- dvCharsM
                           let l = xx652_593
                           if isLowerU l
                            then return ()
                            else throwErrorPackratM "isLowerU l" "not match: " ["dvChars"] d653_592
                           return l]
p_digit = foldl1 mplus [do d655_594 <- get
                           xx654_595 <- dvCharsM
                           let d = xx654_595
                           if isDigit d
                            then return ()
                            else throwErrorPackratM "isDigit d" "not match: " ["dvChars"] d655_594
                           return d]
p_spaces = foldl1 mplus [do d657_596 <- get
                            _ <- dv_spaceM
                            if True
                             then return ()
                             else throwErrorPackratM "True" "not match: " ["dv_space"] d657_596
                            d659_597 <- get
                            _ <- dv_spacesM
                            if True
                             then return ()
                             else throwErrorPackratM "True" "not match: " ["dv_spaces"] d659_597
                            return (),
                         do return ()]
p_space = foldl1 mplus [do d661_598 <- get
                           xx660_599 <- dvCharsM
                           let s = xx660_599
                           if isSpace s
                            then return ()
                            else throwErrorPackratM "isSpace s" "not match: " ["dvChars"] d661_598
                           return (),
                        do d663_600 <- get
                           xx662_601 <- dvCharsM
                           case xx662_601 of
                               '-' -> return ()
                               _ -> throwErrorPackratM "'-'" "not match pattern: " ["dvChars"] d663_600
                           let '-' = xx662_601
                           return ()
                           if True
                            then return ()
                            else throwErrorPackratM "True" "not match: " ["dvChars"] d663_600
                           d665_602 <- get
                           xx664_603 <- dvCharsM
                           case xx664_603 of
                               '-' -> return ()
                               _ -> throwErrorPackratM "'-'" "not match pattern: " ["dvChars"] d665_602
                           let '-' = xx664_603
                           return ()
                           if True
                            then return ()
                            else throwErrorPackratM "True" "not match: " ["dvChars"] d665_602
                           d667_604 <- get
                           _ <- dv_notNLStringM
                           if True
                            then return ()
                            else throwErrorPackratM "True" "not match: " ["dv_notNLString"] d667_604
                           d669_605 <- get
                           _ <- dv_nlM
                           if True
                            then return ()
                            else throwErrorPackratM "True" "not match: " ["dv_nl"] d669_605
                           return (),
                        do d671_606 <- get
                           _ <- dv_commentM
                           if True
                            then return ()
                            else throwErrorPackratM "True" "not match: " ["dv_comment"] d671_606
                           return ()]
p_notNLString = foldl1 mplus [do ddd672_607 <- get
                                 flipMaybe "_:nl[True]" ddd672_607 ["dv_nl"] (do d674_608 <- get
                                                                                 _ <- dv_nlM
                                                                                 if True
                                                                                  then return ()
                                                                                  else throwErrorPackratM "True" "not match: " ["dv_nl"] d674_608)
                                 put ddd672_607
                                 d676_609 <- get
                                 xx675_610 <- dvCharsM
                                 let c = xx675_610
                                 if True
                                  then return ()
                                  else throwErrorPackratM "True" "not match: " ["dvChars"] d676_609
                                 d678_611 <- get
                                 xx677_612 <- dv_notNLStringM
                                 let s = xx677_612
                                 if True
                                  then return ()
                                  else throwErrorPackratM "True" "not match: " ["dv_notNLString"] d678_611
                                 return (cons c s),
                              do return emp]
p_nl = foldl1 mplus [do d680_613 <- get
                        xx679_614 <- dvCharsM
                        case xx679_614 of
                            '\n' -> return ()
                            _ -> throwErrorPackratM "'\\n'" "not match pattern: " ["dvChars"] d680_613
                        let '\n' = xx679_614
                        return ()
                        if True
                         then return ()
                         else throwErrorPackratM "True" "not match: " ["dvChars"] d680_613
                        return ()]
p_comment = foldl1 mplus [do d682_615 <- get
                             xx681_616 <- dvCharsM
                             case xx681_616 of
                                 '{' -> return ()
                                 _ -> throwErrorPackratM "'{'" "not match pattern: " ["dvChars"] d682_615
                             let '{' = xx681_616
                             return ()
                             if True
                              then return ()
                              else throwErrorPackratM "True" "not match: " ["dvChars"] d682_615
                             d684_617 <- get
                             xx683_618 <- dvCharsM
                             case xx683_618 of
                                 '-' -> return ()
                                 _ -> throwErrorPackratM "'-'" "not match pattern: " ["dvChars"] d684_617
                             let '-' = xx683_618
                             return ()
                             if True
                              then return ()
                              else throwErrorPackratM "True" "not match: " ["dvChars"] d684_617
                             ddd685_619 <- get
                             flipMaybe "'#':[True]" ddd685_619 ["dvChars"] (do d687_620 <- get
                                                                               xx686_621 <- dvCharsM
                                                                               case xx686_621 of
                                                                                   '#' -> return ()
                                                                                   _ -> throwErrorPackratM "'#'" "not match pattern: " ["dvChars"] d687_620
                                                                               let '#' = xx686_621
                                                                               return ()
                                                                               if True
                                                                                then return ()
                                                                                else throwErrorPackratM "True" "not match: " ["dvChars"] d687_620)
                             put ddd685_619
                             d689_622 <- get
                             _ <- dv_commentsM
                             if True
                              then return ()
                              else throwErrorPackratM "True" "not match: " ["dv_comments"] d689_622
                             d691_623 <- get
                             _ <- dv_comEndM
                             if True
                              then return ()
                              else throwErrorPackratM "True" "not match: " ["dv_comEnd"] d691_623
                             return ()]
p_comments = foldl1 mplus [do d693_624 <- get
                              _ <- dv_notComStrM
                              if True
                               then return ()
                               else throwErrorPackratM "True" "not match: " ["dv_notComStr"] d693_624
                              d695_625 <- get
                              _ <- dv_commentM
                              if True
                               then return ()
                               else throwErrorPackratM "True" "not match: " ["dv_comment"] d695_625
                              d697_626 <- get
                              _ <- dv_commentsM
                              if True
                               then return ()
                               else throwErrorPackratM "True" "not match: " ["dv_comments"] d697_626
                              return (),
                           do d699_627 <- get
                              _ <- dv_notComStrM
                              if True
                               then return ()
                               else throwErrorPackratM "True" "not match: " ["dv_notComStr"] d699_627
                              return ()]
p_notComStr = foldl1 mplus [do ddd700_628 <- get
                               flipMaybe "_:comment[True]" ddd700_628 ["dv_comment"] (do d702_629 <- get
                                                                                         _ <- dv_commentM
                                                                                         if True
                                                                                          then return ()
                                                                                          else throwErrorPackratM "True" "not match: " ["dv_comment"] d702_629)
                               put ddd700_628
                               ddd703_630 <- get
                               flipMaybe "_:comEnd[True]" ddd703_630 ["dv_comEnd"] (do d705_631 <- get
                                                                                       _ <- dv_comEndM
                                                                                       if True
                                                                                        then return ()
                                                                                        else throwErrorPackratM "True" "not match: " ["dv_comEnd"] d705_631)
                               put ddd703_630
                               d707_632 <- get
                               _ <- dvCharsM
                               if True
                                then return ()
                                else throwErrorPackratM "True" "not match: " ["dvChars"] d707_632
                               d709_633 <- get
                               _ <- dv_notComStrM
                               if True
                                then return ()
                                else throwErrorPackratM "True" "not match: " ["dv_notComStr"] d709_633
                               return (),
                            do return ()]
p_comEnd = foldl1 mplus [do d711_634 <- get
                            xx710_635 <- dvCharsM
                            case xx710_635 of
                                '-' -> return ()
                                _ -> throwErrorPackratM "'-'" "not match pattern: " ["dvChars"] d711_634
                            let '-' = xx710_635
                            return ()
                            if True
                             then return ()
                             else throwErrorPackratM "True" "not match: " ["dvChars"] d711_634
                            d713_636 <- get
                            xx712_637 <- dvCharsM
                            case xx712_637 of
                                '}' -> return ()
                                _ -> throwErrorPackratM "'}'" "not match pattern: " ["dvChars"] d713_636
                            let '}' = xx712_637
                            return ()
                            if True
                             then return ()
                             else throwErrorPackratM "True" "not match: " ["dvChars"] d713_636
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