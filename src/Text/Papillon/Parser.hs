{-# LANGUAGE FlexibleContexts, TemplateHaskell, UndecidableInstances , FlexibleContexts, PackageImports, TypeFamilies, RankNTypes, FlexibleInstances #-}
module  Text.Papillon.Parser (
	Peg,
	Definition,
	Selection,
	ExpressionHs,
	NameLeaf(..),
	NameLeaf_(..),
	parse,
	dv_peg,
	dv_pegFile,
	initialPos,
)  where
import "monads-tf" Control.Monad.State
import "monads-tf" Control.Monad.Error
import Control.Monad.Trans.Error (Error (..))

import Control.Applicative



import Data.Char
import Language.Haskell.TH
import Text.Papillon.SyntaxTree

data ParseError pos = ParseError String String pos deriving (Show)
throwErrorPackratM :: forall a . String -> String -> PackratM a
throwErrorPackratM code msg = do pos <- gets dvPos
                                 throwError (ParseError code msg pos)
instance (Source s, Pos s ~ pos) => Error (ParseError pos)
    where strMsg msg = ParseError "" msg initialPos
flipMaybe :: forall a . PackratM a -> PackratM ()
flipMaybe act = do err <- (act >> return False) `catchError` const (return True)
                   unless err (throwErrorPackratM "" "not not match")
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
          where d = Derivs pegFile pragma pragmaStr pragmaEnd moduleDec moduleDecStr whr preImpPap prePeg afterPeg importPapillon varToken typToken pap peg sourceType peg_ definition selection expressionHs expression nameLeaf_ nameLeaf patOp pat pat1 patList opConName charLit stringLit dq pats leaf test hsExpLam hsExpTyp hsExpOp hsOp opTail hsExp hsExp1 hsExpTpl hsTypeArr hsType hsType1 hsTypeTpl typ variable tvtail integer alpha upper lower digit spaces space notNLString nl comment comments notComStr comEnd char pos___hoge
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
                                             _ -> throwErrorPackratM "" "eof")
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
p_pegFile = foldl1 mplus [do pr <- dv_pragmaM
                             return ()
                             if True then return () else throwErrorPackratM "True" "not match"
                             md <- dv_moduleDecM
                             return ()
                             if True then return () else throwErrorPackratM "True" "not match"
                             pip <- dv_preImpPapM
                             return ()
                             if True then return () else throwErrorPackratM "True" "not match"
                             dv_importPapillonM >> return ()
                             if True then return () else throwErrorPackratM "True" "not match"
                             pp <- dv_prePegM
                             return ()
                             if True then return () else throwErrorPackratM "True" "not match"
                             dv_papM >> return ()
                             if True then return () else throwErrorPackratM "True" "not match"
                             p <- dv_pegM
                             return ()
                             if True then return () else throwErrorPackratM "True" "not match"
                             dv_spacesM >> return ()
                             if True then return () else throwErrorPackratM "True" "not match"
                             xx0_0 <- dvCharsM
                             case xx0_0 of
                                 '|' -> return ()
                                 _ -> throwErrorPackratM "'|'" "not match pattern"
                             let '|' = xx0_0
                             return ()
                             if True then return () else throwErrorPackratM "True" "not match"
                             xx1_1 <- dvCharsM
                             case xx1_1 of
                                 ']' -> return ()
                                 _ -> throwErrorPackratM "']'" "not match pattern"
                             let ']' = xx1_1
                             return ()
                             if True then return () else throwErrorPackratM "True" "not match"
                             xx2_2 <- dvCharsM
                             case xx2_2 of
                                 '\n' -> return ()
                                 _ -> throwErrorPackratM "'\\n'" "not match pattern"
                             let '\n' = xx2_2
                             return ()
                             if True then return () else throwErrorPackratM "True" "not match"
                             atp <- dv_afterPegM
                             return ()
                             if True then return () else throwErrorPackratM "True" "not match"
                             return (mkPegFile pr md pip pp p atp),
                          do pr <- dv_pragmaM
                             return ()
                             if True then return () else throwErrorPackratM "True" "not match"
                             md <- dv_moduleDecM
                             return ()
                             if True then return () else throwErrorPackratM "True" "not match"
                             pp <- dv_prePegM
                             return ()
                             if True then return () else throwErrorPackratM "True" "not match"
                             dv_papM >> return ()
                             if True then return () else throwErrorPackratM "True" "not match"
                             p <- dv_pegM
                             return ()
                             if True then return () else throwErrorPackratM "True" "not match"
                             dv_spacesM >> return ()
                             if True then return () else throwErrorPackratM "True" "not match"
                             xx3_3 <- dvCharsM
                             case xx3_3 of
                                 '|' -> return ()
                                 _ -> throwErrorPackratM "'|'" "not match pattern"
                             let '|' = xx3_3
                             return ()
                             if True then return () else throwErrorPackratM "True" "not match"
                             xx4_4 <- dvCharsM
                             case xx4_4 of
                                 ']' -> return ()
                                 _ -> throwErrorPackratM "']'" "not match pattern"
                             let ']' = xx4_4
                             return ()
                             if True then return () else throwErrorPackratM "True" "not match"
                             xx5_5 <- dvCharsM
                             case xx5_5 of
                                 '\n' -> return ()
                                 _ -> throwErrorPackratM "'\\n'" "not match pattern"
                             let '\n' = xx5_5
                             return ()
                             if True then return () else throwErrorPackratM "True" "not match"
                             atp <- dv_afterPegM
                             return ()
                             if True then return () else throwErrorPackratM "True" "not match"
                             return (mkPegFile pr md emp pp p atp)]
p_pragma = foldl1 mplus [do dv_spacesM >> return ()
                            if True then return () else throwErrorPackratM "True" "not match"
                            xx6_6 <- dvCharsM
                            case xx6_6 of
                                '{' -> return ()
                                _ -> throwErrorPackratM "'{'" "not match pattern"
                            let '{' = xx6_6
                            return ()
                            if True then return () else throwErrorPackratM "True" "not match"
                            xx7_7 <- dvCharsM
                            case xx7_7 of
                                '-' -> return ()
                                _ -> throwErrorPackratM "'-'" "not match pattern"
                            let '-' = xx7_7
                            return ()
                            if True then return () else throwErrorPackratM "True" "not match"
                            xx8_8 <- dvCharsM
                            case xx8_8 of
                                '#' -> return ()
                                _ -> throwErrorPackratM "'#'" "not match pattern"
                            let '#' = xx8_8
                            return ()
                            if True then return () else throwErrorPackratM "True" "not match"
                            s <- dv_pragmaStrM
                            return ()
                            if True then return () else throwErrorPackratM "True" "not match"
                            dv_pragmaEndM >> return ()
                            if True then return () else throwErrorPackratM "True" "not match"
                            dv_spacesM >> return ()
                            if True then return () else throwErrorPackratM "True" "not match"
                            return (just s),
                         do dv_spacesM >> return ()
                            if True then return () else throwErrorPackratM "True" "not match"
                            return nothing]
p_pragmaStr = foldl1 mplus [do ddd9_9 <- get
                               flipMaybe (do dv_pragmaEndM >> return ()
                                             if True
                                              then return ()
                                              else throwErrorPackratM "True" "not match")
                               put ddd9_9
                               xx10_10 <- dvCharsM
                               let c = xx10_10
                               if True then return () else throwErrorPackratM "True" "not match"
                               s <- dv_pragmaStrM
                               return ()
                               if True then return () else throwErrorPackratM "True" "not match"
                               return (cons c s),
                            do return emp]
p_pragmaEnd = foldl1 mplus [do xx11_11 <- dvCharsM
                               case xx11_11 of
                                   '#' -> return ()
                                   _ -> throwErrorPackratM "'#'" "not match pattern"
                               let '#' = xx11_11
                               return ()
                               if True then return () else throwErrorPackratM "True" "not match"
                               xx12_12 <- dvCharsM
                               case xx12_12 of
                                   '-' -> return ()
                                   _ -> throwErrorPackratM "'-'" "not match pattern"
                               let '-' = xx12_12
                               return ()
                               if True then return () else throwErrorPackratM "True" "not match"
                               xx13_13 <- dvCharsM
                               case xx13_13 of
                                   '}' -> return ()
                                   _ -> throwErrorPackratM "'}'" "not match pattern"
                               let '}' = xx13_13
                               return ()
                               if True then return () else throwErrorPackratM "True" "not match"
                               return ()]
p_moduleDec = foldl1 mplus [do xx14_14 <- dvCharsM
                               case xx14_14 of
                                   'm' -> return ()
                                   _ -> throwErrorPackratM "'m'" "not match pattern"
                               let 'm' = xx14_14
                               return ()
                               if True then return () else throwErrorPackratM "True" "not match"
                               xx15_15 <- dvCharsM
                               case xx15_15 of
                                   'o' -> return ()
                                   _ -> throwErrorPackratM "'o'" "not match pattern"
                               let 'o' = xx15_15
                               return ()
                               if True then return () else throwErrorPackratM "True" "not match"
                               xx16_16 <- dvCharsM
                               case xx16_16 of
                                   'd' -> return ()
                                   _ -> throwErrorPackratM "'d'" "not match pattern"
                               let 'd' = xx16_16
                               return ()
                               if True then return () else throwErrorPackratM "True" "not match"
                               xx17_17 <- dvCharsM
                               case xx17_17 of
                                   'u' -> return ()
                                   _ -> throwErrorPackratM "'u'" "not match pattern"
                               let 'u' = xx17_17
                               return ()
                               if True then return () else throwErrorPackratM "True" "not match"
                               xx18_18 <- dvCharsM
                               case xx18_18 of
                                   'l' -> return ()
                                   _ -> throwErrorPackratM "'l'" "not match pattern"
                               let 'l' = xx18_18
                               return ()
                               if True then return () else throwErrorPackratM "True" "not match"
                               xx19_19 <- dvCharsM
                               case xx19_19 of
                                   'e' -> return ()
                                   _ -> throwErrorPackratM "'e'" "not match pattern"
                               let 'e' = xx19_19
                               return ()
                               if True then return () else throwErrorPackratM "True" "not match"
                               s <- dv_moduleDecStrM
                               return ()
                               if True then return () else throwErrorPackratM "True" "not match"
                               dv_whrM >> return ()
                               if True then return () else throwErrorPackratM "True" "not match"
                               return (just s),
                            do return nothing]
p_moduleDecStr = foldl1 mplus [do ddd20_20 <- get
                                  flipMaybe (do dv_whrM >> return ()
                                                if True
                                                 then return ()
                                                 else throwErrorPackratM "True" "not match")
                                  put ddd20_20
                                  xx21_21 <- dvCharsM
                                  let c = xx21_21
                                  if True then return () else throwErrorPackratM "True" "not match"
                                  s <- dv_moduleDecStrM
                                  return ()
                                  if True then return () else throwErrorPackratM "True" "not match"
                                  return (cons c s),
                               do return emp]
p_whr = foldl1 mplus [do xx22_22 <- dvCharsM
                         case xx22_22 of
                             'w' -> return ()
                             _ -> throwErrorPackratM "'w'" "not match pattern"
                         let 'w' = xx22_22
                         return ()
                         if True then return () else throwErrorPackratM "True" "not match"
                         xx23_23 <- dvCharsM
                         case xx23_23 of
                             'h' -> return ()
                             _ -> throwErrorPackratM "'h'" "not match pattern"
                         let 'h' = xx23_23
                         return ()
                         if True then return () else throwErrorPackratM "True" "not match"
                         xx24_24 <- dvCharsM
                         case xx24_24 of
                             'e' -> return ()
                             _ -> throwErrorPackratM "'e'" "not match pattern"
                         let 'e' = xx24_24
                         return ()
                         if True then return () else throwErrorPackratM "True" "not match"
                         xx25_25 <- dvCharsM
                         case xx25_25 of
                             'r' -> return ()
                             _ -> throwErrorPackratM "'r'" "not match pattern"
                         let 'r' = xx25_25
                         return ()
                         if True then return () else throwErrorPackratM "True" "not match"
                         xx26_26 <- dvCharsM
                         case xx26_26 of
                             'e' -> return ()
                             _ -> throwErrorPackratM "'e'" "not match pattern"
                         let 'e' = xx26_26
                         return ()
                         if True then return () else throwErrorPackratM "True" "not match"
                         return ()]
p_preImpPap = foldl1 mplus [do ddd27_27 <- get
                               flipMaybe (do dv_importPapillonM >> return ()
                                             if True
                                              then return ()
                                              else throwErrorPackratM "True" "not match")
                               put ddd27_27
                               ddd28_28 <- get
                               flipMaybe (do dv_papM >> return ()
                                             if True
                                              then return ()
                                              else throwErrorPackratM "True" "not match")
                               put ddd28_28
                               xx29_29 <- dvCharsM
                               let c = xx29_29
                               if True then return () else throwErrorPackratM "True" "not match"
                               pip <- dv_preImpPapM
                               return ()
                               if True then return () else throwErrorPackratM "True" "not match"
                               return (cons c pip),
                            do return emp]
p_prePeg = foldl1 mplus [do ddd30_30 <- get
                            flipMaybe (do dv_papM >> return ()
                                          if True
                                           then return ()
                                           else throwErrorPackratM "True" "not match")
                            put ddd30_30
                            xx31_31 <- dvCharsM
                            let c = xx31_31
                            if True then return () else throwErrorPackratM "True" "not match"
                            pp <- dv_prePegM
                            return ()
                            if True then return () else throwErrorPackratM "True" "not match"
                            return (cons c pp),
                         do return emp]
p_afterPeg = foldl1 mplus [do xx32_32 <- dvCharsM
                              let c = xx32_32
                              if True then return () else throwErrorPackratM "True" "not match"
                              atp <- dv_afterPegM
                              return ()
                              if True then return () else throwErrorPackratM "True" "not match"
                              return (cons c atp),
                           do return emp]
p_importPapillon = foldl1 mplus [do xx33_33 <- dv_varTokenM
                                    case xx33_33 of
                                        "import" -> return ()
                                        _ -> throwErrorPackratM "\"import\"" "not match pattern"
                                    let "import" = xx33_33
                                    return ()
                                    if True
                                     then return ()
                                     else throwErrorPackratM "True" "not match"
                                    xx34_34 <- dv_typTokenM
                                    case xx34_34 of
                                        "Text" -> return ()
                                        _ -> throwErrorPackratM "\"Text\"" "not match pattern"
                                    let "Text" = xx34_34
                                    return ()
                                    if True
                                     then return ()
                                     else throwErrorPackratM "True" "not match"
                                    xx35_35 <- dvCharsM
                                    case xx35_35 of
                                        '.' -> return ()
                                        _ -> throwErrorPackratM "'.'" "not match pattern"
                                    let '.' = xx35_35
                                    return ()
                                    if True
                                     then return ()
                                     else throwErrorPackratM "True" "not match"
                                    dv_spacesM >> return ()
                                    if True
                                     then return ()
                                     else throwErrorPackratM "True" "not match"
                                    xx36_36 <- dv_typTokenM
                                    case xx36_36 of
                                        "Papillon" -> return ()
                                        _ -> throwErrorPackratM "\"Papillon\"" "not match pattern"
                                    let "Papillon" = xx36_36
                                    return ()
                                    if True
                                     then return ()
                                     else throwErrorPackratM "True" "not match"
                                    ddd37_37 <- get
                                    flipMaybe (do xx38_38 <- dvCharsM
                                                  case xx38_38 of
                                                      '.' -> return ()
                                                      _ -> throwErrorPackratM "'.'" "not match pattern"
                                                  let '.' = xx38_38
                                                  return ()
                                                  if True
                                                   then return ()
                                                   else throwErrorPackratM "True" "not match")
                                    put ddd37_37
                                    return ()]
p_varToken = foldl1 mplus [do v <- dv_variableM
                              return ()
                              if True then return () else throwErrorPackratM "True" "not match"
                              dv_spacesM >> return ()
                              if True then return () else throwErrorPackratM "True" "not match"
                              return v]
p_typToken = foldl1 mplus [do t <- dv_typM
                              return ()
                              if True then return () else throwErrorPackratM "True" "not match"
                              dv_spacesM >> return ()
                              if True then return () else throwErrorPackratM "True" "not match"
                              return t]
p_pap = foldl1 mplus [do xx39_39 <- dvCharsM
                         case xx39_39 of
                             '\n' -> return ()
                             _ -> throwErrorPackratM "'\\n'" "not match pattern"
                         let '\n' = xx39_39
                         return ()
                         if True then return () else throwErrorPackratM "True" "not match"
                         xx40_40 <- dvCharsM
                         case xx40_40 of
                             '[' -> return ()
                             _ -> throwErrorPackratM "'['" "not match pattern"
                         let '[' = xx40_40
                         return ()
                         if True then return () else throwErrorPackratM "True" "not match"
                         xx41_41 <- dvCharsM
                         case xx41_41 of
                             'p' -> return ()
                             _ -> throwErrorPackratM "'p'" "not match pattern"
                         let 'p' = xx41_41
                         return ()
                         if True then return () else throwErrorPackratM "True" "not match"
                         xx42_42 <- dvCharsM
                         case xx42_42 of
                             'a' -> return ()
                             _ -> throwErrorPackratM "'a'" "not match pattern"
                         let 'a' = xx42_42
                         return ()
                         if True then return () else throwErrorPackratM "True" "not match"
                         xx43_43 <- dvCharsM
                         case xx43_43 of
                             'p' -> return ()
                             _ -> throwErrorPackratM "'p'" "not match pattern"
                         let 'p' = xx43_43
                         return ()
                         if True then return () else throwErrorPackratM "True" "not match"
                         xx44_44 <- dvCharsM
                         case xx44_44 of
                             'i' -> return ()
                             _ -> throwErrorPackratM "'i'" "not match pattern"
                         let 'i' = xx44_44
                         return ()
                         if True then return () else throwErrorPackratM "True" "not match"
                         xx45_45 <- dvCharsM
                         case xx45_45 of
                             'l' -> return ()
                             _ -> throwErrorPackratM "'l'" "not match pattern"
                         let 'l' = xx45_45
                         return ()
                         if True then return () else throwErrorPackratM "True" "not match"
                         xx46_46 <- dvCharsM
                         case xx46_46 of
                             'l' -> return ()
                             _ -> throwErrorPackratM "'l'" "not match pattern"
                         let 'l' = xx46_46
                         return ()
                         if True then return () else throwErrorPackratM "True" "not match"
                         xx47_47 <- dvCharsM
                         case xx47_47 of
                             'o' -> return ()
                             _ -> throwErrorPackratM "'o'" "not match pattern"
                         let 'o' = xx47_47
                         return ()
                         if True then return () else throwErrorPackratM "True" "not match"
                         xx48_48 <- dvCharsM
                         case xx48_48 of
                             'n' -> return ()
                             _ -> throwErrorPackratM "'n'" "not match pattern"
                         let 'n' = xx48_48
                         return ()
                         if True then return () else throwErrorPackratM "True" "not match"
                         xx49_49 <- dvCharsM
                         case xx49_49 of
                             '|' -> return ()
                             _ -> throwErrorPackratM "'|'" "not match pattern"
                         let '|' = xx49_49
                         return ()
                         if True then return () else throwErrorPackratM "True" "not match"
                         xx50_50 <- dvCharsM
                         case xx50_50 of
                             '\n' -> return ()
                             _ -> throwErrorPackratM "'\\n'" "not match pattern"
                         let '\n' = xx50_50
                         return ()
                         if True then return () else throwErrorPackratM "True" "not match"
                         return ()]
p_peg = foldl1 mplus [do dv_spacesM >> return ()
                         if True then return () else throwErrorPackratM "True" "not match"
                         s <- dv_sourceTypeM
                         return ()
                         if True then return () else throwErrorPackratM "True" "not match"
                         p <- dv_peg_M
                         return ()
                         if True then return () else throwErrorPackratM "True" "not match"
                         return (mkTTPeg s p),
                      do p <- dv_peg_M
                         return ()
                         if True then return () else throwErrorPackratM "True" "not match"
                         return (mkTTPeg tString p)]
p_sourceType = foldl1 mplus [do xx51_51 <- dv_varTokenM
                                case xx51_51 of
                                    "source" -> return ()
                                    _ -> throwErrorPackratM "\"source\"" "not match pattern"
                                let "source" = xx51_51
                                return ()
                                if True then return () else throwErrorPackratM "True" "not match"
                                xx52_52 <- dvCharsM
                                case xx52_52 of
                                    ':' -> return ()
                                    _ -> throwErrorPackratM "':'" "not match pattern"
                                let ':' = xx52_52
                                return ()
                                if True then return () else throwErrorPackratM "True" "not match"
                                dv_spacesM >> return ()
                                if True then return () else throwErrorPackratM "True" "not match"
                                v <- dv_typTokenM
                                return ()
                                if True then return () else throwErrorPackratM "True" "not match"
                                return v]
p_peg_ = foldl1 mplus [do dv_spacesM >> return ()
                          if True then return () else throwErrorPackratM "True" "not match"
                          d <- dv_definitionM
                          return ()
                          if True then return () else throwErrorPackratM "True" "not match"
                          p <- dv_peg_M
                          return ()
                          if True then return () else throwErrorPackratM "True" "not match"
                          return (cons d p),
                       do return emp]
p_definition = foldl1 mplus [do v <- dv_variableM
                                return ()
                                if True then return () else throwErrorPackratM "True" "not match"
                                dv_spacesM >> return ()
                                if True then return () else throwErrorPackratM "True" "not match"
                                xx53_53 <- dvCharsM
                                case xx53_53 of
                                    ':' -> return ()
                                    _ -> throwErrorPackratM "':'" "not match pattern"
                                let ':' = xx53_53
                                return ()
                                if True then return () else throwErrorPackratM "True" "not match"
                                xx54_54 <- dvCharsM
                                case xx54_54 of
                                    ':' -> return ()
                                    _ -> throwErrorPackratM "':'" "not match pattern"
                                let ':' = xx54_54
                                return ()
                                if True then return () else throwErrorPackratM "True" "not match"
                                dv_spacesM >> return ()
                                if True then return () else throwErrorPackratM "True" "not match"
                                t <- dv_hsTypeArrM
                                return ()
                                if True then return () else throwErrorPackratM "True" "not match"
                                dv_spacesM >> return ()
                                if True then return () else throwErrorPackratM "True" "not match"
                                xx55_55 <- dvCharsM
                                case xx55_55 of
                                    '=' -> return ()
                                    _ -> throwErrorPackratM "'='" "not match pattern"
                                let '=' = xx55_55
                                return ()
                                if True then return () else throwErrorPackratM "True" "not match"
                                dv_spacesM >> return ()
                                if True then return () else throwErrorPackratM "True" "not match"
                                sel <- dv_selectionM
                                return ()
                                if True then return () else throwErrorPackratM "True" "not match"
                                dv_spacesM >> return ()
                                if True then return () else throwErrorPackratM "True" "not match"
                                xx56_56 <- dvCharsM
                                case xx56_56 of
                                    ';' -> return ()
                                    _ -> throwErrorPackratM "';'" "not match pattern"
                                let ';' = xx56_56
                                return ()
                                if True then return () else throwErrorPackratM "True" "not match"
                                return (mkDef v t sel)]
p_selection = foldl1 mplus [do ex <- dv_expressionHsM
                               return ()
                               if True then return () else throwErrorPackratM "True" "not match"
                               dv_spacesM >> return ()
                               if True then return () else throwErrorPackratM "True" "not match"
                               xx57_57 <- dvCharsM
                               case xx57_57 of
                                   '/' -> return ()
                                   _ -> throwErrorPackratM "'/'" "not match pattern"
                               let '/' = xx57_57
                               return ()
                               if True then return () else throwErrorPackratM "True" "not match"
                               dv_spacesM >> return ()
                               if True then return () else throwErrorPackratM "True" "not match"
                               sel <- dv_selectionM
                               return ()
                               if True then return () else throwErrorPackratM "True" "not match"
                               return (cons ex sel),
                            do ex <- dv_expressionHsM
                               return ()
                               if True then return () else throwErrorPackratM "True" "not match"
                               return (cons ex emp)]
p_expressionHs = foldl1 mplus [do e <- dv_expressionM
                                  return ()
                                  if True then return () else throwErrorPackratM "True" "not match"
                                  dv_spacesM >> return ()
                                  if True then return () else throwErrorPackratM "True" "not match"
                                  xx58_58 <- dvCharsM
                                  case xx58_58 of
                                      '{' -> return ()
                                      _ -> throwErrorPackratM "'{'" "not match pattern"
                                  let '{' = xx58_58
                                  return ()
                                  if True then return () else throwErrorPackratM "True" "not match"
                                  dv_spacesM >> return ()
                                  if True then return () else throwErrorPackratM "True" "not match"
                                  h <- dv_hsExpLamM
                                  return ()
                                  if True then return () else throwErrorPackratM "True" "not match"
                                  dv_spacesM >> return ()
                                  if True then return () else throwErrorPackratM "True" "not match"
                                  xx59_59 <- dvCharsM
                                  case xx59_59 of
                                      '}' -> return ()
                                      _ -> throwErrorPackratM "'}'" "not match pattern"
                                  let '}' = xx59_59
                                  return ()
                                  if True then return () else throwErrorPackratM "True" "not match"
                                  return (mkExpressionHs e h)]
p_expression = foldl1 mplus [do l <- dv_nameLeaf_M
                                return ()
                                if True then return () else throwErrorPackratM "True" "not match"
                                dv_spacesM >> return ()
                                if True then return () else throwErrorPackratM "True" "not match"
                                e <- dv_expressionM
                                return ()
                                if True then return () else throwErrorPackratM "True" "not match"
                                return (cons l e),
                             do return emp]
p_nameLeaf_ = foldl1 mplus [do xx60_60 <- dvCharsM
                               case xx60_60 of
                                   '!' -> return ()
                                   _ -> throwErrorPackratM "'!'" "not match pattern"
                               let '!' = xx60_60
                               return ()
                               if True then return () else throwErrorPackratM "True" "not match"
                               nl <- dv_nameLeafM
                               return ()
                               if True then return () else throwErrorPackratM "True" "not match"
                               return (notAfter nl),
                            do nl <- dv_nameLeafM
                               return ()
                               if True then return () else throwErrorPackratM "True" "not match"
                               return (here nl)]
p_nameLeaf = foldl1 mplus [do n <- dv_pat1M
                              return ()
                              if True then return () else throwErrorPackratM "True" "not match"
                              xx61_61 <- dvCharsM
                              case xx61_61 of
                                  ':' -> return ()
                                  _ -> throwErrorPackratM "':'" "not match pattern"
                              let ':' = xx61_61
                              return ()
                              if True then return () else throwErrorPackratM "True" "not match"
                              xx62_62 <- dvCharsM
                              case xx62_62 of
                                  '(' -> return ()
                                  _ -> throwErrorPackratM "'('" "not match pattern"
                              let '(' = xx62_62
                              return ()
                              if True then return () else throwErrorPackratM "True" "not match"
                              ex <- dv_selectionM
                              return ()
                              if True then return () else throwErrorPackratM "True" "not match"
                              xx63_63 <- dvCharsM
                              case xx63_63 of
                                  ')' -> return ()
                                  _ -> throwErrorPackratM "')'" "not match pattern"
                              let ')' = xx63_63
                              return ()
                              if True then return () else throwErrorPackratM "True" "not match"
                              xx64_64 <- dvCharsM
                              case xx64_64 of
                                  '*' -> return ()
                                  _ -> throwErrorPackratM "'*'" "not match pattern"
                              let '*' = xx64_64
                              return ()
                              if True then return () else throwErrorPackratM "True" "not match"
                              dv_spacesM >> return ()
                              if True then return () else throwErrorPackratM "True" "not match"
                              return (NameLeafList n ex),
                           do n <- dv_pat1M
                              return ()
                              if True then return () else throwErrorPackratM "True" "not match"
                              xx65_65 <- dvCharsM
                              case xx65_65 of
                                  ':' -> return ()
                                  _ -> throwErrorPackratM "':'" "not match pattern"
                              let ':' = xx65_65
                              return ()
                              if True then return () else throwErrorPackratM "True" "not match"
                              xx66_66 <- dvCharsM
                              case xx66_66 of
                                  '(' -> return ()
                                  _ -> throwErrorPackratM "'('" "not match pattern"
                              let '(' = xx66_66
                              return ()
                              if True then return () else throwErrorPackratM "True" "not match"
                              ex <- dv_selectionM
                              return ()
                              if True then return () else throwErrorPackratM "True" "not match"
                              xx67_67 <- dvCharsM
                              case xx67_67 of
                                  ')' -> return ()
                                  _ -> throwErrorPackratM "')'" "not match pattern"
                              let ')' = xx67_67
                              return ()
                              if True then return () else throwErrorPackratM "True" "not match"
                              xx68_68 <- dvCharsM
                              let c = xx68_68
                              if isQuestion c
                               then return ()
                               else throwErrorPackratM "isQuestion c" "not match"
                              dv_spacesM >> return ()
                              if True then return () else throwErrorPackratM "True" "not match"
                              return (NameLeafOptional n ex),
                           do n <- dv_pat1M
                              return ()
                              if True then return () else throwErrorPackratM "True" "not match"
                              xx69_69 <- dvCharsM
                              case xx69_69 of
                                  ':' -> return ()
                                  _ -> throwErrorPackratM "':'" "not match pattern"
                              let ':' = xx69_69
                              return ()
                              if True then return () else throwErrorPackratM "True" "not match"
                              l <- dv_leafM
                              return ()
                              if True then return () else throwErrorPackratM "True" "not match"
                              return (mkNameLeaf n l),
                           do n <- dv_pat1M
                              return ()
                              if True then return () else throwErrorPackratM "True" "not match"
                              return (mkNameLeaf n ctLeaf)]
p_patOp = foldl1 mplus [do p <- dv_patM
                           return ()
                           if True then return () else throwErrorPackratM "True" "not match"
                           o <- dv_opConNameM
                           return ()
                           if True then return () else throwErrorPackratM "True" "not match"
                           po <- dv_patOpM
                           return ()
                           if True then return () else throwErrorPackratM "True" "not match"
                           return (uInfixP p o po),
                        do p <- dv_patM
                           return ()
                           if True then return () else throwErrorPackratM "True" "not match"
                           return p]
p_pat = foldl1 mplus [do t <- dv_typM
                         return ()
                         if True then return () else throwErrorPackratM "True" "not match"
                         dv_spacesM >> return ()
                         if True then return () else throwErrorPackratM "True" "not match"
                         ps <- dv_patsM
                         return ()
                         if True then return () else throwErrorPackratM "True" "not match"
                         return (conToPatQ t ps),
                      do p <- dv_pat1M
                         return ()
                         if True then return () else throwErrorPackratM "True" "not match"
                         return p]
p_pat1 = foldl1 mplus [do t <- dv_typM
                          return ()
                          if True then return () else throwErrorPackratM "True" "not match"
                          return (conToPatQ t emp),
                       do xx70_70 <- dv_variableM
                          case xx70_70 of
                              "_" -> return ()
                              _ -> throwErrorPackratM "\"_\"" "not match pattern"
                          let "_" = xx70_70
                          return ()
                          if True then return () else throwErrorPackratM "True" "not match"
                          return wildP,
                       do n <- dv_variableM
                          return ()
                          if True then return () else throwErrorPackratM "True" "not match"
                          return (strToPatQ n),
                       do i <- dv_integerM
                          return ()
                          if True then return () else throwErrorPackratM "True" "not match"
                          return (litP (integerL i)),
                       do xx71_71 <- dvCharsM
                          case xx71_71 of
                              '-' -> return ()
                              _ -> throwErrorPackratM "'-'" "not match pattern"
                          let '-' = xx71_71
                          return ()
                          if True then return () else throwErrorPackratM "True" "not match"
                          dv_spacesM >> return ()
                          if True then return () else throwErrorPackratM "True" "not match"
                          i <- dv_integerM
                          return ()
                          if True then return () else throwErrorPackratM "True" "not match"
                          return (litP (integerL $ negate i)),
                       do xx72_72 <- dvCharsM
                          case xx72_72 of
                              '\'' -> return ()
                              _ -> throwErrorPackratM "'\\''" "not match pattern"
                          let '\'' = xx72_72
                          return ()
                          if True then return () else throwErrorPackratM "True" "not match"
                          c <- dv_charLitM
                          return ()
                          if True then return () else throwErrorPackratM "True" "not match"
                          xx73_73 <- dvCharsM
                          case xx73_73 of
                              '\'' -> return ()
                              _ -> throwErrorPackratM "'\\''" "not match pattern"
                          let '\'' = xx73_73
                          return ()
                          if True then return () else throwErrorPackratM "True" "not match"
                          return (charP c),
                       do xx74_74 <- dvCharsM
                          case xx74_74 of
                              '"' -> return ()
                              _ -> throwErrorPackratM "'\"'" "not match pattern"
                          let '"' = xx74_74
                          return ()
                          if True then return () else throwErrorPackratM "True" "not match"
                          s <- dv_stringLitM
                          return ()
                          if True then return () else throwErrorPackratM "True" "not match"
                          xx75_75 <- dvCharsM
                          case xx75_75 of
                              '"' -> return ()
                              _ -> throwErrorPackratM "'\"'" "not match pattern"
                          let '"' = xx75_75
                          return ()
                          if True then return () else throwErrorPackratM "True" "not match"
                          return (stringP s),
                       do xx76_76 <- dvCharsM
                          case xx76_76 of
                              '(' -> return ()
                              _ -> throwErrorPackratM "'('" "not match pattern"
                          let '(' = xx76_76
                          return ()
                          if True then return () else throwErrorPackratM "True" "not match"
                          p <- dv_patListM
                          return ()
                          if True then return () else throwErrorPackratM "True" "not match"
                          xx77_77 <- dvCharsM
                          case xx77_77 of
                              ')' -> return ()
                              _ -> throwErrorPackratM "')'" "not match pattern"
                          let ')' = xx77_77
                          return ()
                          if True then return () else throwErrorPackratM "True" "not match"
                          return (tupP p),
                       do xx78_78 <- dvCharsM
                          case xx78_78 of
                              '[' -> return ()
                              _ -> throwErrorPackratM "'['" "not match pattern"
                          let '[' = xx78_78
                          return ()
                          if True then return () else throwErrorPackratM "True" "not match"
                          p <- dv_patListM
                          return ()
                          if True then return () else throwErrorPackratM "True" "not match"
                          xx79_79 <- dvCharsM
                          case xx79_79 of
                              ']' -> return ()
                              _ -> throwErrorPackratM "']'" "not match pattern"
                          let ']' = xx79_79
                          return ()
                          if True then return () else throwErrorPackratM "True" "not match"
                          return (listP p)]
p_patList = foldl1 mplus [do p <- dv_patOpM
                             return ()
                             if True then return () else throwErrorPackratM "True" "not match"
                             dv_spacesM >> return ()
                             if True then return () else throwErrorPackratM "True" "not match"
                             xx80_80 <- dvCharsM
                             case xx80_80 of
                                 ',' -> return ()
                                 _ -> throwErrorPackratM "','" "not match pattern"
                             let ',' = xx80_80
                             return ()
                             if True then return () else throwErrorPackratM "True" "not match"
                             dv_spacesM >> return ()
                             if True then return () else throwErrorPackratM "True" "not match"
                             ps <- dv_patListM
                             return ()
                             if True then return () else throwErrorPackratM "True" "not match"
                             return (p : ps),
                          do p <- dv_patOpM
                             return ()
                             if True then return () else throwErrorPackratM "True" "not match"
                             return [p],
                          do return []]
p_opConName = foldl1 mplus [do xx81_81 <- dvCharsM
                               case xx81_81 of
                                   ':' -> return ()
                                   _ -> throwErrorPackratM "':'" "not match pattern"
                               let ':' = xx81_81
                               return ()
                               if True then return () else throwErrorPackratM "True" "not match"
                               ot <- dv_opTailM
                               return ()
                               if True then return () else throwErrorPackratM "True" "not match"
                               return (mkName $ colon : ot)]
p_charLit = foldl1 mplus [do xx82_82 <- dvCharsM
                             let c = xx82_82
                             if isAlphaNumOt c
                              then return ()
                              else throwErrorPackratM "isAlphaNumOt c" "not match"
                             return c,
                          do xx83_83 <- dvCharsM
                             case xx83_83 of
                                 '\\' -> return ()
                                 _ -> throwErrorPackratM "'\\\\'" "not match pattern"
                             let '\\' = xx83_83
                             return ()
                             if True then return () else throwErrorPackratM "True" "not match"
                             xx84_84 <- dvCharsM
                             let c = xx84_84
                             if elemNTs c
                              then return ()
                              else throwErrorPackratM "elemNTs c" "not match"
                             return (getNTs c)]
p_stringLit = foldl1 mplus [do ddd85_85 <- get
                               flipMaybe (do dv_dqM >> return ()
                                             if True
                                              then return ()
                                              else throwErrorPackratM "True" "not match")
                               put ddd85_85
                               xx86_86 <- dvCharsM
                               let c = xx86_86
                               if True then return () else throwErrorPackratM "True" "not match"
                               s <- dv_stringLitM
                               return ()
                               if True then return () else throwErrorPackratM "True" "not match"
                               return (cons c s),
                            do return emp]
p_dq = foldl1 mplus [do xx87_87 <- dvCharsM
                        case xx87_87 of
                            '"' -> return ()
                            _ -> throwErrorPackratM "'\"'" "not match pattern"
                        let '"' = xx87_87
                        return ()
                        if True then return () else throwErrorPackratM "True" "not match"
                        return ()]
p_pats = foldl1 mplus [do p <- dv_patM
                          return ()
                          if True then return () else throwErrorPackratM "True" "not match"
                          ps <- dv_patsM
                          return ()
                          if True then return () else throwErrorPackratM "True" "not match"
                          return (cons p ps),
                       do return emp]
p_leaf = foldl1 mplus [do t <- dv_testM
                          return ()
                          if True then return () else throwErrorPackratM "True" "not match"
                          return (boolLeaf t),
                       do v <- dv_variableM
                          return ()
                          if True then return () else throwErrorPackratM "True" "not match"
                          t <- dv_testM
                          return ()
                          if True then return () else throwErrorPackratM "True" "not match"
                          return (ruleLeaf v t),
                       do v <- dv_variableM
                          return ()
                          if True then return () else throwErrorPackratM "True" "not match"
                          return (ruleLeaf v true)]
p_test = foldl1 mplus [do xx88_88 <- dvCharsM
                          case xx88_88 of
                              '[' -> return ()
                              _ -> throwErrorPackratM "'['" "not match pattern"
                          let '[' = xx88_88
                          return ()
                          if True then return () else throwErrorPackratM "True" "not match"
                          h <- dv_hsExpLamM
                          return ()
                          if True then return () else throwErrorPackratM "True" "not match"
                          xx89_89 <- dvCharsM
                          case xx89_89 of
                              ']' -> return ()
                              _ -> throwErrorPackratM "']'" "not match pattern"
                          let ']' = xx89_89
                          return ()
                          if True then return () else throwErrorPackratM "True" "not match"
                          return h]
p_hsExpLam = foldl1 mplus [do xx90_90 <- dvCharsM
                              case xx90_90 of
                                  '\\' -> return ()
                                  _ -> throwErrorPackratM "'\\\\'" "not match pattern"
                              let '\\' = xx90_90
                              return ()
                              if True then return () else throwErrorPackratM "True" "not match"
                              dv_spacesM >> return ()
                              if True then return () else throwErrorPackratM "True" "not match"
                              ps <- dv_patsM
                              return ()
                              if True then return () else throwErrorPackratM "True" "not match"
                              dv_spacesM >> return ()
                              if True then return () else throwErrorPackratM "True" "not match"
                              xx91_91 <- dvCharsM
                              case xx91_91 of
                                  '-' -> return ()
                                  _ -> throwErrorPackratM "'-'" "not match pattern"
                              let '-' = xx91_91
                              return ()
                              if True then return () else throwErrorPackratM "True" "not match"
                              xx92_92 <- dvCharsM
                              let c = xx92_92
                              if isGt c
                               then return ()
                               else throwErrorPackratM "isGt c" "not match"
                              dv_spacesM >> return ()
                              if True then return () else throwErrorPackratM "True" "not match"
                              e <- dv_hsExpTypM
                              return ()
                              if True then return () else throwErrorPackratM "True" "not match"
                              return (lamE ps e),
                           do e <- dv_hsExpTypM
                              return ()
                              if True then return () else throwErrorPackratM "True" "not match"
                              return e]
p_hsExpTyp = foldl1 mplus [do eo <- dv_hsExpOpM
                              return ()
                              if True then return () else throwErrorPackratM "True" "not match"
                              xx93_93 <- dvCharsM
                              case xx93_93 of
                                  ':' -> return ()
                                  _ -> throwErrorPackratM "':'" "not match pattern"
                              let ':' = xx93_93
                              return ()
                              if True then return () else throwErrorPackratM "True" "not match"
                              xx94_94 <- dvCharsM
                              case xx94_94 of
                                  ':' -> return ()
                                  _ -> throwErrorPackratM "':'" "not match pattern"
                              let ':' = xx94_94
                              return ()
                              if True then return () else throwErrorPackratM "True" "not match"
                              dv_spacesM >> return ()
                              if True then return () else throwErrorPackratM "True" "not match"
                              t <- dv_hsTypeArrM
                              return ()
                              if True then return () else throwErrorPackratM "True" "not match"
                              return (sigE eo t),
                           do eo <- dv_hsExpOpM
                              return ()
                              if True then return () else throwErrorPackratM "True" "not match"
                              return eo]
p_hsExpOp = foldl1 mplus [do l <- dv_hsExpM
                             return ()
                             if True then return () else throwErrorPackratM "True" "not match"
                             dv_spacesM >> return ()
                             if True then return () else throwErrorPackratM "True" "not match"
                             o <- dv_hsOpM
                             return ()
                             if True then return () else throwErrorPackratM "True" "not match"
                             dv_spacesM >> return ()
                             if True then return () else throwErrorPackratM "True" "not match"
                             r <- dv_hsExpOpM
                             return ()
                             if True then return () else throwErrorPackratM "True" "not match"
                             return (uInfixE (getEx l) o r),
                          do e <- dv_hsExpM
                             return ()
                             if True then return () else throwErrorPackratM "True" "not match"
                             return (getEx e)]
p_hsOp = foldl1 mplus [do xx95_95 <- dvCharsM
                          let c = xx95_95
                          if isOpHeadChar c
                           then return ()
                           else throwErrorPackratM "isOpHeadChar c" "not match"
                          o <- dv_opTailM
                          return ()
                          if True then return () else throwErrorPackratM "True" "not match"
                          return (varE (mkName (cons c o))),
                       do xx96_96 <- dvCharsM
                          case xx96_96 of
                              ':' -> return ()
                              _ -> throwErrorPackratM "':'" "not match pattern"
                          let ':' = xx96_96
                          return ()
                          if True then return () else throwErrorPackratM "True" "not match"
                          ddd97_97 <- get
                          flipMaybe (do xx98_98 <- dvCharsM
                                        case xx98_98 of
                                            ':' -> return ()
                                            _ -> throwErrorPackratM "':'" "not match pattern"
                                        let ':' = xx98_98
                                        return ()
                                        if True
                                         then return ()
                                         else throwErrorPackratM "True" "not match")
                          put ddd97_97
                          o <- dv_opTailM
                          return ()
                          if True then return () else throwErrorPackratM "True" "not match"
                          return (conE (mkName (':' : o)))]
p_opTail = foldl1 mplus [do xx99_99 <- dvCharsM
                            let c = xx99_99
                            if isOpTailChar c
                             then return ()
                             else throwErrorPackratM "isOpTailChar c" "not match"
                            s <- dv_opTailM
                            return ()
                            if True then return () else throwErrorPackratM "True" "not match"
                            return (cons c s),
                         do return emp]
p_hsExp = foldl1 mplus [do e <- dv_hsExp1M
                           return ()
                           if True then return () else throwErrorPackratM "True" "not match"
                           dv_spacesM >> return ()
                           if True then return () else throwErrorPackratM "True" "not match"
                           h <- dv_hsExpM
                           return ()
                           if True then return () else throwErrorPackratM "True" "not match"
                           return (applyExR e h),
                        do e <- dv_hsExp1M
                           return ()
                           if True then return () else throwErrorPackratM "True" "not match"
                           return (toEx e)]
p_hsExp1 = foldl1 mplus [do xx100_100 <- dvCharsM
                            case xx100_100 of
                                '(' -> return ()
                                _ -> throwErrorPackratM "'('" "not match pattern"
                            let '(' = xx100_100
                            return ()
                            if True then return () else throwErrorPackratM "True" "not match"
                            et <- dv_hsExpTplM
                            return ()
                            if True then return () else throwErrorPackratM "True" "not match"
                            xx101_101 <- dvCharsM
                            case xx101_101 of
                                ')' -> return ()
                                _ -> throwErrorPackratM "')'" "not match pattern"
                            let ')' = xx101_101
                            return ()
                            if True then return () else throwErrorPackratM "True" "not match"
                            return (tupE et),
                         do xx102_102 <- dvCharsM
                            case xx102_102 of
                                '[' -> return ()
                                _ -> throwErrorPackratM "'['" "not match pattern"
                            let '[' = xx102_102
                            return ()
                            if True then return () else throwErrorPackratM "True" "not match"
                            et <- dv_hsExpTplM
                            return ()
                            if True then return () else throwErrorPackratM "True" "not match"
                            xx103_103 <- dvCharsM
                            case xx103_103 of
                                ']' -> return ()
                                _ -> throwErrorPackratM "']'" "not match pattern"
                            let ']' = xx103_103
                            return ()
                            if True then return () else throwErrorPackratM "True" "not match"
                            return (listE et),
                         do v <- dv_variableM
                            return ()
                            if True then return () else throwErrorPackratM "True" "not match"
                            return (varE (mkName v)),
                         do t <- dv_typM
                            return ()
                            if True then return () else throwErrorPackratM "True" "not match"
                            return (conE (mkName t)),
                         do i <- dv_integerM
                            return ()
                            if True then return () else throwErrorPackratM "True" "not match"
                            dv_spacesM >> return ()
                            if True then return () else throwErrorPackratM "True" "not match"
                            return (litE (integerL i)),
                         do xx104_104 <- dvCharsM
                            case xx104_104 of
                                '\'' -> return ()
                                _ -> throwErrorPackratM "'\\''" "not match pattern"
                            let '\'' = xx104_104
                            return ()
                            if True then return () else throwErrorPackratM "True" "not match"
                            c <- dv_charLitM
                            return ()
                            if True then return () else throwErrorPackratM "True" "not match"
                            xx105_105 <- dvCharsM
                            case xx105_105 of
                                '\'' -> return ()
                                _ -> throwErrorPackratM "'\\''" "not match pattern"
                            let '\'' = xx105_105
                            return ()
                            if True then return () else throwErrorPackratM "True" "not match"
                            return (litE (charL c)),
                         do xx106_106 <- dvCharsM
                            case xx106_106 of
                                '"' -> return ()
                                _ -> throwErrorPackratM "'\"'" "not match pattern"
                            let '"' = xx106_106
                            return ()
                            if True then return () else throwErrorPackratM "True" "not match"
                            s <- dv_stringLitM
                            return ()
                            if True then return () else throwErrorPackratM "True" "not match"
                            xx107_107 <- dvCharsM
                            case xx107_107 of
                                '"' -> return ()
                                _ -> throwErrorPackratM "'\"'" "not match pattern"
                            let '"' = xx107_107
                            return ()
                            if True then return () else throwErrorPackratM "True" "not match"
                            return (litE (stringL s)),
                         do xx108_108 <- dvCharsM
                            case xx108_108 of
                                '-' -> return ()
                                _ -> throwErrorPackratM "'-'" "not match pattern"
                            let '-' = xx108_108
                            return ()
                            if True then return () else throwErrorPackratM "True" "not match"
                            dv_spacesM >> return ()
                            if True then return () else throwErrorPackratM "True" "not match"
                            e <- dv_hsExp1M
                            return ()
                            if True then return () else throwErrorPackratM "True" "not match"
                            return (appE (varE $ mkName "negate") e)]
p_hsExpTpl = foldl1 mplus [do e <- dv_hsExpLamM
                              return ()
                              if True then return () else throwErrorPackratM "True" "not match"
                              dv_spacesM >> return ()
                              if True then return () else throwErrorPackratM "True" "not match"
                              xx109_109 <- dvCharsM
                              let c = xx109_109
                              if isComma c
                               then return ()
                               else throwErrorPackratM "isComma c" "not match"
                              dv_spacesM >> return ()
                              if True then return () else throwErrorPackratM "True" "not match"
                              et <- dv_hsExpTplM
                              return ()
                              if True then return () else throwErrorPackratM "True" "not match"
                              return (cons e et),
                           do e <- dv_hsExpLamM
                              return ()
                              if True then return () else throwErrorPackratM "True" "not match"
                              return (cons e emp),
                           do return emp]
p_hsTypeArr = foldl1 mplus [do l <- dv_hsTypeM
                               return ()
                               if True then return () else throwErrorPackratM "True" "not match"
                               xx110_110 <- dvCharsM
                               case xx110_110 of
                                   '-' -> return ()
                                   _ -> throwErrorPackratM "'-'" "not match pattern"
                               let '-' = xx110_110
                               return ()
                               if True then return () else throwErrorPackratM "True" "not match"
                               xx111_111 <- dvCharsM
                               let c = xx111_111
                               if isGt c
                                then return ()
                                else throwErrorPackratM "isGt c" "not match"
                               dv_spacesM >> return ()
                               if True then return () else throwErrorPackratM "True" "not match"
                               r <- dv_hsTypeArrM
                               return ()
                               if True then return () else throwErrorPackratM "True" "not match"
                               return (appT (appT arrowT (getTyp l)) r),
                            do t <- dv_hsTypeM
                               return ()
                               if True then return () else throwErrorPackratM "True" "not match"
                               return (getTyp t)]
p_hsType = foldl1 mplus [do t <- dv_hsType1M
                            return ()
                            if True then return () else throwErrorPackratM "True" "not match"
                            ts <- dv_hsTypeM
                            return ()
                            if True then return () else throwErrorPackratM "True" "not match"
                            return (applyTyp (toTyp t) ts),
                         do t <- dv_hsType1M
                            return ()
                            if True then return () else throwErrorPackratM "True" "not match"
                            return (toTyp t)]
p_hsType1 = foldl1 mplus [do xx112_112 <- dvCharsM
                             case xx112_112 of
                                 '[' -> return ()
                                 _ -> throwErrorPackratM "'['" "not match pattern"
                             let '[' = xx112_112
                             return ()
                             if True then return () else throwErrorPackratM "True" "not match"
                             xx113_113 <- dvCharsM
                             case xx113_113 of
                                 ']' -> return ()
                                 _ -> throwErrorPackratM "']'" "not match pattern"
                             let ']' = xx113_113
                             return ()
                             if True then return () else throwErrorPackratM "True" "not match"
                             dv_spacesM >> return ()
                             if True then return () else throwErrorPackratM "True" "not match"
                             return listT,
                          do xx114_114 <- dvCharsM
                             case xx114_114 of
                                 '[' -> return ()
                                 _ -> throwErrorPackratM "'['" "not match pattern"
                             let '[' = xx114_114
                             return ()
                             if True then return () else throwErrorPackratM "True" "not match"
                             t <- dv_hsTypeArrM
                             return ()
                             if True then return () else throwErrorPackratM "True" "not match"
                             xx115_115 <- dvCharsM
                             case xx115_115 of
                                 ']' -> return ()
                                 _ -> throwErrorPackratM "']'" "not match pattern"
                             let ']' = xx115_115
                             return ()
                             if True then return () else throwErrorPackratM "True" "not match"
                             return (appT listT t),
                          do xx116_116 <- dvCharsM
                             case xx116_116 of
                                 '(' -> return ()
                                 _ -> throwErrorPackratM "'('" "not match pattern"
                             let '(' = xx116_116
                             return ()
                             if True then return () else throwErrorPackratM "True" "not match"
                             dv_spacesM >> return ()
                             if True then return () else throwErrorPackratM "True" "not match"
                             tt <- dv_hsTypeTplM
                             return ()
                             if True then return () else throwErrorPackratM "True" "not match"
                             xx117_117 <- dvCharsM
                             case xx117_117 of
                                 ')' -> return ()
                                 _ -> throwErrorPackratM "')'" "not match pattern"
                             let ')' = xx117_117
                             return ()
                             if True then return () else throwErrorPackratM "True" "not match"
                             return (tupT tt),
                          do t <- dv_typTokenM
                             return ()
                             if True then return () else throwErrorPackratM "True" "not match"
                             return (conT (mkName t)),
                          do xx118_118 <- dvCharsM
                             case xx118_118 of
                                 '(' -> return ()
                                 _ -> throwErrorPackratM "'('" "not match pattern"
                             let '(' = xx118_118
                             return ()
                             if True then return () else throwErrorPackratM "True" "not match"
                             xx119_119 <- dvCharsM
                             case xx119_119 of
                                 '-' -> return ()
                                 _ -> throwErrorPackratM "'-'" "not match pattern"
                             let '-' = xx119_119
                             return ()
                             if True then return () else throwErrorPackratM "True" "not match"
                             xx120_120 <- dvCharsM
                             let c = xx120_120
                             if isGt c
                              then return ()
                              else throwErrorPackratM "isGt c" "not match"
                             xx121_121 <- dvCharsM
                             case xx121_121 of
                                 ')' -> return ()
                                 _ -> throwErrorPackratM "')'" "not match pattern"
                             let ')' = xx121_121
                             return ()
                             if True then return () else throwErrorPackratM "True" "not match"
                             dv_spacesM >> return ()
                             if True then return () else throwErrorPackratM "True" "not match"
                             return arrowT]
p_hsTypeTpl = foldl1 mplus [do t <- dv_hsTypeArrM
                               return ()
                               if True then return () else throwErrorPackratM "True" "not match"
                               xx122_122 <- dvCharsM
                               let c = xx122_122
                               if isComma c
                                then return ()
                                else throwErrorPackratM "isComma c" "not match"
                               dv_spacesM >> return ()
                               if True then return () else throwErrorPackratM "True" "not match"
                               tt <- dv_hsTypeTplM
                               return ()
                               if True then return () else throwErrorPackratM "True" "not match"
                               return (cons t tt),
                            do t <- dv_hsTypeArrM
                               return ()
                               if True then return () else throwErrorPackratM "True" "not match"
                               return (cons t emp),
                            do return emp]
p_typ = foldl1 mplus [do u <- dv_upperM
                         return ()
                         if True then return () else throwErrorPackratM "True" "not match"
                         t <- dv_tvtailM
                         return ()
                         if True then return () else throwErrorPackratM "True" "not match"
                         return (cons u t)]
p_variable = foldl1 mplus [do l <- dv_lowerM
                              return ()
                              if True then return () else throwErrorPackratM "True" "not match"
                              t <- dv_tvtailM
                              return ()
                              if True then return () else throwErrorPackratM "True" "not match"
                              return (cons l t)]
p_tvtail = foldl1 mplus [do a <- dv_alphaM
                            return ()
                            if True then return () else throwErrorPackratM "True" "not match"
                            t <- dv_tvtailM
                            return ()
                            if True then return () else throwErrorPackratM "True" "not match"
                            return (cons a t),
                         do return emp]
p_integer = foldl1 mplus [do dh <- dv_digitM
                             return ()
                             if True then return () else throwErrorPackratM "True" "not match"
                             xx123_123 <- list (foldl1 mplus [do d <- dv_digitM
                                                                 return ()
                                                                 if True
                                                                  then return ()
                                                                  else throwErrorPackratM "True" "not match"
                                                                 return d])
                             let ds = xx123_123
                             return ()
                             return (read (cons dh ds))]
p_alpha = foldl1 mplus [do u <- dv_upperM
                           return ()
                           if True then return () else throwErrorPackratM "True" "not match"
                           return u,
                        do l <- dv_lowerM
                           return ()
                           if True then return () else throwErrorPackratM "True" "not match"
                           return l,
                        do d <- dv_digitM
                           return ()
                           if True then return () else throwErrorPackratM "True" "not match"
                           return d]
p_upper = foldl1 mplus [do xx124_124 <- dvCharsM
                           let u = xx124_124
                           if isUpper u
                            then return ()
                            else throwErrorPackratM "isUpper u" "not match"
                           return u]
p_lower = foldl1 mplus [do xx125_125 <- dvCharsM
                           let l = xx125_125
                           if isLowerU l
                            then return ()
                            else throwErrorPackratM "isLowerU l" "not match"
                           return l]
p_digit = foldl1 mplus [do xx126_126 <- dvCharsM
                           let d = xx126_126
                           if isDigit d
                            then return ()
                            else throwErrorPackratM "isDigit d" "not match"
                           return d]
p_spaces = foldl1 mplus [do dv_spaceM >> return ()
                            if True then return () else throwErrorPackratM "True" "not match"
                            dv_spacesM >> return ()
                            if True then return () else throwErrorPackratM "True" "not match"
                            return (),
                         do return ()]
p_space = foldl1 mplus [do xx127_127 <- dvCharsM
                           let s = xx127_127
                           if isSpace s
                            then return ()
                            else throwErrorPackratM "isSpace s" "not match"
                           return (),
                        do xx128_128 <- dvCharsM
                           case xx128_128 of
                               '-' -> return ()
                               _ -> throwErrorPackratM "'-'" "not match pattern"
                           let '-' = xx128_128
                           return ()
                           if True then return () else throwErrorPackratM "True" "not match"
                           xx129_129 <- dvCharsM
                           case xx129_129 of
                               '-' -> return ()
                               _ -> throwErrorPackratM "'-'" "not match pattern"
                           let '-' = xx129_129
                           return ()
                           if True then return () else throwErrorPackratM "True" "not match"
                           dv_notNLStringM >> return ()
                           if True then return () else throwErrorPackratM "True" "not match"
                           dv_nlM >> return ()
                           if True then return () else throwErrorPackratM "True" "not match"
                           return (),
                        do dv_commentM >> return ()
                           if True then return () else throwErrorPackratM "True" "not match"
                           return ()]
p_notNLString = foldl1 mplus [do ddd130_130 <- get
                                 flipMaybe (do dv_nlM >> return ()
                                               if True
                                                then return ()
                                                else throwErrorPackratM "True" "not match")
                                 put ddd130_130
                                 xx131_131 <- dvCharsM
                                 let c = xx131_131
                                 if True then return () else throwErrorPackratM "True" "not match"
                                 s <- dv_notNLStringM
                                 return ()
                                 if True then return () else throwErrorPackratM "True" "not match"
                                 return (cons c s),
                              do return emp]
p_nl = foldl1 mplus [do xx132_132 <- dvCharsM
                        case xx132_132 of
                            '\n' -> return ()
                            _ -> throwErrorPackratM "'\\n'" "not match pattern"
                        let '\n' = xx132_132
                        return ()
                        if True then return () else throwErrorPackratM "True" "not match"
                        return ()]
p_comment = foldl1 mplus [do xx133_133 <- dvCharsM
                             case xx133_133 of
                                 '{' -> return ()
                                 _ -> throwErrorPackratM "'{'" "not match pattern"
                             let '{' = xx133_133
                             return ()
                             if True then return () else throwErrorPackratM "True" "not match"
                             xx134_134 <- dvCharsM
                             case xx134_134 of
                                 '-' -> return ()
                                 _ -> throwErrorPackratM "'-'" "not match pattern"
                             let '-' = xx134_134
                             return ()
                             if True then return () else throwErrorPackratM "True" "not match"
                             ddd135_135 <- get
                             flipMaybe (do xx136_136 <- dvCharsM
                                           case xx136_136 of
                                               '#' -> return ()
                                               _ -> throwErrorPackratM "'#'" "not match pattern"
                                           let '#' = xx136_136
                                           return ()
                                           if True
                                            then return ()
                                            else throwErrorPackratM "True" "not match")
                             put ddd135_135
                             dv_commentsM >> return ()
                             if True then return () else throwErrorPackratM "True" "not match"
                             dv_comEndM >> return ()
                             if True then return () else throwErrorPackratM "True" "not match"
                             return ()]
p_comments = foldl1 mplus [do dv_notComStrM >> return ()
                              if True then return () else throwErrorPackratM "True" "not match"
                              dv_commentM >> return ()
                              if True then return () else throwErrorPackratM "True" "not match"
                              dv_commentsM >> return ()
                              if True then return () else throwErrorPackratM "True" "not match"
                              return (),
                           do dv_notComStrM >> return ()
                              if True then return () else throwErrorPackratM "True" "not match"
                              return ()]
p_notComStr = foldl1 mplus [do ddd137_137 <- get
                               flipMaybe (do dv_commentM >> return ()
                                             if True
                                              then return ()
                                              else throwErrorPackratM "True" "not match")
                               put ddd137_137
                               ddd138_138 <- get
                               flipMaybe (do dv_comEndM >> return ()
                                             if True
                                              then return ()
                                              else throwErrorPackratM "True" "not match")
                               put ddd138_138
                               _ <- dvCharsM
                               if True then return () else throwErrorPackratM "True" "not match"
                               dv_notComStrM >> return ()
                               if True then return () else throwErrorPackratM "True" "not match"
                               return (),
                            do return ()]
p_comEnd = foldl1 mplus [do xx140_139 <- dvCharsM
                            case xx140_139 of
                                '-' -> return ()
                                _ -> throwErrorPackratM "'-'" "not match pattern"
                            let '-' = xx140_139
                            return ()
                            if True then return () else throwErrorPackratM "True" "not match"
                            xx141_140 <- dvCharsM
                            case xx141_140 of
                                '}' -> return ()
                                _ -> throwErrorPackratM "'}'" "not match pattern"
                            let '}' = xx141_140
                            return ()
                            if True then return () else throwErrorPackratM "True" "not match"
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