{-# LANGUAGE FlexibleContexts, TemplateHaskell , FlexibleContexts, PackageImports, TypeFamilies, RankNTypes #-}
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



import Data.Char
import Language.Haskell.TH

type MaybeString = Maybe String

type Nil = ()
-- type Leaf = Either String ExR
type Leaf = (Maybe String, ExR)
data NameLeaf
	= NameLeaf PatQ Leaf
	| NameLeafList PatQ Selection
data NameLeaf_ = NotAfter NameLeaf | Here NameLeaf
notAfter, here :: NameLeaf -> NameLeaf_
notAfter = NotAfter
here = Here
type Expression = [NameLeaf_]
type ExpressionHs = (Expression, ExR)
type Selection = [ExpressionHs]
type Typ = Name
type Definition = (String, Typ, Selection)
type Peg = [Definition]
type TTPeg = (TypeQ, TypeQ, Peg)

type Ex = ExpQ -> ExpQ
type ExR = ExpQ

ctLeaf :: Leaf
-- ctLeaf = Right $ conE (mkName "True") -- varE (mkName "const") `appE` conE (mkName "True")
ctLeaf = (Nothing, conE $ mkName "True")

ruleLeaf :: String -> ExpQ -> Leaf
boolLeaf :: ExpQ -> Leaf
ruleLeaf r t = (Just r, t) -- Left
boolLeaf p = (Nothing, p) -- Right

true :: ExpQ
true = conE $ mkName "True"

just :: a -> Maybe a
just = Just
nothing :: Maybe a
nothing = Nothing

nil :: Nil
nil = ()

cons :: a -> [a] -> [a]
cons = (:)

type PatQs = [PatQ]

mkNameLeaf :: PatQ -> Leaf -> NameLeaf
mkNameLeaf = NameLeaf

mkNameLeafList :: PatQ -> Selection -> NameLeaf
mkNameLeafList = NameLeafList

strToPatQ :: String -> PatQ
strToPatQ = varP . mkName

conToPatQ :: String -> [PatQ] -> PatQ
conToPatQ t ps = conP (mkName t) ps

mkExpressionHs :: a -> Ex -> (a, ExR)
mkExpressionHs x y = (x, getEx y)

mkDef :: a -> String -> c -> (a, Name, c)
mkDef x y z = (x, mkName y, z)

toExp :: String -> Ex
toExp v = \f -> f `appE` varE (mkName v)

apply :: String -> Ex -> Ex
apply f x = \g -> x (toExp f g)

getEx :: Ex -> ExR
getEx ex = ex (varE $ mkName "id")

emp :: [a]
emp = []

type PegFile = (String, String, TTPeg, String)
mkPegFile :: Maybe String -> Maybe String -> String -> String -> TTPeg -> String
	-> PegFile
mkPegFile (Just p) (Just md) x y z w =
	("{-#" ++ p ++ addPragmas ++ "module " ++ md ++ " where\n" ++
	addModules,
	x ++ "\n" ++ y, z, w)
mkPegFile Nothing (Just md) x y z w =
	(x ++ "\n" ++ "module " ++ md ++ " where\n" ++
	addModules,
	x ++ "\n" ++ y, z, w)
mkPegFile (Just p) Nothing x y z w = (
	"{-#" ++ p ++ addPragmas ++
	addModules,
	x ++ "\n" ++ y
	, z, w)
mkPegFile Nothing Nothing x y z w = (addModules, x ++ "\n" ++ y, z, w)

addPragmas, addModules :: String
addPragmas =
	", FlexibleContexts, PackageImports, TypeFamilies, RankNTypes #-}\n"
addModules =
	"import \"monads-tf\" Control.Monad.State\n" ++
	"import \"monads-tf\" Control.Monad.Error\n" ++
	"import Control.Monad.Trans.Error (Error (..))\n"

charP :: Char -> PatQ
charP = litP . charL
stringP :: String -> PatQ
stringP = litP . stringL

isAlphaNumOt, elemNTs :: Char -> Bool
isAlphaNumOt c = isAlphaNum c || c `elem` "{-#.\":}|[]!;=/ *()"
elemNTs = (`elem` "nt\\'")

isKome, isOpen, isClose :: Char -> Bool
isKome = (== '*')
isOpen = (== '(')
isClose = (== ')')

getNTs :: Char -> Char
getNTs 'n' = '\n'
getNTs 't' = '\t'
getNTs '\\' = '\\'
getNTs '\'' = '\''
getNTs o = o
isLowerU :: Char -> Bool
isLowerU c = isLower c || c == '_'

tString :: String
tString = "String"
mkTTPeg :: String -> Peg -> TTPeg
mkTTPeg s p =
	(conT $ mkName s, conT (mkName "Token") `appT` conT (mkName s), p)

flipMaybe :: forall a . PackratM a -> PackratM ()
flipMaybe act = do pos382_0 <- gets dvPos
                   err <- (act >> return False) `catchError` const (return True)
                   unless err (throwError (strMsg ("not not match: pos: " ++ showPos pos382_0)))
type PackratM = StateT Derivs (Either String)
type Result v = Either String ((v, Derivs))
data Derivs
    = Derivs {dv_pegFile :: (Result PegFile),
              dv_pragma :: (Result MaybeString),
              dv_pragmaStr :: (Result String),
              dv_pragmaEnd :: (Result Nil),
              dv_moduleDec :: (Result MaybeString),
              dv_moduleDecStr :: (Result String),
              dv_whr :: (Result Nil),
              dv_preImpPap :: (Result String),
              dv_prePeg :: (Result String),
              dv_afterPeg :: (Result String),
              dv_importPapillon :: (Result Nil),
              dv_varToken :: (Result String),
              dv_typToken :: (Result String),
              dv_pap :: (Result Nil),
              dv_peg :: (Result TTPeg),
              dv_sourceType :: (Result String),
              dv_peg_ :: (Result Peg),
              dv_definition :: (Result Definition),
              dv_selection :: (Result Selection),
              dv_expressionHs :: (Result ExpressionHs),
              dv_expression :: (Result Expression),
              dv_nameLeaf_ :: (Result NameLeaf_),
              dv_nameLeaf :: (Result NameLeaf),
              dv_pat :: (Result PatQ),
              dv_pat1 :: (Result PatQ),
              dv_charLit :: (Result Char),
              dv_stringLit :: (Result String),
              dv_dq :: (Result Nil),
              dv_pats :: (Result PatQs),
              dv_leaf :: (Result Leaf),
              dv_test :: (Result ExR),
              dv_hsExp :: (Result Ex),
              dv_typ :: (Result String),
              dv_variable :: (Result String),
              dv_tvtail :: (Result String),
              dv_alpha :: (Result Char),
              dv_upper :: (Result Char),
              dv_lower :: (Result Char),
              dv_digit :: (Result Char),
              dv_spaces :: (Result Nil),
              dv_space :: (Result Nil),
              dv_notNLString :: (Result String),
              dv_nl :: (Result Nil),
              dv_comment :: (Result Nil),
              dv_comments :: (Result Nil),
              dv_notComStr :: (Result Nil),
              dv_comEnd :: (Result Nil),
              dvChars :: (Result (Token String)),
              dvPos :: (Pos String)}
parse :: Pos String -> String -> Derivs
parse pos___hoge s = d
          where d = Derivs pegFile pragma pragmaStr pragmaEnd moduleDec moduleDecStr whr preImpPap prePeg afterPeg importPapillon varToken typToken pap peg sourceType peg_ definition selection expressionHs expression nameLeaf_ nameLeaf pat pat1 charLit stringLit dq pats leaf test hsExp typ variable tvtail alpha upper lower digit spaces space notNLString nl comment comments notComStr comEnd char pos___hoge
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
                pat = runStateT p_pat d
                pat1 = runStateT p_pat1 d
                charLit = runStateT p_charLit d
                stringLit = runStateT p_stringLit d
                dq = runStateT p_dq d
                pats = runStateT p_pats d
                leaf = runStateT p_leaf d
                test = runStateT p_test d
                hsExp = runStateT p_hsExp d
                typ = runStateT p_typ d
                variable = runStateT p_variable d
                tvtail = runStateT p_tvtail d
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
                                             _ -> throwError (strMsg "eof"))
dv_pragmaM :: PackratM MaybeString
dv_pragmaStrM :: PackratM String
dv_pragmaEndM :: PackratM Nil
dv_moduleDecM :: PackratM MaybeString
dv_moduleDecStrM :: PackratM String
dv_whrM :: PackratM Nil
dv_preImpPapM :: PackratM String
dv_prePegM :: PackratM String
dv_afterPegM :: PackratM String
dv_importPapillonM :: PackratM Nil
dv_varTokenM :: PackratM String
dv_typTokenM :: PackratM String
dv_papM :: PackratM Nil
dv_pegM :: PackratM TTPeg
dv_sourceTypeM :: PackratM String
dv_peg_M :: PackratM Peg
dv_definitionM :: PackratM Definition
dv_selectionM :: PackratM Selection
dv_expressionHsM :: PackratM ExpressionHs
dv_expressionM :: PackratM Expression
dv_nameLeaf_M :: PackratM NameLeaf_
dv_nameLeafM :: PackratM NameLeaf
dv_patM :: PackratM PatQ
dv_pat1M :: PackratM PatQ
dv_charLitM :: PackratM Char
dv_stringLitM :: PackratM String
dv_dqM :: PackratM Nil
dv_patsM :: PackratM PatQs
dv_leafM :: PackratM Leaf
dv_testM :: PackratM ExR
dv_hsExpM :: PackratM Ex
dv_typM :: PackratM String
dv_variableM :: PackratM String
dv_tvtailM :: PackratM String
dv_alphaM :: PackratM Char
dv_upperM :: PackratM Char
dv_lowerM :: PackratM Char
dv_digitM :: PackratM Char
dv_spacesM :: PackratM Nil
dv_spaceM :: PackratM Nil
dv_notNLStringM :: PackratM String
dv_nlM :: PackratM Nil
dv_commentM :: PackratM Nil
dv_commentsM :: PackratM Nil
dv_notComStrM :: PackratM Nil
dv_comEndM :: PackratM Nil
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
dv_patM = StateT dv_pat
dv_pat1M = StateT dv_pat1
dv_charLitM = StateT dv_charLit
dv_stringLitM = StateT dv_stringLit
dv_dqM = StateT dv_dq
dv_patsM = StateT dv_pats
dv_leafM = StateT dv_leaf
dv_testM = StateT dv_test
dv_hsExpM = StateT dv_hsExp
dv_typM = StateT dv_typ
dv_variableM = StateT dv_variable
dv_tvtailM = StateT dv_tvtail
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
p_pragma :: PackratM MaybeString
p_pragmaStr :: PackratM String
p_pragmaEnd :: PackratM Nil
p_moduleDec :: PackratM MaybeString
p_moduleDecStr :: PackratM String
p_whr :: PackratM Nil
p_preImpPap :: PackratM String
p_prePeg :: PackratM String
p_afterPeg :: PackratM String
p_importPapillon :: PackratM Nil
p_varToken :: PackratM String
p_typToken :: PackratM String
p_pap :: PackratM Nil
p_peg :: PackratM TTPeg
p_sourceType :: PackratM String
p_peg_ :: PackratM Peg
p_definition :: PackratM Definition
p_selection :: PackratM Selection
p_expressionHs :: PackratM ExpressionHs
p_expression :: PackratM Expression
p_nameLeaf_ :: PackratM NameLeaf_
p_nameLeaf :: PackratM NameLeaf
p_pat :: PackratM PatQ
p_pat1 :: PackratM PatQ
p_charLit :: PackratM Char
p_stringLit :: PackratM String
p_dq :: PackratM Nil
p_pats :: PackratM PatQs
p_leaf :: PackratM Leaf
p_test :: PackratM ExR
p_hsExp :: PackratM Ex
p_typ :: PackratM String
p_variable :: PackratM String
p_tvtail :: PackratM String
p_alpha :: PackratM Char
p_upper :: PackratM Char
p_lower :: PackratM Char
p_digit :: PackratM Char
p_spaces :: PackratM Nil
p_space :: PackratM Nil
p_notNLString :: PackratM String
p_nl :: PackratM Nil
p_comment :: PackratM Nil
p_comments :: PackratM Nil
p_notComStr :: PackratM Nil
p_comEnd :: PackratM Nil
p_pegFile = foldl1 mplus [do pos0_1 <- gets dvPos
                             pr <- dv_pragmaM
                             return ()
                             if True
                              then return ()
                              else throwError (strMsg ("not match: pos: " ++ showPos pos0_1))
                             pos1_2 <- gets dvPos
                             md <- dv_moduleDecM
                             return ()
                             if True
                              then return ()
                              else throwError (strMsg ("not match: pos: " ++ showPos pos1_2))
                             pos2_3 <- gets dvPos
                             pip <- dv_preImpPapM
                             return ()
                             if True
                              then return ()
                              else throwError (strMsg ("not match: pos: " ++ showPos pos2_3))
                             pos3_4 <- gets dvPos
                             dv_importPapillonM >> return ()
                             if True
                              then return ()
                              else throwError (strMsg ("not match: pos: " ++ showPos pos3_4))
                             pos4_5 <- gets dvPos
                             pp <- dv_prePegM
                             return ()
                             if True
                              then return ()
                              else throwError (strMsg ("not match: pos: " ++ showPos pos4_5))
                             pos5_6 <- gets dvPos
                             dv_papM >> return ()
                             if True
                              then return ()
                              else throwError (strMsg ("not match: pos: " ++ showPos pos5_6))
                             pos6_7 <- gets dvPos
                             p <- dv_pegM
                             return ()
                             if True
                              then return ()
                              else throwError (strMsg ("not match: pos: " ++ showPos pos6_7))
                             pos7_8 <- gets dvPos
                             dv_spacesM >> return ()
                             if True
                              then return ()
                              else throwError (strMsg ("not match: pos: " ++ showPos pos7_8))
                             xx8_9 <- dvCharsM
                             pos10_10 <- gets dvPos
                             case xx8_9 of
                                 '|' -> return ()
                                 _ -> throwError (strMsg ("not match: pos: " ++ showPos pos10_10))
                             let '|' = xx8_9
                             return ()
                             pos9_11 <- gets dvPos
                             if True
                              then return ()
                              else throwError (strMsg ("not match: pos: " ++ showPos pos9_11))
                             xx11_12 <- dvCharsM
                             pos13_13 <- gets dvPos
                             case xx11_12 of
                                 ']' -> return ()
                                 _ -> throwError (strMsg ("not match: pos: " ++ showPos pos13_13))
                             let ']' = xx11_12
                             return ()
                             pos12_14 <- gets dvPos
                             if True
                              then return ()
                              else throwError (strMsg ("not match: pos: " ++ showPos pos12_14))
                             xx14_15 <- dvCharsM
                             pos16_16 <- gets dvPos
                             case xx14_15 of
                                 '\n' -> return ()
                                 _ -> throwError (strMsg ("not match: pos: " ++ showPos pos16_16))
                             let '\n' = xx14_15
                             return ()
                             pos15_17 <- gets dvPos
                             if True
                              then return ()
                              else throwError (strMsg ("not match: pos: " ++ showPos pos15_17))
                             pos17_18 <- gets dvPos
                             atp <- dv_afterPegM
                             return ()
                             if True
                              then return ()
                              else throwError (strMsg ("not match: pos: " ++ showPos pos17_18))
                             return (id mkPegFile pr md pip pp p atp),
                          do pos18_19 <- gets dvPos
                             pr <- dv_pragmaM
                             return ()
                             if True
                              then return ()
                              else throwError (strMsg ("not match: pos: " ++ showPos pos18_19))
                             pos19_20 <- gets dvPos
                             md <- dv_moduleDecM
                             return ()
                             if True
                              then return ()
                              else throwError (strMsg ("not match: pos: " ++ showPos pos19_20))
                             pos20_21 <- gets dvPos
                             pp <- dv_prePegM
                             return ()
                             if True
                              then return ()
                              else throwError (strMsg ("not match: pos: " ++ showPos pos20_21))
                             pos21_22 <- gets dvPos
                             dv_papM >> return ()
                             if True
                              then return ()
                              else throwError (strMsg ("not match: pos: " ++ showPos pos21_22))
                             pos22_23 <- gets dvPos
                             p <- dv_pegM
                             return ()
                             if True
                              then return ()
                              else throwError (strMsg ("not match: pos: " ++ showPos pos22_23))
                             pos23_24 <- gets dvPos
                             dv_spacesM >> return ()
                             if True
                              then return ()
                              else throwError (strMsg ("not match: pos: " ++ showPos pos23_24))
                             xx24_25 <- dvCharsM
                             pos26_26 <- gets dvPos
                             case xx24_25 of
                                 '|' -> return ()
                                 _ -> throwError (strMsg ("not match: pos: " ++ showPos pos26_26))
                             let '|' = xx24_25
                             return ()
                             pos25_27 <- gets dvPos
                             if True
                              then return ()
                              else throwError (strMsg ("not match: pos: " ++ showPos pos25_27))
                             xx27_28 <- dvCharsM
                             pos29_29 <- gets dvPos
                             case xx27_28 of
                                 ']' -> return ()
                                 _ -> throwError (strMsg ("not match: pos: " ++ showPos pos29_29))
                             let ']' = xx27_28
                             return ()
                             pos28_30 <- gets dvPos
                             if True
                              then return ()
                              else throwError (strMsg ("not match: pos: " ++ showPos pos28_30))
                             xx30_31 <- dvCharsM
                             pos32_32 <- gets dvPos
                             case xx30_31 of
                                 '\n' -> return ()
                                 _ -> throwError (strMsg ("not match: pos: " ++ showPos pos32_32))
                             let '\n' = xx30_31
                             return ()
                             pos31_33 <- gets dvPos
                             if True
                              then return ()
                              else throwError (strMsg ("not match: pos: " ++ showPos pos31_33))
                             pos33_34 <- gets dvPos
                             atp <- dv_afterPegM
                             return ()
                             if True
                              then return ()
                              else throwError (strMsg ("not match: pos: " ++ showPos pos33_34))
                             return (id mkPegFile pr md emp pp p atp)]
p_pragma = foldl1 mplus [do pos34_35 <- gets dvPos
                            dv_spacesM >> return ()
                            if True
                             then return ()
                             else throwError (strMsg ("not match: pos: " ++ showPos pos34_35))
                            xx35_36 <- dvCharsM
                            pos37_37 <- gets dvPos
                            case xx35_36 of
                                '{' -> return ()
                                _ -> throwError (strMsg ("not match: pos: " ++ showPos pos37_37))
                            let '{' = xx35_36
                            return ()
                            pos36_38 <- gets dvPos
                            if True
                             then return ()
                             else throwError (strMsg ("not match: pos: " ++ showPos pos36_38))
                            xx38_39 <- dvCharsM
                            pos40_40 <- gets dvPos
                            case xx38_39 of
                                '-' -> return ()
                                _ -> throwError (strMsg ("not match: pos: " ++ showPos pos40_40))
                            let '-' = xx38_39
                            return ()
                            pos39_41 <- gets dvPos
                            if True
                             then return ()
                             else throwError (strMsg ("not match: pos: " ++ showPos pos39_41))
                            xx41_42 <- dvCharsM
                            pos43_43 <- gets dvPos
                            case xx41_42 of
                                '#' -> return ()
                                _ -> throwError (strMsg ("not match: pos: " ++ showPos pos43_43))
                            let '#' = xx41_42
                            return ()
                            pos42_44 <- gets dvPos
                            if True
                             then return ()
                             else throwError (strMsg ("not match: pos: " ++ showPos pos42_44))
                            pos44_45 <- gets dvPos
                            s <- dv_pragmaStrM
                            return ()
                            if True
                             then return ()
                             else throwError (strMsg ("not match: pos: " ++ showPos pos44_45))
                            pos45_46 <- gets dvPos
                            dv_pragmaEndM >> return ()
                            if True
                             then return ()
                             else throwError (strMsg ("not match: pos: " ++ showPos pos45_46))
                            pos46_47 <- gets dvPos
                            dv_spacesM >> return ()
                            if True
                             then return ()
                             else throwError (strMsg ("not match: pos: " ++ showPos pos46_47))
                            return (id just s),
                         do pos47_48 <- gets dvPos
                            dv_spacesM >> return ()
                            if True
                             then return ()
                             else throwError (strMsg ("not match: pos: " ++ showPos pos47_48))
                            return (id nothing)]
p_pragmaStr = foldl1 mplus [do ddd48_49 <- get
                               flipMaybe (do pos49_50 <- gets dvPos
                                             dv_pragmaEndM >> return ()
                                             if True
                                              then return ()
                                              else throwError (strMsg ("not match: pos: " ++ showPos pos49_50)))
                               put ddd48_49
                               xx50_51 <- dvCharsM
                               let c = xx50_51
                               pos51_52 <- gets dvPos
                               if True
                                then return ()
                                else throwError (strMsg ("not match: pos: " ++ showPos pos51_52))
                               pos52_53 <- gets dvPos
                               s <- dv_pragmaStrM
                               return ()
                               if True
                                then return ()
                                else throwError (strMsg ("not match: pos: " ++ showPos pos52_53))
                               return (id cons c s),
                            do return (id emp)]
p_pragmaEnd = foldl1 mplus [do xx53_54 <- dvCharsM
                               pos55_55 <- gets dvPos
                               case xx53_54 of
                                   '#' -> return ()
                                   _ -> throwError (strMsg ("not match: pos: " ++ showPos pos55_55))
                               let '#' = xx53_54
                               return ()
                               pos54_56 <- gets dvPos
                               if True
                                then return ()
                                else throwError (strMsg ("not match: pos: " ++ showPos pos54_56))
                               xx56_57 <- dvCharsM
                               pos58_58 <- gets dvPos
                               case xx56_57 of
                                   '-' -> return ()
                                   _ -> throwError (strMsg ("not match: pos: " ++ showPos pos58_58))
                               let '-' = xx56_57
                               return ()
                               pos57_59 <- gets dvPos
                               if True
                                then return ()
                                else throwError (strMsg ("not match: pos: " ++ showPos pos57_59))
                               xx59_60 <- dvCharsM
                               pos61_61 <- gets dvPos
                               case xx59_60 of
                                   '}' -> return ()
                                   _ -> throwError (strMsg ("not match: pos: " ++ showPos pos61_61))
                               let '}' = xx59_60
                               return ()
                               pos60_62 <- gets dvPos
                               if True
                                then return ()
                                else throwError (strMsg ("not match: pos: " ++ showPos pos60_62))
                               return (id nil)]
p_moduleDec = foldl1 mplus [do xx62_63 <- dvCharsM
                               pos64_64 <- gets dvPos
                               case xx62_63 of
                                   'm' -> return ()
                                   _ -> throwError (strMsg ("not match: pos: " ++ showPos pos64_64))
                               let 'm' = xx62_63
                               return ()
                               pos63_65 <- gets dvPos
                               if True
                                then return ()
                                else throwError (strMsg ("not match: pos: " ++ showPos pos63_65))
                               xx65_66 <- dvCharsM
                               pos67_67 <- gets dvPos
                               case xx65_66 of
                                   'o' -> return ()
                                   _ -> throwError (strMsg ("not match: pos: " ++ showPos pos67_67))
                               let 'o' = xx65_66
                               return ()
                               pos66_68 <- gets dvPos
                               if True
                                then return ()
                                else throwError (strMsg ("not match: pos: " ++ showPos pos66_68))
                               xx68_69 <- dvCharsM
                               pos70_70 <- gets dvPos
                               case xx68_69 of
                                   'd' -> return ()
                                   _ -> throwError (strMsg ("not match: pos: " ++ showPos pos70_70))
                               let 'd' = xx68_69
                               return ()
                               pos69_71 <- gets dvPos
                               if True
                                then return ()
                                else throwError (strMsg ("not match: pos: " ++ showPos pos69_71))
                               xx71_72 <- dvCharsM
                               pos73_73 <- gets dvPos
                               case xx71_72 of
                                   'u' -> return ()
                                   _ -> throwError (strMsg ("not match: pos: " ++ showPos pos73_73))
                               let 'u' = xx71_72
                               return ()
                               pos72_74 <- gets dvPos
                               if True
                                then return ()
                                else throwError (strMsg ("not match: pos: " ++ showPos pos72_74))
                               xx74_75 <- dvCharsM
                               pos76_76 <- gets dvPos
                               case xx74_75 of
                                   'l' -> return ()
                                   _ -> throwError (strMsg ("not match: pos: " ++ showPos pos76_76))
                               let 'l' = xx74_75
                               return ()
                               pos75_77 <- gets dvPos
                               if True
                                then return ()
                                else throwError (strMsg ("not match: pos: " ++ showPos pos75_77))
                               xx77_78 <- dvCharsM
                               pos79_79 <- gets dvPos
                               case xx77_78 of
                                   'e' -> return ()
                                   _ -> throwError (strMsg ("not match: pos: " ++ showPos pos79_79))
                               let 'e' = xx77_78
                               return ()
                               pos78_80 <- gets dvPos
                               if True
                                then return ()
                                else throwError (strMsg ("not match: pos: " ++ showPos pos78_80))
                               pos80_81 <- gets dvPos
                               s <- dv_moduleDecStrM
                               return ()
                               if True
                                then return ()
                                else throwError (strMsg ("not match: pos: " ++ showPos pos80_81))
                               pos81_82 <- gets dvPos
                               dv_whrM >> return ()
                               if True
                                then return ()
                                else throwError (strMsg ("not match: pos: " ++ showPos pos81_82))
                               return (id just s),
                            do return (id nothing)]
p_moduleDecStr = foldl1 mplus [do ddd82_83 <- get
                                  flipMaybe (do pos83_84 <- gets dvPos
                                                dv_whrM >> return ()
                                                if True
                                                 then return ()
                                                 else throwError (strMsg ("not match: pos: " ++ showPos pos83_84)))
                                  put ddd82_83
                                  xx84_85 <- dvCharsM
                                  let c = xx84_85
                                  pos85_86 <- gets dvPos
                                  if True
                                   then return ()
                                   else throwError (strMsg ("not match: pos: " ++ showPos pos85_86))
                                  pos86_87 <- gets dvPos
                                  s <- dv_moduleDecStrM
                                  return ()
                                  if True
                                   then return ()
                                   else throwError (strMsg ("not match: pos: " ++ showPos pos86_87))
                                  return (id cons c s),
                               do return (id emp)]
p_whr = foldl1 mplus [do xx87_88 <- dvCharsM
                         pos89_89 <- gets dvPos
                         case xx87_88 of
                             'w' -> return ()
                             _ -> throwError (strMsg ("not match: pos: " ++ showPos pos89_89))
                         let 'w' = xx87_88
                         return ()
                         pos88_90 <- gets dvPos
                         if True
                          then return ()
                          else throwError (strMsg ("not match: pos: " ++ showPos pos88_90))
                         xx90_91 <- dvCharsM
                         pos92_92 <- gets dvPos
                         case xx90_91 of
                             'h' -> return ()
                             _ -> throwError (strMsg ("not match: pos: " ++ showPos pos92_92))
                         let 'h' = xx90_91
                         return ()
                         pos91_93 <- gets dvPos
                         if True
                          then return ()
                          else throwError (strMsg ("not match: pos: " ++ showPos pos91_93))
                         xx93_94 <- dvCharsM
                         pos95_95 <- gets dvPos
                         case xx93_94 of
                             'e' -> return ()
                             _ -> throwError (strMsg ("not match: pos: " ++ showPos pos95_95))
                         let 'e' = xx93_94
                         return ()
                         pos94_96 <- gets dvPos
                         if True
                          then return ()
                          else throwError (strMsg ("not match: pos: " ++ showPos pos94_96))
                         xx96_97 <- dvCharsM
                         pos98_98 <- gets dvPos
                         case xx96_97 of
                             'r' -> return ()
                             _ -> throwError (strMsg ("not match: pos: " ++ showPos pos98_98))
                         let 'r' = xx96_97
                         return ()
                         pos97_99 <- gets dvPos
                         if True
                          then return ()
                          else throwError (strMsg ("not match: pos: " ++ showPos pos97_99))
                         xx99_100 <- dvCharsM
                         pos101_101 <- gets dvPos
                         case xx99_100 of
                             'e' -> return ()
                             _ -> throwError (strMsg ("not match: pos: " ++ showPos pos101_101))
                         let 'e' = xx99_100
                         return ()
                         pos100_102 <- gets dvPos
                         if True
                          then return ()
                          else throwError (strMsg ("not match: pos: " ++ showPos pos100_102))
                         return (id nil)]
p_preImpPap = foldl1 mplus [do ddd102_103 <- get
                               flipMaybe (do pos103_104 <- gets dvPos
                                             dv_importPapillonM >> return ()
                                             if True
                                              then return ()
                                              else throwError (strMsg ("not match: pos: " ++ showPos pos103_104)))
                               put ddd102_103
                               ddd104_105 <- get
                               flipMaybe (do pos105_106 <- gets dvPos
                                             dv_papM >> return ()
                                             if True
                                              then return ()
                                              else throwError (strMsg ("not match: pos: " ++ showPos pos105_106)))
                               put ddd104_105
                               xx106_107 <- dvCharsM
                               let c = xx106_107
                               pos107_108 <- gets dvPos
                               if True
                                then return ()
                                else throwError (strMsg ("not match: pos: " ++ showPos pos107_108))
                               pos108_109 <- gets dvPos
                               pip <- dv_preImpPapM
                               return ()
                               if True
                                then return ()
                                else throwError (strMsg ("not match: pos: " ++ showPos pos108_109))
                               return (id cons c pip),
                            do return (id emp)]
p_prePeg = foldl1 mplus [do ddd109_110 <- get
                            flipMaybe (do pos110_111 <- gets dvPos
                                          dv_papM >> return ()
                                          if True
                                           then return ()
                                           else throwError (strMsg ("not match: pos: " ++ showPos pos110_111)))
                            put ddd109_110
                            xx111_112 <- dvCharsM
                            let c = xx111_112
                            pos112_113 <- gets dvPos
                            if True
                             then return ()
                             else throwError (strMsg ("not match: pos: " ++ showPos pos112_113))
                            pos113_114 <- gets dvPos
                            pp <- dv_prePegM
                            return ()
                            if True
                             then return ()
                             else throwError (strMsg ("not match: pos: " ++ showPos pos113_114))
                            return (id cons c pp),
                         do return (id emp)]
p_afterPeg = foldl1 mplus [do xx114_115 <- dvCharsM
                              let c = xx114_115
                              pos115_116 <- gets dvPos
                              if True
                               then return ()
                               else throwError (strMsg ("not match: pos: " ++ showPos pos115_116))
                              pos116_117 <- gets dvPos
                              atp <- dv_afterPegM
                              return ()
                              if True
                               then return ()
                               else throwError (strMsg ("not match: pos: " ++ showPos pos116_117))
                              return (id cons c atp),
                           do return (id emp)]
p_importPapillon = foldl1 mplus [do pos117_118 <- gets dvPos
                                    xx118_119 <- dv_varTokenM
                                    pos119_120 <- gets dvPos
                                    case xx118_119 of
                                        "import" -> return ()
                                        _ -> throwError (strMsg ("not match: pos: " ++ showPos pos119_120))
                                    let "import" = xx118_119
                                    return ()
                                    if True
                                     then return ()
                                     else throwError (strMsg ("not match: pos: " ++ showPos pos117_118))
                                    pos120_121 <- gets dvPos
                                    xx121_122 <- dv_typTokenM
                                    pos122_123 <- gets dvPos
                                    case xx121_122 of
                                        "Text" -> return ()
                                        _ -> throwError (strMsg ("not match: pos: " ++ showPos pos122_123))
                                    let "Text" = xx121_122
                                    return ()
                                    if True
                                     then return ()
                                     else throwError (strMsg ("not match: pos: " ++ showPos pos120_121))
                                    xx123_124 <- dvCharsM
                                    pos125_125 <- gets dvPos
                                    case xx123_124 of
                                        '.' -> return ()
                                        _ -> throwError (strMsg ("not match: pos: " ++ showPos pos125_125))
                                    let '.' = xx123_124
                                    return ()
                                    pos124_126 <- gets dvPos
                                    if True
                                     then return ()
                                     else throwError (strMsg ("not match: pos: " ++ showPos pos124_126))
                                    pos126_127 <- gets dvPos
                                    dv_spacesM >> return ()
                                    if True
                                     then return ()
                                     else throwError (strMsg ("not match: pos: " ++ showPos pos126_127))
                                    pos127_128 <- gets dvPos
                                    xx128_129 <- dv_typTokenM
                                    pos129_130 <- gets dvPos
                                    case xx128_129 of
                                        "Papillon" -> return ()
                                        _ -> throwError (strMsg ("not match: pos: " ++ showPos pos129_130))
                                    let "Papillon" = xx128_129
                                    return ()
                                    if True
                                     then return ()
                                     else throwError (strMsg ("not match: pos: " ++ showPos pos127_128))
                                    return (id nil)]
p_varToken = foldl1 mplus [do pos130_131 <- gets dvPos
                              v <- dv_variableM
                              return ()
                              if True
                               then return ()
                               else throwError (strMsg ("not match: pos: " ++ showPos pos130_131))
                              pos131_132 <- gets dvPos
                              dv_spacesM >> return ()
                              if True
                               then return ()
                               else throwError (strMsg ("not match: pos: " ++ showPos pos131_132))
                              return (id v)]
p_typToken = foldl1 mplus [do pos132_133 <- gets dvPos
                              t <- dv_typM
                              return ()
                              if True
                               then return ()
                               else throwError (strMsg ("not match: pos: " ++ showPos pos132_133))
                              pos133_134 <- gets dvPos
                              dv_spacesM >> return ()
                              if True
                               then return ()
                               else throwError (strMsg ("not match: pos: " ++ showPos pos133_134))
                              return (id t)]
p_pap = foldl1 mplus [do xx134_135 <- dvCharsM
                         pos136_136 <- gets dvPos
                         case xx134_135 of
                             '\n' -> return ()
                             _ -> throwError (strMsg ("not match: pos: " ++ showPos pos136_136))
                         let '\n' = xx134_135
                         return ()
                         pos135_137 <- gets dvPos
                         if True
                          then return ()
                          else throwError (strMsg ("not match: pos: " ++ showPos pos135_137))
                         xx137_138 <- dvCharsM
                         pos139_139 <- gets dvPos
                         case xx137_138 of
                             '[' -> return ()
                             _ -> throwError (strMsg ("not match: pos: " ++ showPos pos139_139))
                         let '[' = xx137_138
                         return ()
                         pos138_140 <- gets dvPos
                         if True
                          then return ()
                          else throwError (strMsg ("not match: pos: " ++ showPos pos138_140))
                         xx140_141 <- dvCharsM
                         pos142_142 <- gets dvPos
                         case xx140_141 of
                             'p' -> return ()
                             _ -> throwError (strMsg ("not match: pos: " ++ showPos pos142_142))
                         let 'p' = xx140_141
                         return ()
                         pos141_143 <- gets dvPos
                         if True
                          then return ()
                          else throwError (strMsg ("not match: pos: " ++ showPos pos141_143))
                         xx143_144 <- dvCharsM
                         pos145_145 <- gets dvPos
                         case xx143_144 of
                             'a' -> return ()
                             _ -> throwError (strMsg ("not match: pos: " ++ showPos pos145_145))
                         let 'a' = xx143_144
                         return ()
                         pos144_146 <- gets dvPos
                         if True
                          then return ()
                          else throwError (strMsg ("not match: pos: " ++ showPos pos144_146))
                         xx146_147 <- dvCharsM
                         pos148_148 <- gets dvPos
                         case xx146_147 of
                             'p' -> return ()
                             _ -> throwError (strMsg ("not match: pos: " ++ showPos pos148_148))
                         let 'p' = xx146_147
                         return ()
                         pos147_149 <- gets dvPos
                         if True
                          then return ()
                          else throwError (strMsg ("not match: pos: " ++ showPos pos147_149))
                         xx149_150 <- dvCharsM
                         pos151_151 <- gets dvPos
                         case xx149_150 of
                             'i' -> return ()
                             _ -> throwError (strMsg ("not match: pos: " ++ showPos pos151_151))
                         let 'i' = xx149_150
                         return ()
                         pos150_152 <- gets dvPos
                         if True
                          then return ()
                          else throwError (strMsg ("not match: pos: " ++ showPos pos150_152))
                         xx152_153 <- dvCharsM
                         pos154_154 <- gets dvPos
                         case xx152_153 of
                             'l' -> return ()
                             _ -> throwError (strMsg ("not match: pos: " ++ showPos pos154_154))
                         let 'l' = xx152_153
                         return ()
                         pos153_155 <- gets dvPos
                         if True
                          then return ()
                          else throwError (strMsg ("not match: pos: " ++ showPos pos153_155))
                         xx155_156 <- dvCharsM
                         pos157_157 <- gets dvPos
                         case xx155_156 of
                             'l' -> return ()
                             _ -> throwError (strMsg ("not match: pos: " ++ showPos pos157_157))
                         let 'l' = xx155_156
                         return ()
                         pos156_158 <- gets dvPos
                         if True
                          then return ()
                          else throwError (strMsg ("not match: pos: " ++ showPos pos156_158))
                         xx158_159 <- dvCharsM
                         pos160_160 <- gets dvPos
                         case xx158_159 of
                             'o' -> return ()
                             _ -> throwError (strMsg ("not match: pos: " ++ showPos pos160_160))
                         let 'o' = xx158_159
                         return ()
                         pos159_161 <- gets dvPos
                         if True
                          then return ()
                          else throwError (strMsg ("not match: pos: " ++ showPos pos159_161))
                         xx161_162 <- dvCharsM
                         pos163_163 <- gets dvPos
                         case xx161_162 of
                             'n' -> return ()
                             _ -> throwError (strMsg ("not match: pos: " ++ showPos pos163_163))
                         let 'n' = xx161_162
                         return ()
                         pos162_164 <- gets dvPos
                         if True
                          then return ()
                          else throwError (strMsg ("not match: pos: " ++ showPos pos162_164))
                         xx164_165 <- dvCharsM
                         pos166_166 <- gets dvPos
                         case xx164_165 of
                             '|' -> return ()
                             _ -> throwError (strMsg ("not match: pos: " ++ showPos pos166_166))
                         let '|' = xx164_165
                         return ()
                         pos165_167 <- gets dvPos
                         if True
                          then return ()
                          else throwError (strMsg ("not match: pos: " ++ showPos pos165_167))
                         xx167_168 <- dvCharsM
                         pos169_169 <- gets dvPos
                         case xx167_168 of
                             '\n' -> return ()
                             _ -> throwError (strMsg ("not match: pos: " ++ showPos pos169_169))
                         let '\n' = xx167_168
                         return ()
                         pos168_170 <- gets dvPos
                         if True
                          then return ()
                          else throwError (strMsg ("not match: pos: " ++ showPos pos168_170))
                         return (id nil)]
p_peg = foldl1 mplus [do pos170_171 <- gets dvPos
                         dv_spacesM >> return ()
                         if True
                          then return ()
                          else throwError (strMsg ("not match: pos: " ++ showPos pos170_171))
                         pos171_172 <- gets dvPos
                         s <- dv_sourceTypeM
                         return ()
                         if True
                          then return ()
                          else throwError (strMsg ("not match: pos: " ++ showPos pos171_172))
                         pos172_173 <- gets dvPos
                         p <- dv_peg_M
                         return ()
                         if True
                          then return ()
                          else throwError (strMsg ("not match: pos: " ++ showPos pos172_173))
                         return (id mkTTPeg s p),
                      do pos173_174 <- gets dvPos
                         p <- dv_peg_M
                         return ()
                         if True
                          then return ()
                          else throwError (strMsg ("not match: pos: " ++ showPos pos173_174))
                         return (id mkTTPeg tString p)]
p_sourceType = foldl1 mplus [do pos174_175 <- gets dvPos
                                xx175_176 <- dv_varTokenM
                                pos176_177 <- gets dvPos
                                case xx175_176 of
                                    "source" -> return ()
                                    _ -> throwError (strMsg ("not match: pos: " ++ showPos pos176_177))
                                let "source" = xx175_176
                                return ()
                                if True
                                 then return ()
                                 else throwError (strMsg ("not match: pos: " ++ showPos pos174_175))
                                xx177_178 <- dvCharsM
                                pos179_179 <- gets dvPos
                                case xx177_178 of
                                    ':' -> return ()
                                    _ -> throwError (strMsg ("not match: pos: " ++ showPos pos179_179))
                                let ':' = xx177_178
                                return ()
                                pos178_180 <- gets dvPos
                                if True
                                 then return ()
                                 else throwError (strMsg ("not match: pos: " ++ showPos pos178_180))
                                pos180_181 <- gets dvPos
                                dv_spacesM >> return ()
                                if True
                                 then return ()
                                 else throwError (strMsg ("not match: pos: " ++ showPos pos180_181))
                                pos181_182 <- gets dvPos
                                v <- dv_typTokenM
                                return ()
                                if True
                                 then return ()
                                 else throwError (strMsg ("not match: pos: " ++ showPos pos181_182))
                                return (id v)]
p_peg_ = foldl1 mplus [do pos182_183 <- gets dvPos
                          dv_spacesM >> return ()
                          if True
                           then return ()
                           else throwError (strMsg ("not match: pos: " ++ showPos pos182_183))
                          pos183_184 <- gets dvPos
                          d <- dv_definitionM
                          return ()
                          if True
                           then return ()
                           else throwError (strMsg ("not match: pos: " ++ showPos pos183_184))
                          pos184_185 <- gets dvPos
                          p <- dv_peg_M
                          return ()
                          if True
                           then return ()
                           else throwError (strMsg ("not match: pos: " ++ showPos pos184_185))
                          return (id cons d p),
                       do return (id emp)]
p_definition = foldl1 mplus [do pos185_186 <- gets dvPos
                                v <- dv_variableM
                                return ()
                                if True
                                 then return ()
                                 else throwError (strMsg ("not match: pos: " ++ showPos pos185_186))
                                pos186_187 <- gets dvPos
                                dv_spacesM >> return ()
                                if True
                                 then return ()
                                 else throwError (strMsg ("not match: pos: " ++ showPos pos186_187))
                                xx187_188 <- dvCharsM
                                pos189_189 <- gets dvPos
                                case xx187_188 of
                                    ':' -> return ()
                                    _ -> throwError (strMsg ("not match: pos: " ++ showPos pos189_189))
                                let ':' = xx187_188
                                return ()
                                pos188_190 <- gets dvPos
                                if True
                                 then return ()
                                 else throwError (strMsg ("not match: pos: " ++ showPos pos188_190))
                                xx190_191 <- dvCharsM
                                pos192_192 <- gets dvPos
                                case xx190_191 of
                                    ':' -> return ()
                                    _ -> throwError (strMsg ("not match: pos: " ++ showPos pos192_192))
                                let ':' = xx190_191
                                return ()
                                pos191_193 <- gets dvPos
                                if True
                                 then return ()
                                 else throwError (strMsg ("not match: pos: " ++ showPos pos191_193))
                                pos193_194 <- gets dvPos
                                dv_spacesM >> return ()
                                if True
                                 then return ()
                                 else throwError (strMsg ("not match: pos: " ++ showPos pos193_194))
                                pos194_195 <- gets dvPos
                                t <- dv_typM
                                return ()
                                if True
                                 then return ()
                                 else throwError (strMsg ("not match: pos: " ++ showPos pos194_195))
                                pos195_196 <- gets dvPos
                                dv_spacesM >> return ()
                                if True
                                 then return ()
                                 else throwError (strMsg ("not match: pos: " ++ showPos pos195_196))
                                xx196_197 <- dvCharsM
                                pos198_198 <- gets dvPos
                                case xx196_197 of
                                    '=' -> return ()
                                    _ -> throwError (strMsg ("not match: pos: " ++ showPos pos198_198))
                                let '=' = xx196_197
                                return ()
                                pos197_199 <- gets dvPos
                                if True
                                 then return ()
                                 else throwError (strMsg ("not match: pos: " ++ showPos pos197_199))
                                pos199_200 <- gets dvPos
                                dv_spacesM >> return ()
                                if True
                                 then return ()
                                 else throwError (strMsg ("not match: pos: " ++ showPos pos199_200))
                                pos200_201 <- gets dvPos
                                sel <- dv_selectionM
                                return ()
                                if True
                                 then return ()
                                 else throwError (strMsg ("not match: pos: " ++ showPos pos200_201))
                                pos201_202 <- gets dvPos
                                dv_spacesM >> return ()
                                if True
                                 then return ()
                                 else throwError (strMsg ("not match: pos: " ++ showPos pos201_202))
                                xx202_203 <- dvCharsM
                                pos204_204 <- gets dvPos
                                case xx202_203 of
                                    ';' -> return ()
                                    _ -> throwError (strMsg ("not match: pos: " ++ showPos pos204_204))
                                let ';' = xx202_203
                                return ()
                                pos203_205 <- gets dvPos
                                if True
                                 then return ()
                                 else throwError (strMsg ("not match: pos: " ++ showPos pos203_205))
                                return (id mkDef v t sel)]
p_selection = foldl1 mplus [do pos205_206 <- gets dvPos
                               ex <- dv_expressionHsM
                               return ()
                               if True
                                then return ()
                                else throwError (strMsg ("not match: pos: " ++ showPos pos205_206))
                               pos206_207 <- gets dvPos
                               dv_spacesM >> return ()
                               if True
                                then return ()
                                else throwError (strMsg ("not match: pos: " ++ showPos pos206_207))
                               xx207_208 <- dvCharsM
                               pos209_209 <- gets dvPos
                               case xx207_208 of
                                   '/' -> return ()
                                   _ -> throwError (strMsg ("not match: pos: " ++ showPos pos209_209))
                               let '/' = xx207_208
                               return ()
                               pos208_210 <- gets dvPos
                               if True
                                then return ()
                                else throwError (strMsg ("not match: pos: " ++ showPos pos208_210))
                               pos210_211 <- gets dvPos
                               dv_spacesM >> return ()
                               if True
                                then return ()
                                else throwError (strMsg ("not match: pos: " ++ showPos pos210_211))
                               pos211_212 <- gets dvPos
                               sel <- dv_selectionM
                               return ()
                               if True
                                then return ()
                                else throwError (strMsg ("not match: pos: " ++ showPos pos211_212))
                               return (id cons ex sel),
                            do pos212_213 <- gets dvPos
                               ex <- dv_expressionHsM
                               return ()
                               if True
                                then return ()
                                else throwError (strMsg ("not match: pos: " ++ showPos pos212_213))
                               return (id cons ex emp)]
p_expressionHs = foldl1 mplus [do pos213_214 <- gets dvPos
                                  e <- dv_expressionM
                                  return ()
                                  if True
                                   then return ()
                                   else throwError (strMsg ("not match: pos: " ++ showPos pos213_214))
                                  pos214_215 <- gets dvPos
                                  dv_spacesM >> return ()
                                  if True
                                   then return ()
                                   else throwError (strMsg ("not match: pos: " ++ showPos pos214_215))
                                  xx215_216 <- dvCharsM
                                  pos217_217 <- gets dvPos
                                  case xx215_216 of
                                      '{' -> return ()
                                      _ -> throwError (strMsg ("not match: pos: " ++ showPos pos217_217))
                                  let '{' = xx215_216
                                  return ()
                                  pos216_218 <- gets dvPos
                                  if True
                                   then return ()
                                   else throwError (strMsg ("not match: pos: " ++ showPos pos216_218))
                                  pos218_219 <- gets dvPos
                                  dv_spacesM >> return ()
                                  if True
                                   then return ()
                                   else throwError (strMsg ("not match: pos: " ++ showPos pos218_219))
                                  pos219_220 <- gets dvPos
                                  h <- dv_hsExpM
                                  return ()
                                  if True
                                   then return ()
                                   else throwError (strMsg ("not match: pos: " ++ showPos pos219_220))
                                  pos220_221 <- gets dvPos
                                  dv_spacesM >> return ()
                                  if True
                                   then return ()
                                   else throwError (strMsg ("not match: pos: " ++ showPos pos220_221))
                                  xx221_222 <- dvCharsM
                                  pos223_223 <- gets dvPos
                                  case xx221_222 of
                                      '}' -> return ()
                                      _ -> throwError (strMsg ("not match: pos: " ++ showPos pos223_223))
                                  let '}' = xx221_222
                                  return ()
                                  pos222_224 <- gets dvPos
                                  if True
                                   then return ()
                                   else throwError (strMsg ("not match: pos: " ++ showPos pos222_224))
                                  return (id mkExpressionHs e h)]
p_expression = foldl1 mplus [do pos224_225 <- gets dvPos
                                l <- dv_nameLeaf_M
                                return ()
                                if True
                                 then return ()
                                 else throwError (strMsg ("not match: pos: " ++ showPos pos224_225))
                                pos225_226 <- gets dvPos
                                dv_spacesM >> return ()
                                if True
                                 then return ()
                                 else throwError (strMsg ("not match: pos: " ++ showPos pos225_226))
                                pos226_227 <- gets dvPos
                                e <- dv_expressionM
                                return ()
                                if True
                                 then return ()
                                 else throwError (strMsg ("not match: pos: " ++ showPos pos226_227))
                                return (id cons l e),
                             do return (id emp)]
p_nameLeaf_ = foldl1 mplus [do xx227_228 <- dvCharsM
                               pos229_229 <- gets dvPos
                               case xx227_228 of
                                   '!' -> return ()
                                   _ -> throwError (strMsg ("not match: pos: " ++ showPos pos229_229))
                               let '!' = xx227_228
                               return ()
                               pos228_230 <- gets dvPos
                               if True
                                then return ()
                                else throwError (strMsg ("not match: pos: " ++ showPos pos228_230))
                               pos230_231 <- gets dvPos
                               nl <- dv_nameLeafM
                               return ()
                               if True
                                then return ()
                                else throwError (strMsg ("not match: pos: " ++ showPos pos230_231))
                               return (id notAfter nl),
                            do pos231_232 <- gets dvPos
                               nl <- dv_nameLeafM
                               return ()
                               if True
                                then return ()
                                else throwError (strMsg ("not match: pos: " ++ showPos pos231_232))
                               return (id here nl)]
p_nameLeaf = foldl1 mplus [do pos232_233 <- gets dvPos
                              n <- dv_pat1M
                              return ()
                              if True
                               then return ()
                               else throwError (strMsg ("not match: pos: " ++ showPos pos232_233))
                              xx233_234 <- dvCharsM
                              pos235_235 <- gets dvPos
                              case xx233_234 of
                                  ':' -> return ()
                                  _ -> throwError (strMsg ("not match: pos: " ++ showPos pos235_235))
                              let ':' = xx233_234
                              return ()
                              pos234_236 <- gets dvPos
                              if True
                               then return ()
                               else throwError (strMsg ("not match: pos: " ++ showPos pos234_236))
                              xx236_237 <- dvCharsM
                              let o = xx236_237
                              pos237_238 <- gets dvPos
                              if id isOpen o
                               then return ()
                               else throwError (strMsg ("not match: pos: " ++ showPos pos237_238))
                              pos238_239 <- gets dvPos
                              ex <- dv_selectionM
                              return ()
                              if True
                               then return ()
                               else throwError (strMsg ("not match: pos: " ++ showPos pos238_239))
                              xx239_240 <- dvCharsM
                              let c = xx239_240
                              pos240_241 <- gets dvPos
                              if id isClose c
                               then return ()
                               else throwError (strMsg ("not match: pos: " ++ showPos pos240_241))
                              xx241_242 <- dvCharsM
                              let k = xx241_242
                              pos242_243 <- gets dvPos
                              if id isKome k
                               then return ()
                               else throwError (strMsg ("not match: pos: " ++ showPos pos242_243))
                              pos243_244 <- gets dvPos
                              dv_spacesM >> return ()
                              if True
                               then return ()
                               else throwError (strMsg ("not match: pos: " ++ showPos pos243_244))
                              return (id mkNameLeafList n ex),
                           do xx244_245 <- dvCharsM
                              let o = xx244_245
                              pos245_246 <- gets dvPos
                              if id isOpen o
                               then return ()
                               else throwError (strMsg ("not match: pos: " ++ showPos pos245_246))
                              pos246_247 <- gets dvPos
                              nl <- dv_nameLeafM
                              return ()
                              if True
                               then return ()
                               else throwError (strMsg ("not match: pos: " ++ showPos pos246_247))
                              xx247_248 <- dvCharsM
                              let c = xx247_248
                              pos248_249 <- gets dvPos
                              if id isClose c
                               then return ()
                               else throwError (strMsg ("not match: pos: " ++ showPos pos248_249))
                              return (id nl),
                           do pos249_250 <- gets dvPos
                              n <- dv_pat1M
                              return ()
                              if True
                               then return ()
                               else throwError (strMsg ("not match: pos: " ++ showPos pos249_250))
                              xx250_251 <- dvCharsM
                              pos252_252 <- gets dvPos
                              case xx250_251 of
                                  ':' -> return ()
                                  _ -> throwError (strMsg ("not match: pos: " ++ showPos pos252_252))
                              let ':' = xx250_251
                              return ()
                              pos251_253 <- gets dvPos
                              if True
                               then return ()
                               else throwError (strMsg ("not match: pos: " ++ showPos pos251_253))
                              pos253_254 <- gets dvPos
                              l <- dv_leafM
                              return ()
                              if True
                               then return ()
                               else throwError (strMsg ("not match: pos: " ++ showPos pos253_254))
                              return (id mkNameLeaf n l),
                           do pos254_255 <- gets dvPos
                              n <- dv_pat1M
                              return ()
                              if True
                               then return ()
                               else throwError (strMsg ("not match: pos: " ++ showPos pos254_255))
                              return (id mkNameLeaf n ctLeaf)]
p_pat = foldl1 mplus [do pos255_256 <- gets dvPos
                         t <- dv_typM
                         return ()
                         if True
                          then return ()
                          else throwError (strMsg ("not match: pos: " ++ showPos pos255_256))
                         pos256_257 <- gets dvPos
                         dv_spacesM >> return ()
                         if True
                          then return ()
                          else throwError (strMsg ("not match: pos: " ++ showPos pos256_257))
                         pos257_258 <- gets dvPos
                         ps <- dv_patsM
                         return ()
                         if True
                          then return ()
                          else throwError (strMsg ("not match: pos: " ++ showPos pos257_258))
                         return (id conToPatQ t ps),
                      do pos258_259 <- gets dvPos
                         p <- dv_pat1M
                         return ()
                         if True
                          then return ()
                          else throwError (strMsg ("not match: pos: " ++ showPos pos258_259))
                         return (id p)]
p_pat1 = foldl1 mplus [do pos259_260 <- gets dvPos
                          xx260_261 <- dv_variableM
                          pos261_262 <- gets dvPos
                          case xx260_261 of
                              "_" -> return ()
                              _ -> throwError (strMsg ("not match: pos: " ++ showPos pos261_262))
                          let "_" = xx260_261
                          return ()
                          if True
                           then return ()
                           else throwError (strMsg ("not match: pos: " ++ showPos pos259_260))
                          return (id wildP),
                       do pos262_263 <- gets dvPos
                          n <- dv_variableM
                          return ()
                          if True
                           then return ()
                           else throwError (strMsg ("not match: pos: " ++ showPos pos262_263))
                          return (id strToPatQ n),
                       do xx263_264 <- dvCharsM
                          pos265_265 <- gets dvPos
                          case xx263_264 of
                              '\'' -> return ()
                              _ -> throwError (strMsg ("not match: pos: " ++ showPos pos265_265))
                          let '\'' = xx263_264
                          return ()
                          pos264_266 <- gets dvPos
                          if True
                           then return ()
                           else throwError (strMsg ("not match: pos: " ++ showPos pos264_266))
                          pos266_267 <- gets dvPos
                          c <- dv_charLitM
                          return ()
                          if True
                           then return ()
                           else throwError (strMsg ("not match: pos: " ++ showPos pos266_267))
                          xx267_268 <- dvCharsM
                          pos269_269 <- gets dvPos
                          case xx267_268 of
                              '\'' -> return ()
                              _ -> throwError (strMsg ("not match: pos: " ++ showPos pos269_269))
                          let '\'' = xx267_268
                          return ()
                          pos268_270 <- gets dvPos
                          if True
                           then return ()
                           else throwError (strMsg ("not match: pos: " ++ showPos pos268_270))
                          return (id charP c),
                       do xx270_271 <- dvCharsM
                          pos272_272 <- gets dvPos
                          case xx270_271 of
                              '"' -> return ()
                              _ -> throwError (strMsg ("not match: pos: " ++ showPos pos272_272))
                          let '"' = xx270_271
                          return ()
                          pos271_273 <- gets dvPos
                          if True
                           then return ()
                           else throwError (strMsg ("not match: pos: " ++ showPos pos271_273))
                          pos273_274 <- gets dvPos
                          s <- dv_stringLitM
                          return ()
                          if True
                           then return ()
                           else throwError (strMsg ("not match: pos: " ++ showPos pos273_274))
                          xx274_275 <- dvCharsM
                          pos276_276 <- gets dvPos
                          case xx274_275 of
                              '"' -> return ()
                              _ -> throwError (strMsg ("not match: pos: " ++ showPos pos276_276))
                          let '"' = xx274_275
                          return ()
                          pos275_277 <- gets dvPos
                          if True
                           then return ()
                           else throwError (strMsg ("not match: pos: " ++ showPos pos275_277))
                          return (id stringP s),
                       do pos277_278 <- gets dvPos
                          t <- dv_typM
                          return ()
                          if True
                           then return ()
                           else throwError (strMsg ("not match: pos: " ++ showPos pos277_278))
                          return (id conToPatQ t emp),
                       do xx278_279 <- dvCharsM
                          pos280_280 <- gets dvPos
                          case xx278_279 of
                              '(' -> return ()
                              _ -> throwError (strMsg ("not match: pos: " ++ showPos pos280_280))
                          let '(' = xx278_279
                          return ()
                          pos279_281 <- gets dvPos
                          if True
                           then return ()
                           else throwError (strMsg ("not match: pos: " ++ showPos pos279_281))
                          pos281_282 <- gets dvPos
                          p <- dv_patM
                          return ()
                          if True
                           then return ()
                           else throwError (strMsg ("not match: pos: " ++ showPos pos281_282))
                          xx282_283 <- dvCharsM
                          pos284_284 <- gets dvPos
                          case xx282_283 of
                              ')' -> return ()
                              _ -> throwError (strMsg ("not match: pos: " ++ showPos pos284_284))
                          let ')' = xx282_283
                          return ()
                          pos283_285 <- gets dvPos
                          if True
                           then return ()
                           else throwError (strMsg ("not match: pos: " ++ showPos pos283_285))
                          return (id p)]
p_charLit = foldl1 mplus [do xx285_286 <- dvCharsM
                             let c = xx285_286
                             pos286_287 <- gets dvPos
                             if id isAlphaNumOt c
                              then return ()
                              else throwError (strMsg ("not match: pos: " ++ showPos pos286_287))
                             return (id c),
                          do xx287_288 <- dvCharsM
                             pos289_289 <- gets dvPos
                             case xx287_288 of
                                 '\\' -> return ()
                                 _ -> throwError (strMsg ("not match: pos: " ++ showPos pos289_289))
                             let '\\' = xx287_288
                             return ()
                             pos288_290 <- gets dvPos
                             if True
                              then return ()
                              else throwError (strMsg ("not match: pos: " ++ showPos pos288_290))
                             xx290_291 <- dvCharsM
                             let c = xx290_291
                             pos291_292 <- gets dvPos
                             if id elemNTs c
                              then return ()
                              else throwError (strMsg ("not match: pos: " ++ showPos pos291_292))
                             return (id getNTs c)]
p_stringLit = foldl1 mplus [do ddd292_293 <- get
                               flipMaybe (do pos293_294 <- gets dvPos
                                             dv_dqM >> return ()
                                             if True
                                              then return ()
                                              else throwError (strMsg ("not match: pos: " ++ showPos pos293_294)))
                               put ddd292_293
                               xx294_295 <- dvCharsM
                               let c = xx294_295
                               pos295_296 <- gets dvPos
                               if True
                                then return ()
                                else throwError (strMsg ("not match: pos: " ++ showPos pos295_296))
                               pos296_297 <- gets dvPos
                               s <- dv_stringLitM
                               return ()
                               if True
                                then return ()
                                else throwError (strMsg ("not match: pos: " ++ showPos pos296_297))
                               return (id cons c s),
                            do return (id emp)]
p_dq = foldl1 mplus [do xx297_298 <- dvCharsM
                        pos299_299 <- gets dvPos
                        case xx297_298 of
                            '"' -> return ()
                            _ -> throwError (strMsg ("not match: pos: " ++ showPos pos299_299))
                        let '"' = xx297_298
                        return ()
                        pos298_300 <- gets dvPos
                        if True
                         then return ()
                         else throwError (strMsg ("not match: pos: " ++ showPos pos298_300))
                        return (id nil)]
p_pats = foldl1 mplus [do pos300_301 <- gets dvPos
                          p <- dv_patM
                          return ()
                          if True
                           then return ()
                           else throwError (strMsg ("not match: pos: " ++ showPos pos300_301))
                          pos301_302 <- gets dvPos
                          ps <- dv_patsM
                          return ()
                          if True
                           then return ()
                           else throwError (strMsg ("not match: pos: " ++ showPos pos301_302))
                          return (id cons p ps),
                       do return (id emp)]
p_leaf = foldl1 mplus [do pos302_303 <- gets dvPos
                          t <- dv_testM
                          return ()
                          if True
                           then return ()
                           else throwError (strMsg ("not match: pos: " ++ showPos pos302_303))
                          return (id boolLeaf t),
                       do pos303_304 <- gets dvPos
                          v <- dv_variableM
                          return ()
                          if True
                           then return ()
                           else throwError (strMsg ("not match: pos: " ++ showPos pos303_304))
                          pos304_305 <- gets dvPos
                          t <- dv_testM
                          return ()
                          if True
                           then return ()
                           else throwError (strMsg ("not match: pos: " ++ showPos pos304_305))
                          return (id ruleLeaf v t),
                       do pos305_306 <- gets dvPos
                          v <- dv_variableM
                          return ()
                          if True
                           then return ()
                           else throwError (strMsg ("not match: pos: " ++ showPos pos305_306))
                          return (id ruleLeaf v true)]
p_test = foldl1 mplus [do xx306_307 <- dvCharsM
                          pos308_308 <- gets dvPos
                          case xx306_307 of
                              '[' -> return ()
                              _ -> throwError (strMsg ("not match: pos: " ++ showPos pos308_308))
                          let '[' = xx306_307
                          return ()
                          pos307_309 <- gets dvPos
                          if True
                           then return ()
                           else throwError (strMsg ("not match: pos: " ++ showPos pos307_309))
                          pos309_310 <- gets dvPos
                          h <- dv_hsExpM
                          return ()
                          if True
                           then return ()
                           else throwError (strMsg ("not match: pos: " ++ showPos pos309_310))
                          xx310_311 <- dvCharsM
                          pos312_312 <- gets dvPos
                          case xx310_311 of
                              ']' -> return ()
                              _ -> throwError (strMsg ("not match: pos: " ++ showPos pos312_312))
                          let ']' = xx310_311
                          return ()
                          pos311_313 <- gets dvPos
                          if True
                           then return ()
                           else throwError (strMsg ("not match: pos: " ++ showPos pos311_313))
                          return (id getEx h)]
p_hsExp = foldl1 mplus [do pos313_314 <- gets dvPos
                           v <- dv_variableM
                           return ()
                           if True
                            then return ()
                            else throwError (strMsg ("not match: pos: " ++ showPos pos313_314))
                           pos314_315 <- gets dvPos
                           dv_spacesM >> return ()
                           if True
                            then return ()
                            else throwError (strMsg ("not match: pos: " ++ showPos pos314_315))
                           pos315_316 <- gets dvPos
                           h <- dv_hsExpM
                           return ()
                           if True
                            then return ()
                            else throwError (strMsg ("not match: pos: " ++ showPos pos315_316))
                           return (id apply v h),
                        do pos316_317 <- gets dvPos
                           v <- dv_variableM
                           return ()
                           if True
                            then return ()
                            else throwError (strMsg ("not match: pos: " ++ showPos pos316_317))
                           return (id toExp v)]
p_typ = foldl1 mplus [do pos317_318 <- gets dvPos
                         u <- dv_upperM
                         return ()
                         if True
                          then return ()
                          else throwError (strMsg ("not match: pos: " ++ showPos pos317_318))
                         pos318_319 <- gets dvPos
                         t <- dv_tvtailM
                         return ()
                         if True
                          then return ()
                          else throwError (strMsg ("not match: pos: " ++ showPos pos318_319))
                         return (id cons u t)]
p_variable = foldl1 mplus [do pos319_320 <- gets dvPos
                              l <- dv_lowerM
                              return ()
                              if True
                               then return ()
                               else throwError (strMsg ("not match: pos: " ++ showPos pos319_320))
                              pos320_321 <- gets dvPos
                              t <- dv_tvtailM
                              return ()
                              if True
                               then return ()
                               else throwError (strMsg ("not match: pos: " ++ showPos pos320_321))
                              return (id cons l t)]
p_tvtail = foldl1 mplus [do pos321_322 <- gets dvPos
                            a <- dv_alphaM
                            return ()
                            if True
                             then return ()
                             else throwError (strMsg ("not match: pos: " ++ showPos pos321_322))
                            pos322_323 <- gets dvPos
                            t <- dv_tvtailM
                            return ()
                            if True
                             then return ()
                             else throwError (strMsg ("not match: pos: " ++ showPos pos322_323))
                            return (id cons a t),
                         do return (id emp)]
p_alpha = foldl1 mplus [do pos323_324 <- gets dvPos
                           u <- dv_upperM
                           return ()
                           if True
                            then return ()
                            else throwError (strMsg ("not match: pos: " ++ showPos pos323_324))
                           return (id u),
                        do pos324_325 <- gets dvPos
                           l <- dv_lowerM
                           return ()
                           if True
                            then return ()
                            else throwError (strMsg ("not match: pos: " ++ showPos pos324_325))
                           return (id l),
                        do pos325_326 <- gets dvPos
                           d <- dv_digitM
                           return ()
                           if True
                            then return ()
                            else throwError (strMsg ("not match: pos: " ++ showPos pos325_326))
                           return (id d)]
p_upper = foldl1 mplus [do xx326_327 <- dvCharsM
                           let u = xx326_327
                           pos327_328 <- gets dvPos
                           if id isUpper u
                            then return ()
                            else throwError (strMsg ("not match: pos: " ++ showPos pos327_328))
                           return (id u)]
p_lower = foldl1 mplus [do xx328_329 <- dvCharsM
                           let l = xx328_329
                           pos329_330 <- gets dvPos
                           if id isLowerU l
                            then return ()
                            else throwError (strMsg ("not match: pos: " ++ showPos pos329_330))
                           return (id l)]
p_digit = foldl1 mplus [do xx330_331 <- dvCharsM
                           let d = xx330_331
                           pos331_332 <- gets dvPos
                           if id isDigit d
                            then return ()
                            else throwError (strMsg ("not match: pos: " ++ showPos pos331_332))
                           return (id d)]
p_spaces = foldl1 mplus [do pos332_333 <- gets dvPos
                            dv_spaceM >> return ()
                            if True
                             then return ()
                             else throwError (strMsg ("not match: pos: " ++ showPos pos332_333))
                            pos333_334 <- gets dvPos
                            dv_spacesM >> return ()
                            if True
                             then return ()
                             else throwError (strMsg ("not match: pos: " ++ showPos pos333_334))
                            return (id nil),
                         do return (id nil)]
p_space = foldl1 mplus [do xx334_335 <- dvCharsM
                           let s = xx334_335
                           pos335_336 <- gets dvPos
                           if id isSpace s
                            then return ()
                            else throwError (strMsg ("not match: pos: " ++ showPos pos335_336))
                           return (id nil),
                        do xx336_337 <- dvCharsM
                           pos338_338 <- gets dvPos
                           case xx336_337 of
                               '-' -> return ()
                               _ -> throwError (strMsg ("not match: pos: " ++ showPos pos338_338))
                           let '-' = xx336_337
                           return ()
                           pos337_339 <- gets dvPos
                           if True
                            then return ()
                            else throwError (strMsg ("not match: pos: " ++ showPos pos337_339))
                           xx339_340 <- dvCharsM
                           pos341_341 <- gets dvPos
                           case xx339_340 of
                               '-' -> return ()
                               _ -> throwError (strMsg ("not match: pos: " ++ showPos pos341_341))
                           let '-' = xx339_340
                           return ()
                           pos340_342 <- gets dvPos
                           if True
                            then return ()
                            else throwError (strMsg ("not match: pos: " ++ showPos pos340_342))
                           pos342_343 <- gets dvPos
                           dv_notNLStringM >> return ()
                           if True
                            then return ()
                            else throwError (strMsg ("not match: pos: " ++ showPos pos342_343))
                           pos343_344 <- gets dvPos
                           dv_nlM >> return ()
                           if True
                            then return ()
                            else throwError (strMsg ("not match: pos: " ++ showPos pos343_344))
                           return (id nil),
                        do pos344_345 <- gets dvPos
                           dv_commentM >> return ()
                           if True
                            then return ()
                            else throwError (strMsg ("not match: pos: " ++ showPos pos344_345))
                           return (id nil)]
p_notNLString = foldl1 mplus [do ddd345_346 <- get
                                 flipMaybe (do pos346_347 <- gets dvPos
                                               dv_nlM >> return ()
                                               if True
                                                then return ()
                                                else throwError (strMsg ("not match: pos: " ++ showPos pos346_347)))
                                 put ddd345_346
                                 xx347_348 <- dvCharsM
                                 let c = xx347_348
                                 pos348_349 <- gets dvPos
                                 if True
                                  then return ()
                                  else throwError (strMsg ("not match: pos: " ++ showPos pos348_349))
                                 pos349_350 <- gets dvPos
                                 s <- dv_notNLStringM
                                 return ()
                                 if True
                                  then return ()
                                  else throwError (strMsg ("not match: pos: " ++ showPos pos349_350))
                                 return (id cons c s),
                              do return (id emp)]
p_nl = foldl1 mplus [do xx350_351 <- dvCharsM
                        pos352_352 <- gets dvPos
                        case xx350_351 of
                            '\n' -> return ()
                            _ -> throwError (strMsg ("not match: pos: " ++ showPos pos352_352))
                        let '\n' = xx350_351
                        return ()
                        pos351_353 <- gets dvPos
                        if True
                         then return ()
                         else throwError (strMsg ("not match: pos: " ++ showPos pos351_353))
                        return (id nil)]
p_comment = foldl1 mplus [do xx353_354 <- dvCharsM
                             pos355_355 <- gets dvPos
                             case xx353_354 of
                                 '{' -> return ()
                                 _ -> throwError (strMsg ("not match: pos: " ++ showPos pos355_355))
                             let '{' = xx353_354
                             return ()
                             pos354_356 <- gets dvPos
                             if True
                              then return ()
                              else throwError (strMsg ("not match: pos: " ++ showPos pos354_356))
                             xx356_357 <- dvCharsM
                             pos358_358 <- gets dvPos
                             case xx356_357 of
                                 '-' -> return ()
                                 _ -> throwError (strMsg ("not match: pos: " ++ showPos pos358_358))
                             let '-' = xx356_357
                             return ()
                             pos357_359 <- gets dvPos
                             if True
                              then return ()
                              else throwError (strMsg ("not match: pos: " ++ showPos pos357_359))
                             ddd359_360 <- get
                             flipMaybe (do xx360_361 <- dvCharsM
                                           pos362_362 <- gets dvPos
                                           case xx360_361 of
                                               '#' -> return ()
                                               _ -> throwError (strMsg ("not match: pos: " ++ showPos pos362_362))
                                           let '#' = xx360_361
                                           return ()
                                           pos361_363 <- gets dvPos
                                           if True
                                            then return ()
                                            else throwError (strMsg ("not match: pos: " ++ showPos pos361_363)))
                             put ddd359_360
                             pos363_364 <- gets dvPos
                             dv_commentsM >> return ()
                             if True
                              then return ()
                              else throwError (strMsg ("not match: pos: " ++ showPos pos363_364))
                             pos364_365 <- gets dvPos
                             dv_comEndM >> return ()
                             if True
                              then return ()
                              else throwError (strMsg ("not match: pos: " ++ showPos pos364_365))
                             return (id nil)]
p_comments = foldl1 mplus [do pos365_366 <- gets dvPos
                              dv_notComStrM >> return ()
                              if True
                               then return ()
                               else throwError (strMsg ("not match: pos: " ++ showPos pos365_366))
                              pos366_367 <- gets dvPos
                              dv_commentM >> return ()
                              if True
                               then return ()
                               else throwError (strMsg ("not match: pos: " ++ showPos pos366_367))
                              pos367_368 <- gets dvPos
                              dv_commentsM >> return ()
                              if True
                               then return ()
                               else throwError (strMsg ("not match: pos: " ++ showPos pos367_368))
                              return (id nil),
                           do pos368_369 <- gets dvPos
                              dv_notComStrM >> return ()
                              if True
                               then return ()
                               else throwError (strMsg ("not match: pos: " ++ showPos pos368_369))
                              return (id nil)]
p_notComStr = foldl1 mplus [do ddd369_370 <- get
                               flipMaybe (do pos370_371 <- gets dvPos
                                             dv_commentM >> return ()
                                             if True
                                              then return ()
                                              else throwError (strMsg ("not match: pos: " ++ showPos pos370_371)))
                               put ddd369_370
                               ddd371_372 <- get
                               flipMaybe (do pos372_373 <- gets dvPos
                                             dv_comEndM >> return ()
                                             if True
                                              then return ()
                                              else throwError (strMsg ("not match: pos: " ++ showPos pos372_373)))
                               put ddd371_372
                               _ <- dvCharsM
                               pos374_374 <- gets dvPos
                               if True
                                then return ()
                                else throwError (strMsg ("not match: pos: " ++ showPos pos374_374))
                               pos375_375 <- gets dvPos
                               dv_notComStrM >> return ()
                               if True
                                then return ()
                                else throwError (strMsg ("not match: pos: " ++ showPos pos375_375))
                               return (id nil),
                            do return (id nil)]
p_comEnd = foldl1 mplus [do xx376_376 <- dvCharsM
                            pos378_377 <- gets dvPos
                            case xx376_376 of
                                '-' -> return ()
                                _ -> throwError (strMsg ("not match: pos: " ++ showPos pos378_377))
                            let '-' = xx376_376
                            return ()
                            pos377_378 <- gets dvPos
                            if True
                             then return ()
                             else throwError (strMsg ("not match: pos: " ++ showPos pos377_378))
                            xx379_379 <- dvCharsM
                            pos381_380 <- gets dvPos
                            case xx379_379 of
                                '}' -> return ()
                                _ -> throwError (strMsg ("not match: pos: " ++ showPos pos381_380))
                            let '}' = xx379_379
                            return ()
                            pos380_381 <- gets dvPos
                            if True
                             then return ()
                             else throwError (strMsg ("not match: pos: " ++ showPos pos380_381))
                            return (id nil)]

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
    where newtype ListPos Char = CharPos ((Int, Int))
          listToken (c : s) = Just (c, s)
          listToken _ = Nothing
          listInitialPos = CharPos (1, 1)
          listUpdatePos '\n' (CharPos (y, _)) = CharPos (y + 1, 0)
          listUpdatePos _ (CharPos (y, x)) = CharPos (y, x + 1)
          listShowPos (CharPos pos) = show pos
instance SourceList c => Source ([c])
    where type Token ([c]) = c
          newtype Pos ([c]) = ListPos (ListPos c)
          getToken = listToken
          initialPos = ListPos listInitialPos
          updatePos c (ListPos p) = ListPos (listUpdatePos c p)
          showPos (ListPos p) = listShowPos p
