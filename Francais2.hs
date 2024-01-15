module Francais where

import Control.Applicative (Alternative (many, some, (<|>)), optional)
import Control.Monad (join)
import Data.Char (isDigit, isLetter, isSpace, isUpper, isUpperCase)
import Data.Either (rights)
import Data.Functor
import Data.Map (Map, empty, insert, lookup, member, notMember)
import Data.Maybe (fromMaybe, mapMaybe)
import GHC.Float
import Parser

espace = satisfy isSpace

espaces = many espace

lettre = satisfy isLetter

chiffre = satisfy isDigit

frChiffreSingulier =
  choice
    [ symZero,
      symUn
    ]

frChiffrePluriel =
  choice
    [ symDeux,
      symTrois,
      symQuatre,
      symCinq,
      symSix,
      symSept,
      symHuit,
      symNeuf,
      symDix,
      symOnze,
      symDouze,
      symTreize,
      symQuatorze,
      symQuinze,
      symSeize,
      (:) <$> chiffre <*> some chiffre
    ]

frChiffre = choice [frChiffreSingulier, frChiffrePluriel]

entier =
  collect
    [ ("-" <$ symbole "moins") <|$> "",
      frChiffre
    ]

decimal =
  collect
    [ ("-" <$ symbole "moins") <|$> "",
      some chiffre,
      "." <$ choice [string ",", symbole "virgule"],
      some chiffre
    ]

texte =
  between (string "« ") (string "»") $ many $ satisfy (/= '»')

caractere =
  choice
    [ ' ' <$ symbole "espace",
      '\n' <$ symbole "nouvelle ligne",
      '\t' <$ symbole "tabulation",
      between espaces espaces anyChar
    ]

nomVar =
  (:)
    <$> satisfy isUpperCase
    <*> collect
      [ many lettre,
        many $ choice [lettre, chiffre]
      ]

enumNomVar =
  collect
    [ sepBy nomVar (symbole ","),
      symbole "et" *> nomVar <&> (: [])
    ]

symbole s = espaces *> string s <* espaces

data FrError
  = FrErrVarInconnue FrVar
  | FrErrBinOp String FrObj FrObj
  | FrErrOp String FrObj
  | FrErrIndex FrObj Int Int
  | FrErrParse [Error Char]
  | FrErrDecl FrVar String
  | FrErrAppelFonction FrVar String
  | Empty -- Used in `Alternative` implementation of `empty`
  | --- Contrôle de flux
    FrCtrlRetourner FrObj

frErrToString :: FrError -> String
frErrToString err = "Error"

type FrVar = String

data FrObj
  = FrEntier Int
  | FrDecimal Double
  | FrTexte String
  | FrCaractere Char
  | FrTableau [FrObj]
  | FrBool Bool
  | FrIdent FrVar
  | FrFonction {appelerFonc :: [FrObj] -> Either FrError (FrObj, IO ())}
  | FrProcedure {appelerProc :: [FrObj] -> Either FrError (IO ())}
  | FrNul

instance Eq FrObj where
  (==) FrNul FrNul = True
  (==) (FrEntier v1) (FrEntier v2) = v1 == v2
  (==) (FrDecimal v1) (FrEntier v2) = v1 == int2Double v2
  (==) (FrEntier v1) (FrDecimal v2) = int2Double v1 == v2
  (==) (FrDecimal v1) (FrDecimal v2) = v1 == v2
  (==) (FrTexte v1) (FrTexte v2) = v1 == v2
  (==) (FrCaractere v1) (FrCaractere v2) = v1 == v2
  (==) (FrTableau v1) (FrTableau v2) = v1 == v2
  (==) (FrBool v1) (FrBool v2) = v1 == v2
  (==) (FrFonction v1) (FrFonction v2) = False
  (==) (FrProcedure v1) (FrProcedure v2) = False
  (==) v1 v2 = False

instance Ord FrObj where
  (<=) (FrEntier v1) (FrEntier v2) = v1 <= v2
  (<=) (FrDecimal v1) (FrEntier v2) = v1 <= int2Double v2
  (<=) (FrEntier v1) (FrDecimal v2) = int2Double v1 <= v2
  (<=) (FrCaractere v1) (FrCaractere v2) = v1 <= v2

frObj =
  choice
    [ FrEntier . read <$> entier,
      FrDecimal . read <$> decimal,
      FrTexte <$> texte,
      FrCaractere <$> (symbole "le caractère" *> caractere),
      FrIdent <$> nomVar
    ]

data FrExpr
  = FrExConst FrObj
  | FrExTableau [FrExpr]
  | FrExRien
  | -- Opérations unaires
    FrExMoinsUn FrExpr
  | -- Opérations arithmétique
    FrExPlus FrExpr FrExpr
  | FrExMoins FrExpr FrExpr
  | FrExFois FrExpr FrExpr
  | FrExDiv FrExpr FrExpr
  | FrExModulo FrExpr FrExpr
  | -- Opérations de comparaison
    FrExEq FrExpr FrExpr
  | FrExMoinsQue FrExpr FrExpr
  | FrExPlusQue FrExpr FrExpr
  | FrExMoinsEq FrExpr FrExpr
  | FrExPlusEq FrExpr FrExpr
  | -- Version variadique des opérations arithmétique
    FrExSomme [FrExpr]
  | FrExDiff [FrExpr]
  | FrExProduit [FrExpr]
  | FrExQuotient [FrExpr]
  | FrExConcat [FrExpr]
  | -- Opérations d'indexation
    FrExIndex FrExpr FrExpr
  | FrExPosition FrExpr FrExpr
  | FrExCarIndex FrExpr FrExpr
  | FrExCarPosition FrExpr FrExpr
  | -- Appel de fonction
    FrExAppelFonc FrExpr [FrExpr]

frEnumeration =
  collect
    [ sepBy frExpr (symbole ","),
      symbole "et" *> frExpr <&> (: [])
    ]

frExprUnaire =
  choice
    [ FrExTableau
        <$> choice
          [ [] <$ symbole "un tableau vide",
            symbole "un tableau contenant seulement" *> frExpr <&> (: []),
            symbole "un tableau contenant"
              *> frEnumeration
          ],
      FrExConst <$> frObj,
      FrExMoinsUn <$> (symbole "moins" *> frExprUnaire)
    ]

frExpr :: Parser Char FrExpr
frExpr =
  choice
    [ between (char '"') (char '"') frExpr,
      -- arithmétique
      FrExPlus <$> frExprUnaire <*> (symbole "plus" *> frExpr),
      FrExMoins <$> frExprUnaire <*> (symbole "moins" *> frExpr),
      FrExFois <$> frExprUnaire <*> (symbole "fois" *> frExpr),
      FrExDiv <$> frExprUnaire <*> (symbole "sur" *> frExpr),
      FrExModulo <$> frExprUnaire <*> (symbole "modulo" *> frExpr),
      -- comparaison
      FrExPlusQue <$> frExprUnaire <*> (symbole "vaut" *> frExpr),
      FrExPlusQue <$> frExprUnaire <*> (symbole "vaut plus que" *> frExpr),
      FrExMoinsQue <$> frExprUnaire <*> (symbole "vaut moins que" *> frExpr),
      FrExMoinsEq <$> frExprUnaire <*> (symbole "vaut moins ou autant que" *> frExpr),
      FrExPlusEq <$> frExprUnaire <*> (symbole "vaut plus ou autant que" *> frExpr),
      -- arithmétique variadique
      FrExSomme <$> (symbole "la somme de" *> frEnumeration),
      FrExDiff <$> (symbole "la différence entre" *> frEnumeration),
      FrExProduit <$> (symbole "le produit de" *> frEnumeration),
      FrExQuotient <$> (symbole "le quotient de" *> frEnumeration),
      FrExConcat <$> (symbole "la concaténation de" *> frEnumeration),
      -- indexation
      FrExIndex <$> (symbole "élément de" *> frExpr) <*> (symbole "à l'index" *> frExpr),
      FrExPosition <$> (symbole "élément de" *> frExpr) <*> (symbole "à la position" *> frExpr),
      FrExCarIndex <$> (symbole "caractère de" *> frExpr) <*> (symbole "à l'index" *> frExpr),
      FrExCarPosition <$> (symbole "caractère de" *> frExpr) <*> (symbole "à la position" *> frExpr),
      -- appel
      FrExAppelFonc
        <$> (symbole "l'appel à la fonction" *> frExpr)
        <*> choice
          [ (symbole "avec l'argument" *> frExpr) <&> (: []),
            symbole "avec les arguments" *> frEnumeration
          ],
      -- autre
      frExprUnaire
    ]

data FrPhrase
  = FrPhImprimer FrExpr
  | FrPhPosons FrVar FrExpr
  | FrPhMaintenant FrVar FrExpr
  | FrPhAppelerFonc FrExpr [FrExpr]
  | FrPhAppelerProc FrExpr [FrExpr]
  | FrPhDefFonction FrVar [FrVar] [FrPhrase]
  | FrPhDefProcedure FrVar [FrVar] [FrPhrase]
  | FrPhRetourner FrExpr
  | FrPhSi [FrPhrase] FrExpr [FrPhrase]

frPhrase =
  choice
    [ FrPhImprimer <$> (symbole "Imprimer" *> frExpr),
      FrPhPosons <$> (symbole "Posons que" *> nomVar) <*> (symbole "vaut" *> frExpr),
      FrPhMaintenant <$> (symbole "Maintenant," *> nomVar) <*> (symbole "vaut" *> frExpr),
      FrPhAppelerFonc
        <$> (symbole "Appeler la fonction" *> frExpr)
        <*> choice
          [ (symbole "avec l'argument" *> frExpr) <&> (: []),
            symbole "avec les arguments" *> frEnumeration
          ],
      FrPhAppelerProc
        <$> (symbole "Appeler la procédure" *> frExpr)
        <*> choice
          [ (symbole "avec l'argument" *> frExpr) <&> (: []),
            symbole "avec les arguments" *> frEnumeration
          ],
      FrPhSi
        <$> ( do
                nbEnonces <-
                  read
                    <$> ( symbole "Exécuter"
                            *> choice
                              [ frChiffreSingulier <* symbole "énoncé",
                                frChiffrePluriel <* symbole "énoncés"
                              ]
                        )
                manyN nbEnonces frPhrase
            )
        <*> (symbole "si" *> frExpr)
        <*> ( do
                nbEnonces <-
                  read
                    <$> ( symbole ";"
                            >> symbole "sinon exécuter"
                              *> choice
                                [ frChiffreSingulier <* symbole "énoncé",
                                  frChiffrePluriel <* symbole "énoncés"
                                ]
                        )
                manyN nbEnonces frPhrase
            )
          <|$> [],
      FrPhDefFonction
        <$> (symbole "Début de la définition de la fonction nommée" *> nomVar)
        <*> choice
          [ (symbole "acceptant le paramètre" *> nomVar) <&> (: []),
            symbole "acceptant les paramètres" *> enumNomVar
          ]
        <* symbole "."
        <*> manyUntil
          (symbole "Fin de la définition de la fonction")
          ( choice
              [ FrPhRetourner <$> (symbole "Retourner" *> frExpr) <* symbole ".",
                frPhrase
              ]
          ),
      FrPhDefProcedure
        <$> (symbole "Début de la définition de la procédure nommée" *> nomVar)
        <*> choice
          [ (symbole "acceptant le paramètre" *> nomVar) <&> (: []),
            symbole "acceptant les paramètres" *> enumNomVar
          ]
        <* symbole "."
        <*> manyUntil
          (symbole "Fin de la définition de la procédure")
          ( choice
              [ FrPhRetourner <$> (FrExRien <$ symbole "Retourner") <* symbole ".",
                frPhrase
              ]
          )
    ]
    <* symbole "."

frBinOp :: String -> (Double -> Double -> Double) -> FrObj -> FrObj -> Either FrError FrObj
frBinOp opName op (FrEntier g) (FrEntier d) = pure . FrEntier $ double2Int $ int2Double g `op` int2Double d
frBinOp opName op (FrDecimal g) (FrDecimal d) = pure . FrDecimal $ g `op` d
frBinOp opName op (FrEntier g) (FrDecimal d) = pure . FrDecimal $ int2Double g `op` d
frBinOp opName op (FrDecimal g) (FrEntier d) = pure . FrDecimal $ g `op` int2Double d
frBinOp opName op g d = Left $ FrErrBinOp opName g d

frBinOpIO :: (FrObj -> FrObj -> Either FrError FrObj) -> (FrObj, IO ()) -> (FrObj, IO ()) -> Either FrError (FrObj, IO ())
frBinOpIO binOp (g, io) (d, io') = binOp g d >>= \r -> Right (r, io >> io')

frPlus = frBinOpIO $ frBinOp "plus" (+)

frMoins = frBinOpIO $ frBinOp "moins" (-)

frFois = frBinOpIO $ frBinOp "fois" (*)

frDiv = frBinOpIO $ frBinOp "sur" (/)

frConcat :: [FrObj] -> Either FrError FrObj
frConcat objs = pure . FrTexte . foldr1 (++) $ map frToString objs

frCompOpIO :: String -> (FrObj -> FrObj -> Bool) -> (FrObj, IO ()) -> (FrObj, IO ()) -> Either FrError (FrObj, IO ())
frCompOpIO opNom compOp (g, io) (d, io') = Right (FrBool $ compOp g d, io >> io')

frEq = frCompOpIO "vaut" (==)

frMoinsQue = frCompOpIO "vaut moins que" (<)

frPlusQue = frCompOpIO "vaut plus que" (>)

frMoinsEq = frCompOpIO "vaut moins ou autant que" (<=)

frPlusEq = frCompOpIO "vaut plus ou autant que" (>=)

------ Eval d'expressions ------
frEvalExpr :: FrEnv -> FrExpr -> Either FrError (FrObj, IO ())
----- Constantes -----
frEvalExpr env (FrExConst (FrIdent var)) = case Data.Map.lookup var env of
  Just result -> Right (result, pure ())
  Nothing -> Left $ FrErrVarInconnue var
frEvalExpr env (FrExConst obj) = Right (obj, pure ())
frEvalExpr env (FrExTableau tabEx) = do
  tabIO <- mapM (frEvalExpr env) tabEx
  let (tab, io) = unzip tabIO
  Right (FrTableau tab, foldr1 (>>) io)

----- Arithmétique -----
frEvalExpr env (FrExPlus exprG exprD) = do
  g <- frEvalExpr env exprG
  d <- frEvalExpr env exprD
  frPlus g d
frEvalExpr env (FrExMoins exprG exprD) = do
  g <- frEvalExpr env exprG
  d <- frEvalExpr env exprD
  frMoins g d
frEvalExpr env (FrExMoinsUn expr) = do
  v <- frEvalExpr env expr
  frMoins (FrEntier 0, pure ()) v
frEvalExpr env (FrExFois exprG exprD) = do
  g <- frEvalExpr env exprG
  d <- frEvalExpr env exprD
  frFois g d
frEvalExpr env (FrExDiv exprG exprD) = do
  g <- frEvalExpr env exprG
  d <- frEvalExpr env exprD
  frDiv g d

----- Opération de comparaison -----
frEvalExpr env (FrExEq exprG exprD) = do
  g <- frEvalExpr env exprG
  d <- frEvalExpr env exprD
  frEq g d
frEvalExpr env (FrExPlusQue exprG exprD) = do
  g <- frEvalExpr env exprG
  d <- frEvalExpr env exprD
  frPlusQue g d
frEvalExpr env (FrExMoinsQue exprG exprD) = do
  g <- frEvalExpr env exprG
  d <- frEvalExpr env exprD
  frMoinsQue g d
frEvalExpr env (FrExPlusEq exprG exprD) = do
  g <- frEvalExpr env exprG
  d <- frEvalExpr env exprD
  frPlusEq g d
frEvalExpr env (FrExMoinsEq exprG exprD) = do
  g <- frEvalExpr env exprG
  d <- frEvalExpr env exprD
  frMoinsEq g d

----- Arithmétique variadique -----
frEvalExpr env (FrExConcat exprs) = do
  objsIO <- mapM (frEvalExpr env) exprs
  let (objs, io) = unzip objsIO
  frConcat objs >>= \r -> Right (r, foldr1 (>>) io)

----- Indexation de tableau -----
frEvalExpr env (FrExIndex exprTab exprIdx) = do
  (tab, io) <- frEvalExpr env exprTab
  tab <- case tab of
    (FrTableau tab) -> pure tab
    other -> Left $ FrErrOp "index" other
  (idx, io') <- frEvalExpr env exprTab
  idx <-
    case idx of
      (FrEntier idx) -> pure idx
      other -> Left $ FrErrBinOp "index" (FrTableau tab) other
  let maxIdx = length tab
   in if idx >= maxIdx
        then Left $ FrErrIndex (FrTableau tab) idx (maxIdx - 1)
        else Right (tab !! idx, io >> io')
frEvalExpr env (FrExPosition exprTab exprIdx) = do
  (tab, io) <- frEvalExpr env exprTab
  tab <- case tab of
    (FrTableau tab) -> pure tab
    other -> Left $ FrErrOp "position" other
  (idx, io') <- frEvalExpr env exprTab
  idx <-
    case idx of
      (FrEntier idx) -> pure idx
      other -> Left $ FrErrBinOp "position" (FrTableau tab) other
  let maxIdx = length tab
   in if idx > maxIdx
        then Left $ FrErrIndex (FrTableau tab) idx maxIdx
        else Right (tab !! (idx - 1), io >> io')

----- Indexation de texte -----
frEvalExpr env (FrExCarIndex exprTxt exprIdx) = do
  (txt, io) <- frEvalExpr env exprTxt
  txt <- case txt of
    (FrTexte tab) -> pure tab
    other -> Left $ FrErrOp "index" other
  (idx, io') <- frEvalExpr env exprIdx
  idx <-
    case idx of
      (FrEntier idx) -> pure idx
      other -> Left $ FrErrBinOp "index" (FrTexte txt) other
  let maxIdx = length txt
   in if idx >= maxIdx
        then Left $ FrErrIndex (FrTexte txt) idx (maxIdx - 1)
        else Right (FrCaractere $ txt !! idx, io >> io')
frEvalExpr env (FrExCarPosition exprTxt exprIdx) = do
  (txt, io) <- frEvalExpr env exprTxt
  txt <- case txt of
    (FrTexte tab) -> pure tab
    other -> Left $ FrErrOp "position" other
  (idx, io') <- frEvalExpr env exprIdx
  idx <-
    case idx of
      (FrEntier idx) -> pure idx
      other -> Left $ FrErrBinOp "position" (FrTexte txt) other
  let maxIdx = length txt
   in if idx > maxIdx
        then Left $ FrErrIndex (FrTexte txt) idx maxIdx
        else Right (FrCaractere $ txt !! (idx - 1), io >> io')
frEvalExpr env (FrExAppelFonc foncExpr argsExpr) = do
  (fonc, io) <- frEvalExpr env foncExpr
  argsIO <- mapM (frEvalExpr env) argsExpr
  let (args, ios) = unzip argsIO
  case appelerFonc fonc args of
    Right (obj, io') -> Right (obj, io >> foldr1 (>>) ios >> io')
    Left err -> Left err

------ Eval de phrase ------
frEval :: FrEnv -> FrPhrase -> Either FrError (FrEnv, IO ())
frEval env (FrPhImprimer expr) = do
  (result, io) <- frEvalExpr env expr
  pure (env, io >> putStrLn (frToString result))
----- Déclarations -----
frEval env (FrPhPosons var expr) = do
  (value, io) <- frEvalExpr env expr
  if notMember var env
    then pure (insert var value env, io)
    else Left $ FrErrDecl var "La variable a déjà été déclarée."
frEval env (FrPhMaintenant var expr) = do
  (value, io) <- frEvalExpr env expr
  if member var env
    then pure (insert var value env, io)
    else Left $ FrErrDecl var "La variable n'a pas été déclarée."

----- Définition fonction -----
frEval env (FrPhRetourner FrExRien) = Left $ FrCtrlRetourner FrNul
frEval env (FrPhRetourner expr) = do
  (obj, io) <- frEvalExpr env expr
  Left $ FrCtrlRetourner obj
frEval env (FrPhDefFonction nom params corps)
  | member nom env = Left $ FrErrDecl nom "La variable a déjà été déclarée."
  | otherwise = pure (insert nom fonc env, pure ())
  where
    fonc = FrFonction $ \args ->
      let envFonc = foldr1 (<>) $ zipWith (\var val -> insert var val env) params args
       in case execPh envFonc corps of
            (Left (FrCtrlRetourner obj), io) -> Right (obj, foldr1 (>>) io)
            (Right _, io) -> Left $ FrErrAppelFonction nom "Les fonctions doivent retourner une valeur."
            (Left err, io) -> Left err
frEval env (FrPhDefProcedure nom params corps)
  | member nom env = Left $ FrErrDecl nom "La variable a déjà été déclarée."
  | otherwise = pure (insert nom fonc env, pure ())
  where
    fonc = FrProcedure $ \args ->
      let envFonc = foldr1 (<>) $ zipWith (\var val -> insert var val env) params args
       in case execPh envFonc corps of
            (Right _, io) -> Right $ foldr1 (>>) io
            (Left (FrCtrlRetourner _), io) -> Right $ foldr1 (>>) io
            (Left err, io) -> Left err
----- Appel fonction -----
frEval env (FrPhAppelerProc foncExpr argsExpr) = do
  (fonc, io) <- frEvalExpr env foncExpr
  argsIO <- mapM (frEvalExpr env) argsExpr
  let (args, ios) = unzip argsIO
  case appelerProc fonc args of
    Right io' -> Right (env, io >> foldr1 (>>) ios >> io')
    Left err -> Left err
frEval env (FrPhAppelerFonc foncExpr argsExpr) = do
  (fonc, io) <- frEvalExpr env foncExpr
  argsIO <- mapM (frEvalExpr env) argsExpr
  let (args, ios) = unzip argsIO
  case appelerFonc fonc args of
    Right (obj, io') -> Right (env, io >> foldr1 (>>) ios >> io')
    Left err -> Left err
frEval env (FrPhSi corpsVrai condExpr corpsFaux) = do
  (condObj, io) <- frEvalExpr env condExpr
  case condObj of
    (FrBool cond) ->
      let corps = if cond then corpsVrai else corpsFaux
       in case execPh env corps of
            (Right env, ios) -> Right (env, io >> foldr1 (>>) ios)
            (Left err, io) -> Left err

parse s = runParser frExpr s 0

type FrEnv = Map FrVar FrObj

eval env s =
  case runParser frExpr s 0 of
    (Right (_, expr, _)) -> do
      (result, io) <- frEvalExpr env expr
      pure $ frToString result
    (Left err) -> Left $ FrErrParse err

exec :: FrEnv -> String -> (Either FrError FrEnv, [IO ()])
exec env s =
  case runParser (many frPhrase) s 0 of
    (Right (_, phrases, _)) -> execPh env phrases
    (Left err) -> (Right env, [print err])

execPh :: FrEnv -> [FrPhrase] -> (Either FrError FrEnv, [IO ()])
execPh env phrases =
  foldr
    ( \phrase ctx ->
        case ctx of
          (Right env, io) ->
            case frEval env phrase of
              Right (env', io') -> (Right env', io' : io)
              Left err -> (Left err, io)
          (Left err, io) -> (Left err, io)
    )
    (Right env, [pure ()])
    (reverse phrases)

runFr s = case exec empty s of
  (Right _, io) -> foldr1 (>>) (reverse io)
  (Left err, io) -> print $ frErrToString err

frToString :: FrObj -> String
frToString FrNul = "nul"
frToString (FrTexte s) = s
frToString (FrEntier i) =
  ( if i < 0 && i > -16
      then "moins "
      else ""
  )
    ++ case abs i of
      0 -> "zéro"
      1 -> "un"
      2 -> "deux"
      3 -> "trois"
      4 -> "quatre"
      5 -> "cinq"
      6 -> "six"
      7 -> "sept"
      8 -> "huit"
      9 -> "neuf"
      10 -> "dix"
      11 -> "onze"
      12 -> "douze"
      13 -> "treize"
      14 -> "quatorze"
      15 -> "quinze"
      16 -> "seize"
      _ -> show i
frToString (FrDecimal d) = show d
frToString (FrTableau []) = "un tableau vide"
frToString (FrTableau [x]) = "un tableau contenant seulement " ++ frToString x
frToString (FrTableau x) = "un tableau contenant " ++ tableauToString x
  where
    tableauToString [x, x'] = frToString x ++ " et " ++ frToString x'
    tableauToString (x : xs) = frToString x ++ ", " ++ tableauToString xs
frToString (FrFonction _) = "Fonction"

-------------------- Symboles --------------------

symZero = "0" <$ symbole "zéro"

symUn = "1" <$ symbole "un"

symDeux = "2" <$ symbole "deux"

symTrois = "3" <$ symbole "trois"

symQuatre = "4" <$ symbole "quatre"

symCinq = "5" <$ symbole "cinq"

symSix = "6" <$ symbole "six"

symSept = "7" <$ symbole "sept"

symHuit = "8" <$ symbole "huit"

symNeuf = "9" <$ symbole "neuf"

symDix = "10" <$ symbole "dix"

symOnze = "11" <$ symbole "onze"

symDouze = "12" <$ symbole "douze"

symTreize = "13" <$ symbole "treize"

symQuatorze = "14" <$ symbole "quatorze"

symQuinze = "15" <$ symbole "quinze"

symSeize = "16" <$ symbole "seize"
