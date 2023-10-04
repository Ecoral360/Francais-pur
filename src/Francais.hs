{-# LANGUAGE LambdaCase #-}

module Francais where

import Control.Applicative (Alternative (many, some, (<|>)), optional)
import Control.Monad (join)
import Data.Char (isDigit, isLetter, isSpace, isUpper, isUpperCase)
import Data.Either (rights)
import Data.Functor
import Data.Map (Map, empty, insert, lookup)
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

symbole s = espaces *> string s <* espaces

data FrError
  = FrErrVarInconnue FrVar
  | FrErrBinOp String FrObj FrObj
  | FrErrOp String FrObj
  | FrErrIndex FrObj Int Int
  | FrErrParse [Error Char]
  | Empty -- Used in `Alternative` implementation of `empty`
  deriving (Eq, Show)

type FrVar = String

data FrObj
  = FrEntier Int
  | FrDecimal Double
  | FrTexte String
  | FrCaractere Char
  | FrTableau [FrObj]
  | FrBool Bool
  | FrIdent FrVar
  | FrNul
  deriving (Show, Eq)

frObj =
  choice
    [ FrTableau
        <$> choice
          [ [] <$ symbole "un tableau vide",
            symbole "un tableau contenant seulement" *> frObj <&> (: []),
            symbole "un tableau contenant"
              *> collect
                [ sepBy frObj (symbole ","),
                  symbole "et" *> frObj <&> (: [])
                ]
          ],
      FrEntier . read <$> entier,
      FrDecimal . read <$> decimal,
      FrTexte <$> texte,
      FrCaractere <$> (symbole "le caractère" *> caractere),
      FrIdent <$> nomVar
    ]

data FrExpr
  = FrExConst FrObj
  | FrExPlus FrExpr FrExpr
  | FrExMoins FrExpr FrExpr
  | FrExFois FrExpr FrExpr
  | FrExSur FrExpr FrExpr -- division
  | FrExIndex FrExpr FrExpr
  | FrExPosition FrExpr FrExpr
  | FrExCarIndex FrExpr FrExpr
  | FrExCarPosition FrExpr FrExpr
  deriving (Show, Eq)

frExpr =
  choice
    [ FrExPlus <$> (FrExConst <$> frObj) <*> (symbole "plus" *> frExpr),
      FrExMoins <$> (FrExConst <$> frObj) <*> (symbole "moins" *> frExpr),
      FrExFois <$> (FrExConst <$> frObj) <*> (symbole "fois" *> frExpr),
      FrExSur <$> (FrExConst <$> frObj) <*> (symbole "sur" *> frExpr),
      FrExIndex <$> (symbole "élément de" *> frExpr) <*> (symbole "à l'index" *> frExpr),
      FrExPosition <$> (symbole "élément de" *> frExpr) <*> (symbole "à la position" *> frExpr),
      FrExCarIndex <$> (symbole "caractère de" *> frExpr) <*> (symbole "à l'index" *> frExpr),
      FrExCarPosition <$> (symbole "caractère de" *> frExpr) <*> (symbole "à la position" *> frExpr),
      FrExConst <$> frObj
    ]

data FrPhrase
  = FrPhImprimer FrExpr
  | FrPhDecl FrVar FrExpr
  deriving (Show, Eq)

frPhrase =
  choice
    [ FrPhImprimer <$> (symbole "Imprimer" *> frExpr),
      FrPhDecl <$> (symbole "Posons que" *> nomVar) <*> (symbole "vaut" *> frExpr)
    ]
    <* symbole "."

frBinOp :: String -> (Double -> Double -> Double) -> FrObj -> FrObj -> Either FrError FrObj
frBinOp opName op g (FrTexte s) = pure . FrTexte $ frToString g ++ s
frBinOp opName op (FrTexte s) d = pure . FrTexte $ s ++ frToString d
frBinOp opName op (FrEntier g) (FrEntier d) = pure . FrEntier $ double2Int $ int2Double g `op` int2Double d
frBinOp opName op (FrDecimal g) (FrDecimal d) = pure . FrDecimal $ g `op` d
frBinOp opName op (FrEntier g) (FrDecimal d) = pure . FrDecimal $ int2Double g `op` d
frBinOp opName op (FrDecimal g) (FrEntier d) = pure . FrDecimal $ g `op` int2Double d
frBinOp opName op g d = Left $ FrErrBinOp opName g d

frPlus = frBinOp "plus" (+)

frMoins = frBinOp "moins" (-)

frFois = frBinOp "fois" (*)

frDiv = frBinOp "sur" (/)

frEvalExpr :: FrEnv -> FrExpr -> Either FrError FrObj
frEvalExpr env (FrExConst (FrIdent var)) = case Data.Map.lookup var env of
  Just result -> Right result
  Nothing -> Left $ FrErrVarInconnue var
frEvalExpr env (FrExConst obj) = Right obj
frEvalExpr env (FrExPlus exprG exprD) = do
  g <- frEvalExpr env exprG
  d <- frEvalExpr env exprD
  frPlus g d
frEvalExpr env (FrExMoins exprG exprD) = do
  g <- frEvalExpr env exprG
  d <- frEvalExpr env exprD
  frMoins g d
frEvalExpr env (FrExFois exprG exprD) = do
  g <- frEvalExpr env exprG
  d <- frEvalExpr env exprD
  frFois g d
frEvalExpr env (FrExSur exprG exprD) = do
  g <- frEvalExpr env exprG
  d <- frEvalExpr env exprD
  frDiv g d
frEvalExpr env (FrExIndex exprTab exprIdx) = do
  tab <-
    frEvalExpr env exprTab >>= \case
      (FrTableau tab) -> pure tab
      other -> Left $ FrErrOp "index" other
  idx <-
    frEvalExpr env exprIdx >>= \case
      (FrEntier idx) -> pure idx
      other -> Left $ FrErrBinOp "index" (FrTableau tab) other
  let maxIdx = length tab
   in if idx >= maxIdx
        then Left $ FrErrIndex (FrTableau tab) idx (maxIdx - 1)
        else Right $ tab !! idx
frEvalExpr env (FrExPosition exprTab exprIdx) = do
  tab <-
    frEvalExpr env exprTab >>= \case
      (FrTableau tab) -> pure tab
      other -> Left $ FrErrOp "position" other
  idx <-
    frEvalExpr env exprIdx >>= \case
      (FrEntier idx) -> pure idx
      other -> Left $ FrErrBinOp "position" (FrTableau tab) other
  let maxIdx = length tab
   in if idx > maxIdx
        then Left $ FrErrIndex (FrTableau tab) idx maxIdx
        else Right $ tab !! (idx - 1)
frEvalExpr env (FrExCarIndex exprTxt exprIdx) = do
  txt <-
    frEvalExpr env exprTxt >>= \case
      (FrTexte txt) -> pure txt
      other -> Left $ FrErrOp "index" other
  idx <-
    frEvalExpr env exprIdx >>= \case
      (FrEntier idx) -> pure idx
      other -> Left $ FrErrBinOp "index" (FrTexte txt) other
  let maxIdx = length txt
   in if idx >= maxIdx
        then Left $ FrErrIndex (FrTexte txt) idx (maxIdx - 1)
        else Right $ FrCaractere $ txt !! idx
frEvalExpr env (FrExCarPosition exprTxt exprIdx) = do
  txt <-
    frEvalExpr env exprTxt >>= \case
      (FrTexte txt) -> pure txt
      other -> Left $ FrErrOp "position" other
  idx <-
    frEvalExpr env exprIdx >>= \case
      (FrEntier idx) -> pure idx
      other -> Left $ FrErrBinOp "position" (FrTexte txt) other
  let maxIdx = length txt
   in if idx > maxIdx
        then Left $ FrErrIndex (FrTexte txt) idx maxIdx
        else Right $ FrCaractere $ txt !! (idx - 1)

frEval :: FrEnv -> FrPhrase -> Either FrError (FrEnv, IO ())
frEval env (FrPhImprimer expr) = do
  result <- frEvalExpr env expr
  pure (env, putStrLn $ frToString result)
frEval env (FrPhDecl var expr) = do
  value <- frEvalExpr env expr
  pure (insert var value env, pure ())

parse s = runParser frExpr s 0

type FrEnv = Map FrVar FrObj

eval env s =
  case runParser frExpr s 0 of
    (Right (_, expr, _)) -> do
      result <- frEvalExpr env expr
      pure $ frToString result
    (Left err) -> Left $ FrErrParse err

exec :: FrEnv -> String -> Either FrError (FrEnv, [IO ()])
exec env s =
  case runParser (many frPhrase) s 0 of
    (Right (_, phrases, _)) ->
      foldr
        ( \phrase ctx ->
            case ctx of
              Right (env, io) -> do
                (env', io') <- frEval env phrase
                Right (env', io' : io)
              Left err -> Left err
        )
        (Right (env, [pure ()]))
        (reverse phrases)
    (Left err) -> pure (env, [print err])

runFr s = case exec empty s of
  (Right (_, results)) -> foldr1 (>>) results
  (Left err) -> print err

frToString :: FrObj -> String
frToString FrNul = "nul"
frToString (FrTexte s) = s
frToString (FrEntier i) = case i of
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

symImprimer = symbole "Imprimer"

symDeclarer = symbole "Déclarer"

symMaintenant = symbole "Maintenant"

symDemander = symbole "Demander puis enregistrer dans"

symAppelA = symbole "le résultat de l'appel à"

symImporter = symbole "Importer le module"

symExecuter = choice [symbole "Exécuter", symbole "exécuter"]

symEnonce = symbole "énoncé"

symPuis = symbole "puis"

symAllerA = symbole "aller à"

symSauter = symbole "sauter"

symSi = symbole "si"

symSinon = symbole "sinon,"

symTantQue = symbole "tant que"

symPourChaque = symbole "pour chaque"

symDefFonction = symbole "Début de la définition de la fonction nommée"

symFinFonction = symbole "Fin de la définition de la fonction"

symAppeler = symbole "Appeler"
