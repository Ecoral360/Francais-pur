module FrancaisParse where

import Control.Applicative (Alternative (many, some, (<|>)), optional)
import Control.Monad (guard, join)
import Data.Char (isDigit, isLetter, isSpace, isUpper, isUpperCase, toLower, toUpper)
import Data.Either (rights)
import Data.Functor
import Data.Map (Map, empty, insert, lookup, member, notMember)
import Data.Maybe (fromMaybe, mapMaybe)
import FrancaisObj
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
  (string "« " <|> string "<< ") *> manyUntil (string " »" <|> string " >>") anyChar

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

frErrToString :: FrError -> String
frErrToString err = "Error"

frObj =
  choice
    [ FrEntier . read <$> entier,
      FrDecimal . read <$> decimal,
      FrTexte <$> texte,
      FrCaractere <$> (symbole "le caractère" *> caractere),
      FrIdent <$> nomVar
    ]

frEnumeration =
  collect
    [ sepBy frExpr (symbole ","),
      symbole "et" *> frExpr <&> (: [])
    ]

frPhEnumeration =
  collect
    [ sepBy frPhraseIncomplete (symbole ","),
      symbole "et" *> frPhraseIncomplete <&> (: [])
    ]
    <|> (frPhraseIncomplete <&> (: []))

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
      frExprUnaire >>= \gauche ->
        choice
          [ -- arithmétique
            FrExPlus gauche <$> (symbole "plus" *> frExpr),
            FrExMoins gauche <$> (symbole "moins" *> frExpr),
            FrExFois gauche <$> (symbole "fois" *> frExpr),
            FrExDiv gauche <$> (symbole "sur" *> frExpr),
            FrExModulo gauche <$> (symbole "modulo" *> frExpr),
            -- comparaison
            FrExEq gauche <$> (symbole "vaut" *> frExpr),
            FrExNEq gauche <$> (symbole "ne vaut pas" *> frExpr),
            FrExPlusQue gauche <$> (symbole "vaut plus que" *> frExpr),
            FrExMoinsQue gauche <$> (symbole "vaut moins que" *> frExpr),
            FrExMoinsEq gauche <$> (symbole "vaut moins ou autant que" *> frExpr),
            FrExPlusEq gauche <$> (symbole "vaut plus ou autant que" *> frExpr)
          ],
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

frPhraseIncomplete =
  choice
    [ FrPhImprimer <$> (symbole "imprimer" *> frExpr),
      FrPhPosons <$> (symbole "posons que" *> nomVar) <*> (symbole "vaut" *> frExpr),
      FrPhMaintenant <$> (symbole "maintenant," *> nomVar) <*> (symbole "vaut" *> frExpr),
      FrPhAppelerFonc
        <$> (symbole "appeler la fonction" *> frExpr)
        <*> choice
          [ (symbole "avec l'argument" *> frExpr) <&> (: []),
            symbole "avec les arguments" *> frEnumeration
          ],
      FrPhAppelerProc
        <$> (symbole "appeler la procédure" *> frExpr)
        <*> choice
          [ (symbole "avec l'argument" *> frExpr) <&> (: []),
            symbole "avec les arguments" *> frEnumeration
          ],
      FrPhSachantQue
        <$> (symbole "sachant que" *> nomVar)
        <*> (symbole "vaut" *> frExpr)
        <*> (symbole "," *> frPhEnumeration),
      FrPhSachantQueMaintenant
        <$> (symbole "sachant que," >> symbole "maintenant," *> nomVar)
        <*> (symbole "vaut" *> frExpr)
        <*> (symbole "," *> frPhEnumeration),
      FrPhSi
        <$> (symbole "si" *> frExpr <* symbole ",")
        <*> frPhEnumeration
        <*> ((symbole "." *> symbole "Sinon," *> frPhEnumeration) <|$> []),
      FrPhTantQue
        <$> (symbole "tant que" *> frExpr <* symbole ",")
        <*> frPhEnumeration,
      FrPhDefFonction
        <$> (symbole "début de la définition de la fonction nommée" *> nomVar)
        <*> ( choice
                [ [] <$ symbole "n'acceptant aucun paramètre",
                  (symbole "acceptant le paramètre" *> nomVar) <&> (: []),
                  symbole "acceptant les paramètres" *> enumNomVar
                ]
                <* symbole "."
            )
        <*> manyUntil
          (symbole "Fin de la définition de la fonction")
          frPhrase,
      FrPhDefProcedure
        <$> (symbole "début de la définition de la procédure nommée" *> nomVar)
        <*> ( choice
                [ [] <$ symbole "n'acceptant aucun paramètre",
                  (symbole "acceptant le paramètre" *> nomVar) <&> (: []),
                  symbole "acceptant les paramètres" *> enumNomVar
                ]
                <* symbole "."
            )
        <*> manyUntil
          (symbole "Fin de la définition de la procédure")
          frPhrase,
      FrPhRetourner <$> ((symbole "retourner" *> frExpr) <|> (FrExRien <$ symbole "retourner"))
    ]

toFrPhrase phraseIncomplete = espaces *> satisfyPeek isUpperCase >> mapCharPeek toLower >> phraseIncomplete <* symbole "."

frPhrase =
  espaces
    *> satisfyPeek isUpperCase
    *> mapCharPeek toLower
    -- \*> optional (symbole "attention:" >> manyUntil (symbole "!") anyChar)
    *> frPhraseIncomplete
    <* symbole "."

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
