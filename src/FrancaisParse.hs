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

texteOp = symbole "`" *> manyUntil (char '`') anyChar

mot = espaces *> manyUntil espace anyChar

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

enumNomVar = frEnumeration nomVar

symbole s = espaces *> string s <* espaces

frObj =
  choice
    [ FrEntier . read <$> entier,
      FrDecimal . read <$> decimal,
      FrTexte <$> texte,
      FrCaractere <$> (symbole "le caractère" *> caractere),
      FrBool <$> ((True <$ symbole "vrai") <|> (False <$ symbole "faux")),
      FrIdent <$> nomVar
    ]

frEnumeration p =
  collect
    [ sepBy p (symbole ","),
      symbole "et" *> p <&> (: [])
    ]
    <|> (p <&> (: []))

frEnumeration1 p =
  collect
    [ sepBy p (symbole ","),
      symbole "et" *> p <&> (: [])
    ]

frExEnum = frEnumeration1 frExpr

frPhEnum =
  (<|>)
    (symbole ":" *> frEnumeration1 frPhraseIncomplete)
    $ (symbole "," *> frPhraseIncomplete) <&> (: [])

frMetaEnumeration =
  collect
    [ sepBy frPhraseIncomplete (symbole ","),
      symbole "et" *> frPhraseIncomplete <&> (: [])
    ]
    <|> (frPhraseIncomplete <&> (: []))

frExprUnaire =
  choice
    [ between (char '"') (char '"') frExpr,
      FrExTableau
        <$> choice
          [ [] <$ symbole "un tableau vide",
            symbole "un tableau contenant seulement" *> frExpr <&> (: []),
            symbole "un tableau contenant"
              *> frExEnum
          ],
      FrExConst <$> frObj,
      FrExMoinsUn <$> (symbole "moins" *> frExprUnaire)
    ]

frExpr :: Parser Char FrExpr
frExpr =
  choice
    [ frExprUnaire >>= \gauche ->
        choice
          [ -- arithmétique
            FrExPlus gauche <$> (symbole "plus" *> frExpr),
            FrExMoins gauche <$> (symbole "moins" *> frExpr),
            FrExFois gauche <$> (symbole "fois" *> frExpr),
            FrExDiv gauche <$> (symbole "sur" *> frExpr),
            FrExModulo gauche <$> (symbole "modulo" *> frExpr),
            FrExExposant gauche <$> (symbole "exposant" *> frExpr),
            -- comparaison
            FrExEq gauche <$> (symbole "vaut" *> frExpr),
            FrExNEq gauche <$> (symbole "ne vaut pas" *> frExpr),
            FrExPlusQue gauche <$> (symbole "vaut plus que" *> frExpr),
            FrExMoinsQue gauche <$> (symbole "vaut moins que" *> frExpr),
            FrExMoinsEq gauche <$> (symbole "vaut moins ou autant que" *> frExpr),
            FrExPlusEq gauche <$> (symbole "vaut plus ou autant que" *> frExpr)
          ],
      -- arithmétique variadique
      FrExSomme <$> (symbole "la somme de" *> frExEnum),
      FrExDiff <$> (symbole "la différence entre" *> frExEnum),
      FrExProduit <$> (symbole "le produit de" *> frExEnum),
      FrExQuotient <$> (symbole "le quotient de" *> frExEnum),
      FrExConcat <$> (symbole "la concaténation de" *> frExEnum),
      -- indexation
      FrExIndex <$> (symbole "élément de" *> frExpr) <*> (symbole "à l'index" *> frExpr),
      FrExIndex
        <$> (symbole "élément de" *> frExpr)
        <*> (FrExPlus (FrExConst (FrEntier (-1))) <$> (symbole "à la position" *> frExpr)),
      FrExCarIndex <$> (symbole "caractère de" *> frExpr) <*> (symbole "à l'index" *> frExpr),
      FrExCarIndex
        <$> (symbole "caractère de" *> frExpr)
        <*> (FrExPlus (FrExConst (FrEntier (-1))) <$> (symbole "à la position" *> frExpr)),
      -- FrExCarPosition <$> (symbole "caractère de" *> frExpr) <*> (symbole "à la position" *> frExpr),
      -- appel
      FrExAppelFonc
        <$> (symbole "le résultat de l'appel à la fonction" *> frExpr)
        <*> choice
          [ (symbole "avec l'argument" *> frExpr) <&> (: []),
            symbole "avec les arguments" *> frExEnum
          ],
      FrExDefFonction
        <$> ( symbole "une fonction"
                *> ( choice
                       [ [] <$ symbole "n'acceptant aucun paramètre",
                         (symbole "acceptant le paramètre" *> nomVar) <&> (: []),
                         symbole "acceptant les paramètres" *> enumNomVar
                       ]
                       <* symbole "."
                   )
            )
        <*> (symbole "Lorsqu'appelée" *> frPhEnum),
      FrExOpInfixe <$> frExprUnaire <*> choice [texteOp] <*> frExpr,
      -- autre
      frExprUnaire
    ]
    <|> parseError (NoChoiceMatched "Expression")

frPhraseIncomplete =
  choice
    [ FrPhImprimer <$> (symbole "imprimer" *> frExpr),
      FrPhPosons <$> (symbole "posons que" *> nomVar) <*> (symbole "vaut" *> frExpr),
      FrPhMaintenant <$> (symbole "maintenant," *> frExpr) <*> (symbole "vaut" *> frExpr),
      FrPhAppelerFonc
        <$> (symbole "appeler la fonction" *> frExpr)
        <*> choice
          [ [] <$ symbole "sans argument",
            (symbole "avec l'argument" *> frExpr) <&> (: []),
            symbole "avec les arguments" *> frExEnum
          ],
      FrPhAppelerProc
        <$> (symbole "appeler la procédure" *> frExpr)
        <*> choice
          [ [] <$ symbole "sans argument",
            (symbole "avec l'argument" *> frExpr) <&> (: []),
            symbole "avec les arguments" *> frExEnum
          ],
      FrPhSachantQue
        <$> (symbole "sachant que" *> nomVar)
        <*> (symbole "vaut" *> frExpr)
        <*> frPhEnum,
      FrPhSachantQueMaintenant
        <$> (symbole "sachant que," >> symbole "maintenant," *> nomVar)
        <*> (symbole "vaut" *> frExpr)
        <*> frPhEnum,
      FrPhSi
        <$> (symbole "si" *> frExpr)
        <*> frPhEnum
        <*> ((symbole "." *> symbole "Sinon" *> frPhEnum) <|$> []),
      FrPhTantQue
        <$> (symbole "tant que" *> frExpr)
        <*> frPhEnum,
      FrPhPourChaqueCar
        <$> (symbole "pour chaque caractère" *> nomVar)
        <*> (symbole "dans" *> frExpr)
        <*> frPhEnum,
      FrPhPourChaqueEl
        <$> (symbole "pour chaque élément" *> nomVar)
        <*> (symbole "dans" *> frExpr)
        <*> frPhEnum,
      FrPhAjouter
        <$> (symbole "ajouter" *> frExpr)
        <*> choice
          [ FrDebut <$ symbole "au début",
            FrFin <$ symbole "à la fin",
            FrIdx <$> (symbole "à l'index" *> frExpr),
            FrIdx . FrExPlus (FrExConst (FrEntier (-1))) <$> (symbole "à la position" *> frExpr)
          ]
        <*> (symbole "de" *> nomVar),
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
      FrPhDefOperationInfixe
        <$> (symbole "début de la définition de l'opération infixe" *> texteOp)
        <*> (symbole "acceptant les paramètres" *> ((:) <$> nomVar <*> (symbole "et" *> nomVar <&> (: []))) <* symbole ".")
        <*> manyUntil
          (symbole "Fin de la définition de l'opération infixe")
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

frMetaEnum = frEnumeration frMeta

frMeta =
  choice
    [ FrMetaInclure
        <$> ( symbole "inclure"
                *> choice
                  [ [] <$ symbole "tout le contenu",
                    enumNomVar
                  ]
            )
        <*> (symbole "du fichier" *> texte)
    ]

frMetas =
  toFrPhrase (symbole "avant toutes choses," *> frMetaEnum)
    <|$> []

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
