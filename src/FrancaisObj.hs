module FrancaisObj where

import Control.Applicative (Alternative (many, some, (<|>)), optional)
import Control.Monad (join)
import Data.Char (isDigit, isLetter, isSpace, isUpper, isUpperCase)
import Data.Either (rights)
import Data.Functor
import Data.Map (Map, empty, insert, lookup, member, notMember)
import Data.Maybe (fromMaybe, mapMaybe)
import GHC.Float
import Parser

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
  | FrExExposant FrExpr FrExpr
  | FrExModulo FrExpr FrExpr
  | -- Opérations de comparaison
    FrExEq FrExpr FrExpr
  | FrExNEq FrExpr FrExpr
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
  | FrExCarIndex FrExpr FrExpr
  | -- def fonctions
    FrExDefFonction [FrVar] [FrPhrase]
  | FrExDefProcedure [FrVar] [FrPhrase]
  | -- Appel de fonction
    FrExAppelFonc FrExpr [FrExpr]

data FrPositon = FrDebut | FrFin | FrIdx FrExpr

data FrPhrase
  = FrPhImprimer FrExpr
  | FrPhPosons FrVar FrExpr
  | FrPhMaintenant FrExpr FrExpr
  | FrPhAppelerFonc FrExpr [FrExpr]
  | FrPhAppelerProc FrExpr [FrExpr]
  | FrPhDefFonction FrVar [FrVar] [FrPhrase]
  | FrPhDefProcedure FrVar [FrVar] [FrPhrase]
  | FrPhRetourner FrExpr
  | FrPhSachantQue FrVar FrExpr [FrPhrase]
  | FrPhSachantQueMaintenant FrVar FrExpr [FrPhrase]
  | FrPhSi FrExpr [FrPhrase] [FrPhrase]
  | FrPhTantQue FrExpr [FrPhrase]
  | FrPhPourChaqueCar FrVar FrExpr [FrPhrase]
  | FrPhPourChaqueEl FrVar FrExpr [FrPhrase]
  | FrPhAjouter FrExpr FrPositon FrVar

data FrMeta
  = FrMetaInclure [FrVar] String
