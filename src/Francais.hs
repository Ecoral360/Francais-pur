module Francais where

import Control.Applicative (Alternative (many, some, (<|>)), optional)
import Control.Monad (join)
import Data.Char (isDigit, isLetter, isSpace, isUpper, isUpperCase)
import Data.Either (rights)
import Data.Functor
import Data.List (find)
import qualified Data.List as List
import Data.Map (Map, delete, empty, filterWithKey, insert, lookup, member, notMember)
import Data.Maybe (fromMaybe, isJust, mapMaybe)
import FrancaisObj
import FrancaisParse
import GHC.Float
import Parser

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

frNEq = frCompOpIO "ne vaut pas" (/=)

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
  Right (FrTableau tab, applyFrIO io)

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
frEvalExpr env (FrExNEq exprG exprD) = do
  g <- frEvalExpr env exprG
  d <- frEvalExpr env exprD
  frNEq g d
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
  frConcat objs >>= \r -> Right (r, applyFrIO io)

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
    Right (obj, io') -> Right (obj, io >> applyFrIO ios >> io')
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
frEval env (FrPhSachantQue var expr corps) = do
  (value, io) <- frEvalExpr env expr
  if notMember var env
    then
      let sachantEnv = insert var value env
       in case execPh sachantEnv corps of
            (Right nouvelEnv, io') -> pure (delete var nouvelEnv, io >> applyFrIO io')
            (Left err, _) -> Left err
    else Left $ FrErrDecl var "La variable a déjà été déclarée."
frEval env (FrPhSachantQueMaintenant var expr corps) = do
  (value, io) <- frEvalExpr env expr
  if member var env
    then
      let sachantEnv = insert var value env
       in case execPh sachantEnv corps of
            (Right nouvelEnv, io') -> pure (delete var nouvelEnv, io >> applyFrIO io')
            (Left err, _) -> Left err
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
            (Left (FrCtrlRetourner obj), io) -> Right (obj, applyFrIO io)
            (Right _, io) -> Left $ FrErrAppelFonction nom "Les fonctions doivent retourner une valeur."
            (Left err, io) -> Left err
frEval env (FrPhDefProcedure nom params corps)
  | member nom env = Left $ FrErrDecl nom "La variable a déjà été déclarée."
  | otherwise = pure (insert nom fonc env, pure ())
  where
    fonc = FrProcedure $ \args ->
      let envFonc = foldr1 (<>) $ zipWith (\var val -> insert var val env) params args
       in case execPh envFonc corps of
            (Right _, io) -> Right $ applyFrIO io
            (Left (FrCtrlRetourner _), io) -> Right $ applyFrIO io
            (Left err, io) -> Left err
----- Appel fonction -----
frEval env (FrPhAppelerProc foncExpr argsExpr) = do
  (fonc, io) <- frEvalExpr env foncExpr
  argsIO <- mapM (frEvalExpr env) argsExpr
  let (args, ios) = unzip argsIO
  case appelerProc fonc args of
    Right io' -> Right (env, io >> applyFrIO ios >> io')
    Left err -> Left err
frEval env (FrPhAppelerFonc foncExpr argsExpr) = do
  (fonc, io) <- frEvalExpr env foncExpr
  argsIO <- mapM (frEvalExpr env) argsExpr
  let (args, ios) = unzip argsIO
  case appelerFonc fonc args of
    Right (obj, io') -> Right (env, io >> applyFrIO ios >> io')
    Left err -> Left err
frEval env (FrPhSi condExpr corpsVrai corpsFaux) = do
  (condObj, io) <- frEvalExpr env condExpr
  case condObj of
    (FrBool cond) ->
      let corps = if cond then corpsVrai else corpsFaux
       in case execPh env corps of
            (Right nouvelEnv, ios) -> Right (nouvelEnv, io >> applyFrIO ios)
            (Left err, io) -> Left err
frEval env boucle@(FrPhTantQue condExpr corps) = do
  (condObj, io) <- frEvalExpr env condExpr
  case condObj of
    (FrBool True) ->
      case execPh env $ corps ++ [boucle] of
        (Right nouvelEnv, ios) -> Right (nouvelEnv, io >> applyFrIO ios)
        (Left err, io) -> Left err
    (FrBool False) -> Right (env, io)
frEval env (FrPhPourChaqueCar var expr corps) = do
  (texte, io) <- frEvalExpr env expr
  case texte of
    (FrTexte (c : reste)) ->
      let pourchaqueEnv = insert var (FrCaractere c) env
       in case execPh pourchaqueEnv $ corps ++ [FrPhPourChaqueCar var (FrExConst $ FrTexte reste) corps] of
            (Right nouvelEnv, ios) -> Right (nouvelEnv, io >> applyFrIO ios)
            (Left err, io) -> Left err
    (FrTexte []) -> Right (delete var env, io)
frEval env (FrPhPourChaqueEl var expr corps) = do
  (tab, io) <- frEvalExpr env expr
  case tab of
    (FrTableau (e : reste)) ->
      let pourchaqueEnv = insert var e env
       in case execPh pourchaqueEnv $ corps ++ [FrPhPourChaqueEl var (FrExConst $ FrTableau reste) corps] of
            (Right nouvelEnv, ios) -> Right (nouvelEnv, io >> applyFrIO ios)
            (Left err, io) -> Left err
    (FrTableau []) -> Right (delete var env, io)

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
  case runParser (many frPhrase <* eof) s 0 of
    (Right (_, phrases, _)) -> execPh env phrases
    (Left err) -> (Left $ FrErrParse err, [print err])

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

applyFrIO ios = foldr1 (>>) (reverse ios)

frEvalMeta :: [FrMeta] -> IO FrEnv
frEvalMeta =
  foldr
    ( \meta env -> case meta of
        FrMetaInclure [] fichier -> do
          code <- readFile fichier
          case exec empty code of
            (Right env', io) ->
              applyFrIO io >> (<>) env' <$> env
        FrMetaInclure frVars fichier -> do
          code <- readFile fichier
          case exec empty code of
            (Right env', io) ->
              applyFrIO io >> (<>) (filterWithKey (\k _ -> isJust $ find (k ==) frVars) env') <$> env
    )
    $ pure empty

runFr s = case runParser frMetas s 0 of
  (Right (_, metas, rest)) -> do
    env <- frEvalMeta metas
    case exec env rest of
      (Right _, io) -> applyFrIO io
      (Left err, io) -> applyFrIO io >> print (frErrToString err)

frToString :: FrObj -> String
frToString FrNul = "nul"
frToString (FrBool v)
  | v = "Vrai"
  | otherwise = "Faux"
frToString (FrTexte s) = s
frToString (FrCaractere c) = "le caractère " ++ [c]
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
