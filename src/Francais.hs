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

frDiv = frBinOpIO $ frBinOp "div" (/)

frExposant = frBinOpIO $ frBinOp "exposant" (**)

frMod (FrEntier g, io) (FrEntier d, io') = Right (FrEntier $ g `mod` d, io >> io')

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

frGetCar :: FrEnv -> FrExpr -> FrExpr -> Either FrError (String, Int, IO ())
frGetCar env txtExpr idxExpr = do
  (texte, io) <- frEvalExpr env txtExpr
  texte <- case texte of
    (FrTexte texte) -> pure texte
    other -> Left $ FrErrOp "index" other
  (idx, io') <- frEvalExpr env idxExpr
  idx <-
    case idx of
      (FrEntier idx) -> pure idx
      other -> Left $ FrErrBinOp "index" (FrTexte texte) other
  pure (texte, idx, io >> io')

frGetEl :: FrEnv -> FrExpr -> FrExpr -> Either FrError ([FrObj], Int, IO ())
frGetEl env tabExpr idxExpr = do
  (tab, io) <- frEvalExpr env tabExpr
  tab <- case tab of
    (FrTableau tab) -> pure tab
    other -> Left $ FrErrOp "index" other
  (idx, io') <- frEvalExpr env idxExpr
  idx <-
    case idx of
      (FrEntier idx) -> pure idx
      other -> Left $ FrErrBinOp "index" (FrTableau tab) other
  pure (tab, idx, io >> io')

frAjouter :: FrPositon -> FrEnv -> [a] -> a -> Either FrError ([a], IO ())
frAjouter FrDebut _ els el = pure (el : els, pure ())
frAjouter FrFin _ els el = pure (els ++ [el], pure ())
frAjouter (FrIdx idxExpr) env els el = do
  (idxObj, io) <- frEvalExpr env idxExpr
  let (FrEntier idx) = idxObj
  pure (take idx els ++ el : drop idx els, io)

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
frEvalExpr env (FrExExposant exprG exprD) = do
  g <- frEvalExpr env exprG
  d <- frEvalExpr env exprD
  frExposant g d
frEvalExpr env (FrExModulo exprG exprD) = do
  g <- frEvalExpr env exprG
  d <- frEvalExpr env exprD
  frMod g d
frEvalExpr env (FrExOpInfixe exprG op exprD) = do
  (fonc, _) <- case Data.Map.lookup op env of
    Just result -> Right (result, pure () :: IO ())
    Nothing -> Left $ FrErrVarInconnue op
  (g, io) <- frEvalExpr env exprG
  (d, io') <- frEvalExpr env exprD
  case appelerOpInfixe fonc g d of
    Right (obj, io'') -> Right (obj, io >> io' >> io'')
    Left err -> Left err
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
  (tab, idx, io) <- frGetEl env exprTab exprIdx
  let maxIdx = length tab
   in if idx >= maxIdx
        then Left $ FrErrIndex (FrTableau tab) idx (maxIdx - 1)
        else Right (tab !! idx, io)
-- frEvalExpr env (FrExPosition exprTab exprIdx) = do
--   (tab, io) <- frEvalExpr env exprTab
--   tab <- case tab of
--     (FrTableau tab) -> pure tab
--     other -> Left $ FrErrOp "position" other
--   (idx, io') <- frEvalExpr env exprTab
--   idx <-
--     case idx of
--       (FrEntier idx) -> pure idx
--       other -> Left $ FrErrBinOp "position" (FrTableau tab) other
--   let maxIdx = length tab
--    in if idx > maxIdx
--         then Left $ FrErrIndex (FrTableau tab) idx maxIdx
--         else Right (tab !! (idx - 1), io >> io')

----- Indexation de texte -----
frEvalExpr env (FrExCarIndex exprTxt exprIdx) = do
  (texte, idx, io) <- frGetCar env exprTxt exprIdx
  let maxIdx = length texte
   in if idx >= maxIdx
        then Left $ FrErrIndex (FrTexte texte) idx (maxIdx - 1)
        else Right (FrCaractere $ texte !! idx, io)
-- frEvalExpr env (FrExCarPosition exprTxt exprIdx) = do
--   (txt, io) <- frEvalExpr env exprTxt
--   txt <- case txt of
--     (FrTexte tab) -> pure tab
--     other -> Left $ FrErrOp "position" other
--   (idx, io') <- frEvalExpr env exprIdx
--   idx <-
--     case idx of
--       (FrEntier idx) -> pure idx
--       other -> Left $ FrErrBinOp "position" (FrTexte txt) other
--   let maxIdx = length txt
--    in if idx > maxIdx
--         then Left $ FrErrIndex (FrTexte txt) idx maxIdx
--         else Right (FrCaractere $ txt !! (idx - 1), io >> io')
frEvalExpr env (FrExAppelFonc foncExpr argsExpr) = do
  (fonc, io) <- frEvalExpr env foncExpr
  argsIO <- mapM (frEvalExpr env) argsExpr
  let (args, ios) = unzip argsIO
  case appelerFonc fonc args of
    Right (obj, io') -> Right (obj, io >> applyFrIO ios >> io')
    Left err -> Left err
frEvalExpr env (FrExDefFonction params corps) =
  pure
    ( FrFonction $ \args ->
        let envFonc = foldr1 (<>) $ zipWith (\var val -> insert var val env) params args
         in case execPh envFonc corps of
              (Left (FrCtrlRetourner obj), io) -> Right (obj, applyFrIO io)
              (Right _, io) -> Left $ FrErrAppelFonction "anonyme" "Les fonctions doivent retourner une valeur."
              (Left err, io) -> Left err,
      pure ()
    )

------ Eval de phrase ------
frEval :: FrEnv -> FrPhrase -> Either FrError (FrEnv, IO ())
frEval env (FrPhImprimer expr) = do
  (result, io) <- frEvalExpr env expr
  pure (env, io >> putStrLn (frToString result))
frEval env (FrPhAjouter valueExpr position var) = do
  (value, io) <- frEvalExpr env valueExpr
  (contenant, io') <- frEvalExpr env (FrExConst (FrIdent var))
  case contenant of
    (FrTableau tab) -> do
      (nouveauTab, io'') <- frAjouter position env tab value
      pure (insert var (FrTableau nouveauTab) env, io >> io' >> io'')
    (FrTexte texte) -> do
      let (FrCaractere c) = value
      (nouveauTexte, io'') <- frAjouter position env texte c
      pure (insert var (FrTexte nouveauTexte) env, io >> io' >> io'')
----- Déclarations -----
frEval env (FrPhPosons var expr) = do
  (value, io) <- frEvalExpr env expr
  if notMember var env
    then pure (insert var value env, io)
    else Left $ FrErrDecl var "La variable a déjà été déclarée."
frEval env (FrPhMaintenant varExpr expr) = do
  (value, io) <- frEvalExpr env expr
  case varExpr of
    (FrExConst (FrIdent var)) ->
      if member var env
        then pure (insert var value env, io)
        else Left $ FrErrDecl var "La variable n'a pas été déclarée."
    (FrExIndex varExpr'@(FrExConst (FrIdent var)) idxExpr) -> do
      (tab, idx, io') <- frGetEl env varExpr idxExpr
      let nouveauTab = FrTableau $ take idx tab ++ value : drop (idx + 1) tab
      pure (insert var nouveauTab env, io' >> io)
    (FrExCarIndex varExpr'@(FrExConst (FrIdent var)) idxExpr) -> do
      let (FrCaractere c) = value
      (texte, idx, io') <- frGetCar env varExpr idxExpr
      let nouveauTexte = FrTexte $ take idx texte ++ c : drop (idx + 1) texte
      pure (insert var nouveauTexte env, io' >> io)
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
frEval env (FrPhDefOperationInfixe nom params corps)
  | member nom env = Left $ FrErrDecl nom "La variable a déjà été déclarée."
  | otherwise = pure (insert nom fonc env, pure ())
  where
    fonc = FrOperationInfixe $ \gauche droite ->
      let envFonc = foldr1 (<>) $ zipWith (\var val -> insert var val env) params [gauche, droite]
       in case execPh envFonc corps of
            (Left (FrCtrlRetourner obj), io) -> Right (obj, applyFrIO io)
            (Right _, io) -> Left $ FrErrAppelFonction nom "Les opérations doivent retourner une valeur."
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

frToString :: FrObj -> String
frToString FrNul = "nul"
frToString (FrBool v)
  | v = "Vrai"
  | otherwise = "Faux"
frToString (FrTexte s) = s
frToString (FrCaractere c) = "le caractère " ++ [c]
frToString (FrEntier i) =
  ( if i < 0 && i > -17
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

frErrToString :: FrError -> String
frErrToString (FrErrParse err) = show err
frErrToString (FrErrDecl var raison) = var ++ " " ++ raison
frErrToString (FrErrAppelFonction nomFonc raison) = show nomFonc ++ " " ++ raison
frErrToString (FrErrBinOp op gauche droite) =
  op
    ++ " "
    ++ frToString gauche
    ++ frToString droite
frErrToString (FrErrOp op obj) = op ++ " " ++ frToString obj
frErrToString (FrErrVarInconnue var) = var ++ " n'existe pas"
frErrToString err = "Error"

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
    (Left err) -> (Left $ FrErrParse err, [print $ last err])

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

applyFrIO [] = pure ()
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
