module Main where

import Francais
import System.Environment

-- write the command: cabal run francais -- rouler "Bonjour le monde"
main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> error "Tu dois fournir une option"
    ["rouler", code] -> roulerFr code
    [fichier] -> roulerFichier fichier

