module Main where

import Data.Map (empty)
import Francais

main :: IO ()
main = result
  where
    (Right (_, result)) = exec empty "Imprimer un tableau contenant seulement dix."
