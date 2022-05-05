module Chapter3.MoreModules where

import Data.List

permutationsStartingWith :: Char -> String -> [String]
permutationsStartingWith letter
  = filter (\l -> head l == letter) . permutations
