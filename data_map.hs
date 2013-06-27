-- data_map.hs
{-
 - Simple program to learn how to use the built-in map type
 -}

import qualified Data.Map as Map

m = Map.fromList [("a", 5), ("b", 10)
                 ,("c", 15), ("d", 20)
                 ,("e", 25), ("f", 30)]

main = print $ Map.lookup "e" m
