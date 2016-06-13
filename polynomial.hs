{-# LANGUAGE GADTs #-}
import Data.List (intercalate)
import qualified Data.IntMap as M
import Debug.Trace

data Polynomial where
  Polynomial :: M.IntMap Int -> Polynomial

instance Show Polynomial where
  show (Polynomial coeffMap)
    | M.null coeffMap = "0"
    | otherwise       = intercalate " + " (map showCoeffPair $ M.toList coeffMap)

showCoeffPair :: (Int, Int) -> String
showCoeffPair (0, c) = show c
showCoeffPair (1, c) = (if c == 1 then "" else show c) ++ "x"
showCoeffPair (n, c) = (if c == 1 then "" else show c) ++ "x^" ++ show n

(Polynomial as) `multiply` (Polynomial bs) =
  Polynomial $ M.fromListWith (+) [ (na + nb, ca*cb) | (na, ca) <- M.toList as, (nb, cb) <- M.toList bs ]

pnull (Polynomial ps) = M.null ps
empty = Polynomial (M.empty)
singleton n = Polynomial (M.singleton n 1)

scale (Polynomial ps) c = Polynomial $ M.map (*c) ps
(Polynomial as) `plus` (Polynomial bs) = Polynomial $ M.filter (>0) $ M.unionWith (+) as bs
(Polynomial as) `minus` (Polynomial bs) = Polynomial $ M.filter (>0) $ M.unionWith (-) as bs

a@(Polynomial as) `divmod` b@(Polynomial bs)
  | pnull a = (a, empty)
  | otherwise = 
  let
    ((na, ca), (nb, cb)) = (M.findMax as, M.findMax bs)
    (n, c) = (na - nb, ca `div` cb)
    p = scale (singleton n) c
    a' = a `minus` (b `multiply` p)
    b' = b `minus` p
  in traceShow (a, b, p, a', b') $ if n == 0 then p else p `plus` (a' `divide` b')
