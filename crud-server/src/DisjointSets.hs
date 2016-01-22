module DisjointSets where

import qualified Data.Map as M
import qualified Data.Set as S

newtype DisjointSets a t = DisjointSets (M.Map a t) deriving (Show)

empty :: DisjointSets a t
empty = DisjointSets M.empty

addToSet :: Ord a => t -> [a] -> DisjointSets a t -> DisjointSets a t
-- The order here is important, as M.union is left-biased, and we want to overwrite
-- the values in `m` with the new (t, a) values
addToSet t as (DisjointSets m) = DisjointSets $ M.union (M.fromList $ zip as (repeat t)) m

elementsBy :: Eq t => DisjointSets a t -> t -> S.Set a
elementsBy (DisjointSets m) t =
    M.keysSet $ M.filter (== t) m

all :: DisjointSets a t -> S.Set a
all (DisjointSets m) = M.keysSet m

