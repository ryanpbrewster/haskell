module DeltaSet where

import qualified Data.Set as S
import qualified DisjointSets as DJS

data AddOrRemove = Added | Removed deriving (Eq, Show)
newtype DeltaSet a = DeltaSet (DJS.DisjointSets a AddOrRemove) deriving (Show)

empty :: DeltaSet a
empty = DeltaSet DJS.empty

added :: DeltaSet a -> S.Set a
added (DeltaSet djs) = djs `DJS.elementsBy` Added

removed :: DeltaSet a -> S.Set a
removed (DeltaSet djs) = djs `DJS.elementsBy` Removed

plus :: Ord a => DeltaSet a -> S.Set a -> DeltaSet a
(DeltaSet djs) `plus` xs = DeltaSet (DJS.addToSet Added (S.elems xs) djs)

minus :: Ord a => DeltaSet a -> S.Set a -> DeltaSet a
(DeltaSet djs) `minus` xs = DeltaSet (DJS.addToSet Removed (S.elems xs) djs)
