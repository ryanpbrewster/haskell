module Lib
    ( someFunc
    ) where

import qualified Data.Set as S
import qualified DeltaSet as DS

someFunc :: IO ()
someFunc = print $ DS.empty `DS.plus` (S.fromList [1,2,3]) `DS.minus` (S.fromList [2,4,6])
