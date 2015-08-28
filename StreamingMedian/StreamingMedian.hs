module StreamingMedian
( streamingMedian
) where

import qualified Data.PQueue.Min as Min
import qualified Data.PQueue.Max as Max

streamingMedian :: [Double] -> [Double]
streamingMedian [] = []
streamingMedian (x:xs) = streamingMedian' (Max.singleton x) (Min.singleton x) 1 xs


-- we have processed `k` elements so far
streamingMedian' lo_max_pq hi_min_pq k [] = [avg (Max.findMax lo_max_pq) (Min.findMin hi_min_pq)]
streamingMedian' lo_max_pq hi_min_pq k (x:xs) =
  let lo = Max.findMax lo_max_pq
      hi = Min.findMin hi_min_pq
      median = avg lo hi
      (lo_max_pq', hi_min_pq') = if odd k
                                 then ( Max.deleteMax (Max.insert x lo_max_pq)
                                      , Min.deleteMin (Min.insert x hi_min_pq)
                                      )
                                 else if x <= median
                                      then ( Max.insert x lo_max_pq, Min.insert (max x lo) hi_min_pq )
                                      else ( Max.insert (min x hi) lo_max_pq, Min.insert x hi_min_pq )
  in median : streamingMedian' lo_max_pq' hi_min_pq' (k+1) xs

avg :: Double -> Double -> Double
avg x y = 0.5 * (x+y)
