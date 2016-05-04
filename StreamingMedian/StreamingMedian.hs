module StreamingMedian
( streamingMedian
) where

import qualified Data.PQueue.Min as Min
import qualified Data.PQueue.Max as Max

streamingMedian :: [Double] -> [Double]
streamingMedian [] = []
streamingMedian (x:xs) = streamingMedian' (Max.singleton x) Min.empty xs
  where
  streamingMedian' maxPQ minPQ [] = [medianFromHeaps maxPQ minPQ]
  streamingMedian' maxPQ minPQ (x:xs) =
    let median = medianFromHeaps maxPQ minPQ
        (maxPQ', minPQ') = balancePQs $ if x <= median then (Max.insert x maxPQ, minPQ) else (maxPQ, Min.insert x minPQ)
    in median : streamingMedian' maxPQ' minPQ' xs
  medianFromHeaps maxPQ minPQ
    | Max.size maxPQ == Min.size minPQ = avg (Max.findMax maxPQ) (Min.findMin minPQ)
    | Max.size maxPQ > Min.size minPQ = Max.findMax maxPQ
    | otherwise                       = Min.findMin minPQ
  balancePQs (maxPQ, minPQ)
    | abs (Max.size maxPQ - Min.size minPQ) <= 1 = (maxPQ, minPQ)
    | Max.size maxPQ > Min.size minPQ = balancePQs (Max.deleteMax maxPQ, Min.insert (Max.findMax maxPQ) minPQ)
    | Max.size maxPQ < Min.size minPQ = balancePQs (Max.insert (Min.findMin minPQ) maxPQ, Min.deleteMin minPQ)

avg :: Double -> Double -> Double
avg x y = 0.5 * (x+y)
