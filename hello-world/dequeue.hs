-- dequeue.hs
{-
 - Exploration of the Data.Dequeue package
 -}

import qualified Data.Dequeue as DQ

main = do
    let q = DQ.fromList [1,2,3,4,5] :: DQ.BankersDequeue Int
    let (Just f, q') = DQ.popFront q
    let q'' = DQ.pushBack q' 6
    print f
    print q''
