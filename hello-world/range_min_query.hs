-- range_min_query.hs

import Data.Array
import System.Random
import Data.List (scanl, scanl1, tails)


naive xs i j = snd $ minimum [(x, idx) | (idx, x) <- zip [i..j] (drop i xs) ]

-- construct an auxiliary matrix B
-- where B(i,j) = min { A[i], ..., A[j] }
-- then query that directly in O(1) time
dp xs = let n = length xs - 1
            xs' = zip xs [0..n]
            -- ys == drop i xs'
            -- v == (x, idx), where x is the min value in xs[i..j] and idx is the location of x
            dp_arr = [ ((i,j), snd v) | (i, ys) <- zip [0..n] (tails xs')
                                      , (j, v) <- zip [i..n] (scanl1 min ys) ]
        in dp' $ array ((0,0),(n,n)) dp_arr
    where dp' b i j = b ! (i,j)

log2 x = log x / log 2

st :: [Double] -> Int -> Int -> Int
st xs = st' $ makeSparseTable xs
    where st' sparse_table i j = querySparseTable sparse_table i j

makeSparseTable :: [Double] -> Array (Int,Int) (Double,Int)
makeSparseTable xs =
    let xs' = zip xs [0..]
        n = length xs
        a = listArray (0,n-1) xs'
        k = ceiling $ log2 (fromIntegral n)
        st_rows = scanl nextSparseRow xs' [0..k-1]
        st_arr =  [ ((i,j),v) | (j,row) <- zip [0..] st_rows
                              , (i, v) <- zip [0..] row ]
    in accumArray (flip const) (0,-1) ((0,0), (n-1,k)) st_arr

nextSparseRow :: [(Double,Int)] -> Int -> [(Double,Int)]
nextSparseRow row j = zipWith min row (drop (2^j) row)

querySparseTable :: Array (Int,Int) (Double,Int) -> Int -> Int -> Int
querySparseTable sparse_table i j =
    let k = floor $ log2 (fromIntegral $ j-i+1)
    in snd $ min (sparse_table ! (i,k)) (sparse_table ! (j-2^k+1,k))


main = do
    let n = 10000  -- length of A
    let m = 100000 -- max(A)
    let t = 100000   -- number of queries
    let xs = take n $ randomRs (1, m) (mkStdGen 0) :: [Double]
    let qs = take t [(min i j, max i j) | i <- randomRs (0,n-1) (mkStdGen 1)
                                        , j <- randomRs (0,n-1) (mkStdGen 2) ]
    --print $ sum [ naive xs i j | (i,j) <- qs ]
    --print $ sum [ dp    xs i j | (i,j) <- qs ]
    print $ sum [ st    xs i j | (i,j) <- qs ]
