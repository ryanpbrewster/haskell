import qualified Data.Array as A
import Control.Monad (forM_, when)

clockwise :: [[a]] -> [a]
clockwise [] = []
clockwise grid =
    let (m,n) = (length grid, length $ head grid)
        arr = A.listArray ((1,1), (m,n)) (concat grid)
    in clockwiseHelper arr 1 m 1 n

-- clockwiseHelper arr "top" "bottom" "left" "right"
clockwiseHelper arr t b l r
    | (t > b)  || (l > r)  = []
    | (t == b) || (l == r) = [ arr A.! (i,j) | i <- [t..b], j <- [l..r] ]
    | otherwise =
        let out = [ arr A.! (t,j) | j <- [l,l+1..r-1] ] ++ -- top edge
                  [ arr A.! (i,r) | i <- [t,t+1..b-1] ] ++ -- right edge
                  [ arr A.! (b,j) | j <- [r,r-1..l+1] ] ++ -- bot edge
                  [ arr A.! (i,l) | i <- [b,b-1..t+1] ]    -- left edge
        in out ++ clockwiseHelper arr (t+1) (b-1) (l+1) (r-1)


clockwiseTests = [ ([], [])
                 , (["A"], "A")
                 , (["AB","CD"], "ABDC")
                 , (["ABC","DEF","GHI"], "ABCFIHGDE")
                 , (["ABCDE"], "ABCDE")
                 , (["ABCDEF"], "ABCDEF")
                 , (["A","B","C","D","E"], "ABCDE")
                 , (["A","B","C","D","E","F"], "ABCDEF")
                 , (["ABCD","EFGH","IJKL"], "ABCDHLKJIEFG")
                 , (["ABCD","EFGH","IJKL","MNOP"], "ABCDHLPONMIEFGKJ")
                 ]

runTests f test_answer_pairs = do
    let failures = [ (t, a, r) | (t,a) <- test_answer_pairs
                               , let r = f t
                               , r /= a
                               ]
    forM_ failures $ \(t,a,r) ->
        putStrLn $ "For " ++ show t ++ 
                    " expected " ++ show a ++
                    " but got " ++ show r
    when (null failures) $ putStrLn "All tests passed!"
