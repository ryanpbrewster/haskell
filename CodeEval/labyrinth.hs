-- labyrinth.hs
{-
 - You are given a text with a pseudo-graphical schema of labyrinth. Walls are marked by ‘*’ character. Your job is to write a program which finds the shortest way from the upper entrance to the bottom exit and prints out the labyrinth schema with a path marked with ‘+’ symbols.
 - Input sample:
 -
 - The text file with labyrinth schema:
 -
 - ************************* *************************
 - *                                   * *           *
 - * * *** *** ******************* ***** * * * * * ***
 - * * *   * *   *   * * *                 * * * *   *
 - ***** * * *** * *** * * *** *** * ***** *** *******
 - *     * *   * *     *   * * *   *     * * *       *
 - *** ******* * ***** *** * * ******* * *** * *** * *
 - * *     *     *   *     *     *     * *       * * *
 - * * *********** * ********* * ******* * *** * *****
 - *     * *   * * *     *     * *   *   * *   *     *
 - * ***** * *** * ***** *** *** * * * ******* ***** *
 - * *     *   * * *       * * *   * * * *   *     * *
 - * * ***** *** *** *** *** * ***** *** *** ***** ***
 - *     *   * * *     * *       * *       * *     * *
 - * * ***** * * * *** * *** ***** *** ***** *** * * *
 - * * *           *   * *   *     *     *     * *   *
 - * ******* ******* * *** ******* *** * * ********* *
 - *   *       *     * *   *         * * * *     *   *
 - *** * * ***** * ***** ******* ******* * * * * * ***
 - *     *   *   *         *       * *   * * * * *   *
 - *** * *** * *** ***** ******* * * * *** *** * *** *
 - * * * * * * * *     * * *     *       *   * * * * *
 - * * *** * * * *** *** * * ********* ***** * * * * *
 - * * *   * * *     *   * *   *     *   *     * * * *
 - * * * *** ******* ***** * ******* *** * *** *** * *
 - * * *     *   *   *     * *     * * * *   *   * * *
 - * ***** * * * *** * ***** ***** * * * ***** * * * *
 - * *     * * * *     * *     *           * * *   * *
 - * ***** * *** * ***** *********** ******* * * * * *
 - *     * * * *             *   *     * * *   * * * *
 - * * * *** * *** * ***** ***** ******* * *** * * * *
 - * * *   * * *   *     * *             *     * * * *
 - * ***** * * *********** ******* *** * ******* * * *
 - * *     *   *   *     * *   *   * * *       * *   *
 - * * * ********* * ***** * *** *** *** * ***** * ***
 - * * *       *           *   * * *   * *   *   *   *
 - * ******* ***** ******* * *** * * *** *** * *******
 - *   *   *   *   *   *     *         * * * * * * * *
 - * ***** * *** ***** * ******* * ***** * *** * * * *
 - *     *           *     *     * * *   *   *     * *
 - *** *** ********************* *** *** *** *** * * *
 - *   *   *     *               * * *   *       *   *
 - *** *** * ***** * ******* *** * * *** * *** ***** *
 - *       *       *   *   * * *   *     *   * *   * *
 - *** ***** ***** *** *** *** ***** * * *** *** * * *
 - *       *   *   * * *       *   * * *   * *   *   *
 - *** *** * ***** * ***** *** *** *** *** ******* ***
 - *   *     *   *   *     * * * *     * * *     *   *
 - * ***** *** ***** ******* * * *** *** * *** ***** *
 - *   *                 *           *         *     *
 - ************************* *************************
 -
 - Output sample:
 -
 - Print to stdout the labyrinth schema with the shortest way using ‘+’ symbols:
 -
 - *************************+*************************
 - *                        +++++++    * *           *
 - * * *** *** *******************+***** * * * * * ***
 - * * *   * *   *   * * *    +++++        * * * *   *
 - ***** * * *** * *** * * ***+*** * ***** *** *******
 - *     * *   * *     *   * *+*   *     * * *       *
 - *** ******* * ***** *** * *+******* * *** * *** * *
 - * *     *     *   *     *  +  *     * *       * * *
 - * * *********** * *********+* ******* * *** * *****
 - *     * *   * * *     *  +++* *   *   * *   *     *
 - * ***** * *** * ***** ***+*** * * * ******* ***** *
 - * *     *   * * *       *+* *   * * * *   *     * *
 - * * ***** *** *** *** ***+* ***** *** *** ***** ***
 - *     *   * * *     * *  +    * *       * *     * *
 - * * ***** * * * *** * ***+***** *** ***** *** * * *
 - * * *           *   * *+++*     *     *     * *   *
 - * ******* ******* * ***+******* *** * * ********* *
 - *   *       *     * *+++*         * * * *     *   *
 - *** * * ***** * *****+******* ******* * * * * * ***
 - *     *   *   *+++++++  *       * *   * * * * *   *
 - *** * *** * ***+***** ******* * * * *** *** * *** *
 - * * * * * * * *+++  * * *     *       *   * * * * *
 - * * *** * * * ***+*** * * ********* ***** * * * * *
 - * * *   * * *    +*   * *   *     *   *     * * * *
 - * * * *** *******+***** * ******* *** * *** *** * *
 - * * *     *   *  +*     * *     * * * *   *   * * *
 - * ***** * * * ***+* ***** ***** * * * ***** * * * *
 - * *     * * * *+++  * *     *           * * *   * *
 - * ***** * *** *+***** *********** ******* * * * * *
 - *     * * * *  +++++++++  *   *     * * *   * * * *
 - * * * *** * *** * *****+***** ******* * *** * * * *
 - * * *   * * *   *     *+*      +++++++*     * * * *
 - * ***** * * ***********+*******+*** *+******* * * *
 - * *     *   *   *     *+*   *+++* * *+      * *   *
 - * * * ********* * *****+* ***+*** ***+* ***** * ***
 - * * *       *  +++++++++*   *+* *   *+*   *   *   *
 - * ******* *****+******* * ***+* * ***+*** * *******
 - *   *   *   *+++*   *     *  +      *+* * * * * * *
 - * ***** * ***+***** * *******+* *****+* *** * * * *
 - *     *+++++++    *     *    +* * *  +*   *     * *
 - *** ***+*********************+*** ***+*** *** * * *
 - *   *  +*     *+++++++++++++++* * *  +*       *   *
 - *** ***+* *****+* ******* *** * * ***+* *** ***** *
 - *  +++++*+++++++*   *   * * *   *  +++*   * *   * *
 - ***+*****+***** *** *** *** ***** *+* *** *** * * *
 - *  +++++*+  *   * * *       *   * *+*   * *   *   *
 - *** ***+*+***** * ***** *** *** ***+*** ******* ***
 - *   *  +++*   *   *     * * * *  +++* * *     *   *
 - * ***** *** ***** ******* * * ***+*** * *** ***** *
 - *   *                 *  +++++++++*         *     *
 - *************************+*************************
 -
 - Constraints:
 - The size of a labyrinth is up to 101×101 cells. There can be more than one way to pass the labyrinth, but the shortest way is always unambiguous and never has branchings. 
 -}


import System.Environment (getArgs)
import Data.Array
import Data.Maybe (fromJust)
import Data.List (foldl')

main = do
    args <- getArgs
    txt <- readFile (head args)
    putStr $ solveProblem txt

solveProblem txt =
    let grid = parseGrid txt
        path = solveLabyrinth grid
        ans = case path of
                  Just p -> grid `drawPath` p
                  Nothing -> "No path found"
    in ans

type Grid = Array (Int,Int) Char

parseGrid :: String -> Grid
parseGrid txt = let lns = lines txt
                    (r, c) = (length lns, length $ head lns)
                in listArray ((1,1), (r,c)) $ concat lns

solveLabyrinth :: Grid -> Maybe [(Int,Int)]
solveLabyrinth grid =
    let ((1,1),(r,c)) = bounds grid
        s = head [j | j <- [1..c], grid ! (1,j) == ' ']
        d = head [j | j <- [1..c], grid ! (r,j) == ' ']
    in bfs grid (Queue [[(2,s),(1,s)]] []) (r,d)

neighbors :: (Int,Int) -> [(Int,Int)]
neighbors (i,j) = [(i-1,j),(i+1,j), (i,j-1),(i,j+1)]

bfs :: Grid -> Queue [(Int,Int)] -> (Int,Int) -> Maybe [(Int,Int)]
bfs grid q dest
    | nullQ q = Nothing -- no path from start to finish
    | head (front q) == dest = Just (front q)
    | otherwise = 
            let (p, q') = dequeue q
                cur = head p
                ns = [ n | n <- neighbors cur, not (grid ! n == '*'), not (n `elem` p) ]
                q'' = foldl' enqueue q' $ map (:p) ns
            in bfs grid q'' dest

grid `drawPath` path =
    let g' = grid // zip path (repeat '+')
        ((1,1),(r,c)) = bounds g'
    in unlines [[ g' ! (i,j) | j <- [1..c]] | i <- [1..r]]




{-----------------------------------------------}
{- My own homerolled implementation of a Queue -}
{-----------------------------------------------}
data Queue a = Queue [a] [a] deriving (Show)

emptyQ = Queue [] []

nullQ (Queue [] []) = True
nullQ _ = False

enqueue :: Queue a -> a -> Queue a
enqueue (Queue back front) v = Queue (v:back) front

dequeue :: Queue a -> (a, Queue a)
dequeue (Queue [] []) = error "Tried to dequeue from an empty queue"
dequeue (Queue back (f:rest)) = (f, Queue back rest)
dequeue (Queue back []) = dequeue (Queue [] (reverse back))

front :: Queue a -> a
front q = fst $ dequeue q
