-- NOT YET WORKING
-- puzzle.hs
-- Cutting Jigsaw Puzzle
{-
 - The Hedgehog recently remembered one of his favorite childhood activities,
 - --- solving puzzles, and got into it with new vigor. He would sit day in,
 - day out with his friend buried into thousands of tiny pieces of the picture,
 - looking for the required items one by one.
 -
 - Soon the Hedgehog came up with a brilliant idea: instead of buying
 - ready-made puzzles, one can take his own large piece of paper with some
 - picture and cut it into many small rectangular pieces, then mix them and
 - solve the resulting puzzle, trying to piece together the picture. The
 - resulting task is even more challenging than the classic puzzle: now all the
 - fragments have the same rectangular shape, and one can assemble the puzzle
 - only relying on the picture drawn on the pieces.
 -
 - All puzzle pieces turn out to be of the same size X × Y, because the picture
 - is cut first by horizontal cuts with the pitch of X, then with vertical cuts
 - with the pitch of Y. If we denote the initial size of the picture as A × B,
 - then A must be divisible by X and B must be divisible by Y (X and Y are
 - integer numbers).
 -
 - However, not every such cutting of the picture will result in a good puzzle.
 - The Hedgehog finds a puzzle good if no two pieces in it are the same (It is
 - allowed to rotate the pieces when comparing them, but it is forbidden to
 - turn them over).
 -
 - Your task is to count for a given picture the number of good puzzles that
 - you can make from it, and also to find the puzzle with the minimal piece
 - size.
 -}

import qualified Data.List as DL

main = do
    input <- getContents
    processProblem (words input)

processProblem (sb:sa:puzzle) = do
    let a = read sa
    let b = read sb
    let (num_sols, min_sol) = solveProblem a b puzzle
    print num_sols
    let (min_x, min_y) = min_sol
    putStrLn ( (show min_y) ++ " " ++ (show min_x) )

solveProblem a b puzzle =
    let xx = divisors a
        yy = divisors b
        legitPieceSizes = [ (x,y) | x <- xx, y <- yy, legit x y puzzle ]
        num_sols = length legitPieceSizes
        min_sol  = DL.minimumBy cmpAreaThenX legitPieceSizes
    in (num_sols, min_sol)

cmpAreaThenX (x,y) (x',y') | x*y /= x'*y' = compare (x*y) (x'*y')
                           | otherwise    = compare x x'


splitIntoGroups gs [] = []
splitIntoGroups gs xs = let (g,xs') = splitAt gs xs
                        in g:(splitIntoGroups gs xs')

puzzlePieces x y [] = []
puzzlePieces x y puzzle = let (top_rows, rest) = splitAt y puzzle
                              hslices = [ splitIntoGroups x row | row <- top_rows ]
                              pieces = DL.transpose hslices
                          in pieces ++ (puzzlePieces x y rest)

distinctPairs [] = []
distinctPairs (x:xs) = [ (x,y) | y <- xs ] ++ (distinctPairs xs)

rotateLeft piece = reverse $ DL.transpose piece

equivalentPieces p1 p2 = or [ p1 == p2' | p2' <- take 4 $ iterate rotateLeft p2 ]

legit x y puzzle = let pieces = puzzlePieces x y puzzle
                       piece_pairs = distinctPairs pieces
                   in not $ or [ equivalentPieces p1 p2 | (p1,p2) <- piece_pairs ]

divisors n = [ k | k <- [1..n], n `mod` k == 0 ]
