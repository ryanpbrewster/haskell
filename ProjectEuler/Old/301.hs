-- 301.hs
{-
 - Nim is a game played with heaps of stones, where two players take it in
 - turn to remove any number of stones from any heap until no stones remain.
 -
 - We'll consider the three-heap normal-play version of Nim, which works as
 - follows:
 -    At the start of the game there are three heaps of stones.
 -    On his turn the player removes any positive number of stones from any
 -        single heap.
 -    The first player unable to move (because no stones remain) loses.
 -
 - If (n1,n2,n3) indicates a Nim position consisting of heaps of size n1, n2
 - and n3 then there is a simple function X(n1,n2,n3) — that you may look up
 - or attempt to deduce for yourself — that returns:
 -
 -     zero if, with perfect strategy, the player about to move will
 -     eventually lose;
 -
 -     non-zero if, with perfect strategy, the player about to move will
 -     eventually win.
 -
 - For example X(1,2,3) = 0 because, no matter what the current player does,
 - his opponent can respond with a move that leaves two heaps of equal size,
 - at which point every move by the current player can be mirrored by his
 - opponent until no stones remain; so the current player loses. To
 - illustrate: current player moves to (1,2,1) opponent moves to (1,0,1)
 - current player moves to (0,0,1) opponent moves to (0,0,0), and so wins.
 -
 - For how many positive integers n <= 2^30 does X(n,2n,3n) = 0 ?
 -}

{-
 - It is fairly well-known that X(a,b,c) == 0 iff a xor b xor c == 0
 - That is, you can always win if (foldl1 xor [p1,p2,p3,...]) == 0
 -}

import Data.Bits (xor)

solveProblem bound = length $ filter legit [1..bound]

legit :: Integer -> Bool
legit n = foldl1 xor [n,2*n,3*n] == 0

main = print $ solveProblem (2^30)
