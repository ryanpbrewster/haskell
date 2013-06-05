-- 091.hs
{-
 - The points P (x1, y1) and Q (x2, y2) are plotted at integer co-ordinates and
 - are joined to the origin, O (0,0), to form ΔOPQ.
 -
 -
 - There are exactly fourteen triangles containing a right angle that can be
 - formed when each co-ordinate lies between 0 and 2 inclusive; that is, 0  x1,
 - y1, x2, y2  2.
 -
 -
 - Given that 0 <= x1, y1, x2, y2 <= 50, how many right triangles can be formed?
 -}

{-
 - There are 3n^2 trivial solutions (n^2 for P and Q on the axes, n^2 for P only,
 - and n^2 for Q only). The non-trivial solutions require non-axial points. Suppose
 - we have point P at location (x1,y1). Since the slope to point P is y1/x1,
 - the slope from P to Q has to be -x1/y1. Thus, Q has to be at position
 -     (x2,y2) == (x1 + s*y1, y1 - s*x1)
 - for some scalar s. In order for both x2 and y2 to be integers, we need
 - s to be a rational number such that s*y1 is an integer and s*x1 is an integer.
 - The first value of s that will work is 1/gcd(x1,y1), since that is the smallest
 - rational number where s*y1 and s*x1 are both integers.
 - Any multiple of 1/gcd(x1,y1) will also work.
 -
 - Thus, keep on adding y1/gcd(x1,y1) to x1 and
 -          subtracting x1/gcd(x1,y1) from y1
 - until you run out of space in the box.
 -
 - There are also exactly symmetrical solutions where you have
 -     (x2,y2) == (x1 - s*y1, y1 + s*x1)
 - instead (moving up and left instead of down and right). Just multiply by 2.
 -}

numSolutions x y n = length $ takeWhile inBounds lattice_points
    where g = gcd x y
          x' = x `div` g
          y' = y `div` g
          lattice_points = [ (x+c*y', y-c*x') | c <- [1..]]
          inBounds (p,q) = 0 <= p && p <= n && 0 <= q && q <= n

solveProblem n = 3*n^2 + 2*(sum [numSolutions x y n | x <- [1..n], y <- [1..n]])

main = print $ solveProblem 50
