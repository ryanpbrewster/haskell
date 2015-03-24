-- hill.hs
{-
 - The problem solved in this program is as follows:
 - You are given an array of integers, A
 -     Ex. A = {5,4,8,2,9}
 - Your goal is to modify the elements of A until each element is strictly
 - greater than the previous element
 -
 - You are allowed to modify the elements by increasing or decreasing them by
 - some amount between 0 and X
 -
 - What is the smallest value of X for which this problem is solvable?
 -}

{-
 - Some observations:
 -     If X == 0, the problem is only solvable if A is already in increasing
 -     order. Corrolary: len(A) <= 1 means that X=0 is possible.
 -
 -     If X == infinity, the problem is always solvable
 -
 -     For a given X, it is always desireable to make every element as small as
 -     possible
 -
 - One strategy I think will work is to go through the array element-by-element
 - and keep track of the minimum value of X that is possible SO FAR. By the end,
 - we will have our solution.
 -
 -
 -}


{-
 - Suppose we have a current working value of X, and we're looking at the elements
 -     [..., cur, a, ...]
 - and we're assuming that we have been aggressively minimizing every element
 - as we go
 - 
 - There are a few options:
 -     Our current value of X is fine, meaning that there is some way to get
 -     a > cur by incrementing it. It is sufficient to show that a+X > cur. In
 -     this case, we want to continue aggressively minimizing the element, so
 -     we make a as small as possible while still being larger than cur
 -         a = max (a-X) (cur+1)
 -
 -     Alternately, our value is X is not large enough, meaning that a+X <= cur.
 -     We need to increase X by some amount dX, until
 -         a+X+dX > cur-dX --> 2*dX > cur-(a+X) --> dX > (cur-a-X)/2
 -                                              --> dX = 1 + (cur-a-X) `quot` 2
 -}

minX (a:as) = minX' as a 0

minX'   []   cur x = x
minX' (a:as) cur x
    | a+x > cur = minX' as (max (a-x) (cur+1)) x
    | otherwise = let dx = 1 + (cur - a - x) `quot` 2
                      cur' = cur - dx
                      a' = max (a-x-dx) (cur'+1)
                  in minX' as a' (x+dx)
