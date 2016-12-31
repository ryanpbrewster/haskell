-- lagrange_polynomials.hs
{-
 - In numerical analysis, Lagrange polynomials are used for polynomial interpolation. For a given set of distinct points x_j and numbers y_j, the Lagrange polynomial is the polynomial of the least degree that at each point x_j assumes the corresponding value y_j (i.e. the functions coincide at each point). The interpolating polynomial of the least degree is unique, however, and it is therefore more appropriate to speak of "the Lagrange form" of that unique polynomial rather than "the Lagrange interpolation polynomial", since the same polynomial can be arrived at through multiple methods. Although named after Joseph Louis Lagrange, who published it in 1795, it was first discovered in 1779 by Edward Waring and it is also an easy consequence of a formula published in 1783 by Leonhard Euler.[1]
 -
 - Given a set of k + 1 data points
 -
 -     (x_0, y_0),\ldots,(x_j, y_j),\ldots,(x_k, y_k)
 -
 - where no two x_j are the same, the interpolation polynomial in the Lagrange
 - form is a linear combination
 -
 -     L(x) := \sum_{j=0}^{k} y_j \ell_j(x)
 -
 - of Lagrange basis polynomials
 -
 -     \ell_j(x) := \prod_{\begin{smallmatrix}0\le m\le k\\ m\neq
 -     j\end{smallmatrix}} \frac{x-x_m}{x_j-x_m} = \frac{(x-x_0)}{(x_j-x_0)}
 -     \cdots \frac{(x-x_{j-1})}{(x_j-x_{j-1})}
 -     \frac{(x-x_{j+1})}{(x_j-x_{j+1})} \cdots \frac{(x-x_k)}{(x_j-x_k)},
 -
 - where 0\le j\le k. Note how, given the initial assumption that no two x_i
 - are the same, x_j - x_m \neq 0, so this expression is always well-defined.
 - The reason pairs x_i = x_j with y_i\neq y_j are not allowed is that no
 - interpolation function L such that y_i = L(x_i) would exist; a function can
 - only get one value for each argument x_i. On the other hand, if also y_i
 - = y_j, then those two points would actually be one single point.
 -
 - For all j\neq i, \ell_j(x) includes the term (x-x_i) in the numerator, so
 - the whole product will be zero at x=x_i:
 -
 -     \ell_{j\ne i}(x_i) = \prod_{m\neq j} \frac{x_i-x_m}{x_j-x_m}
 -     = \frac{(x_i-x_0)}{(x_j-x_0)} \cdots \frac{(x_i-x_i)}{(x_j-x_i)} \cdots
 -     \frac{(x_i-x_k)}{(x_j-x_k)} = 0.
 -
 - On the other hand,
 -
 -     \ell_i(x_i) := \prod_{m\neq i} \frac{x_i-x_m}{x_i-x_m} = 1
 -
 - In other words, all basis polynomials are zero at x=x_i, except \ell_i(x),
 - for which it holds that \ell_i(x_i)=1, because it lacks the (x-x_i) term.
 -
 - It follows that y_i \ell_i(x_i)=y_i, so at each point x_i,
 - L(x_i)=y_i+0+0+\dots +0=y_i, showing that L interpolates the function
 - exactly.
 -}

interpolatingPolynomial :: [(Double, Double)] -> (Double -> Double)
interpolatingPolynomial pts =
    let (xs,ys) = unzip pts
        l xj x = product [ (x-xm)/(xj-xm) | xm <- xs, xm /= xj ]
        f x = sum [ yj * (l xj) x | (xj,yj) <- pts ]
    in f
