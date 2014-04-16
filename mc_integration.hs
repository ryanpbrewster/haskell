-- mc_integration.hs
{-
 - Use Metropolis Monte-Carlo to integrate a function
 -     Integrate[ f[x] * rho[x] ]
 - where rho[x] is a normalized sampling function.
 -}

import System.Random.Mersenne
import System.Environment
import Data.List (intercalate)


foo :: Double -> Double
foo x = x ** 4

rho :: Double -> Double
rho x = exp (-x*x) / sqrt pi

main = do
    rng <- newMTGen Nothing
    rands <- randoms rng
    let ans = mcIntegrate foo rho rands
    putStrLn $ prettyPrint ans

prettyPrint (x,dx) = show x ++ " \\pm " ++ show dx

mcIntegrate f p rands =
    let walk = randomWalk p rands
        ci = rollingCI $ map f walk
    in head $ dropWhile (\(_,stdev) -> stdev > 0.01) $ drop 50 ci

linearShift (a,b) (c,d) = map shift
    where shift x = c + (d-c)/(b-a) * (x-a)

randomWalk rho rands = randomWalk' rho rands 0.0
randomWalk' rho (dx:xi:rands) x =
    let x' = x + (dx - 0.5)
        r = (rho x') / (rho x)
    in x : randomWalk' rho rands (if xi < r then x' else x)

rollingAverage xs = zipWith (/) (scanl1 (+) xs) [1..]

rollingCI xs = let x2s = zipWith (*) xs xs
                   xcs  = scanl1 (+) xs
                   x2cs = scanl1 (+) x2s
                   xbars  = zipWith (/) xcs [1..]
                   x2bars = zipWith (/) x2cs [1..]
                   vars   = [ x2bar - xbar**2 | (xbar, x2bar) <- zip xbars x2bars ]
                   stdevs = [ sqrt (var / n) | (var, n) <- zip vars [1..] ]
               in [ (xbar, 5*stdev) | (xbar, stdev) <- zip xbars stdevs ]
