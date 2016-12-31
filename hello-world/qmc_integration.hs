import GSL.Random.Quasi
import GSL.Random.Gen
import Control.Monad
import Control.Exception
import Data.List (mapAccumL, transpose)
import Text.Printf

data Function = Function Int ([Double] -> Double)

main = do
    let n = round 1e4
    let (f,d) = (myf2, 3)
    r <- newRNG mt19937
    q <- newQRNG halton d
    let fn = Function d f
    ys  <- sequence $ take n $  mcUniform r fn
    ys' <- sequence $ take n $ qmcUniform q fn
    let avgs = transpose $ map rollingMean [ys, ys']
    putStr $ unlines [ printf "%18.8f %18.8f" v v' | [v,v'] <- avgs ]

mean :: [Double] -> Double
mean xs = sum xs / (fromIntegral $ length xs)

rollingMean :: [Double] -> [Double]
rollingMean xs = snd $ mapAccumL accum (0,0) xs
    where accum (cs, cl) x =  -- (current sum, current length)
              let (cs', cl') = (cs + x, cl + 1)
              in ((cs', cl'), cs' / cl')

getUniformPseudo :: Int -> RNG -> IO [Double]
getUniformPseudo d r = replicateM d (getUniform r)

getUniformQuasi :: Int -> QRNG -> IO [Double]
getUniformQuasi d q = do
    d' <- getDimension q
    assert (d == d') $ getListSample q

qmcUniform :: QRNG -> Function -> [IO Double]
qmcUniform q (Function d f) = [ fmap f x | x <- repeat $ getUniformQuasi d q ]

mcUniform :: RNG -> Function -> [IO Double]
mcUniform r (Function d f) = [ fmap f x | x <- repeat $ getUniformPseudo d r ]

myf1 :: [Double] -> Double
myf1 [x,y] = x*y

myf2 [x,y,z] = exp (-(x**2 + y**2 + z**2))
