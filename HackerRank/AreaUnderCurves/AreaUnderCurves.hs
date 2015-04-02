import Text.Printf (printf)

range :: Int -> Int -> Double -> [Double]
range a b dx =
    let a' = fromIntegral a
        b' = fromIntegral b
    in [a', a'+dx .. b']
    
makeFunction :: [Int] -> [Int] -> (Double -> Double)
makeFunction as bs =
    let as' = map fromIntegral as
        bs' = map fromIntegral bs
        f x = sum [ a * x**b | (a,b) <- zip as' bs' ]
    in f

computeArea :: Double -> [Double] -> Double
computeArea = trapezoidRule

computeVolume dx rs = trapezoidRule dx [ pi*r**2 | r <- rs ]

trapezoidRule dx hs = 0.5 * dx * hs_sum
    where hs_sum = sum $ zipWith (+) (init hs) (tail hs)
    
-- This function should return a list [area, volume].
solve :: Int -> Int -> [Int] -> [Int] -> [Double]
solve l r a b =
    let f = makeFunction a b
        dx = 0.001
        xs = range l r dx
        ys = map f xs
        area   = computeArea dx ys
        volume = computeVolume dx (map abs ys)
    in [area, volume]

--Input/Output.
main :: IO ()
main = getContents >>= mapM_ (printf "%.1f\n"). (\[a, b, [l, r]] -> solve l r a b). map (map read. words). lines
