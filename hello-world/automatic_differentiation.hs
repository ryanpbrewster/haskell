data AD = AD { getX :: Double
             , getDX :: Double
             } deriving (Show, Eq, Ord)
instance Num AD where
    (AD x dx) + (AD y dy) = AD (x+y) (dx+dy)
    (AD x dx) - (AD y dy) = AD (x-y) (dx-dy)
    (AD x dx) * (AD y dy) = AD (x*y) (x*dy + y*dx)
    abs (AD x dx) = AD (abs x) (abs dx)
    signum (AD x dx) = AD (signum x) (signum dx)
    fromInteger x = AD (fromInteger x) 0

instance Fractional AD where
    (AD x dx) / (AD y dy) = AD (x/y) (dx/y - x/y^2*dy)
    fromRational x = AD (fromRational x) 0

mySin :: Fractional a => a -> a
mySin x = x - x^3/6 + x^5/120 - x^7/5040

myExp :: Fractional a => a -> a
myExp x = 1 + x + x^2/2 + x^3/6 + x^4/24 + x^5/120 + x^6/720 + x^7/5040

d f x = getDX $ f (AD x 1.0)

--example:
cos = d mySin
