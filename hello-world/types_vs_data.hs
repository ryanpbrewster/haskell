data Value = Value Int deriving (Show, Eq, Ord)
f :: Value -> Value
f (Value x) = Value (x*x)

type Weight = Int
g :: Weight -> Weight
g w = w*w
