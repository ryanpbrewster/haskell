class Addable a where
    plus  :: a -> a -> a
    minus :: a -> a -> a
    neg   :: a -> a

data Weight = Weight Int deriving (Show)
data Value = Value Int deriving (Show)

instance Addable Weight where
    Weight x `plus`  Weight y = Weight (x+y)
    Weight x `minus` Weight y = Weight (x-y)
    neg (Weight x) = Weight (-x)

instance Addable Value where
    Value x `plus`  Value y = Value (x+y)
    Value x `minus` Value y = Value (x-y)
    neg (Value x) = Value (-x)
