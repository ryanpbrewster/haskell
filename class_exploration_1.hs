class Plus a where
    (#) :: a -> a -> a
    (%) :: a -> a -> a

data W = W Int deriving (Show)

instance Plus W where
    W x # W y = W (x+y)
    W x % W y = W (x-y)
