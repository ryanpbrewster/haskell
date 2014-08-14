-- define a typeclass of things that can be "negated"
class Foo a where
    foo :: a -> a

data Weight = Weight Int deriving (Show, Eq, Ord)

instance Foo Weight where
    foo (Weight w) = Weight (-w)
