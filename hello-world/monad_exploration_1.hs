-- monad_exploration.hs
import Control.Monad

{- Minimal definition of a monadic Wrapper -}
data Wrapper a = Wrapper a deriving (Show, Eq, Ord)
instance Monad Wrapper where
    return x        = Wrapper x
    Wrapper x >>= f = f x


{- To add two wrappers, we need to ``lift'' the (+) function -}
main = do
    print $ liftM2 (+) (Wrapper 1) (Wrapper 2)
    print $ foldl1 (liftM2 (+)) [Wrapper i | i <- [1..10]]
