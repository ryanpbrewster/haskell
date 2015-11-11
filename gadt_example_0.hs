data Foo a where
  Bar :: Foo a
  Baz :: a -> Foo a -> Foo a

instance Show a => Show (Foo a) where 
  show Bar = ""
  show (Baz f r) = show f ++ show r
