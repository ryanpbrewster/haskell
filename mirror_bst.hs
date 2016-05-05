data Tree a = Nil | Tree a (Tree a) (Tree a)
instance Show a => Show (Tree a) where
  show Nil = "_"
  show (Tree v l r) = "[" ++ show v ++ "," ++ show l ++ "," ++ show r ++ "]"
leaf n = Tree n Nil Nil

mirror Nil = Nil
mirror (Tree a l r) = Tree a (mirror r) (mirror l)
