data BookInfo = Book ISBN Title Authors
data ISBN = ISBN' Integer
data Title = Title' String
data Authors = Authors' [String]

instance Show BookInfo where
    show b = showBook b
instance Show Title where
    show t = showTitle t
instance Show ISBN where
    show n = showISBN n
instance Show Authors where
    show as = showAuthors as

showBook (Book isbn title authors) =
    show title ++ " by " ++ show authors ++ " (ISBN #" ++ show isbn ++ ")"
showISBN (ISBN' isbn) = show isbn
showTitle (Title' title) = show title
showAuthors (Authors' authors) = show authors






data BillingInfo = CreditCard CardNumber CardHolder Address
                 | CashOnDelivery
                 | Invoice CustomerID
                 deriving (Show)
type CardNumber = String
type CardHolder = String
type Address = [String]
type CustomerID = String

-- Selective pricing, depending on how the customer is paying
costOfProduct :: BillingInfo -> Integer
costOfProduct (CreditCard _ _ _) = 100
costOfProduct (CashOnDelivery) = 50
costOfProduct (Invoice _) = 200







data Tree a = Node { leftBranch :: Tree a
                   , value :: a
                   , rightBranch :: Tree a
                   }
            | Nil

leaf :: a -> Tree a
leaf v = Node Nil v Nil

treeHeight :: Tree a -> Int
treeHeight Nil = 0
treeHeight (Node lb _ rb) = 1 + max (treeHeight lb) (treeHeight rb)

instance Show a => Show (Tree a) where
    show t = showTree t

showTree Nil = "<Nil>"
showTree (Node Nil v Nil) = show v
showTree (Node lb v rb) = "(" ++ show v ++ "," ++ show lb ++ "," ++ show rb ++ ")"
