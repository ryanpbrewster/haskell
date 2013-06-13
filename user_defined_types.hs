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
