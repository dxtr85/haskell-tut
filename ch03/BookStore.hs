-- file: ch03/BookStore.hs
type CustomerID = Int
type ReviewBody = String
type CardHolder = String
type CardNumber = String
type Address = [String]

data BookInfo = Book Int String [String]
                deriving (Show)
data MagazineInfo = Magazine Int String [String]
                    deriving (Show)

myInfo = Book 995949594 "Algebra of Programming"
         ["Richard Bird", "Oege de Moor"]

data BookReview = BookReview BookInfo CustomerID String
data BetterReview = BetterReview BookInfo CustomerID ReviewBody
type BookRecord = (BookInfo, BookReview)

data BillingInfo = CreditCard CardNumber CardHolder Address
                 | CashOnDelivery
                 | Invoice CustomerID
                 deriving (Show)
bookId      (Book id title authors) = id
bookTitle   (Book id title authors) = title
bookAuthors (Book id title authors) = authors

nicerId      (Book id _ _) = id
nicerTitle   (Book _ title _) = title
nicerAuthors (Book _ _ authors) = authors

data Customer = Customer {
  customerID      :: CustomerID,
  customerName    :: String,
  customerAddress :: Address
  } deriving (Show)

customer1 = Customer 271828 "J.R. Hacker"
            ["255 Syntax Ct",
             "Milpitas, CA 95134",
             "USA"]

customer2 = Customer {
  customerID = 271829,
  customerAddress = ["1048576 Disk Drive",
                     "Milpitas, CA 95134",
                     "USA"],
  customerName = "Jane Q. Citizen"
  }
