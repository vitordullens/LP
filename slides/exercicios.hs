allEqual :: Int -> Int -> Int -> Bool
allEqual n m p = (n == m) && (m == p)

all4Equal :: Int -> Int -> Int -> Int -> Bool
--all4Equal x y z w = (x==y) && (y==z) && (z==w)
all4Equal x y z w = (allEqual x y z) && (w == x)


howManyEqual :: Int -> Int -> Int -> Int

howManyEqual a b c 
  | allEqual a b c = 3
  | (a == b) || (a==c) || (b==c) = 2
  | otherwise = 0 


f :: Int->Int->Int
f s n 
   | (n==0) && (sales 0 == s) = 1
   | (n==0) && (sales 0 /= s) = 0
   | (n>0) && (sales n == s) = 1 + f s (n-1)
   | (n>0) && (sales n /= s) = f s (n-1)

sales :: Int -> Int
sales 0 = 10
sales 1 = 20
sales 2 = 2
sales 3 = 10

makeSpaces :: Int -> String
makeSpaces n 
   | n == 0 = "" 
   | n > 0 = " " ++ makeSpaces (n-1)


pushRight :: Int -> String -> String
pushRight n s = makeSpaces n ++ s 

totalSales :: Int -> Int
totalSales 0 = sales 0
totalSales n = totalSales (n-1) + sales n

averageSales :: Int -> Float
averageSales n 
    | n >= 0 = fromIntegral(totalSales n) / fromIntegral (n+1)



------------------------------------------------------------------------------


myReverse [] = []
myReverse (a:as) = myReverse as  ++ [a]

myLast (x:[])  = x
myLast (x:xs)  = myLast xs

myLast2 list = head (myReverse list)


type Person = String
type Book = String
type Database = [(Person, Book)]

exampleBase = [("jose","haskell"), ("maria","java"), ("pedro","prolog"), ("jose","pascal"), ("jose","prolog")]

books :: Database -> Person -> [Book]
books [] p = []
books (emprestimo:emprestimos) p 
  | fst emprestimo == p = snd emprestimo : books emprestimos p
  | otherwise = books emprestimos p


borrowers :: Database -> Book ->[Person]
borrowers [] b = []
borrowers ((pessoa,livro):emprestimos) b
  | livro == b = pessoa : borrowers emprestimos b
  | otherwise = borrowers emprestimos b


borrowed :: Database -> Book -> Bool
borrowed db b = borrowers db b /= []


numBorrowed :: Database -> Person -> Int
numBorrowed db p = length (books db p)

makeLoan :: Database -> Person -> Book -> Database
makeLoan db p b = (p,b) : db

makeLoan2 :: Database -> Person -> Book -> Database
makeLoan2 db p b 
   | not (elem (p,b) db) = (p,b) : db
   | otherwise = db



returnLoan :: Database -> Person -> Book -> Database
returnLoan [] p b = []
returnLoan ((p,b):es) person book 
  |(p,b) == (person,book) = returnLoan es person book
  |otherwise = (p,b) : returnLoan es person book


returnLoan2 :: Database -> Person -> Book -> Database
returnLoan2 [] p b = []
returnLoan2 ((p,b):es) person book 
  |(p,b) == (person,book) = es 
  |otherwise = (p,b) : returnLoan2 es person book


member :: [Int] -> Int -> Bool
member list element = [e | e <- list, e == element] /= []


booksCL :: Database -> Person -> [Book]
booksCL db person = [ b | (p,b) <- db, person==p]

borrowersCL :: Database -> Book ->[Person]
borrowersCL db book = [ p | (p,b) <- db, book==b]

borrowedCL :: Database -> Book -> Bool
borrowedCL db book = [p | (p,b) <- db, book==b] /= []

numBorrowedCL :: Database -> Person -> Int
numBorrowedCL db person = length [b | (p,b) <- db, person==p]

returnLoanCL :: Database -> Person -> Book -> Database
returnLoanCL db person book = [ (p,b) | (p,b) <- db, (p,b) /= (person,book)]

quickSort [] = []
quickSort (a:as) = quickSort [e | e <- as, e < a] ++ [a] ++ quickSort [e | e <- as, e >= a]

quickSort2 [] = []
quickSort2 (a:as) = quickSort2 [e | e <- as, e < a] ++ [a] ++ quickSort2 [e | e <- as, e > a]























