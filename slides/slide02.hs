import Data.List
import Data.Char

allEqual :: Int -> Int ->  Int -> Bool
allEqual n m p = (n==m) && (m==p)

all4Equal :: Int -> Int -> Int -> Int -> Bool
all4Equal n m p q = (allEqual n m p) && (allEqual m p q)

howManyEqual :: Int -> Int -> Int -> Int
howManyEqual n m p 
                | (allEqual n m p) == True = 4
                | (n==m) || (m==p) || (n==p) = 2
                | otherwise = 0

sales :: Int -> Int
sales 0 = 1
sales 1 = 1
sales 2 = 1
sales 3 = 4

salesEqualWeek :: Int -> Int -> Int
salesEqualWeek s n
                | (n==0) && (sales 0 == s) = 1
                | (n==0) && (sales 0 /= s) = 0
                | (n>0) && (sales n == s) = 1 + salesEqualWeek s (n-1)
                | (n>0) && (sales n /= s) = salesEqualWeek s (n-1)   
                
totalSales :: Int -> Int
totalSales n
                | n == 0 = 1
                | n>0 = sales n + totalSales(n-1)

avgSales :: Int -> Float
avgSales n = fromIntegral (totalSales n) / fromIntegral (n+1)

makeSpaces :: Int -> String
makeSpaces n 
   | n == 0 = "" 
   | n > 0 = " " ++ makeSpaces (n-1)


pushRight :: Int -> String -> String
pushRight n s = makeSpaces n ++ s 

addPair :: (Int, Int) -> Int
addPair (x,y) = x+y

type Name = String
type Age = Int
type Phone = Int
type Person = (Name, Age, Phone)

name :: Person -> Name
name (n,a,p) = n

age :: Person -> Age
age (n,a,p) = a

phone :: Person -> Phone
phone (n,a,p) = p

sumSquares :: Int -> Int -> Int
sumSquares x y = sq x + sq y
    where sq z = z*z

umaRaiz :: Float -> Float -> Float -> Float
umaRaiz a b c = -b/(2*a)

duasRaiz :: Float -> Float -> Float -> (Float, Float)
duasRaiz a b c = (x-d, x+d)
    where 
        x = -b/(2*a)
        d = sqrt (b^2-4*a*c) / (2*a)

segundoGrau :: Float -> Float -> Float -> String
segundoGrau a b c 
    | b^2 == 4*a*c = show (umaRaiz a b c)
    | b^2 > 4*a*c = show f ++ " " ++ show s
    | otherwise = "sem raizes"
    where (f,s) = duasRaiz a b c