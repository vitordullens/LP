import Data.List
import Data.Char

-- dobra a lista sem map
double :: [Int] -> [Int]
double [] = []
double (a:as) = (2*a) : double as
-- dobra a lista com map - ideia eh a mesma, map faz a msm coisa que o outro
times2 :: Int -> Int
times2 n = 2*n
doubleList :: [Int] -> [Int]
doubleList a = map times2 a

sqrList :: [Int] -> [Int]
sqrList [] = []
sqrList (a:as)= (a*a) : sqrList as

sales :: Int -> Int
sales 0 = 0
sales 1 = 1
sales 2 = 1
sales 3 = 4

total :: (Int->Int)-> Int -> Int
total f 0 = f 0
total f n = total f (n-1) + f n
-- totalSales top
totalSales :: Int -> Int
totalSales n = total sales n
-- totalSales medio
totalSales2 :: Int -> Int
totalSales2 n
                | n == 0 = 1
                | n>0 = sales n + totalSales2(n-1)

-- jeito paia
zeroInRange :: (Int -> Int) -> Int -> Bool
zeroInRange f 0 = (f 0 == 0)
zeroInRange f n = zeroInRange f (n-1) || (f n == 0)


sumList :: [Int] -> Int
sumList l = foldr (+) 0 l

myand :: [Bool] -> Bool
myand xs = foldr (&&) True xs
myor :: [Bool] -> Bool
myor xs = foldr (||) False xs

hackGio :: [Int] -> [Int] -> [Int]
hackGio as bs = [2*x| x <- as++bs, not (x `mod` 2 == 0)]


-- eleva os itens ao quadrado • mapping
square :: Int -> Int
square x = x*x

squareAll :: [Int] -> [Int]
squareAll l = map square l

-- retorna a soma dos quadrados dos itens • folding
sumSqrAll :: [Int] -> Int
sumSqrAll l = foldr (+) 0 (squareAll l)

-- manter na lista todos os itens maiores que zero. • filtering
-- só mandar um filter (>0) [(-10)..10]
