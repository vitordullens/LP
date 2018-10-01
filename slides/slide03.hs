import Data.List
import Data.Char

sumList :: [Int] -> Int
sumList [] = 0
sumList (a:as) = a + sumList(as)