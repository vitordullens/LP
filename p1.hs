revqsort ::  [Int] -> [Int]
revqsort [] = []
revqsort (x:xs) = revqsort greater ++ [x] ++ revqsort lesser
    where
        lesser  = filter (< x) xs
        greater = filter (> x) xs

decrescente :: [Int] -> Bool
decrescente (a:[]) = True
decrescente [] = True
decrescente (a:b:abs) 
        | a >= b = decrescente(b:abs)
        | otherwise = False