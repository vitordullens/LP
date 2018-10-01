-- 1) Implemente funções que satisfaçam a cada um dos requisitos abaixo:
-- a) Retorna a diferença entre duas listas. O resultado é uma lista.
diff :: [Int] -> [Int] -> [Int]
diff a b = [x | x <- a, not ( x `elem` b) ]
-- b) Retorna a interseção entre duas listas. O resultado é uma lista.
inter :: [Int] -> [Int] -> [Int]
inter a b = [x | x <- a, x `elem` b]
-- c) Retorna a união entre duas listas (pode haver repetição de elementos). O resultado é uma lista.
uni :: [Int] -> [Int] -> [Int]
uni a b = [x | x<-a++b]
-- d) Retorna a união entre duas listas ( não há repetição de elementos). O resultado é uma lista.
unico :: [Int] -> [Int]
unico [] = []
unico (l:ls)
        | l `elem` ls = unico ls
        | otherwise = l : unico ls

uniNoRep :: [Int] -> [Int] -> [Int]
uniNoRep a b = unico (a++b)
-- e) Retorna o último elemento de uma lista.
ultimo :: [Int] -> Int
ultimo (l:[]) = l
ultimo (l:ls) = ultimo ls
-- f) Retorna o n-ésimo elemento de uma lista.
n_esimo :: [Int] -> Int -> Int
n_esimo (l:ls) 1 = l
n_esimo (l:ls) n = n_esimo ls (n-1)
-- g) Inverte uma lista.
invert :: [Int] -> [Int]
invert [] = []
invert (l:ls) = invert ls ++ [l]
-- h) Ordena uma lista em ordem descrescente, removendo as eventuais repetições de elementos.
qsort ::  [Int] -> [Int]
qsort [] = []
qsort (x:xs) = qsort lesser ++ [x] ++ qsort greater
    where
        lesser  = filter (< x) xs
        greater = filter (> x) xs
-- qsort (x:xs) = (qsort [l | l <- xs, l < x]) ++ [x] ++ (qsort [h | h <- xs, h > x])

-- i) Retorna um booleano indicando se uma lista de inteiros é decrescente ou não. Proponha 3 soluções:
-- b) usando apenas recursão;
decR :: [Int] -> Bool
decR (a:[]) = True
decR [] = True
decR (a:b:abs) 
        | a >= b = decR(b:abs)
        | otherwise = False


-- 2) Com relação ao material de Tipos Algébricos (última aula), estenda o tipo Expr para poder
--    também representar multiplicação. Altere também a definição da função de avaliação eval
data Expr t = Lit t         |
            Add (Expr t) (Expr t)   |
            Sub (Expr t) (Expr t)   |
            Mul (Expr t) (Expr t)   |
            Div (Expr t) (Expr t) 
            
eval :: (Fractional t) => Expr t -> t
eval (Lit n) = n
eval (Add lhs rhs) = (eval lhs) + (eval rhs)
eval (Sub lhs rhs) = (eval lhs) - (eval rhs)
eval (Mul lhs rhs) = (eval lhs) * (eval rhs)
eval (Div lhs rhs) = (eval lhs) / (eval rhs)

-- 3) e 4)
data Tree t = Nil | Node t (Tree t) (Tree t)


foldTree :: (t -> t -> t) -> Tree t -> t -> t
foldTree f Nil def = def 
foldTree f (Node n (lhs) (rhs)) def = f n (f (foldTree f lhs def) (foldTree f rhs def)) 

addTree :: (Num a) => Tree a -> a
addTree Nil = 0
addTree (Node n lhs rhs) = n + addTree lhs + addTree rhs

mapTree :: (a -> b) -> Tree a -> Tree b
mapTree f Nil = Nil
mapTree f (Node a lhs rhs) = (Node (f a) (mapTree f lhs) (mapTree f rhs))

depth :: Tree a -> Int
depth Nil = 0
depth (Node _ lhs rhs) = 1 + max (depth lhs) (depth rhs)

collapse :: Tree t -> [t]
collapse Nil = []
collapse (Node n lhs rhs) = [n] ++ (collapse lhs) ++ (collapse rhs)

-- 5)
addNum :: Int -> (Int -> Int)
addNum n = (+n)

addNumProf :: Int -> (Int -> Int)
addNumProf n = h
    where h m = n + m

addNumLambda :: Int -> (Int -> Int)
addNumLambda n = (\m -> n+m)