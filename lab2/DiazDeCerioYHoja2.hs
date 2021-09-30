--1--
quitaUno :: Eq t => t -> [t] -> [t]
quitaUno n [] = []
quitaUno n (x:s)
 |n == x = s
 |otherwise = x:quitaUno n s

--2--
quitarRep :: Eq a => [a] -> [a]
quitarRep [] = []
quitarRep (x:s) = x:quitarRep (filter (/=x) s)

--3--
dif:: (Eq a) => [a] -> [a] -> [a]
dif xs [] = xs
dif xs (y:ys) = dif (quitaUno y xs) ys

--4--
perm :: Eq a => [a] -> [a] -> Bool
perm xs ys = (dif xs ys == []) && (dif ys xs == [])

--5--
sonpermde1 :: Eq a => [[a]] -> [[a]]
sonpermde1 [] = []
sonpermde1 (x:xs) = filter (perm x) xs

--6--
aDecimal :: [Int] -> Int
aDecimal = foldl (\x y -> 10 * x + y) 0

aDigitos :: Int -> [Int]
aDigitos n
 | 0 <= n && n < 10 = [n]
 | otherwise =  aDigitos (div n 10) ++ [mod n 10]

--7--
decimalAbinario:: Int -> Int
decimalAbinario = aDecimal . binario

binario:: Int -> [Int]
binario n
 | n < 0 = error "Es negativo"
 | 0 <= n && n <= 1 = [n]
 | otherwise = binario (div n 2) ++ [mod n 2]

binarioAdecimal:: Int -> Int
binarioAdecimal = foldl1 (\x y -> x * 2 + y) . aDigitos

--8--
ordenada :: (Ord a) => [a] -> Bool
ordenada [] = True
ordenada [x] = True
ordenada (x:xs) = x <= head xs && ordenada xs

--9--
