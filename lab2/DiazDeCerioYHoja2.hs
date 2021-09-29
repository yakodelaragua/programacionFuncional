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
sonpermde1 :: [[a]] -> [[a]]
sonpermde1 [] = []
sonpermde1 xxs = filter (perm x xs) xs