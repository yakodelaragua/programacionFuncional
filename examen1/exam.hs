--Usando span separar Lista asi: func 2 [1,2,3,5,2,7,6,2,3] = [[1],[3,5],[7,6],[3]]
func :: (Eq a) => a -> [a] -> [[a]]
func elem [] = [] 
func elem (x:xs) = if contiene elem (x:xs) then prim : func elem (tail sec) else [x:xs]
 where (prim,sec) = span (/=elem) (x:xs)

funcpro :: (Eq a) => a -> [a] -> [[a]]
funcpro elem [] = [] 
funcpro elem (x:xs) 
 | contiene elem (x:xs) && x /= elem = prim : funcpro elem (tail sec) 
 | x == elem = funcpro elem (tail sec)
 | otherwise = [x:xs]
 where (prim,sec) = span (/=elem) (x:xs)

contiene :: Eq a => a -> [a] -> Bool
contiene n [] = False
contiene n (x:xs)
 | x == n = True
 | otherwise = contiene n xs

--Usando listas intensionales
listas :: Integral a => a -> [a]
listas n = [x-1 | x <- [1..n], mod x 2 == 0]

--
anteriorA :: (Int, Int, Int) -> (Int, Int, Int) -> Bool 
anteriorA (d1, m1, a1) (d2, m2, a2) 
 | a1 < a2 = True
 | a1 == a2 && m1 < m2 = True 
 | a1 == a2 && m1 == m2 && d1 < d2 = True
 | otherwise = False

--temperaturaMedia :: [(Double, (Int,Int,Int))] -> (Int, Int, Int) -> (Int, Int, Int) -> Double 
--temperaturaMedia =  