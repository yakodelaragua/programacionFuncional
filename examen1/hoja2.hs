quitaUno :: Eq a => a -> [a] -> [a]
quitaUno e [] = []
quitaUno e (x:s)
 | e == x = s
 | otherwise = x : quitaUno e s

quitaRep :: Eq a => [a] -> [a]
quitaRep [] = []
quitaRep (x:s) = x : quitaRep (filter (/= x) s)

dif :: Eq a => [a] -> [a] -> [a]
dif l1 [] = l1
dif [] l1 = []
dif (x:xs) (y:ys) = dif (quitaUno y (x:xs)) ys

perm :: Eq a => [a] -> [a] -> Bool
perm x y = (dif x y == []) && (dif y x == [])

--ATT---------------------------------------------------------------- no entiendo ese perm x con un solo parametro-
sonpermde1 :: Eq a => [[a]] -> [[a]]
sonpermde1 (x:xss) =  x : filter (perm x) xss

aDecimal :: [Int] -> Int
aDecimal = foldl1 (\x y -> x * 10 + y)

aDigitos :: Int -> [Int]
aDigitos n
 | n >= 0 && n < 10 = [n]
 |otherwise =  aDigitos (div n 10) ++ [mod n 10]

--MAL HAZLO------------------------------------------------------------
decimalAbinario :: Int -> Int
decimalAbinario n 
 |n == 0 || n == 1 = n
 |otherwise = (mod n 2) * 10 + decimalAbinario (div n 2)


ordenada :: Ord a => [a] -> Bool
ordenada [] = True 
ordenada [x] = True
ordenada (x:s) = x <= head s && ordenada s

palabras :: String -> [String]
palabras [] = []
palabras x = takeWhile (/=' ') x : palabras siguiente
 where sinEspacios = dropWhile (/=' ') x
       siguiente = dropWhile (==' ') sinEspacios

crearLista :: Int -> Int -> [Int]
crearLista p f 
 | p >= f = [f]
 |otherwise = p : crearLista (p+1) f

devolverLista :: Int -> [(Int, Int)] -> [Int]
devolverLista n [] = []
devolverLista n (x:s) = if n == snd x then fst x : devolverLista n s else devolverLista n s

posiciones :: Int -> [Int] -> [Int]
posiciones x (y:ys) = devolverLista x unido
 where pos = crearLista 0 (length (y:ys) - 1)
       unido = zip pos (y:ys)

menor5 :: Int -> Bool
menor5 n = n < 5 

paraTodo :: (a -> Bool) -> [a] -> Bool 
paraTodo p xs = length (filter p xs) == length xs

----------------------------------------------------no se hacerlo
--permutar :: [a] -> [[a]]
--permutar (x:xs)

blancos :: Int -> String
blancos n =  [' ' | i<-[1..n]]


diagAux :: String -> Int -> String
diagAux [] l =  "\n"
diagAux (x:s) l = x : "\n" ++ blancos (l- length s) ++ diagAux s l

diag :: String -> IO()
diag s = putStr (diagAux s (length s))