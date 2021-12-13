-----------------------------------------------------------------------NO ENTIENDO
-- 1 -- TRATAMIENTO DE ARBOLES
data Arbus a = Vac | Nod (Arbus a) a (Arbus a)

foldArbus :: (a -> b -> a -> a) -> a -> Arbus b -> a
foldArbus f e Vac = e
foldArbus f e (Nod ai r ad) = f (foldArbus f e ai) r (foldArbus f e ad)

numVerif :: (a -> Bool) -> Arbus a -> Int 
numVerif p ar = foldArbus f 0 ar where f ar1 r ar2 = if p r then 1 + ar1 + ar2 else ar1 + ar2

type ArPares a = Arbus (a, a)
numParesCorr :: Ord a => ArPares a -> Int 
numParesCorr ar = numVerif esCorr ar 
                   where esCorr (x, y) = x < y

-- 2 --
--  TIPOS ALGEBRAICOS
data Persona = Per Nombre Apellido
type Nombre = String 
type Apellido = String 

instance Eq Persona where
    (==) (Per n1 a1) (Per n2 a2) = a1 == a2 && n1 == n2

instance Ord Persona where
    (<=) (Per n1 a1) (Per n2 a2) = a1 < a2 || a1 == a2 && n1 < n2

instance Show Persona where
    show (Per n a) = a ++ ", " ++ n

miLista :: [(Persona,Float)]
miLista = [(Per "Ana" "Navarro" , 8),  
           (Per "Marisa" "Navarro", 8.5), 
           (Per "Jon" "Aguirre" ,5.5), 
           (Per "Jon" "Aguirre" ,9.5),
           (Per "Ana" "Navarro"  , 10),
           (Per "Maite" "Urreta", 8)]

ordenarConMedia :: [(Persona, Float)] -> IO()
ordenarConMedia [] = putStr ""
ordenarConMedia l = putStrLn (convertirString (quicksort (hacerMedia l)))

hacerMedia :: [(Persona, Float)] -> [(Persona, Float)]
hacerMedia [] = []
hacerMedia ((p, f):r) = (p, media) : hacerMedia notasEliminadas
       where 
       media = (f + sum lista) / longLista
       lista = [nota | (persona, nota) <- r, p == persona]
       longLista = longitud lista + 1
       notasEliminadas = [pl | pl <- r, fst pl /= p ]


longitud :: Num a => [a] -> Float 
longitud [] = 0
longitud (x:xs) = 1 + longitud xs 

convertirString :: [(Persona, Float)] -> String 
convertirString [] = ""
convertirString ((p, f):s) = show p ++ ", " ++ show f ++ "\n" ++ convertirString s

quicksort :: [(Persona, Float)] -> [(Persona, Float)]
quicksort [] = []
quicksort ((p,f):xs) = menores ++ [(p,f)] ++ mayores
                    where 
                     menores = quicksort [y | y <- xs, fst y <= p]
                     mayores = quicksort [y | y <- xs, fst y > p]
        
--quicksort :: (Ord a) => [a] -> [a]
--quicksort (x:xs) = quicksort [y | y <- xs, y <= x] ++ [x] ++ quicksort [y | y <- xs, y > x]