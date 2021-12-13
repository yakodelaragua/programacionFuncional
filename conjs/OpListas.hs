   
   module OpListas where

-- Operaciones auxiliares (sobre listas) 
-- que se pueden usar en los modulos Conjunto

   quitarRep :: Eq a => [a] -> [a]
   quitarRep [] = []
   quitarRep (x:xs) = x: filter (/=x) (quitarRep xs)

   permutacion :: Eq a => [a] -> [a] -> Bool
   --Precondicion: listas de datos sin repetidos dentro de ellas
   permutacion xs ys 
         = diferencia xs ys == [] && length xs == length ys

   diferencia ::  Eq a => [a] -> [a] -> [a]
   --Precondicion: listas de datos sin repetidos dentro de ellas
   diferencia xs ys = [x |x<-xs, not (elem x ys)]

   mezclaOrd :: Ord a => [a] -> [a] -> [a]
   -- Precondicion: Las listas datos estan ordenadas y sin repetidos
   -- Obtiene la mezcla ordenada y sin repetidos
   mezclaOrd [] t = t
   mezclaOrd s [] = s
   mezclaOrd (x:s) (y:t) 
           | x<y        = x: mezclaOrd s (y:t)
           | x==y       = x: mezclaOrd s t
           | otherwise  = y: mezclaOrd (x:s) t

   ordenar :: Ord a => [a] -> [a]     -- metodo quicksort
   -- Ordena una lista de menor a mayor y sin repetidos
   ordenar [] = []
   ordenar (x:s) 
        = ordenar [y | y<-s, y<x] ++ [x] ++ ordenar [y | y<-s, y>x] 

   --------------------------------------------------------------