module Conjunto2
  (Conj,                                
   vacio,                       -- :: Conj a
   simple,                      -- :: a -> Conj a
   miembro,                     -- :: a -> Conj a -> Bool
   union, inter, dif,           -- :: Conj a -> Conj a -> Conj a
     -- dif x y devuelve el conjunto x menos el conjunto y
   card,                        -- :: Conj a -> Int
   subConj,                     -- :: Conj a -> Conj a -> Bool
   -- subConj x y decide si x es subconjunto de y
  hacerConj,                    -- :: [a] -> Conj a
  mapConj,                      -- :: (a->b) -> Conj a -> Conj b
  filterConj,                   -- :: (a ->Bool) -> Conj a -> Conj a
  foldConj                      -- :: (a -> b -> b) -> b -> Conj a -> b
  ) where

   ----------------------------------------------------------------
   -- Modulo importado:
   import OpListas (mezclaOrd, ordenar)
   -- importamos aqui sólo algunas funciones sobre listas
   ----------------------------------------------------------------

   ----------------------------------------------------------------
   --Representacion de datos: listas ordenadas y sin repeticiones
   data Conj a =  Co [a]
   ----------------------------------------------------------------

   ----------------------------------------------------------------
   -- Instancia de Show 
   instance  Show a => Show (Conj a) where
      show (Co s) = mostrar s                     --Redefinición de show

   -- Instancia de Eq
   instance  Eq a  => Eq (Conj a) where 
      (Co s) == (Co t) =  (s == t)

   ----------------------------------------------------------------

   ----------------------------------------------------------------
   -- Implementacion de operaciones no exportadas:
   
   -- La funcion mostrar es auxiliar, no se exporta.
   mostrar :: Show a => [a] -> String
   mostrar  [] = "{}"
   mostrar (x:xs)
    =  "{"++ show  x ++ resto xs
        where
        resto [] = "}"
        resto (y:ys) = ","++ show y ++ resto ys
   ----------------------------------------------------------------
   ----------------------------------------------------------------
   -- Implementacion de las operaciones de la signatura:

   vacio :: Conj a
   vacio = Co []

   simple :: a -> Conj a
   simple x = Co [x]

   miembro :: Eq a => a -> Conj a -> Bool  -- Eq a por el uso de elem
   miembro x (Co lis) = elem x lis

   union :: Ord a => Conj a -> Conj a -> Conj a
   union (Co xs) (Co ys) = Co (mezclaOrd xs ys)

   inter :: Eq a => Conj a -> Conj a -> Conj a
   inter (Co xs) (Co ys) = Co [x | x<-xs, elem x ys]

   dif :: Eq a => Conj a -> Conj a -> Conj a
   dif (Co xs) (Co ys) = Co [x | x<-xs, not (elem x ys)]

   card :: Conj a -> Int
   card (Co xs) = length xs

   subConj :: Eq a => Conj a -> Conj a -> Bool
   -- subConj x y indica que x es subConjunto de y
   subConj (Co xs) (Co ys) = and [elem x ys | x<-xs]

   hacerConj :: Ord a => [a] -> Conj a
   hacerConj xs = Co (ordenar xs)

   mapConj :: Ord b => (a->b) -> Conj a -> Conj b
   mapConj f (Co xs) = hacerConj (map f xs)

   filterConj :: (a->Bool) -> Conj a -> Conj a
   filterConj p (Co xs) = Co (filter p xs)

   foldConj :: (a -> b -> b) -> b -> Conj a -> b
   foldConj f e (Co xs) = foldr f e xs

   ----------------------------------------------------------------


