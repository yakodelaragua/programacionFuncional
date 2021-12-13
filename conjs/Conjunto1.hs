module Conjunto1
 (Conj,                                
   vacio,                    -- :: Conj a
   simple,                   -- :: a -> Conj a
   miembro,                  -- :: a -> Conj a -> Bool 
   union, inter, dif,        -- :: Conj a -> Conj a -> Conj a
   -- dif x y devuelve el conjunto x menos el conjunto y
   card,                     -- :: Conj a -> Int
   subConj,                  -- :: Conj a -> Conj a -> Bool
   -- subConj x y decide si x es subconjunto de y
   hacerConj,                -- :: [a] -> Conj a
   mapConj,                  -- :: (a->b) -> Conj a -> Conj b
   filterConj,               -- :: (a ->Bool) -> Conj a -> Conj a
   foldConj,                 -- :: (a->b->b) -> b -> Conj a -> b
   ) where
   
   ------------------------------------------------------------
   -- Modulo importado:
   import OpListas (permutacion, quitarRep)
   -- importamos aqui sólo algunas funciones sobre listas
   ------------------------------------------------------------

   ------------------------------------------------------------
   --Representacion de datos: listas cualesquiera
   data Conj a = Co [a]
   ------------------------------------------------------------

   ------------------------------------------------------------
   -- Instancia de Show 
   instance (Show a, Eq a) => Show (Conj a) where   --Redefinición de show
      show = mostrarConj

   -- Instancia de Eq
   instance  Eq a  => Eq (Conj a) where
      (==) = igual                                   --Redefinición de Eq
   ------------------------------------------------------------

   ------------------------------------------------------------
   -- Implementacion de operaciones no exportadas:

   -- La funcion " mostrarConj" no se exporta
   -- (se usara la funcion show de la clase Show)

   mostrarConj  :: (Show a, Eq a) => Conj a -> String
   mostrarConj (Co xs) = mostrar (quitarRep xs)
   
   -- La funcion mostrar es auxiliar, no se exporta
   mostrar :: Show a => [a] -> String
   mostrar  [] = "{}"
   mostrar (x:xs)
    =  "{"++ show  x ++ resto xs
        where
         resto [] = "}"
         resto (y:ys) = ","++ show y ++ resto ys

   -- La funcion "igual" no se exporta
   -- (se usara el operador == de la clase Eq)

   igual :: Eq a => Conj a -> Conj a -> Bool
   igual (Co xs) (Co ys) = permutacion zs us
                           where
                           zs = quitarRep xs
                           us = quitarRep ys
   -----------------------------------------------------

   -----------------------------------------------------
   -- Implementacion de las operaciones de la signatura:

   vacio :: Conj a
   vacio = Co []

   simple :: a -> Conj a
   simple x = Co [x]

   miembro :: Eq a => a -> Conj a -> Bool    -- Eq a por el uso de elem
   miembro x (Co lis) = elem x lis

   union :: Conj a -> Conj a -> Conj a
   union (Co xs) (Co ys) = Co (xs ++ ys)

   inter :: Eq a => Conj a -> Conj a -> Conj a    
   inter (Co xs) (Co ys) = Co [x | x<-xs, elem x ys]

   dif :: Eq a => Conj a -> Conj a -> Conj a
   dif (Co xs) (Co ys) = Co [x | x<-xs, not (elem x ys)]

   card :: Eq a => Conj a -> Int              -- Eq a por el uso de quitarRep
   card (Co xs) = length (quitarRep xs)

   subConj :: Eq a => Conj a -> Conj a -> Bool
   -- subConj x y indica que x es subConjunto de y
   subConj (Co xs) (Co ys) = and [elem x ys | x<-xs]

   hacerConj :: [a] -> Conj a
   hacerConj xs = Co xs

   mapConj :: (a->b) -> Conj a -> Conj b
   mapConj f (Co xs) = Co (map f xs)

   filterConj :: (a->Bool) -> Conj a -> Conj a
   filterConj p (Co xs) = Co (filter p xs)

   foldConj :: Eq a => (a->b->b) -> b -> Conj a -> b
   foldConj f e (Co xs) = foldr f e (quitarRep xs)

   ----------------------------------------------------------------


