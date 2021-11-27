-- ej1 --
data Circulo = Cir Radio deriving (Eq, Ord, Show)
data TrianguloRect = Tri Base Altura deriving (Eq, Ord, Show)
data Cuadrilatero = Cuad Lado | Rect Base Altura deriving (Eq, Ord, Show)

type Radio = Float
type Lado = Float
type Altura = Float
type Base = Float

-- a --
class FigurasPlanas a where
 perimetro :: a -> Float
 area :: a -> Float

-- b --
instance FigurasPlanas Circulo where
 perimetro (Cir r) = 2 * pi * r
 area (Cir r) = pi * (r ^ 2)

instance FigurasPlanas TrianguloRect where
 perimetro (Tri b a) = a + b + c where c = sqrt (a^2 + b^2)
 area (Tri b a) = (b * a) / 2

instance FigurasPlanas Cuadrilatero where
 perimetro (Cuad l) = l * 4
 perimetro (Rect b a) = b * 2 + a * 2
 area (Cuad l) = l * l
 area (Rect b a) = b * a


-- ej2 --
data CatLista a = Nil | Unit a | Conc (CatLista a) (CatLista a)

-- a --
instance (Eq a) => Eq (CatLista a) where
 l1 == l2 = lista l1 == lista l2

lista :: (Eq a) => CatLista a -> [a]
lista Nil = []
lista (Unit x) = [x]
lista (Conc l1 l2) = lista l1 ++ lista l2

-- b --
primero :: (Eq a) => CatLista a -> a
primero Nil = error "Lista vacia"
primero lis = head (lista lis)


-- ej3 --
-- a --
--module Conjunto1 where

data Conj a = Co [a] deriving (Show, Eq)

vacio :: Conj a
vacio = Co []

simple :: a -> Conj a
simple x = Co [x]

miembro :: Eq a => a -> Conj a -> Bool
miembro m (Co []) = False
miembro m (Co (x:xs)) = (x == m) || miembro m (Co xs)

union, inter, dif :: Eq a => Conj a -> Conj a -> Conj a

union (Co l1) (Co l2) = Co (eliminarRepetidos (l1 ++ l2))

inter (Co l1) (Co l2) = Co (interAux l1 l2)

dif (Co l1) (Co l2) = Co (difAux l1 l2)

card :: Conj a -> Int 
card (Co l1) = length l1

subConj :: Eq a => Conj a -> Conj a -> Bool
subConj (Co []) _ = True
subConj (Co (x:xs)) (Co ys) = miembro x (Co ys) && subConj (Co xs) (Co ys)

hacerConj :: [a] -> Conj a
hacerConj = Co

mapConj :: (a -> b) -> Conj a -> Conj b 
mapConj f (Co l) = Co (map f l)

filterConj :: (a -> Bool) -> Conj a -> Conj a
filterConj f (Co l) = Co (filter f l)

foldConj :: (a -> b -> b) -> b -> Conj a -> b 
foldConj f x (Co l) = foldr f x l

-- b --
-- Comentado para evitar errores por la repeticion de los nombres de funciones en a)
-- module Conjunto2 where
-- data Conj a = Co [a] deriving (Show, Eq)

-- vacio :: Conj a
-- vacio = Co []

-- simple :: a -> Conj a
-- simple x = Co [x]

-- miembro :: Eq a => a -> Conj a -> Bool
-- miembro m (Co []) = False
-- miembro m (Co (x:xs)) = (x == m) || miembro m (Co xs)

-- union, inter, dif :: Eq a => Conj a -> Conj a -> Conj a

-- union (Co l1) (Co l2) = Co (quickSort(eliminarRepetidos (l1 ++ l2)))

-- inter (Co l1) (Co l2) = Co (quickSort (interAux l1 l2))

-- dif (Co l1) (Co l2) = Co (quickSort (difAux l1 l2))

-- card :: Conj a -> Int 
-- card (Co l1) = length l1

-- subConj :: Eq a => Conj a -> Conj a -> Bool
-- subConj (Co []) _ = True
-- subConj (Co (x:xs)) (Co ys) = miembro x (Co ys) && subConj (Co xs) (Co ys)

-- hacerConj :: [a] -> Conj a
-- hacerConj x = Co (quickSort x)

-- mapConj :: (a -> b) -> Conj a -> Conj b 
-- mapConj f (Co l) = Co (map f l)

-- filterConj :: (a -> Bool) -> Conj a -> Conj a
-- filterConj f (Co l) = Co (filter f l)

-- foldConj :: (a -> b -> b) -> b -> Conj a -> b 
-- foldConj f x (Co l) = foldr f x l


-- module OpListas where
-- Importaciones del modulo 1
eliminarRepetidos :: (Eq a) => [a] -> [a]
eliminarRepetidos [] = []
eliminarRepetidos (x:xs) = x : eliminarRepetidos (filter (/= x) xs)
 
interAux :: Eq a => [a] -> [a] -> [a]
interAux l1 l2 = [x | x <- l1, miembro x (Co l2)]

difAux :: Eq a => [a] -> [a] -> [a]
difAux l1 l2 = [x | x <- l1, not (miembro x (Co l2))]

-- Importaciones de modulo 2
quickSort::Ord a => [a] -> [a]
quickSort [] = []
quickSort (x:xs) = quickSort menores ++ [x] ++ quickSort mayores 
 where
  menores = [y | y <-xs, y < x]
  mayores = [z | z <-xs, z >= x]




