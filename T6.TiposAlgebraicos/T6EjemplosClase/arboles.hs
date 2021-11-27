import Distribution.Simple.Utils (xargs)
-- Arboles binarios: solo tiene valores en las hojas
-- Unir solo es la union de 2 arboles
data Arbin a = Hoja a | Unir (Arbin a) (Arbin a) deriving Show

--Definicion de arbol
a1 :: Arbin Int 
a2 :: Arbin Char 

a1 = Unir (Unir (Hoja 5) (Hoja 1)) (Hoja 6)
a2 = Unir (Hoja 'a') (Hoja 'v')

-- Funciones sencillas, siempre se sigue el mismo patron, primero caso hoja, luego caso Unir
-- Calcular profundidad
prof :: Arbin a -> Int 
prof (Hoja x) = 0
prof (Unir ai ad) = 1 + max (prof ai) (prof ad)

-- Calcular tamano, numero de hojas
tamano :: Arbin a -> Int
tamano (Hoja x) = 1
tamano (Unir ai ad) = tamano ai + tamano ad

-- funcion tipo map
maparbin :: (a -> b) -> Arbin a -> Arbin b
maparbin f (Hoja x) = Hoja (f x)
maparbin f (Unir ai ad) = Unir (maparbin f ai) (maparbin f ad)

-- funcion tipo fold 
foldarbin :: (a -> b) -> (b -> b -> b) -> Arbin a -> b
foldarbin f g (Hoja x) = f x
foldarbin f g (Unir ai ad) = g (foldarbin f g ai) (foldarbin f g ad)

tamanio2 :: Arbin a -> Integer
tamanio2 = foldarbin (const 1) (+)

prof2 :: Arbin a -> Integer
prof2 = foldarbin (const 0) g where g m n = 1 + max m n 