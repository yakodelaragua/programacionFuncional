--Arboles definicion
data Arbin a = Hoja a | Unir (Arbin a) (Arbin a)
a1:: Arbin Int 
a2:: Arbin Char
a1 = Unir (Unir (Hoja 5) (Hoja 1)) (Hoja 6)
a2 = Unir (Hoja 'a') (Hoja 'v')

--Profundidad
prof :: Arbin a -> Int 
prof (Hoja x) = 0
prof (Unir ai ad) = 1 + max (prof ai) (prof ad)

--Tamaño arbol
tamaño :: Arbin a -> Int
tamaño (Hoja x) = 1
tamaño (Unir ai ad) = tamaño ai + tamaño ad




--map sobre arbol
--p.e. maparbin (+3) a1 -> No se puede ejecutar sin un show pero modifica todas las hojas sin mas
--p.e. maparbin ord a1
maparbin :: (a->b) -> Arbin a -> Arbin b
maparbin f (Hoja x) = Hoja (f x)
maparbin f (Unir ai ad) = Unir (maparbin f ai)(maparbin f ad)

--fold sobre arbol
foldarbin :: (a->b) -> (b->b->b) -> Arbin a ->b
foldarbin f g (Hoja x) = f x
foldarbin f g (Unir ai ad) = g (foldarbin f g ai) (foldarbin f g ad)

--tamaño y profundidad con fold
tamañof :: Arbin a -> Int
tamañof = foldarbin (const 1) (+)
proff :: Arbin a -> Int
proff = foldarbin (const 0) g where g m n = 1 + max m n

--instancias de arboles: deriving eq, deriving show
--Lo he hecho yo, desastre
--mostrar :: Show a => Arbin a -> String
--mostrar (Hoja x) = show x
--mostrar (Unir ai ad) = mostrar ad ++ " * " ++ mostrar ai

--instance Show a => Show (Arbin a)
 --where show = mostrar


--ARBOLES BINARIOS DE BUSQUEDA  
--Definicion de arboles binarios de busqueda
data Arbus a = Vac | Nod (Arbus a) a (Arbus a)
ar1 :: Arbus Int
ar1 = Nod (Nod Vac 2 Vac) 4 (Nod Vac 6 Vac)

ar2 :: Arbus Int
ar2 = Nod ar1 7 (Nod Vac 9 Vac)

ar3 :: Arbus String
ar3 = Nod (Nod Vac "al" Vac) "f" (Nod Vac "mus" Vac)

--Funcion para aplanar
aplanar :: Arbus a -> [a]
aplanar Vac = []
aplanar (Nod ai r ad) = aplanar ai ++ [r] ++ aplanar ad

--Esta
esta :: Ord a => a -> Arbus a -> Bool
esta x Vac = False
esta x (Nod ai r ad) 
 | x<r = esta x ai
 | x==r = True
 | x>r = esta x ad

meter :: Ord a => a -> Arbus a -> Arbus a
meter x Vac = Nod Vac x Vac
meter x (Nod ai r ad) 
 | x < r = Nod (meter x ai) r ad
 | x == r = Nod ai r ad
 | x > r = Nod ai r (meter x ad) 

borrar :: Ord a => a -> Arbus a -> Arbus a
borrar x Vac = Vac
borrar x (Nod ai r ad) 
 | x<r = Nod (borrar x ai) r ad
 -- | x==r = une ai ad
 | x>r = Nod ai r (borrar x ad) 

--une :: Arbus a -> Arbus a -> Arbus a
--une xt yt = if vacio yt then xt else Nod xt (primeroArbol yt) (restoArbol yt)

instance (Show a) => Show (Arbus a) where
         show Vac = ""
         show t = show' t 0 (maxLong t + 1)

-- Definir 
show' :: (Show a) => Arbus a -> Int -> Int -> String
-- (show' t c a)  muestra el arbol t empezando en la columna c 
-- y usando a caracteres para cada nodo.
show' Vac _ _ = ""
show' (Nod ai r ad) desde_col long_nodo 
     = dibujo_ai ++ "\n" ++ dibujo_raiz ++ dibujo_ad
       where dibujo_raiz = [' '| i<-[1..desde_col]] ++ show r
             dibujo_ai = show' ai (desde_col+long_nodo) long_nodo
             dibujo_ad = show' ad (desde_col+long_nodo) long_nodo

maxLong :: (Show a) => Arbus a -> Int
maxLong Vac = 0
maxLong (Nod ai r ad) = maximum [(length.show) r,maxLong ai,maxLong ad]
