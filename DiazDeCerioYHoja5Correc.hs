--1----------------------------------------------
data Arbus a = Vac | Nod (Arbus a) a (Arbus a)  deriving (Eq, Ord, Show)

--a--
foldArbus :: (a -> b -> a -> a) -> a -> Arbus b -> a
foldArbus f e Vac = e
foldArbus f e (Nod ai r ad) = f (foldArbus f e ai) r (foldArbus f e ad)

--b--
numVerif :: (a -> Bool) -> Arbus a -> Int
numVerif p = foldArbus ar 0
             where ar izq x der = if p x then 1 + izq + der else izq + der

--c--
type ArPares a = Arbus (a,a)
numParesCorr :: Ord a => ArPares a -> Int 
numParesCorr = numVerif esCorrecto where esCorrecto (x,y) = x < y

--Ejemplo para probar
miarbol1 :: ArPares Int 
miarbol1 = Nod (Nod Vac (3,8) Vac)(7,7)(Nod(Nod Vac (8,4) Vac)(9,10) Vac)

--2----------------------------------------------
data ArGen a = N a [ArGen a] deriving Show

--a--
ar1 :: ArGen Int
ar1 = N 25 [N 35 [], N 45 [], N 55 []]

ar2 :: ArGen Int 
ar2 = N 20 [N 12 [], ar1, N 36 [N 52 []]]

--b--
preorden :: ArGen a -> [a]
preorden (N x []) = [x]
preorden (N x a) = x : (concat . map preorden) a

postorden :: ArGen a -> [a]
postorden (N x []) = [x]
postorden (N x a) = ((concat . map preorden) a) ++ [x]

--su correcion en fotos 12/11/2021

--c--
esta :: Eq a => ArGen a -> a -> Bool
esta ar elem = length (filter (==elem)(preorden ar)) > 0


--3----------------------------------------------
data Arbol a b = Hoja a | Nodo (Arbol a b) b (Arbol a b)

--a--
type ExpA = Arbol Integer String

--b--
exp1 :: ExpA
exp1 = Nodo (Nodo (Hoja 9) "-" (Nodo (Hoja 10) "+" (Hoja 6))) "+" (Nodo (Hoja 3) "*" (Hoja 5))

--c--
instance Show a => Show (Arbol a b) 
 where show = mostrar

mostrar :: Show a => Arbol a b -> String
mostrar exp1 = "ab"
--foton 12/11/2021