--arboles de prueba
ar11 :: Arbus Int
ar11 = Nod (Nod Vac 2 Vac) 4 (Nod Vac 6 Vac)

ar12 :: Arbus Int
ar12 = Nod ar11 7 (Nod Vac 9 Vac)

--1--
data Arbus a = Vac | Nod (Arbus a) a (Arbus a)  deriving (Eq, Ord, Show)

--a--
foldArbus :: (t1 -> t2 -> t1 -> t1) -> t1 -> Arbus t2 -> t1
foldArbus f e Vac = e
foldArbus f e (Nod ai r ad) = f (foldArbus f e ai) r (foldArbus f e ad)

--b--
aplanar :: Arbus a -> [a]
aplanar Vac = []
aplanar (Nod ai r ad) = aplanar ai ++ [r] ++ aplanar ad


--c--
type ArPares a = Arbus (a,a)


--2--
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

--c--
esta :: Eq a => ArGen a -> a -> Bool
esta ar elem = length (filter (==elem)(preorden ar)) > 0


--3--
data Arbol a b = Hoja a | Nodo (Arbol a b) b (Arbol a b) deriving Show 

--a--
type ExpA = Arbol Integer String

--b--
exp1 :: ExpA
exp1 = Nodo (Nodo (Hoja 9) "-" (Nodo (Hoja 10) "+" (Hoja 6))) "+" (Nodo (Hoja 3) "*" (Hoja 5))

expPrueba :: ExpA
expPrueba = Nodo (Hoja 3) "*" (Hoja 5)

expH :: ExpA
expH = Hoja 2

--c--
--instance Show a => Show (Arbol a) 
 --where show = mostrar

--mostrar :: Show a => Arbol a -> String
