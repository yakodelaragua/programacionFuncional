data Arbus a = Vac | Nod (Arbus a) a (Arbus a)

foldArbus :: (a -> b -> a -> a) -> a -> Arbus b -> a
foldArbus f e Vac = e
foldArbus f e (Nod ai r ad) = f (foldArbus f e ai) r (foldArbus f e ad)

-- NO ENTIENDO
numVerif :: (a -> Bool) -> Arbus a -> Int
numVerif p = foldArbus ar 0
              where ar ai r ad = if p r then
                                   1 + ai + ad
                                 else
                                   ai + ad

-- 2 --
data ArGen a = N a [ArGen a]

ar1 :: ArGen Int
ar1 = N 25 [N 35 [], N 45 [], N 55 []]

ar2 :: ArGen Int
ar2 = N 20 [N 12 [], ar1, N 36 [N 52 []]]

preorden :: ArGen a -> [a]
preorden (N n []) = [n]
preorden (N n sub) = n : concat (map preorden sub)

postorden :: ArGen a -> [a]
postorden (N n []) = [n]
postorden (N n sub) = concat (map preorden sub) ++ [n]

esta :: Eq a => ArGen a -> a -> Bool
esta ar e = e `elem` preorden ar

-- 3 --
data Arbol a b = Hoja a | Nodo (Arbol a b) b (Arbol a b)

-- a --
type ExpArit = Arbol Integer String 

-- b --
exp1 :: Arbol Integer String
exp1 = Nodo (Nodo (Hoja 9) "-" (Nodo (Hoja 10) "+" (Hoja 6))) "+" (Nodo (Hoja 3) "*" (Hoja 5))

-- c --
instance (Show a, Show b) => Show (Arbol a b) where
 show = mostrar

mostrar :: (Show a, Show b) => Arbol a b -> String
mostrar a = ver2 a 0 

ver :: (Show a, Show b) => Arbol a b -> Int -> String
ver (Hoja x) n = blancos n ++ show x ++ "\n"
ver (Nodo ai r ad) n = ver ad (n + 5) ++ blancos n ++ show r ++ "\n" ++ ver ai (n + 5)

ver2 :: (Show a, Show b) => Arbol a b -> Int -> String
ver2 (Hoja x) n = show n ++ "blancos" ++ show x ++ "/n"
ver2 (Nodo ai r ad) n = ver2 ad (n + 5) ++ show n ++ "blancos" ++ show r ++ "/n" ++ ver2 ai (n + 5)

blancos :: Int -> String
blancos n = [' '| i <- [1..n]]