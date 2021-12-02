
type Figura = [String]

a :: Figura
a = [['X', 'X', 'X', 'X'],
     ['0', '0', '0', '0']]

b :: Figura
b = [['X', 'X', 'X'],
     ['0', 'X', '0']]

c :: Figura
c = [['X', 'X'],
     ['X', 'X']]


d :: Figura
d = [['1', '3'],
     ['0', '2'], 
     ['4','5']]
-- 1 --
-- n1: numero de filas
-- n2: numero de columnas 
figuraVacia :: (Int,Int) -> Figura
figuraVacia (n1, 0) = []
figuraVacia (n1, n2) = crearFilaVacia n1 : figuraVacia (n1, n2-1)

-- Crea una fila vacia
crearFilaVacia :: Int -> String
crearFilaVacia 0 = []
crearFilaVacia n = '0' : crearFilaVacia (n-1)


-- 2 --
tamanoFigura :: Figura -> (Int,Int)
tamanoFigura fig = (length (head fig), length fig)


-- 3 --
numBlancos :: Figura -> Int
numBlancos = foldr ((+) . numBlancosFila) 0

-- Devuelve el numero de cuadros no vacios que contiene una figura
numBlancosFila :: String -> Int
numBlancosFila [] = 0
numBlancosFila (x:xs) = if x == 'X' then 1 + numBlancosFila xs else numBlancosFila xs


-- 4 --
rotarFigura :: Figura -> Figura 
rotarFigura fig = rotarFiguraAux 0 (fst(tamanoFigura fig)) fig

-- Concatena cada fila de 1 en 1
rotarFiguraAux :: Int -> Int -> Figura -> Figura
rotarFiguraAux n tam l 
 | n == tam = []
 | otherwise = cogerFilaN n l ++ rotarFiguraAux (n+1) tam l

-- Consigue el numero de fila n
cogerFilaN :: Int -> Figura -> Figura
cogerFilaN n [] = []
cogerFilaN n (x:xs)
 | length x <= 1 = [[head x]]
 | otherwise = [conc ([head (drop n x)] : cogerFilaN n xs)]

-- Concatena los elementos para conseguir un string invertido
conc :: Figura -> String
conc l = invertir (foldl1 (++) l)

--Invierte un string
invertir :: String -> String
invertir [] = []
invertir (x:xs) = invertir xs ++ [x] 


-- 5 -- 
--Devuelve True cuando todas las X de una figura estan en la misma posicion
encajan :: Figura -> Figura -> Bool
encajan [] (_:_) = True
encajan (_:_)  [] = True
encajan (f1:f1x) (f2:f2x) = if encajaFila f1 f2 then False else encajan f1x f2x

--True si contiene cuadros no vacios en las misma posicion
encajaFila :: String -> String -> Bool
encajaFila [] [] = True 
encajaFila [] (_:_) = True 
encajaFila (_:_) [] = True
encajaFila (x:xs) (y:ys) 
 | x == 'X' = if y /= 'X' then False else encajaFila xs ys
 | y == 'X' = if x /= 'X' then False else encajaFila xs ys
 | otherwise = encajaFila xs ys


-- 6 --
-- Une dos figuras de la siguiente manera: 
-- zipFiguraWith (++) a b --> ['X', 'X', 'X', 'X','X', 'X', 'X'], ['0', '0', '0', '0','0', 'X', '0']
zipFiguraWith :: (Figura->Figura->Figura) -> Figura -> Figura -> Figura 
zipFiguraWith f fig1 fig2 = zipWith (++) fig1 fig2


-- 7 --
combinar :: Figura -> Figura -> Figura 
combinar fig1 fig2 = zipFiguraWith (++) fig1 fig2
