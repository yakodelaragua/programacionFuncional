------------------------------------------------------------------
--  PRACTICA: CALENDARIO           --               PF  2019-20

--  Nombre(s): Yara Diaz de Cerio, Iván Garoña   
------------------------------------------------------------------

-- Llamada principal es:  printCalendario c n 
-- donde c = columnas (3 o 4) y 
--       n = a�o cuyo calendario deseamos imprimir
------------------------------------------------------------------

module Calendarios where
import Data.Char

d1 :: Dibujo
d1 = dibujomes ("Enero", 2020, 5, 31)

type Dibujo = [Linea]  -- cada dibujo es una lista de lineas 
type Linea = String    -- cada linea es una lista de caracteres
type Year = Int
type Columna = Int     -- es 3 o 4

-- Para imprimir un dibujo en pantalla:
printDibujo :: Dibujo -> IO()
printDibujo dib = do
                   putStr "\n"
                   (putStr . concat . map (++"\n")) dib

-- Imprime, con un numero de columnas, el calendario de un a�o: 
printCalendario :: Columna ->  Year -> IO()
printCalendario c a = printDibujo (calendario c a)


-- Dibujo de un calendario (en c columnas) de un a�o dado:
calendario :: Columna -> Year -> Dibujo
calendario c  =  bloque c . map dibujomes . meses

---------------------------------------------------
--  Define las siguientes funciones sobre dibujos:
---------------------------------------------------

-- comprueba que las lineas de un dibujo tienen igual longitud,
-- debe dar un mensaje de error si el dibujo es vac�o ([]).
dibEsCorrecto :: Dibujo -> Bool
dibEsCorrecto [] = error "Dibujo vacío"
dibEsCorrecto (x:xs)
 | null xs = True
 | otherwise = length x == length (head xs) && dibEsCorrecto xs


-- comprueba que los dibujos de la lista dada son correctos y 
-- ademas tienen todos las mismas dimensiones.
listaDibCorrectos :: [Dibujo] -> Bool
listaDibCorrectos [] = error "Lista vacía"
listaDibCorrectos (x:xs)
  | null xs = True
  | otherwise = dibEsCorrecto x && listaDibCorrectos xs


-- Pre: dib es un dibujo correcto.
-- alto dib da la altura de dib.
alto :: Dibujo -> Int
alto (x : xs)
  | null xs = 1
  | otherwise = 1 + alto xs


-- Pre: dib es un dibujo correcto.
-- ancho dib da la anchura de dib.
ancho :: Dibujo -> Int
ancho (x:xs) = length x


-- Precondicion: los dibujos d1 y d2 tienen la misma anchura.
-- sobre d1 d2 pone el dibujo d1 sobre el dibujo d2.
sobre :: Dibujo -> Dibujo -> Dibujo
sobre d1 d2 = d1 ++ d2


-- Precondicion: los dibujos d1 y d2 tienen la misma altura.
-- alLado d1 d2 da un dibujo con d1 a la izquierda de d2.
alLado :: Dibujo -> Dibujo -> Dibujo
alLado (d1:ds1) (d2:ds2) = if not(null ds1) then  (d1 ++ d2) : alLado ds1 ds2 else [d1++d2]


-- apila s da el dibujo obtenido apilando todos los elementos de s
--         (el primero de s queda en la cima de la pila).
-- Si s no es una lista de dibujos correctos debe dar error.
apilar :: [Dibujo] -> Dibujo
apilar (x:xs)
  | not(listaDibCorrectos (x:xs)) = error "Agún dibujo de la lista no es correcto"
  | not(iguales (map ancho (x:xs))) = error "Los dibujos no tienen la misma anchura"
  | otherwise = foldl sobre x xs


-- extiende s da el dibujo obtenido al extender todos los elementos 
-- de s (el primero de s queda el m�s a la izquierda).
-- Si s no es una lista de dibujos correctos debe dar error.
extender :: [Dibujo] -> Dibujo
extender (x : xs)
  | not(listaDibCorrectos (x:xs)) = error "Algún dibujo de la lista no es correcto"
  | not (iguales (map alto (x:xs))) = error "Los dibujos no tienen la misma altura"
  | otherwise = foldl alLado x xs


-- Precondicion: al>0 && an>0.
-- dibBlanco (al,an) devuelve el dibujo de caracteres blancos con 
--                   altura al y anchura an
dibBlanco :: (Int,Int) -> Dibujo
dibBlanco (al,an)
  | al > 1 = blancos " " (an-1) : dibBlanco (al-1, an)
  | otherwise = [blancos " " (an-1)]


-- bloque n lisDib es el dibujo formado al agrupar de n en n los
--               dibujos de lisDib, extender cada sublista
--               y luego apilar los resultados.
bloque :: Int -> [Dibujo] -> Dibujo
bloque n (d:ds) = apilar(init(agruparDeNEnN n (d:ds)))


-- otras funciones auxiliares sobre dibujos que se necesiten:

-- Devuelve una lista de dibujos formada por la agrupación de n en n de los dibujos de la lista original
agruparDeNEnN :: Int -> [Dibujo] -> [Dibujo]
agruparDeNEnN n (d:ds)
  | null ds = [d]
  | otherwise = extender(primerosNDibujos n (d:ds)) : agruparDeNEnN n sublista
    where sublista = quitarElems (d:ds) (n-1)

-- Devuelve los primeros n dibujos de una lista de dibujos
primerosNDibujos :: Int -> [Dibujo] -> [Dibujo]
primerosNDibujos n (d:ds)
  | n == 1 = [d]
  | otherwise = d : primerosNDibujos (n-1) ds

-- Dada una lista retira sus primeros n elementos
quitarElems :: [a] -> Int -> [a]
quitarElems (d:ds) n
  | null ds = [d]
  | n == 0 = ds
  | otherwise = quitarElems ds (n-1)

-- Comprueba si todos los elementos de una lista son iguales
iguales :: Eq a =>[a] -> Bool
iguales (x:xs)
  | null xs = True
  | otherwise = x == head xs && iguales xs

------------------------------------------------------------------  
-- Define constantes y funciones para calcular y dibujar los meses 
------------------------------------------------------------------

-- meses n devuelve una lista de 12 elementos con los datos 
--         relevantes de cada uno de los meses del a�o n: 
--         (nombre_mes, n, primer_d�a_mes, longitud_mes)
meses ::  Year -> [(String, Year, Int, Int)]
meses a = mesesrec nombresmeses a (pdias a) (longitudesmeses a)

-- dibujomes (nm,a,pd,lm) devuelve un dibujo de dimensiones 10x25 
-- formado por el titulo y la tabla del mes de nombre nm y a�o a.
-- Necesita como par�metros: pd=primer dia y lm=longitud del mes.
dibujomes ::(String, Year, Int, Int) -> Dibujo
dibujomes (nm,a,pd,lm) =   apilar [[" " ++ nm ++ " " ++ show a ++ blancos " " (24 - (length nm + 6))], dibBlanco (1, 25), [" Lu Ma Mi Ju Vi Sa Do    "],bloque 7(fechas pd lm), dibBlanco(1,25)]


-- ene1 a devuelve el dia de la semana del 1 de enero del a�o a 
--        siendo 1=lunes, 2=martes, ..., 6=sabado, 0=domingo
ene1 :: Year -> Int
ene1 a = mod (a + div (a-1) 4 - div (a-1) 100 + div (a-1) 400) 7


-- pdias a  devuelve una lista con 12 dias que son los dias de la
--          semana en que comienza cada mes del a�o a siendo 
--          1=lunes, 2=martes, ..., 6=sabado y 7=domingo
-- Ejemplo: pdias 2019 es [2,5,5,1,3,6,1,4,7,2,5,7]
pdias :: Year -> [Int]
pdias a = [en, fe, mr, ab, my, jn, jl, ag, se, oc, no, di]
  where
    en = ene1 a
    fe = if en + 3 > 7 then en - 4 else en + 3
    mr = if bisiesto a then if fe + 1 > 7 then fe - 6 else fe + 1 else fe
    ab = if mr + 3 > 7 then mr - 4 else mr + 3
    my = if ab + 2 > 7 then ab - 5 else ab + 2
    jn = if my + 3 > 7 then my - 4 else my + 3
    jl = if jn + 2 > 7 then jn - 5 else jn + 2
    ag = if jl + 3 > 7 then jl - 4 else jl + 3
    se = if ag + 3 > 7 then ag - 4 else ag + 3
    oc = if se + 2 > 7 then se - 5 else se + 2
    no = if oc + 3 > 7 then oc - 4 else oc + 3
    di = if no + 2 > 7 then no - 5 else no + 2


nombresmeses :: [String]
nombresmeses =
 ["Enero","Febrero","Marzo","Abril","Mayo","Junio","Julio",
  "Agosto","Septiembre","Octubre","Noviembre","Diciembre"]

-- fechas pd lm da una lista de 42 dibujos de 1*3 (alguno blanco)
--              con los dias de un mes cuyo primer dia de semana 
--              es pd y cuya longitud de mes es lm
{- Ejemplo:
fechas 3 30
[["   "],["   "],["  1"],["  2"],["  3"],["  4"],["  5"],
 ["  6"],["  7"],["  8"],["  9"],[" 10"],[" 11"],[" 12"],
 [" 13"],[" 14"],[" 15"],[" 16"],[" 17"],[" 18"],[" 19"],
 [" 20"],[" 21"],[" 22"],[" 23"],[" 24"],[" 25"],[" 26"],
 [" 27"],[" 28"],[" 29"],[" 30"],["   "],["   "],["   "],
 ["   "],["   "],["   "],["   "],["   "],["   "],["   "]]
-}
fechas :: Int -> Int -> [Dibujo]
fechas pd lm = fechasrec pd 1 lm 42

-- otras funciones que se necesiten:

-- Dado un año, calcula si este es bisiesto o no
bisiesto :: Int -> Bool
bisiesto a = mod a 4 == 0 || mod a 100 == 0 && mod a 400 == 0

-- Lista con las longitudes de cada mes
longitudesmeses :: Int -> [Int]
longitudesmeses a = [31, f, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
  where f = if bisiesto a then 29 else 28

-- Función recursiva para devolver la información de cada mes
mesesrec :: [String] -> Year -> [Int] -> [Int] -> [(String, Year, Int, Int)]
mesesrec [] a [] [] = []
mesesrec (x:xs) a (y:ys) (z:zs) = (x, a, y, z) : mesesrec xs a ys zs

-- Función recursiva pque devuelve una lista con los días del calendario
fechasrec :: Int -> Int -> Int -> Int -> [Dibujo]
fechasrec pd d lm lc
  | lc == 1 = [["   " ++ blancoB]]
  | pd > 1 || d > lm =  ["   " ++ blancoB] : fechasrec (pd-1) d lm (lc-1)
  | otherwise  = [blancoA ++ show d ++ blancoB] : fechasrec 0 (d+1) lm (lc-1)
    where
      blancoA = if d<10 then "  " else " "
      blancoB = if lc < 42 && mod (lc-1) 7 == 0 then "    " else ""

-- Función recursiva que concatena a un string dado el número de blancos especificado
blancos :: String -> Int -> String
blancos b a
  | a == 1 = " " ++ b
  | otherwise = blancos (" " ++ b) (a-1)

--------------------------------------------------------------------