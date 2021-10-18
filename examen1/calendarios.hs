------------------------------------------------------------------
--  PRACTICA: CALENDARIO           --               PF  2019-20

--  Nombre(s):   
------------------------------------------------------------------

-- Llamada principal es:  printCalendario c n 
-- donde c = columnas (3 o 4) y 
--       n = a�o cuyo calendario deseamos imprimir
------------------------------------------------------------------

module Calendarios where
import Data.Char

type Dibujo = [Linea]  -- cada dibujo es una lista de lineas 
type Linea = [Char]    -- cada linea es una lista de caracteres
type Year = Int
type Columna = Int     -- es 3 o 4

-- Para imprimir un dibujo en pantalla:
printDibujo :: Dibujo -> IO()
printDibujo dib = do
                   putStr "\n"
                   (putStr . concat . map (++"\n")) dib

-- Imprime, con un numero de columnas, el calendario de un a�o: 
-- printCalendario :: Columna ->  Year -> IO()
-- printCalendario c a = printDibujo (calendario c a)


-- Dibujo de un calendario (en c columnas) de un a�o dado:
-- calendario :: Columna -> Year -> Dibujo   
-- calendario c  =  bloque c . map dibujomes . meses

---------------------------------------------------
--  Define las siguientes funciones sobre dibujos:
---------------------------------------------------

-- dibEsCorrecto :: Dibujo -> Bool   
-- comprueba que las lineas de un dibujo tienen igual longitud,
-- debe dar un mensaje de error si el dibujo es vac�o ([]).
dibEsCorrecto :: Dibujo -> Bool
dibEsCorrecto [] = True
dibEsCorrecto [xs] = True
dibEsCorrecto (x:xs) = length x == length (head xs) && dibEsCorrecto xs

-- listaDibCorrectos ::[Dibujo] -> Bool 
-- comprueba que los dibujos de la lista dada son correctos y 
-- ademas tienen todos las mismas dimensiones.
listaDibCorrectos ::[Dibujo] -> Bool
listaDibCorrectos [] = True
listaDibCorrectos [xs] = dibEsCorrecto xs
listaDibCorrectos (x:xs) = dibEsCorrecto x && length x == length (head xs) && listaDibCorrectos xs
-- alto :: Dibujo -> Int   
-- Pre: dib es un dibujo correcto.
-- alto dib da la altura de dib.
alto :: Dibujo -> Int 
alto xs = length xs

-- ancho :: Dibujo -> Int
-- Pre: dib es un dibujo correcto.
-- ancho dib da la anchura de dib.
ancho :: Dibujo -> Int
ancho (x:xs) = length x

-- sobre :: Dibujo -> Dibujo -> Dibujo 
-- Precondicion: los dibujos d1 y d2 tienen la misma anchura.
-- sobre d1 d2 pone el dibujo d1 sobre el dibujo d2.
sobre :: Dibujo -> Dibujo -> Dibujo
sobre d1 d2 = d1 ++ d2

-- alLado :: Dibujo -> Dibujo -> Dibujo   
-- Precondicion: los dibujos d1 y d2 tienen la misma altura.
-- alLado d1 d2 da un dibujo con d1 a la izquierda de d2.
alLado :: Dibujo -> Dibujo -> Dibujo
alLado [] [] = []
alLado (d1:d1s) (d2:d2s) = (d1 ++ d2) : alLado d1s d2s

-- apilar :: [Dibujo] -> Dibujo
-- apila s da el dibujo obtenido apilando todos los elementos de s
--         (el primero de s queda en la cima de la pila).
-- Si s no es una lista de dibujos correctos debe dar error.


-- extender :: [Dibujo] -> Dibujo
-- extiende s da el dibujo obtenido al extender todos los elementos --            de s (el primero de s queda el m�s a la izquierda).
-- Si s no es una lista de dibujos correctos debe dar error.


-- dibBlanco :: (Int,Int) -> Dibujo
-- Precondicion: al>0 && an>0.
-- dibBlanco (al,an) devuelve el dibujo de caracteres blancos con 
--                   altura al y anchura an
dibBlanco :: (Int,Int) -> Dibujo
dibBlanco (al, an)
 | al <= 1 = [blancos an]
 | otherwise  = blancos an : dibBlanco (al - 1, an)

-- bloque :: Int -> [Dibujo] -> Dibujo
-- bloque n lisDib es el dibujo formado al agrupar de n en n los
--               dibujos de lisDib, extender cada sublista
--               y luego apilar los resultados.


-- otras funciones auxiliares sobre dibujos que se necesiten:
dividirEnGrupos :: [String]
dividirEnGrupos = [take 25 (crearListaMes 31 5)]  

crearListaMes :: Int -> Int -> String
crearListaMes dm pm = meterEspacios pm ++ crearLista 1 dm

crearLista :: Int -> Int -> String
crearLista p f 
 | p >= f = show f
 |otherwise = show p ++ (if length (show p) == 1 then "  " else " ") ++ crearLista (p+1) f

meterEspacios :: Int -> String 
meterEspacios n = blancos (3 * n)
------------------------------------------------------------------  
-- Define constantes y funciones para calcular y dibujar los meses 
------------------------------------------------------------------

meses ::  Year -> [(String, Year, Int, Int)]
meses y = [(head (drop i nombresmeses), y, iniciomes (i+1) y, diasmes (head (drop i nombresmeses)) y) | i <- [0..length nombresmeses-1]]
-- meses n devuelve una lista de 12 elementos con los datos 
--         relevantes de cada uno de los meses del a�o n: 
--         (nombre_mes, n, primer_d�a_mes, longitud_mes)


dibujomes ::(String, Year, Int, Int) -> Dibujo
dibujomes (nm,a,pd,lm) = [' ' : nm ++ blancos (25 - length nm - 1)] ++ [blancos 25] ++ [" Lu Ma Mi Ju Vi Sa Do    "] ++ [""] ++ [blancos 25] ++ [blancos 25]
-- dibujomes (nm,a,pd,lm) devuelve un dibujo de dimensiones 10x25 
-- formado por el titulo y la tabla del mes de nombre nm y a�o a.
-- Necesita como par�metros: pd=primer dia y lm=longitud del mes.

blancos :: Int-> String
blancos n = [' ' | i <- [1..n]]


ene1 :: Year -> Int
ene1 a = mod (a + div (a-1) 4 - div (a-1) 100 + div (a-1) 400) 7
-- ene1 a devuelve el dia de la semana del 1 de enero del a�o a 
--        siendo 1=lunes, 2=martes, ..., 6=sabado, 0=domingo
 

-- pdias :: Int -> [Int]
-- pdias a  devuelve una lista con 12 dias que son los dias de la
--          semana en que comienza cada mes del a�o a siendo 
--          1=lunes, 2=martes, ..., 6=sabado y 7=domingo
-- Ejemplo: pdias 2019 es [2,5,5,1,3,6,1,4,7,2,5,7]


nombresmeses :: [String]
nombresmeses =
 ["Enero","Febrero","Marzo","Abril","Mayo","Junio","Julio",
  "Agosto","Septiembre","Octubre","Noviembre","Diciembre"]

--Devuelve el numero de dias del mes introducido
diasmes :: String -> Int -> Int
diasmes mes anyo 
 | mes == "Enero" || mes == "Marzo" || mes == "Mayo" || mes == "Julio" || mes == "Agosto" || mes == "Octubre" || mes == "Diciembre" = 31
 | mes == "Febrero" = if esbisiesto anyo then 29 else 28
 | otherwise = 30

esbisiesto :: Int -> Bool 
esbisiesto y = mod y 4 == 0 && (mod y 100 /= 0 || mod y 400 == 0)

--Calcula el dia de la semana del primer dia del mes utilizando la congruencia de Zeller
iniciomes :: Int -> Int -> Int 
iniciomes mes anyo = h
     where m = if mes <= 2 then  mes + 12 else mes
           a = if m > 12 then anyo - 1 else anyo
           x = mod a 100
           y = div a 100
           h = mod (div (13 * (m + 1)) 5 + x + div x 4 + div y 4 - (2 * y)) 7

-- fechas :: Int -> Int -> [Dibujo]
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


-- otras funciones que se necesiten:



--------------------------------------------------------------------
