
------------------------------------------------------------------
--  PRACTICA: CALENDARIO           --               PF  2019-20

--  Nombre(s):   Yara
------------------------------------------------------------------

-- Llamada principal es:  printCalendario c n 
-- donde c = columnas (3 o 4) y 
--       n = a�o cuyo calendario deseamos imprimir
------------------------------------------------------------------

module CalendarioN(printCalendario) where


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
printCalendario :: Columna ->  Year -> IO()
printCalendario c a = printDibujo (calendario c a)


-- Dibujo de un calendario (en c columnas) de un a�o dado:
calendario :: Columna -> Year -> Dibujo   
calendario c  =  bloque c . map dibujomes . meses
---------------------------------------------------
--  Define las siguientes funciones sobre dibujos:
---------------------------------------------------
--dibujo1 de prueba
d1 :: Dibujo
d1 = dibujomes ("Enero", 2020, 5, 31)
d2 :: Dibujo
d2 = dibujomes("Febrero", 2020, 2, 28)

dibEsCorrecto :: Dibujo -> Bool   
dibEsCorrecto [] = error "Dibujo vacio"
dibEsCorrecto (x:xs)
  | null xs = True
  | otherwise = length x == length (head xs) && dibEsCorrecto xs
-- comprueba que las lineas de un dibujo tienen igual longitud,
-- debe dar un mensaje de error si el dibujo es vac�o ([]).


listaDibCorrectos ::[Dibujo] -> Bool 
listaDibCorrectos [] = error "Lista vacia"
listaDibCorrectos (x:xs) 
 | null xs = True 
 | otherwise = dibEsCorrecto x && listaDibCorrectos xs
-- comprueba que los dibujos de la lista dada son correctos y 
-- ademas tienen todos las mismas dimensiones.


alto :: Dibujo -> Int   
alto = length
-- Pre: dib es un dibujo correcto.
-- alto dib da la altura de dib.


ancho :: Dibujo -> Int
ancho (x:xs) = length x
-- Pre: dib es un dibujo correcto.
-- ancho dib da la anchura de dib.


sobre :: Dibujo -> Dibujo -> Dibujo 
sobre d1 d2 = d1 ++ dibBlanco(1,25) ++ d2 
-- Precondicion: los dibujos d1 y d2 tienen la misma anchura.
-- sobre d1 d2 pone el dibujo d1 sobre el dibujo d2.


alLado :: Dibujo -> Dibujo -> Dibujo   
alLado [] [] = []
alLado (d1:d1r) (d2:d2r) = (d1 ++ "   " ++ d2) : alLado d1r d2r
-- Precondicion: los dibujos d1 y d2 tienen la misma altura.
-- alLado d1 d2 da un dibujo con d1 a la izquierda de d2.


apilar :: [Dibujo] -> Dibujo
apilar (x:xs) = foldl sobre x xs
-- apila s da el dibujo obtenido apilando todos los elementos de s
--         (el primero de s queda en la cima de la pila).
-- Si s no es una lista de dibujos correctos debe dar error.


extender :: [Dibujo] -> Dibujo
extender (x:xs) = foldl alLado x xs
-- extiende s da el dibujo obtenido al extender todos los elementos --            de s (el primero de s queda el m�s a la izquierda).
-- Si s no es una lista de dibujos correctos debe dar error.


dibBlanco :: (Int,Int) -> Dibujo
dibBlanco (0, an) = []
dibBlanco (al, an) = blancos an : dibBlanco (al-1, an)
 
-- Precondicion: al>0 && an>0.
-- dibBlanco (al,an) devuelve el dibujo de caracteres blancos con 
--                   altura al y anchura an


bloque :: Int -> [Dibujo] -> Dibujo
bloque n [] = []
bloque n lisDib = apilar (extender (take n lisDib) : [bloque n (drop n lisDib)])
-- bloque n lisDib es el dibujo formado al agrupar de n en n los
--               dibujos de lisDib, extender cada sublista
--               y luego apilar los resultados.


-- otras funciones auxiliares sobre dibujos que se necesiten:



------------------------------------------------------------------  
-- Define constantes y funciones para calcular y dibujar los meses 
------------------------------------------------------------------

meses ::  Year -> [(String, Year, Int, Int)]
meses n = [(head (drop i nombresmeses), n, head (drop i (pdias n)), head (drop i (longmeses n))) | i <- [0..11]]
-- meses n devuelve una lista de 12 elementos con los datos 
--         relevantes de cada uno de los meses del a�o n: 
--         (nombre_mes, n, primer_d�a_mes, longitud_mes)

--Crea el dibujo completo de un mes con las dimensiones correspondientes
dibujomes ::(String, Year, Int, Int) -> Dibujo
dibujomes (nm,a,pd,lm) = cabecera nm a ++ ajustardibujo pd lm
-- dibujomes (nm,a,pd,lm) devuelve un dibujo de dimensiones 10x25 
-- formado por el titulo y la tabla del mes de nombre nm y a�o a.
-- Necesita como par�metros: pd=primer dia y lm=longitud del mes.


-- Dibujo cabecera
cabecera :: String -> Int -> Dibujo
cabecera mes a = [" " ++ mes ++ " " ++ show a ++ ajusteblancos, blancos 21, " Lu Ma Mi Ju Vi Sa Do"]
 where ajusteblancos = blancos (25 - length mes - length (show a) - 6)

-- Dibujo fechas completo
ajustardibujo :: Int -> Int -> Dibujo
ajustardibujo primdia longmes = ajustardibujoaux (agrupardibujos primdia longmes)

-- casi lo mismo que extender
-- Funcion recursiva para ajustar el dibujo al formato
ajustardibujoaux :: [Linea] -> Dibujo
ajustardibujoaux [] = []
ajustardibujoaux lista = foldl1 (++) (take 7 lista) : ajustardibujoaux (drop 7 lista)

--Agrupa dibujos de "fechas" en uno
agrupardibujos :: Int -> Int -> [Linea]
agrupardibujos primdia longmes = foldl1 (++) (fechas primdia longmes)  

-- Primer dia mes
ene1 :: Year -> Int
ene1 a = mod (a + div (a-1) 4 - div (a-1) 100 + div (a-1) 400) 7
-- ene1 a devuelve el dia de la semana del 1 de enero del a�o a 
--        siendo 1=lunes, 2=martes, ..., 6=sabado, 0=domingo
 

pdias :: Int -> [Int]
pdias a = [ene1 a, iniciomes 2 a, iniciomes 3 a, iniciomes 4 a, iniciomes 5 a, iniciomes 6 a, iniciomes 7 a, iniciomes 8 a, iniciomes 9 a, iniciomes 10 a, iniciomes 11 a, iniciomes 12 a]
-- pdias a  devuelve una lista con 12 dias que son los dias de la
--          semana en que comienza cada mes del a�o a siendo 
--          1=lunes, 2=martes, ..., 6=sabado y 7=domingo
-- Ejemplo: pdias 2019 es [2,5,5,1,3,6,1,4,7,2,5,7]


nombresmeses :: [String]
nombresmeses =
 ["Enero","Febrero","Marzo","Abril","Mayo","Junio","Julio",
  "Agosto","Septiembre","Octubre","Noviembre","Diciembre"]


-- longitud meses de anyo dado
longmeses :: Int -> [Int]
longmeses a = [31, if esbisiesto a then 29 else 28, 31, 30, 31, 30, 31, 31, 30, 31, 30 ,31]

fechas :: Int -> Int -> [Dibujo]
fechas primdia longmes = fechasaux primdia longmes 1

fechasaux :: Int -> Int -> Int -> [Dibujo]
fechasaux primdia longmes total 
 | total >= 42 = [["   "]]
 | total < primdia || (total - primdia + 1) > longmes = ["   "] : fechasaux primdia longmes (total + 1)
 | otherwise = if (total - primdia + 1) < 10 then ["  " ++ show (total - primdia + 1)] : fechasaux primdia longmes (total + 1) else [" " ++ show (total - primdia + 1)] : fechasaux primdia longmes (total + 1)
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
esbisiesto :: Int -> Bool 
esbisiesto y = mod y 4 == 0 && (mod y 100 /= 0 || mod y 400 == 0)


--Calcula el dia de la semana del primer dia del mes utilizando la congruencia de Zeller
iniciomes :: Int -> Int -> Int 
iniciomes mes anyo = if h == 0 then 7 else h
     where m = if mes <= 2 then  mes + 12 else mes
           a = if m > 12 then anyo - 1 else anyo
           x = mod a 100
           y = div a 100
           h = mod (div (13 * (m + 1)) 5 + x + div x 4 + div y 4 - (2 * y)) 7

-- Devuelve n blancos
blancos :: Int -> String
blancos n = [' ' | i <- [1..n]]

--------------------------------------------------------------------
