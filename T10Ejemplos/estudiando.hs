import Language.Haskell.TH.Syntax (Quasi(qAddDependentFile))
import Data.Char
par::Int->Bool
par = even

{-
function f(x:Int) return Int
 begin
  read y
  if par y then return x else return x + 1
 end
-}

--leer n caracteres WHERE
leer :: Int -> IO String 
leer 0 = return []
leer n = getChar >>= q
 where q c = leer (n-1) >>= f
        where f cs = return (c:cs)

--leer n caracteres DO
leer2 :: Int -> IO String
leer2 0 = return []
leer2 n = do
    c <- getChar
    cs <- leer2 (n-1)
    return (c:cs)

--leer hasta salto de linea WHERE
leerLinea :: IO String 
leerLinea = getChar >>= q
 where 
     q c = if c == '\n'
     then return []
     else leerLinea >>=f
        where f cs = return (c:cs)

--leer hasta salto de linea DO
leerLinea2 :: IO String
leerLinea2 = do
    c <- getChar
    if c == '\n'
        then return []
    else do
        cs <- leerLinea2
        return (c:cs)

--Leer Int
leerEnt :: IO Int 
leerEnt = do e <- getLine 
             return (read e)

--leer lista Int
leerListEnt :: IO [Int] 
leerListEnt = do lin <- getLine 
                 return (read lin)

--leer lista Int y aplicar funcion
mapLisEnt :: Show a => (Int -> a) -> IO()
mapLisEnt f = do lis <- leerListEnt
                 print (map f lis)

--Lee enteros, uno por linea y devuelve lista
leerLisEnt2 :: IO [Int]
leerLisEnt2 = do lis <- getLine 
                 if lis == "" then return []
                 else do resto <- leerLisEnt2
                         return ((read lis):resto)

--Lee enteros por linea e imprime la suma
leeYsuma :: IO ()
leeYsuma = do lisEnt <-leerLisEnt2
              print (sum lisEnt)

--Convierte entrada.txt a mayusculas en salida.txt
prog2 :: IO ()
prog2 = do
 s <- readFile "D:/Documents/Ingenieria informatica/5. Curso/PF/T10Ejemplos/entrada.txt"
 writeFile "D:/Documents/Ingenieria informatica/5. Curso/PF/T10Ejemplos/salida.txt" (map toUpper s)
 putStrLn "salida.txt es entrada.txt en mayúsculas"

--Añade lo de entrada a salida al final del documento
prog3 :: IO ()
prog3 = do 
 s <- readFile "D:/Documents/Ingenieria informatica/5. Curso/PF/T10Ejemplos/entrada.txt"
 appendFile "D:/Documents/Ingenieria informatica/5. Curso/PF/T10Ejemplos/salida.txt" ("\n\n\t\t -- En mayúsculas queda: --\n\n" ++ (map toUpper s))
 putStrLn "añadido a salida.txt"