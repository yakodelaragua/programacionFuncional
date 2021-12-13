--  PROGRAMACION FUNCIONAL    BEGO FERRERO   Curso 2021-22    --

{- Enunciado: 
Se trata de encontrar todos los caminos que llevan al saltamontes desde la posición donde está en el jardín hasta el suelo (altura 0), saltando siempre de flor en flor, desde una flor más alta a otra más baja. El saltamontes puede cambiar su orientación en cada salto.
(Se muestra una ejecución).
-}

-- Tipos descritos en el enunciado --------------------

data Orientacion = N|E|S|W deriving (Eq,Enum,Show,Read)
data Saltamonte = Sal Orientacion Posicion deriving Show
type Posicion = (Fila,Columna)
type Flor = (Altura, Posicion)
type Fila = Int
type Columna = Int
type Altura = Int
type Jardin = [Flor]

type Camino = [Saltamonte]

-- El jardin de ejemplo: ----------------------------------

mijardin :: Jardin
mijardin 
  = 
-- TO DO: Escribe el jardín ejemplo del enunciado --

maxFila, maxCol :: Int

maxFila = 4
maxCol = 6


-- Programa principal (interactivo) ----------------------------

main :: IO()
main 
 = do 
   putStrLn "\nAqui se muestra el jardin con las alturas de las flores (el 0 es el suelo)"


   --TO DO: llamar a la función que muestra el jardín del ejemplo. Tened en cuenta que la parte impura (funciones IO) sólo debe tener
   --       contener el código para interactuar con el usuario. El resto deben ser llamadas a funciones puras de Haskell.


   putStr "Dame la orientacion del saltamontes (N, E, S o W): "

   -- TO DO: recoger la respuesta del usuario

   putStr "Dame la posicion del saltamontes, par (fila,columna): "

   -- TO DO: recoger la respuesta del usuario

   putStrLn "Los caminos para llegar al suelo son:"

   -- TO DO: mostrar todos los caminos localizados

leerParInt :: IO (Int,Int)
leerParInt = do 

-- TO DO: recoge el par tecleado por el usuario
 

leerOrientacion :: IO Orientacion
leerOrientacion = do

-- TO DO: función que recoge la orientación del saltamontes tecleada por el usuario

-- Funciones -----------------------------------------------------

mide :: Jardin -> Posicion -> Altura

-- TO DO: altura de la flor que está en la posicion p de j

puedeSaltar :: Jardin -> Posicion -> Posicion -> Bool

-- TO DO puedeSaltar j p1 p2 decide si se puede saltar en j de p1 a p2

en_suelo :: Jardin -> Posicion -> Bool

-- TO DO: en_suelo j pos decide si en la posicion pos del jardín j hay suelo


caminos :: Jardin -> Saltamonte -> [Camino]
-- caminos j salt = lista de caminos al suelo desde salt en el jardín j
caminos j (Sal ori pos) 
  | en_suelo j pos  = [[Sal ori pos]]
  | null lista      = []
  | otherwise       = map ((Sal ori pos) :) lista
         where  
         lista = concat [(caminos j salN) | 
                          oriN <- giros ori,
                          salN <- avanzar j (Sal oriN pos)]

avanzar :: Jardin -> Saltamonte -> [Saltamonte]
-- avanzar j salt = lista de saltamontes en 1 paso desde salt en j
avanzar j (Sal o (f,c)) 
  | o==N && f<maxFila && puedeSaltar j (f,c)(f+1,c) =[Sal o (f+1,c)]
  | o==S && f>1 && puedeSaltar j (f,c)(f-1,c)       =[Sal o (f-1,c)]
  | o==E && c<maxCol && puedeSaltar j (f,c) (f,c+1) =[Sal o (f,c+1)]
  | o==W && c>1 && puedeSaltar j (f,c) (f,c-1)      =[Sal o (f,c-1)]
  | otherwise                                       = []

giros :: Orientacion -> [Orientacion]
-- giros ori = lista con las 4 orientaciones desde ori
giros o = [o,vuelta o, vuelta(vuelta o), vuelta(vuelta(vuelta o))]

vuelta :: Orientacion -> Orientacion  

-- TO DO: vuelta ori es la orientacion 90 grados a la derecha de ori

vuelta W = N
vuelta ori = succ ori

---- impresion ----------------------------------------------------

imprimir:: [Camino] -> IO()
imprimir lisCaminos 
    = if null lisCaminos then putStrLn "No hay solucion"
      else putStrLn (concat (map dibujar lisCaminos))

dibujar :: [Saltamonte] -> String
dibujar [] = []
dibujar [sal] = show sal ++ "\n"
dibujar (sal:resto) = show sal ++ " --> " ++ dibujar resto 

dibujoMijardin :: String
dibujoMijardin = linea ++ "4 | 8 | 8 | 1 | 9 |16 |13 |\n" ++ 
                 linea ++ "3 | 0 | 4 | 4 | 2 |12 | 0 |\n" ++
                 linea ++ "2 | 10|12 | 5 | 0 | 9 |12 |\n" ++
                 linea ++ "1 | 4 | 7 |11 |12 |10 | 3 |\n" ++ 
                 linea ++ lineaColumnas                  
                  where 
                   linea = "   -----------------------\n"
                   lineaColumnas = "    1   2   3   4   5   6 \n"


---------------------------------------------------------------------