-- Autores: Yara Diaz de Cerio e Iván Garoña


--  PROGRAMACION FUNCIONAL       Curso 2021-22    --


{- Enunciado: 
Se trata de encontrar todos los caminos que llevan al saltamontes desde la posici�n donde est� en el jard�n hasta el suelo (altura 0), saltando siempre de flor en flor, desde una flor m�s alta a otra m�s baja. El saltamontes puede cambiar su orientaci�n en cada salto.
(Se muestra una ejecuci�n).
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
mijardin = 
    [(8, (4,1)), (8, (4,2)), (1, (4,3)), (9, (4,4)), (16, (4,5)), (13, (4,6)),
    (0, (3,1)), (4, (3,2)), (4, (3,3)), (2, (3,4)), (12, (3,5)), (0, (3,6)),
    (10, (2,1)), (12, (2,2)), (5, (2,3)), (0, (2,4)), (9, (2,5)), (12, (2,6)),
    (4, (1,1)), (7, (1,2)), (11, (1,3)), (12, (1,4)), (10, (1,5)), (3, (1,6))]


maxFila, maxCol :: Int

maxFila = 4
maxCol = 6


-- Programa principal (interactivo) ----------------------------

main :: IO()
main 
 = do 
   putStrLn "\nAqui se muestra el jardin con las alturas de las flores (el 0 es el suelo)"
   putStrLn dibujoMijardin 

   putStr "Dame la orientacion del saltamontes (N, E, S o W): " 
   ori <- leerOrientacion

   putStr "Dame la posicion del saltamontes, par (fila,columna): "
   pos <- leerParInt

   putStrLn "Los caminos para llegar al suelo son:"
   imprimir (caminos mijardin (Sal ori pos))

leerParInt :: IO (Int,Int)
leerParInt = do 
    n <- getLine 
    if comprobarRango (read n) then return (read n) else putStr "Introduce una tupla válida: " >> leerParInt
    
comprobarRango :: (Int,Int) -> Bool
comprobarRango t = (0,0) < t && t < (maxFila, maxCol)

leerOrientacion :: IO Orientacion
leerOrientacion = do
     o <- getLine 
     if comprobarOrientacion o then return (read o) else putStr "Introduce una orientación válida: " >> leerOrientacion

comprobarOrientacion :: String -> Bool
comprobarOrientacion s = s == "N" || s == "E" || s == "S" || s == "W"
    

-- Funciones -----------------------------------------------------

mide :: Jardin -> Posicion -> Altura
mide [] _ = error "No se encuentra la posicion"
mide (j:js) p = if snd j == p then fst j else mide js p


--puedeSaltar j p1 p2 decide si se puede saltar en j de p1 a p2
puedeSaltar :: Jardin -> Posicion -> Posicion -> Bool
puedeSaltar j posOrigen posDestino = mide j posDestino < mide j posOrigen

--en_suelo j pos decide si en la posicion pos del jard�n j hay suelo
en_suelo :: Jardin -> Posicion -> Bool
en_suelo j pos = mide j pos == 0

caminos :: Jardin -> Saltamonte -> [Camino]
-- caminos j salt = lista de caminos al suelo desde salt en el jard�n j
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
dibujoMijardin = linea ++ "4 | 8 | 8 | 1 | 9 | 16| 13|\n" ++ 
                 linea ++ "3 | 0 | 4 | 4 | 2 | 12| 0 |\n" ++
                 linea ++ "2 | 10| 12| 5 | 0 | 9 | 12|\n" ++
                 linea ++ "1 | 4 | 7 | 11| 12| 10| 3 |\n" ++ 
                 linea ++ lineaColumnas                  
                  where 
                   linea = "   -----------------------\n"
                   lineaColumnas = "    1   2   3   4   5   6 \n"


---------------------------------------------------------------------

