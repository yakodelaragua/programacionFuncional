-- Algunas de tipo either... miralas despues

-- 4 --
-- data TipoBobo = Uno | Dos | Tres deriving (Show, Eq)
--Algunos que no hay que hacer nada

-- 7 --
type Direccion = (Persona, Dir, Ciudad)
type Nombre = String
type Apellido = String
type Ciudad = String
data Persona = Per Nombre Apellido
data Dir = Calle String Int | Casa String

-- Definir elementos con tipos
dirJon :: Direccion
dirMiren :: Direccion

dirJon = (Per "Jon" "Prieto", Casa "Enea", "Orio")
dirMiren = (Per "Miren" "Artola", Calle "Aldamar" 15, "Donostia")

escribirDireccion :: Direccion -> String
escribirDireccion (Per nom ape, Calle ca num, c) = nom ++ " " ++ ape ++ "\n" ++ "c/ " ++ ca ++ ", " ++ show num ++ "\n" ++ c ++ "\n\n"
escribirDireccion (Per nom ape, Casa ca, c) = nom ++ " " ++ ape ++ "\n" ++ "casa " ++ ca ++ "\n" ++ c ++ "\n\n"

escribir :: [Direccion] -> IO()
escribir [] = putStrLn ""
escribir dirs = putStrLn (concat (map escribirDireccion dirs))

-- 8 --
data Elemento = E String Int deriving Show

-- Definir instancias de un elemento
instance Eq Elemento where
 (==) (E s1 n1) (E s2 n2) = s1 == s2

instance Ord Elemento where
 (<=) (E s1 n1) (E s2 n2) = s1 < s2

-- 
dosMayores :: [Elemento] -> (Elemento, Elemento)
dosMayores [] = error "Lista vacia"
dosMayores [x] = error "Solo hay un elemento"
dosMayores [x, xs] = (x, xs)
dosMayores (x:xs:ys)
 | x < xs = dosMayores (xs:x:ys)
 | x < head ys = dosMayores (head ys:x:tail ys)
 | xs < head ys = dosMayores (x:ys)
 | otherwise = if length (x:xs:ys) > 3 then dosMayores (x:xs:tail ys) else (x, xs)



-- 9 --
data Racional = Int :/ Int
instance Eq Racional where
 (==) (x1:/x2) (y1:/y2) = div x1 x2 == div y1 y2

instance Ord Racional where
 (<=) (x1:/x2) (y1:/y2) = div x1 x2 <= div y1 y2

instance Show Racional where
 show (x1:/x2) 
  | mod x1 x2 == 0 = show (div x1 x2) 
  | otherwise = show (div x1 divi) ++ ":/" ++ show (div x2 divi) 
                 where divi = gcd x1 x2

-- 10 --
data Operador = Mas | Menos | Prod | Div
data ExprArit = N Int | Apli Operador ExprArit ExprArit

ej1 :: ExprArit
ej1 = Apli Mas (N 6) (Apli Prod (N 3) (N 4))
ej2 :: ExprArit
ej2 = Apli Mas (Apli Prod (N 3) (N 2)) (N 12)
ej3 :: ExprArit
ej3 = Apli Prod (Apli Div (N 5) (N 2)) (Apli Menos (N 8) (Apli Mas (N 2) (N 4))) 

evaluar :: ExprArit -> Int 
evaluar (N i) = i
evaluar (Apli Mas i1 i2) = evaluar i1 + evaluar i2
evaluar (Apli Menos i1 i2) = evaluar i1 - evaluar i2
evaluar (Apli Prod i1 i2) = evaluar i1 * evaluar i2
evaluar (Apli Div i1 i2) = div (evaluar i1) (evaluar i2)

instance Eq ExprArit where
    (==) ej1 ej2 = evaluar ej1 == evaluar ej2

instance Show ExprArit where
    show (N i) = show i
    show (Apli Mas i1 i2) = "(" ++ show i1 ++ " + " ++ show i2 ++ ")"
    show (Apli Menos i1 i2) = "(" ++ show i1 ++ " - " ++ show i2 ++ ")"
    show (Apli Prod i1 i2) = "(" ++ show i1 ++ " * " ++ show i2 ++ ")"
    show (Apli Div i1 i2) = "(" ++ show i1 ++ " div " ++ show i2 ++ ")"