
--1--
constante :: p1 -> p2 -> p1
constante x y = x 

subst :: (t1 -> t2 -> t3) -> (t1 -> t2) -> t1 -> t3
subst f g x = f x (g x)

aplicar :: (t1 -> t2) -> t1 -> t2
aplicar f x = f x

fliparg :: (t1 -> t2 -> t3) -> t2 -> t1 -> t3
fliparg f x y = f y x 

cuadrado :: Num a => a -> a
cuadrado x = x * x

rara :: ((t1 -> t2) -> t1) -> (t1 -> t2) -> t2
rara f g = g ( f g)

--error?
--masRara :: (t1 -> t2) -> t2
--masRara f = f f

--2--
data TipoBobo = Uno | Dos | Tres deriving (Show, Eq)
-- Tres        -> Tres
-- Uno == Uno  -> True
-- Uno == Dos  -> False
-- Uno < Dos   -> Para poder comparar el orden también debería de ser deriving Ord

--3--
data MisEnteros = Mi Int
instance Show MisEnteros where
    show (Mi n)
     | n > 0 = "+" ++ show n
     | n < 0 = "%" ++ show (abs n)
     | otherwise = "<0>"
-- Mi 756      -> +756
-- Mi (-23)    -> %23
-- Mi 0        -> <0>

--4--
data TusEnteros = Tu Int deriving Show
-- Tu 756      -> Tu 756
-- Tu (-23)    -> Tu (-23)
-- Tu 0        -> Tu 0

--5--
type Direccion = (Persona, Dir, Ciudad)
type Nombre = String
type Apellido = String
type Ciudad = String
data Persona = Per Nombre Apellido
data Dir = Calle String Int | Casa String


dirJon :: Direccion
dirJon = (Per "Jon" "Prieto" , Casa "Enea", "Orio")

dirMiren :: Direccion
dirMiren = (Per "Miren" "Artola", Calle "Aldamar" 15, "Donostia")

escribirNom :: Persona -> String
escribirNom (Per nom ape) = nom ++ ape

escribirDireccion :: Direccion -> String
escribirDireccion (Per nom ape, Casa cas, ciu) = nom ++ " " ++ ape ++ "\n" ++ "casa " ++ cas ++ "\n" ++ ciu ++ "\n\n"
escribirDireccion (Per nom ape, Calle cal num, ciu) = nom ++ " " ++ ape ++ "\n" ++ "c/ " ++ cal ++ ", " ++ show num ++ "\n" ++ ciu ++ "\n\n"

escribir :: [Direccion] -> IO()
escribir dir = putStr ((concat . map escribirDireccion) dir)

--6--
data Elemento = E String Int deriving Show
instance Eq Elemento where
 (==) (E str n) (E str1 n1)
    | str == str1 = True
    | otherwise = False

instance Ord Elemento where
 (<=) (E str n) (E str1 n1)
    | str <= str1 = True
    | otherwise = False


dosMayores :: [Elemento] -> (Elemento, Elemento)
dosMayores [] = error "lista vacia"
dosMayores lE = if length lE < 2 then error "Debe de tener al menos dos elementos" else dosMayoresAux lE (head lE) (head (tail lE))


dosMayoresAux :: [Elemento] -> Elemento -> Elemento -> (Elemento, Elemento)
dosMayoresAux [] max1 max2 = (max1, max2)
dosMayoresAux (x:xs) max1 max2 
 | max1 < max2 = dosMayoresAux (x:xs) max2 max1
 | x >= max1 = dosMayoresAux xs x max2
 | x >= max2 = dosMayoresAux xs max1 x
 | otherwise = dosMayoresAux xs max1 max2 

--7--
data Racional = Int :/ Int
instance Eq Racional where
 (a1:/b1) == (a2:/b2) = a1 * b2 == b1 * a2

instance Ord Racional where
 (a1:/b1) <= (a2:/b2) = div a1 b1 <= div a2 b2
 
instance Show Racional where
 show (a:/b) = show (div a b)

--8--
data Operador = Mas | Menos | Prod | Div
data ExprArit = N Int | Apli Operador ExprArit ExprArit

evaluar :: ExprArit -> Int 
evaluar (N n) = n 
evaluar (Apli Mas e1 e2) = evaluar e1 + evaluar e2
evaluar (Apli Menos e1 e2) = evaluar e1 - evaluar e2
evaluar (Apli Prod e1 e2) = evaluar e1 * evaluar e2
evaluar (Apli Div e1 e2) = div (evaluar e1) (evaluar e2)

instance Eq ExprArit where
 e1 == e2 = evaluar e1 == evaluar e2

instance Show ExprArit where
 show (N n) = show n
 show (Apli Mas e1 e2) = "(" ++ show e1 ++ " + " ++ show e2 ++ ")"
 show (Apli Menos e1 e2) = "(" ++ show e1 ++ " - " ++ show e2 ++ ")"
 show (Apli Prod e1 e2) = "(" ++ show e1 ++ " * " ++ show e2 ++ ")"
 show (Apli Div e1 e2) = "(" ++ show e1 ++ " / " ++ show e2 ++ ")"

ej1 = Apli Mas (N 6) (Apli Prod (N 3) (N 4))
ej2 = Apli Mas (Apli Prod (N 3) (N 2)) (N 12)
ej3 = Apli Prod (Apli Div (N 5) (N 2)) (Apli Menos (N 8) (Apli Mas (N 2) (N 4)))