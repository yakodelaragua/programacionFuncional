-- 1 --
-- data siempre en mayus
data Circulo = Cir Radio
data TrianguloRect = Tri Base Altura
data Cuadrilatero = Cuad Lado | Rect Base Altura

type Radio = Float 
type Altura = Float
type Base = Float 
type Lado = Float 

-- a --
class FigurasPlanas a where
    perimetro :: a -> Float 
    area :: a -> Float 

instance FigurasPlanas Circulo where
    perimetro (Cir r) = 2 * pi * r
    area (Cir r) = pi * (r ^ 2)

instance FigurasPlanas TrianguloRect where
    perimetro (Tri b a) = a + b + c where c = sqrt (a^2 + b^2)
    area (Tri b a) = (b * a) / 2

instance FigurasPlanas Cuadrilatero where
    perimetro (Cuad l) = l * 4
    perimetro (Rect b a) = b * 2 + a * 2
    area (Cuad l) = l * l
    area (Rect b a) = b * a

-- 2 --
data CatLista a = Nil | Unit a | Conc (CatLista a) (CatLista a)

aLista :: CatLista a -> [a]
aLista Nil = []
aLista (Unit a) = [a]
aLista (Conc a b) = aLista a ++ aLista b

instance Eq a => Eq (CatLista a) where
    (==) a b = aLista a == aLista b 

primero :: CatLista a -> a
primero Nil = error "Lista vacia"
primero l = head (aLista l) 



