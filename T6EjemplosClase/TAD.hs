data Conj a = Co [a]

vacio :: Conj a
vacio = Co [] 

simple :: a -> Conj a
miembro :: a -> Conj a -> Bool
union, inter, dif :: Conj a -> Conj a -> Conj a
-- dif x y devuelve el conjunto x menos el conjunto y
card :: Conj a -> Int
subConj :: Conj a -> Conj a -> Bool
-- subConj x y decide si x es subconjunto de y
hacerConj :: [a] -> Conj a