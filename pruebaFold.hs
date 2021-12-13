newLength :: [a] -> Integer
newLength = foldr (\x -> (+) 1) 0

data Arbin a = Hoja a | Unir (Arbin a) (Arbin a)

foldarbin :: (a->b) -> (b->b->b) -> Arbin a -> b
foldarbin f g (Hoja x) = f x
foldarbin f g (Unir ai ad) = g (foldarbin f g ai) (foldarbin f g ad)

lengthArbin :: Arbin a -> Integer
lengthArbin = foldarbin (const 1) (+)
