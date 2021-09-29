quitaUno :: Eq t => t -> [t] -> [t]
quitaUno n [] = []
quitaUno n (x:s)
 |n == x = s
 |otherwise = x:quitaUno n s