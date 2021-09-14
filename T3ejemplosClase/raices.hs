raices :: (Float, Float, Float) -> (Float, Float)
raices (a,b,c)
 |a==0 = error "no es de grado 2"
 |e < 0 = error "raices complejas"
 |otherwise = ((-b-s)/d, (-b+s)/d)
 where 
   e = b * b - 4 * a * c
   s = sqrt e
   d = 2 * a