nAND :: Bool -> Bool -> Bool
nAND x y
 |x == y = True
 |x == not y = False 
 |otherwise = False

nOR :: Bool -> Bool -> Bool
nOR x y 
 |x == not y = True 
 |y = True 
 |otherwise = True
