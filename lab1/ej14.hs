triangulo::(Int, Int, Int)->String
triangulo (x, y, z)
 |x + y <= z = error "no es triangulo"
 |x == y && y == z = "Equilatero"
 |x == y || y == z = "Isosceles"
 |otherwise = "Escaleno"
 