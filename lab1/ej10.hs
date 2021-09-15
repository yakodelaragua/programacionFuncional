edad :: (Int , Int, Int) -> (Int, Int, Int) -> Int
edad (d, m, a) (d2, m2, a2) 
 |a2 >= a && (m2 > m || (m2 == m && d2 >= d)) = a2 - a
 |a2 >= a && (m2 == m && d2 < d) || m2 < m = a2 - a - 1
 |otherwise = error "Fechas incorrectas"