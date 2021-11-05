prod:: Int -> Int -> Int 
prod n m 
 | n == m = n
 | n < m = n * prod(n+1) m
 | otherwise = error "Numeros incorrectos"
