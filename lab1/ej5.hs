sumcuad :: Int -> Int -> Int -> Int 
sumcuad x y z
 | x <= y && x <= z = y * y + z * z
 | y <= x && y <= z = x * x + z * z
 | otherwise = y * y + x * x