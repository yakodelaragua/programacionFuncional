par::Int->Bool 
par x = mod x 2 == 0

filtrar::(a->Bool)->[a]->[a]
filtrar p[] = []
filtrar p(x:s) = if p x then x:filtrar p s else filtrar p s