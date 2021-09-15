hms::Int->(Int, Int, Int)
hms s = (horas, minutos, segundos)
 where horas = s `div` 3600
       minutos = (s - horas*3600) `div` 60
       segundos = s - minutos * 60 - horas * 3600
 