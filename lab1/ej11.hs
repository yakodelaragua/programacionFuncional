orExclusivo::Bool->Bool->Bool 
orExclusivo b1 b2 
 | (b1 && b2) || (not b1 && not b2) = False
 | (not b1 && b2) || (b1 && not b2) = True 