triplets = [ [a,b,c] | a <- [1..500], b <- [1..500], c <- [1..500]
           , a^2 + b^2 == c^2
           , (a < b) && (b < c) 
           , a + b + c == 1000]
