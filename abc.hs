solve 0 0 0 = error "infinity"
solve 0 0 c = error "No solutions"
solve 0 b c = (-c/b,-c/b)
solve a b c = (x1,x2) where d = b*b - 4*a*c 
                            x1 = ((- b + sqrt (d)) / 2 / a)
                            x2 = ((- b - sqrt (d)) / 2 / a)