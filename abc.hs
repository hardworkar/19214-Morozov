solve 0 0 0 = error "Infinity"
solve 0 0 c = error "No solutions"
solve 0 b c = (-c/b,-c/b)
solve a b c | d >= 0 = (((- b - sqrt (d)) / 2 / a),((- b + sqrt (d)) / 2 / a))
            | otherwise = error "Negative discriminant"
            where d = b*b - 4*a*c