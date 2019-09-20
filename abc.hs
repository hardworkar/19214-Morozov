d a b c = b*b-4*a*c
abs0 x | x == -0 = 0
       | True = x
x1 a b c = abs0((-b + sqrt(d a b c))/2/a)
x2 a b c = abs0((-b - sqrt(d a b c))/2/a)
solve a b c | d a b c >= 0 && a /= 0 = (x1 a b c, x2 a b c)
            | a == 0 && b /= 0 = (-c/b,-c/b)
            | a == 0 && b == 0 && c == 0 = error "Infinity"
            | a == 0 && b == 0 && c /= 0 = error "No solutions"
            | True = error "No real solutions"