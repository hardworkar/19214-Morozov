d a b c = sqrt(b*b-4*a*c)
x1 a b c = abs0((-b + d a b c)/2/a)
x2 a b c = abs0((-b - d a b c)/2/a)
abs0 x | x == -0 = 0
       | True = x
solve a b c | d a b c >= 0 = (x1 a b c, x2 a b c)
            | True = error "No real solutions"