fib :: Int -> Int
fib 1 = 0
fib 2 = 1
fib x = fib (x-1) + fib (x-2)