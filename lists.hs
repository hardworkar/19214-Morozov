---------------------------------------------------------
get' :: [a] -> Integer -> a
get' [] n = error "out of list"
get' (x:xs) 0 = x
get' (x:xs) n = get' xs (n-1)
---------------------------------------------------------
head' :: [a] -> a
head' [] = error "empty list"
head' (x:xs) = x
---------------------------------------------------------
last' :: [a] -> a
last' [] = error "empty list"
last' [x] = x
last' (x:xs) = last xs
---------------------------------------------------------
tail' :: [a] -> [a]
tail' [] = error "empty list"
tail' (x:xs) = xs
---------------------------------------------------------
init' :: [a] -> [a]
init' [] = error "empty list"
init' [x] = []
init' (x:xs) = x:(init' xs)
---------------------------------------------------------
reverse' :: [a] -> [a]
reverse' [] = []
reverse' xs = helper xs [] where
              helper [] acc = acc
              helper (x:xs) acc = helper xs (x:acc)
---------------------------------------------------------
length' :: [a] -> Integer
length' [] = 0
length' xs = helper xs 0 where
             helper [] acc = acc
             helper (x:xs) acc = helper xs (acc+1)
---------------------------------------------------------
append' :: [a] -> a -> [a]
append' [] a = [a]
append' (y:xs) x = y:append' xs x
---------------------------------------------------------
concat' :: [a] -> [a] -> [a]
concat' [] xy = xy
concat' (x:xs) xy = x:concat' xs xy
---------------------------------------------------------
drop' :: Integer -> [a] -> [a]
drop' n [] = []
drop' 0 xs = xs
drop' n (x:xs) = drop' (n-1) xs
---------------------------------------------------------
take' :: Integer -> [a] -> [a]
take' n [] = error "empty list"
take' 0 xs = []
take' n (x:xs) = x:take' (n-1) xs
---------------------------------------------------------
splitAt' :: Integer -> [a] -> ([a],[a])
splitAt' n [] = error "empty list"
splitAt' n xs = (take' n xs,drop' n xs)
---------------------------------------------------------
null' :: [a] -> Bool
null' [] = True
null' xs = False
---------------------------------------------------------
elem' :: (Eq a) => [a] -> a -> Bool
elem' [] x = False
elem' (x:xs) y = if x == y then True else elem' xs y
---------------------------------------------------------
filter' :: (a->Bool) -> [a] -> [a]
filter' test [] = []
filter' test (x:xs) = if (test x) then x:filter' test xs else filter' test xs
---------------------------------------------------------
map' :: (a->b) -> [a] -> [b]
map' f [] = []
map' f (x:xs) = (f x):(map' f xs)
---------------------------------------------------------
zip' :: [a] -> [b] -> [(a,b)]
zip' [] [] = []
zip' [] xs = error "|y|!=|x|"
zip' xs [] = error "|y|!=|x|"
zip' (x:xs) (y:ys) = (x,y):(zip' xs ys)
---------------------------------------------------------