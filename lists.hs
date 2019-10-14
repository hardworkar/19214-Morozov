---------------------------------------------------------
get' :: [a] -> Int -> a
get' [] n = error "empty list"
get' xs n = helper xs n where
            helper [] n = error "out of list"
            helper (x:xs) 0 = x
            helper (x:xs) n = helper xs (n-1)
---------------------------------------------------------
head' :: [a] -> a
head' [] = error "empty list"
head' (x:xs) = x
---------------------------------------------------------
last' :: [a] -> a
last' [] = error "empty list"
last' xs = helper xs where
           helper [x] = x
           helper (x:xs) = helper xs
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
reverse' [] = error "empty list"
reverse' xs = helper xs [] where
              helper [] acc = acc
              helper (x:xs) acc = helper xs (x:acc)
---------------------------------------------------------
length' :: [a] -> Int
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
concat' xs xy = helper xs xy [] where
                helper (x:xs) xy acc = helper xs xy (x:acc)
                helper [] xy (a:acc) = helper [] (a:xy) acc
                helper [] xy [] = xy
---------------------------------------------------------
drop' :: Int -> [a] -> [a]
drop' n [] = error "empty list"
drop' n xs = helper n xs where
             helper 0 xs = xs
             helper n [] = error "out of list"
             helper n (x:xs) = helper (n-1) xs
---------------------------------------------------------
take' :: Int -> [a] -> [a]
take' n [] = error "empty list"
take' n xs = helper n xs [] where
             helper 0 xs acc = reverse' acc
             helper n [] acc = error "out of list"
             helper n (x:xs) acc = helper (n-1) xs (x:acc)
---------------------------------------------------------
splitAt' :: Int -> [a] -> ([a],[a])
splitAt' n [] = error "empty list"
splitAt' n xs = (take' n xs,drop' n xs)
---------------------------------------------------------
null' :: [a] -> Bool
null' [] = True
null' xs = False
---------------------------------------------------------
elem' :: (Eq a) => [a] -> a -> Bool
elem' [] x = error "empty list"
elem' xs y = helper xs y False where
             helper [] y acc = acc
             helper xs y True = True
             helper (x:xs) y acc = helper xs y (x==y) 
---------------------------------------------------------
filter' :: (a->Bool) -> [a] -> [a]
filter' test [] = []
filter' test (x:xs) = helper (test x) where
                      helper False = filter' test xs
                      helper True = x:(filter' test xs)
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
{-
d:::::::::::::::::::::::::::::::::ckWMMMMMMMMMW0xddddddddddddddddddddddddddddddddx0WMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMM
Xxc::::::::::::::::::::::::::::::::lkNMMMMMMMMMN0xddddddddddddddddddddddddddddddddx0WMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMM
MNkl::::::::::::::::::::::::::::::::cxXMMMMMMMMMWKkddddddddddddddddddddddddddddddddxONMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMM
MMW0o:::::::::::::::::::::::::::::::::o0WMMMMMMMMWXkdddddddddddddddddddddddddddddddddkKWMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMM
MMMMXxc::::::::::::::::::::::::::::::::lkNMMMMMMMMMN0xddddddddddddddddddddddddddddddddx0WMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMM
MMMMMNkl::::::::::::::::::::::::::::::::cxXMMMMMMMMMWKkddddddddddddddddddddddddddddddddxONMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMM
MMMMMMW0o:::::::::::::::::::::::::::::::::o0WMMMMMMMMMXOxddddddddddddddddddddddddddddddddkXWMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMM
MMMMMMMMXxc::::::::::::::::::::::::::::::::lkNMMMMMMMMMN0xddddddddddddddddddddddddddddddddx0WMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMM
MMMMMMMMMNkl::::::::::::::::::::::::::::::::cxXMMMMMMMMMWKkddddddddddddddddddddddddddddddddxONMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMM
MMMMMMMMMMW0o:::::::::::::::::::::::::::::::::o0WMMMMMMMMWXOxddddddddddddddddddddddddddddddddkKWMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMM
MMMMMMMMMMMMXxc::::::::::::::::::::::::::::::::lkNMMMMMMMMMN0xddddddddddddddddddddddddddddddddx0WMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMM
MMMMMMMMMMMMMNkl::::::::::::::::::::::::::::::::cdXMMMMMMMMMWKkddddddddddddddddddddddddddddddddxONMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMM
MMMMMMMMMMMMMMW0o:::::::::::::::::::::::::::::::::o0WMMMMMMMMWXOxddddddddddddddddddddddddddddddddkXWMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMM
MMMMMMMMMMMMMMMMXxc::::::::::::::::::::::::::::::::lkNMMMMMMMMMN0xddddddddddddddddddddddddddddddddx0XWMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMM
MMMMMMMMMMMMMMMMMNkl::::::::::::::::::::::::::::::::cdXMMMMMMMMMWKkddddddddddddddddddddddddddddddddxxONMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMM
MMMMMMMMMMMMMMMMMMW0o:::::::::::::::::::::::::::::::::o0WMMMMMMMMWXOxdddddddddddddddddddddddddddddddddkKWMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMM
MMMMMMMMMMMMMMMMMMMMXxc::::::::::::::::::::::::::::::::lkNMMMMMMMMMN0xdddddddddddddddddddddddddddddddddx0WMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMM
MMMMMMMMMMMMMMMMMMMMMNOl::::::::::::::::::::::::::::::::cdXWMMMMMMMMWKkdddddddddddddddddddddddddddddddddxONMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMM
MMMMMMMMMMMMMMMMMMMMMMW0o:::::::::::::::::::::::::::::::::o0WMMMMMMMMMXOddddddddddddddddddddddddddddddddddkKWMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMM
MMMMMMMMMMMMMMMMMMMMMMMMXxc::::::::::::::::::::::::::::::::lkNMMMMMMMMMN0xdddddddddddddddddddddddddddddddddx0WMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMM
MMMMMMMMMMMMMMMMMMMMMMMMMNkl::::::::::::::::::::::::::::::::cxXMMMMMMMMMWKkdddddddddddddddddddddddddddddddddxONMMMMMMMMMMMNXKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKK
MMMMMMMMMMMMMMMMMMMMMMMMMMW0o:::::::::::::::::::::::::::::::::o0WMMMMMMMMMXOxdddddddddddddddddddddddddddddddddkKWMMMMMMMMMXxcccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
MMMMMMMMMMMMMMMMMMMMMMMMMMMMXxc::::::::::::::::::::::::::::::::ckNMMMMMMMMMN0xdddddddddddddddddddddddddddddddddx0WMMMMMMMMMNkc::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
MMMMMMMMMMMMMMMMMMMMMMMMMMMMMNOl::::::::::::::::::::::::::::::::cdXMMMMMMMMMWKkdddddddddddddddddddddddddddddddddxONMMMMMMMMMW0l:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
MMMMMMMMMMMMMMMMMMMMMMMMMMMMMMW0o:::::::::::::::::::::::::::::::::o0WMMMMMMMMWXOxdddddddddddddddddddddddddddddddddkKWMMMMMMMMWKd::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
MMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMXxc::::::::::::::::::::::::::::::::lkNMMMMMMMMMN0xdddddddddddddddddddddddddddddddddx0WMMMMMMMMMNkc::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
MMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMNOl::::::::::::::::::::::::::::::::cdXMMMMMMMMMWKkdddddddddddddddddddddddddddddddddxONMMMMMMMMMW0o:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
MMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMWKo:::::::::::::::::::::::::::::::::o0WMMMMMMMMMXOxdddddddddddddddddddddddddddddddddkKWMMMMMMMMWKd::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
MMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMXxc::::::::::::::::::::::::::::::::lkNMMMMMMMMMN0xdddddddddddddddddddddddddddddddddx0NMMMMMMMMMNkc::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
MMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMNOl::::::::::::::::::::::::::::::::cdXMMMMMMMMMWKkdddddddddddddddddddddddddddddddddxONMMMMMMMMMW0l:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
MMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMWKo:::::::::::::::::::::::::::::::::o0WMMMMMMMMMXOxdddddddddddddddddddddddddddddddddkKWMMMMMMMMWKd::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
MMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMXxc::::::::::::::::::::::::::::::::ckNMMMMMMMMMN0xdddddddddddddddddddddddddddddddddx0NMMMMMMMMMNkc::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
MMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMNOl::::::::::::::::::::::::::::::::cdXMMMMMMMMMWKkdddddddddddddddddddddddddddddddddxONMMMMMMMMMWKkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkk
MMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMWKo:::::::::::::::::::::::::::::::::o0WMMMMMMMMMXOxdddddddddddddddddddddddddddddddddkKWMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMM
MMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMXxc::::::::::::::::::::::::::::::::ckNMMMMMMMMMN0xdddddddddddddddddddddddddddddddddx0NMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMM
MMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMXd:::::::::::::::::::::::::::::::::cOWMMMMMMMMMNOddddddddddddddddddddddddddddddddddxONMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMM
MMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMXxc::::::::::::::::::::::::::::::::ckNMMMMMMMMMW0xddddddddddddddddddddddddddddddddddddkKWMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMM
MMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMWKo:::::::::::::::::::::::::::::::::l0WMMMMMMMMMNOxddddddddddddddddddddddddddddddddddddddx0NMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMM
MMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMNOl:::::::::::::::::::::::::::::::::dXWMMMMMMMMWKkdddddddddddddddddddddddddddddddddddddddddxOXMMMMMMMMMMWKkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkk
MMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMXxc::::::::::::::::::::::::::::::::ckNMMMMMMMMMN0xddddddddddddddddddddddddddddddddddddddddddddkKWMMMMMMMMWKdc:::::::::::::::::::::::::::::::::::::::::::::::::::
MMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMWKo:::::::::::::::::::::::::::::::::o0WMMMMMMMMMXOxddddddddddddddddddddddddddddddddddddddddddddddx0NMMMMMMMMMNkc::::::::::::::::::::::::::::::::::::::::::::::::::
MMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMNOl::::::::::::::::::::::::::::::::cdXWMMMMMMMMWKkdddddddddddddddddddddddddddddddddddddddddddddddddxONMMMMMMMMMW0o:::::::::::::::::::::::::::::::::::::::::::::::::
MMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMXxc::::::::::::::::::::::::::::::::ckNMMMMMMMMMN0xddddddddddddddddddddddddddddddddddddddddddddddddddddkKWMMMMMMMMWXdc:::::::::::::::::::::::::::::::::::::::::::::::
MMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMWKo:::::::::::::::::::::::::::::::::o0WMMMMMMMMMXOxddddddddddddddddddddddddddddddddddddddddddddddddddddddx0NMMMMMMMMMNkc::::::::::::::::::::::::::::::::::::::::::::::
MMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMNOl::::::::::::::::::::::::::::::::cdXMMMMMMMMMWKkdddddddddddddddddddddddddddddddddddddddddddddddddddddddddxOXWMMMMMMMMW0o:::::::::::::::::::::::::::::::::::::::::::::
MMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMXxc::::::::::::::::::::::::::::::::ckNMMMMMMMMMN0xddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddkKWMMMMMMMMWXdc:::::::::::::::::::::::::::::::::::::::::::
MMMMMMMMMMMMMMMMMMMMMMMMMMMMMMWKo:::::::::::::::::::::::::::::::::o0WMMMMMMMMWXOxddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddx0NMMMMMMMMMNkc::::::::::::::::::::::::::::::::::::::::::
MMMMMMMMMMMMMMMMMMMMMMMMMMMMMNOl::::::::::::::::::::::::::::::::cdXWMMMMMMMMWKkdddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddxOXMMMMMMMMMW0o:::::::::::::::::::::::::::::::::::::::::
MMMMMMMMMMMMMMMMMMMMMMMMMMMMXxc::::::::::::::::::::::::::::::::ckNMMMMMMMMMN0xdddddddddddddddddddddddddddddddddxxxddddddddddddddddddddddddddddddddkKWMMMMMMMMWXdc:::::::::::::::::::::::::::::::::::::::
MMMMMMMMMMMMMMMMMMMMMMMMMMWKo:::::::::::::::::::::::::::::::::o0WMMMMMMMMMXOxdddddddddddddddddddddddddddddddddkKN0xddddddddddddddddddddddddddddddddx0NMMMMMMMMMNklc::ccccccccccccccccccccccccccccccccccc
MMMMMMMMMMMMMMMMMMMMMMMMMNOl:::::::::::::::::::::::::::::::::dXMMMMMMMMMWKkdddddddddddddddddddddddddddddddddxOXMMWKkddddddddddddddddddddddddddddddddxOXMMMMMMMMMWXKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKK
MMMMMMMMMMMMMMMMMMMMMMMMXxc::::::::::::::::::::::::::::::::ckNMMMMMMMMMN0xdddddddddddddddddddddddddddddddddx0NMMMMMXOxddddddddddddddddddddddddddddddddkKWMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMM
MMMMMMMMMMMMMMMMMMMMMMWKo:::::::::::::::::::::::::::::::::o0WMMMMMMMMMXOxdddddddddddddddddddddddddddddddddkKWMMMMMMMN0xddddddddddddddddddddddddddddddddx0NMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMM
MMMMMMMMMMMMMMMMMMMMMNOl::::::::::::::::::::::::::::::::cdXWMMMMMMMMWKkdddddddddddddddddddddddddddddddddxONMMMMMMMMMMWKkddddddddddddddddddddddddddddddddxOXMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMM
MMMMMMMMMMMMMMMMMMMMXxc::::::::::::::::::::::::::::::::ckNMMMMMMMMMN0xdddddddddddddddddddddddddddddddddx0NMMMMMMMMMMMMMXOxddddddddddddddddddddddddddddddddkKWMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMM
MMMMMMMMMMMMMMMMMMWKo:::::::::::::::::::::::::::::::::o0WMMMMMMMMMXOxdddddddddddddddddddddddddddddddddkKWMMMMMMMMMMMMMMMN0xddddddddddddddddddddddddddddddddx0NMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMM
MMMMMMMMMMMMMMMMMNOl::::::::::::::::::::::::::::::::cdXMMMMMMMMMWKkddddddddddddddddddddddddddddddddxxOXMMMMMMMMMMMMMMMMMMWKkddddddddddddddddddddddddddddddddxOXWMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMM
MMMMMMMMMMMMMMMMXxc::::::::::::::::::::::::::::::::ckNMMMMMMMMMN0xddddddddddddddddddddddddddddddddx0XNMMMMMMMMMMMMMMMMMMMMMXOxddddddddddddddddddddddddddddddddkKWMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMM
MMMMMMMMMMMMMMWKo:::::::::::::::::::::::::::::::::o0WMMMMMMMMWXOxddddddddddddddddddddddddddddddddkKWMMMMMMMMMMMMMMMMMMMMMMMMN0xddddddddddddddddddddddddddddddddx0NMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMM
MMMMMMMMMMMMMNOl::::::::::::::::::::::::::::::::cdXMMMMMMMMMWKkddddddddddddddddddddddddddddddddxOXMMMMMMMMMMMMMMMMMMMMMMMMMMMWKkddddddddddddddddddddddddddddddddxOXWMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMM
MMMMMMMMMMMMXxc::::::::::::::::::::::::::::::::ckNMMMMMMMMMN0xddddddddddddddddddddddddddddddddx0NMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMXOxddddddddddddddddddddddddddddddddkKWMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMM
MMMMMMMMMMWKo:::::::::::::::::::::::::::::::::o0WMMMMMMMMMXOxddddddddddddddddddddddddddddddddkKWMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMN0xddddddddddddddddddddddddddddddddx0NMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMM
MMMMMMMMMNOl:::::::::::::::::::::::::::::::::dXMMMMMMMMMWKkddddddddddddddddddddddddddddddddxOXMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMWKkddddddddddddddddddddddddddddddddxOXMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMM
MMMMMMMMXxc::::::::::::::::::::::::::::::::ckNMMMMMMMMMN0xddddddddddddddddddddddddddddddddx0NMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMNOxddddddddddddddddddddddddddddddddkKWMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMM
MMMMMMWKo:::::::::::::::::::::::::::::::::o0WMMMMMMMMMXOxddddddddddddddddddddddddddddddddkKWMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMN0xddddddddddddddddddddddddddddddddx0NMMMMMMMMMMMMMMMMMMMMMMMMMMMMMM
MMMMMNOl::::::::::::::::::::::::::::::::cdXWMMMMMMMMWKkddddddddddddddddddddddddddddddddxONMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMWKkddddddddddddddddddddddddddddddddxOXMMMMMMMMMMMMMMMMMMMMMMMMMMMMM
MMMMXxc::::::::::::::::::::::::::::::::ckNMMMMMMMMMN0xddddddddddddddddddddddddddddddddx0NMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMNOxddddddddddddddddddddddddddddddddkKWMMMMMMMMMMMMMMMMMMMMMMMMMMM
MMW0o:::::::::::::::::::::::::::::::::o0WMMMMMMMMWXOxddddddddddddddddddddddddddddddddkKWMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMN0xddddddddddddddddddddddddddddddddx0NMMMMMMMMMMMMMMMMMMMMMMMMMM
MNOl::::::::::::::::::::::::::::::::cdXMMMMMMMMMWKkddddddddddddddddddddddddddddddddxOXMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMWKkddddddddddddddddddddddddddddddddxOXWMMMMMMMMMMMMMMMMMMMMMMMM
Xxc::::::::::::::::::::::::::::::::lkNMMMMMMMMMN0xddddddddddddddddddddddddddddddddx0NMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMXOxddddddddddddddddddddddddddddddddkKWMMMMMMMMMMMMMMMMMMMMMMM
d:::::::::::::::::::::::::::::::::ckWMMMMMMMMMW0xddddddddddddddddddddddddddddddddx0WMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMNOdddddddddddddddddddddddddddddddddxKWMMMMMMMMMMMMMMMMMMMMMM
-}