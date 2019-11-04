import Data.Char
import Data.List
toDecimal :: Int -> String -> String
toDecimal base [] = "0"
toDecimal 1 snumber = if all (=='1') snumber then show(length snumber - 1) else error "wait, that's illegal"

toDecimal base snumber = show (foldl (\acc x -> acc*base + (digitToInt'(x) base)) 0 snumber) where
                              digitToInt' a base | ((elem a ['0'..'9']) && (ord a - ord '0' + 0 < base)) = ord a - ord '0' + 0
                                                 | ((elem a ['a'..'z']) && (ord a - ord 'a' + 10 < base)) = ord a - ord 'a' + 10
                                                 | ((elem a ['A'..'Z']) && (ord a - ord 'A' + 36 < base)) = ord a - ord 'A' + 36
                                                 | otherwise = error "digit is not ok"

fromDecimal :: Int -> String -> String
fromDecimal 1 snumber = replicate (read(snumber)+1) '1'
fromDecimal toBase snumber = helper toBase (read(snumber)) [] where
                             helper toBase 0 [] = "0"
                             helper toBase 0 acc = acc
                             helper toBase number acc = helper toBase (div number toBase) (chr(numToChr number toBase):acc)
                             numToChr a toBase | (elem (mod a toBase) [0..9]) = (mod a toBase + ord '0' - 0)
                                               | (elem (mod a toBase) [10..35]) = (mod a toBase + ord 'a' - 10)
                                               | (elem (mod a toBase) [36..61]) = (mod a toBase + ord 'A' - 36)
                                               | otherwise = error "digit is not ok"
convertFromTo :: Int -> Int -> String -> String
convertFromTo fromBase toBase snumber = fromDecimal toBase (toDecimal fromBase snumber)
-- гварды
-- foldl toDecimal --> разбор через степени 10
-- разобрать проверку через ['a'..'z']