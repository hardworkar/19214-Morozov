import Data.Char
import Data.List
toDecimal:: Int -> String -> String
toDecimal base [] = "0"
toDecimal 1 snumber = if null((filter (/='1') snumber)) then show(length snumber - 1) else error "that's illegal"
toDecimal base snumber = helper base snumber 0 where
                         helper base [] acc = show acc
                         helper base (x:snumber) acc = helper base snumber (acc + (digitToInt'(x) base)*base^length(snumber))
                         digitToInt' a base = (if ((ord a > 47) && (ord a < 58) && (ord a - 48 < base)) then (ord a - 48) else (if ((ord a > 64) && (ord a < 91) && (ord a - 29 < base)) then (ord a - 29) else (if ((ord a > 96) && (ord a < 123) && (ord a - 87 < base)) then (ord a - 87) else error "digit is not ok")))
fromDecimal:: Int -> String -> String
fromDecimal 1 snumber = helper [] (wordToInt(snumber)) where
                        helper acc 0 = '1':acc
                        helper acc n = helper ('1':acc) (n-1)
                        wordToInt [] = 0
                        wordToInt (x:word) = (digitToInt'(x)*10^length(word) + wordToInt word)
                        digitToInt' a = (if ((ord a > 47) && (ord a < 58) && (ord a - 48 < 10)) then (ord a - 48) else error "digit is not ok")
fromDecimal toBase snumber = helper toBase (wordToInt(snumber)) [] where
                             helper toBase 0 [] = "0"
                             helper toBase 0 acc = acc
                             helper toBase number acc = helper toBase (div number toBase) ((chr ((if (mod number toBase) < 10 then (mod number toBase+48) else (if ((mod number toBase) > 9 && (mod number toBase) < 36) then (mod number toBase+87) else (if ((mod number toBase) > 35 && (mod number toBase) < 62) then (mod number toBase+29) else error "bad")))):acc))
                             wordToInt [] = 0
                             wordToInt (x:word) = (digitToInt'(x)*10^length(word) + wordToInt word)
                             digitToInt' a = (if ((ord a > 47) && (ord a < 58) && (ord a - 48 < 10)) then (ord a - 48) else error "digit is not ok")
convertFromTo::Int -> Int -> String -> String
convertFromTo fromBase toBase snumber = fromDecimal toBase (toDecimal fromBase snumber)