--правки: Num для комплексных
--        List comprehensions

data Complex a = Complex a a

instance (Eq a) => Eq (Complex a) where
   (==) (Complex r1 i1) (Complex r2 i2) = r1==r2 && i1==i2

instance (Show a) => Show (Complex a) where
   show (Complex r i) = show r ++ "+" ++ "(" ++ show i ++ "i)"

instance (Num a, Ord a, Eq a) => Num (Complex a) where
   (+) (Complex r1 i1) (Complex r2 i2) = (Complex (r1+r2) (i1+i2))
   (*) (Complex r1 i1) (Complex r2 i2) = (Complex (r1*r2-i1*i2) (i1*r2+r1*i2))
   (abs) (Complex r i) = (Complex ((r*r+i*i)) 0)
   (signum) (Complex r i) | r > 0 = 1
                          | r == 0 = 0
                          | otherwise = -1
   (negate) (Complex r i) = (Complex (-1*r) (-1*i))

x = (Complex 4 7)
y = (Complex 1 3)

data QuantumState a = QuantumState a String

instance Functor QuantumState where
   fmap f (QuantumState comp str) = QuantumState (f comp) str

instance (Eq a) => Eq (QuantumState a) where
   (==) (QuantumState c1 str1) (QuantumState c2 str2) = c1==c2 && str1==str2

instance (Show a) => Show (QuantumState a) where
   show (QuantumState c s) = show c ++ " note: " ++ s

type Qubit a = [QuantumState a]

toList :: Qubit (Complex a) -> [Complex a]
toList qub = [c | (QuantumState c s) <- qub]

toLabelList :: Qubit (Complex a) -> [String]
toLabelList qub = [s | (QuantumState c s) <- qub]

fromList :: [Complex a]->[String] -> Qubit (Complex a)
fromList cs ss  = [(QuantumState c s) | c <- cs, s <- ss]

toPairList :: Qubit (Complex a) -> [(Complex a,String)]
toPairList qub = [(c,s) | (QuantumState c s) <- qub]

fromPairList :: [(Complex a,String)] -> Qubit (Complex a)
fromPairList cslist = [(QuantumState c s) | (c,s) <- cslist]

scalarProduct:: (Num a, Ord a) => Qubit (Complex a) -> Qubit (Complex a) -> Complex a
scalarProduct qub1 qub2 = foldl1 (+) (zipWith (*) (toList qub1) (toList qub2))

entagle::(Num a, Ord a) => Qubit (Complex a) ->Qubit (Complex a) ->Qubit (Complex a)
entagle qub1 qub2 = [QuantumState (c1*c2) (s1++s2) | (QuantumState c1 s1) <- qub1,(QuantumState c2 s2) <- qub2]

q = QuantumState x "uuu"
w = QuantumState y "www"
qub = [q,w]