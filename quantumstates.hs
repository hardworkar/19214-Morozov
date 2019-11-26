data Complex a = Complex a a

instance (Eq a) => Eq (Complex a) where
   (==) (Complex r1 i1) (Complex r2 i2) = r1==r2 && i1==i2

instance (Show a) => Show (Complex a) where
   show (Complex r i) = show r ++ "+" ++ "(" ++ show i ++ "i)"


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
toList [] = []
toList ((QuantumState comp str):xs) = comp:(toList xs)

toLabelList :: Qubit (Complex a) -> [String]
toLabelList [] = []
toLabelList ((QuantumState comp str):xs) = str:(toLabelList xs)

fromList :: [Complex a]->[String]->Qubit (Complex a)
fromList [] [] = []
fromList (c:cmps) (s:strs) = (QuantumState c s):fromList cmps strs

toPairList :: Qubit (Complex a) -> [(Complex a,String)]
toPairList [] = []
toPairList ((QuantumState comp str):xs) = (comp,str):(toPairList xs)

fromPairList :: [(Complex a,String)] -> Qubit (Complex a)
fromPairList [] = []
fromPairList ((c,s):xs) = (QuantumState c s):fromPairList xs

scalarProduct:: (Num a) => Qubit (Complex a) -> Qubit (Complex a) -> Complex a
scalarProduct qub1 qub2 = foldl1 (\(Complex rAcc iAcc) (Complex rCurr iCurr)-> Complex (rAcc+rCurr) (iAcc+iCurr)) (zipWith (\(Complex r1 i1) (Complex r2 i2) -> (Complex (r1*r2-i1*i2) (i1*r2+r1*i2))) (toList qub1) (toList qub2))

entagle::(Num a) => Qubit (Complex a) ->Qubit (Complex a) ->Qubit (Complex a)
entagle qub1 qub2 = [QuantumState (Complex (r1*r2-i1*i2) (i1*r2+r1*i2)) (s1++s2) | (QuantumState (Complex r1 i1) s1) <- qub1,(QuantumState (Complex r2 i2) s2) <- qub2]


qub = [q,w]

q = QuantumState x "uuu"
w = QuantumState y "www"