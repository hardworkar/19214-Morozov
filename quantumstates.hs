data Complex a = Complex a a

instance (Eq a) => Eq (Complex a) where
   (==) (Complex r1 i1) (Complex r2 i2) = r1==r2 && i1==i2

instance (Show a) => Show (Complex a) where
   show (Complex r i) = show r ++ "+" ++ "(" ++ show i ++ "i)"


x = Complex 0 0
y = Complex 12 32

data QuantumState a = QuantumState a String
instance Functor QuantumState where
   fmap f (QuantumState comp str) = QuantumState (f comp) str

instance (Eq a) => Eq (QuantumState a) where
   (==) (QuantumState c1 str1) (QuantumState c2 str2) = c1==c2 && str1==str2
instance (Show a) => Show (QuantumState a) where
   show (QuantumState c s) = show c ++ " metka: " ++ s

type Qubit a = [QuantumState a]

toList::Qubit a -> [a]
toList [] = []
toList ((QuantumState comp str):xs) = comp:(toList xs)

toLabelList::Qubit a -> [String]
toLabelList [] = []
toLabelList ((QuantumState comp str):xs) = str:(toLabelList xs)

fromList:: [a]->[String]->Qubit a
fromList [] [] = []
fromList (c:cmps) (s:strs) = (QuantumState c s):fromList cmps strs

toPairList:: Qubit a->[(a,String)]
toPairList [] = []
toPairList ((QuantumState comp str):xs) = (comp,str):(toPairList xs)

fromPairList:: [(a,String)] -> Qubit a
fromPairList [] = []
fromPairList ((c,s):xs) = (QuantumState c s):fromPairList xs



qub = [x,x,y]

q = QuantumState x "uuu"
w = QuantumState y "www"