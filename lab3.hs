import System.IO
import Data.Char
import Data.List
type Capacity = Int
type Size = Int
data HashTable k v = HashTable [[(k,v)]] Capacity Size deriving (Show)

main::IO()
main = do
      contents <- lines <$> readFile "result.txt"
      let pairList = [(head $ words x , last $ words x) | x <- contents]
      let table = (fromList pairList)
      print (erase table "0")
      print (at table "9999")
      print (contains table "100")


defaultHashTable :: HashTable k v
defaultHashTable = HashTable (replicate 10 []) 10 0

hash :: (Show k) => k -> Int -> Int
hash k cap = (foldl (\acc x -> acc + ord(x)) 0 (show k)) `mod` cap

fromList :: (Show k, Eq k)=> [(k,v)]-> HashTable k v
fromList xs = foldl (\acc (a,b) -> (insert' acc a b)) (HashTable (replicate len []) len 0) xs where
                                                                                                  len = (length xs)


clear :: HashTable k v -> HashTable k v
clear table = defaultHashTable 

insert' ::(Show k, Eq k)=>HashTable k v ->k->v->HashTable k v
insert' (HashTable table cap size) k v = if (2*size < cap) then (HashTable (left ++ mid ++ (drop 1 right)) cap newSize) 
                                              else insert' (rehash table cap) k v where
                                                                        newSize = size + 1
                                                                        mid = [(k,v):(filter (\x -> (fst x) /= k) current)]
                                                                        current = head right
                                                                        (left,right) = splitAt (hash k cap) table

rehash :: (Show k, Eq k)=>[[(k,v)]] -> Capacity -> HashTable k v
rehash table cap = foldl (\acc (a,b) -> (insert' acc a b)) newBox (concat table) where
                                                                                newCap = 2*cap
                                                                                newBox = (HashTable (replicate newCap []) newCap 0)

erase::(Show k, Eq k)=>HashTable k v->k->HashTable k v
erase (HashTable table cap size) k = (HashTable (left ++ mid ++ (drop 1 right)) cap newSize) where
                                                                        newSize = size - 1
                                                                        mid = [filter (\x -> (fst x) /= k) (head right)]
                                                                        (left,right) = splitAt (hash k cap) table

contains::(Show k, Eq k)=>HashTable k v -> k -> Bool
contains (HashTable table cap _) k = elem k [key | (key,v) <- table !! (hash k cap)]

at::(Show k, Eq k)=>HashTable k v -> k -> Maybe v
at (HashTable table cap _) k =  lookup k (table !! (hash k cap))

size::(Show k, Eq k)=>HashTable k v -> Integer
size (HashTable _ _ size) = toInteger(size)

empty::(Show k, Eq k)=>HashTable k v -> Bool
empty (HashTable _ _ size) = size == 0