import System.IO
import Data.Char
import Data.List
type Size = Int
type Occupied = Int
data HashTable k v = HashTable [[(k,v)]] Size Occupied deriving (Show)

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
hash k size = (foldl (\acc x -> acc + ord(x)) 0 (show k)) `mod` size

fromList :: (Show k, Eq k)=> [(k,v)]-> HashTable k v
fromList xs = foldl (\acc (a,b) -> (insert' acc a b)) (HashTable (replicate len []) len 0) xs where
                                                                                                  len = (length xs)


clear :: HashTable k v -> HashTable k v
clear table = defaultHashTable 

insert' ::(Show k, Eq k)=>HashTable k v ->k->v->HashTable k v
insert' (HashTable table size occupied) k v = if (2*occupied < size) then (HashTable (left ++ mid ++ (drop 1 right)) size newOccupied) 
                                              else insert' (rehash table size) k v where
                                                                        newOccupied = occupied + 1
                                                                        mid | elem k [key | (key,v) <- current] = [current]
                                                                            | otherwise = [(k,v):current]
                                                                        splited = splitAt (hash k size) table
                                                                        left = fst(splited)
                                                                        current = head(right)
                                                                        right = snd(splited)

rehash :: (Show k, Eq k)=>[[(k,v)]] -> Size -> HashTable k v
rehash table size = foldl (\acc (a,b) -> (insert' acc a b)) newBox allPairs where
                                                                                newSize = 2*size
                                                                                newBox = (HashTable (replicate newSize []) newSize 0)
                                                                                allPairs = [x | x <- (foldl (++) [] table)]

erase::(Show k, Eq k)=>HashTable k v->k->HashTable k v
erase (HashTable table size occupied) k = (HashTable (left ++ mid ++ (drop 1 right)) size newSize) where
                                                                        newSize = occupied - 1
                                                                        mid = [filter (\x -> (fst x) /= k) current]
                                                                        splited = splitAt (hash k size) table
                                                                        left = fst(splited)
                                                                        current = head(right)
                                                                        right = snd(splited)

contains::(Show k, Eq k)=>HashTable k v -> k -> Bool
contains (HashTable table size occupied) k = elem k [key | (key,v) <- current] where
                                                            right = drop (hash k size) table
                                                            current = head(right)

at::(Show k, Eq k)=>HashTable k v -> k -> Maybe v
at (HashTable table size occupied) k =  lookup k current where
                                                            right = drop (hash k size) table
                                                            current = head(right)

size::(Show k, Eq k)=>HashTable k v -> Integer
size (HashTable table size occupied) = toInteger(occupied)

empty::(Show k, Eq k)=>HashTable k v -> Bool
empty (HashTable table size occupied) = occupied == 0

x = defaultHashTable