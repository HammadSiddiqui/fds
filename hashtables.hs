type HashTable = ( Int, [[Int]] )
ht :: HashTable
ht = (2, [[0, 4, 8, 12], [5, 1, 9, 13], [2, 6, 10, 42], [43]])
 
 
initHashTable :: Int -> HashTable
initHashTable d = (d, take (2^d) (repeat []))
 
dimension :: HashTable -> Int
dimension ht = fst ht
 
keys :: HashTable -> [Int]
keys ht = concat (snd ht)
 
keyCount :: HashTable -> Int
keyCount ht = length (keys ht)
 
 
--hash :: Int -> Int -> Int 
-- giving an error if include the type signature
hash d k = ((8 * k) `mod` (2^32)) `div` (2^(32-d))
 
 
--returns [k] if k exists in hash table
find :: HashTable -> Int -> [Int]
find ht k 
    | k `elem` (snd ht) !! (hash (dimension ht) k) = [k]
    |otherwise = []
 
 
--returns [] if k exists in hash table
findInv :: HashTable -> Int -> [Int]
findInv ht k 
    | k `elem` (snd ht) !! (hash (dimension ht) k) = []
    |otherwise = [k]
 
add :: HashTable -> Int -> HashTable
add ht k = (dimension ht, take (hash (dimension ht) k) (snd ht) ++ [findInv ht k ++ ((snd ht) !! (hash (dimension ht) k))] ++ (drop ((hash (dimension ht) k) + 1) (snd ht)))
 
 
 
sub :: HashTable -> Int -> HashTable
sub ht k = (dimension ht, take (hash (dimension ht) k) (snd ht) ++ [[ x | x <- ((snd ht) !! (hash (dimension ht) k)) , x /= k ]] ++ (drop ((hash (dimension ht) k) + 1) (snd ht)))
 
 
rehash :: HashTable -> Int -> HashTable
rehash ht d = rehashhelper (initHashTable d) (keys ht)
 
rehashhelper :: HashTable -> [Int] -> HashTable
rehashhelper ht [] = ht
rehashhelper ht keys = rehashhelper (add ht (head keys)) (tail keys)
 
insert :: HashTable -> Int -> HashTable
insert ht k 
    | (find ht k == []) && (keyCount ht + 1) > 2^(dimension ht) = rehash (add ht k) ( (dimension ht) + 1 )
    | otherwise = add ht k
 
remove ::  HashTable -> Int -> HashTable
remove ht k 
    | (findInv ht k == []) && (keyCount ht - 1) < ( div (2^(dimension ht)) 3) = rehash (sub ht k) ( (dimension ht) - 1 )
    | otherwise = sub ht k