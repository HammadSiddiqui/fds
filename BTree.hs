-- data BTree a = Node a (Tree a) (Tree a)|Null deriving (Show, Read, Eq)
data BTree = Node (Maybe Int) (Maybe Int) (Maybe Int) (BTree) (BTree) (BTree) (BTree) | Empty deriving (Show, Read, Eq)
--(Maybe Int) (Maybe Int) (BTree) (BTree) (BTree) (BTree) | Empty
 
bt =                    Node (Just 2) (Just 5) (Just 8) 
    (Node (Just 1) (Just 2) (Nothing) (Empty)(Empty)(Empty)(Empty)) 
                            (Node (Just 3) (Nothing) (Nothing) (Empty) (Empty) (Empty) (Empty)) 
    (Empty) 
    (Empty) 
 
 
find :: Maybe Int -> BTree -> BTree
find _ Empty = Empty
find num tree@(Node a b c t1 t2 t3 t4)
    | num `elem` [a,b,c] = tree
    | num <= a = find num t1
    | num <= b = find num t2
    | num <= c = find num t3
    | num > c = find num t4
 
 
insert :: Maybe Int -> BTree -> BTree
insert num tree@(Node a b c t1 t2 t3 t4)
                                                              
    | num <= a && t1 /= Empty = Node a b c (insert num t1) t2 t3 t4
    | b == Nothing || num <= b && t2 /= Empty = Node a b c t1 (insert num t2) t3 t4
    | c == Nothing || num <= c && t3 /= Empty= Node a b c t1 t2 (insert num t3) t4
    | num >  c && t4 /= Empty= Node a b c t1 t2 t3 (insert num t4)


    |  b == Nothing && (num <= a ) = Node num a c t1 t2 t3 t4   -- [a, Nothing, Nothing] -> [num, a, Nothing]
    |  b == Nothing && (num > a) = Node a num c t1 t2 t3 t4     -- [a, Nothing, Nothing] -> [a, num, Nothing]
    |  c == Nothing && (num <= a) = Node num a b t1 t2 t3 t4    -- [a, b, Nothing] -> [num, a, b]
    |  c == Nothing && (num <= b) = Node a num b t1 t2 t3 t4    -- [a, b, Nothing] -> [a, num, b]
    |  c == Nothing && (num > b) = Node a b num t1 t2 t3 t4     -- [a, b, Nothing] -> [a, b, num] 


    | num <= a && t1 == Empty = Node b Nothing Nothing (Node num a Nothing Empty Empty Empty Empty) (Node c Nothing Nothing Empty Empty Empty Empty) (Empty) (Empty) 
    | num <= b && t2 == Empty = Node b Nothing Nothing (Node a num Nothing Empty Empty Empty Empty) (Node c Nothing Nothing Empty Empty Empty Empty) (Empty) (Empty) 
    | num <= c && t3 == Empty = Node b Nothing Nothing (Node a Nothing Nothing Empty Empty Empty Empty) (Node num c Nothing Empty Empty Empty Empty) (Empty) (Empty) 
    | num > c && t4 == Empty = Node b Nothing Nothing (Node a Nothing Nothing Empty Empty Empty Empty) (Node c num Nothing Empty Empty Empty Empty) (Empty) (Empty) 

    


     --                   5      
     --            2               6 7 8


     --          5
     --     2           8
     --  01   34    678    9 10