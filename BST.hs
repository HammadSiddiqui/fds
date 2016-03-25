{-data Tree a = Node a (Tree a) (Tree a)|Null deriving (Show, Read, Eq)


t = Node 10 (Node 20 (Node 30 Null Null) Null) (Node 10 Null Null)

--depth :: Tree -> a -> int
depth Null _ = -1
depth (Node x Null Null) n
	| x == n = 0 
	| otherwise = -1

depth (Node x lt rt) n 
	| x==n = 0
	| depth lt n> -1 =  1 + (depth lt n)
	| depth rt n > -1 = 1 + (depth rt n)



postfix :: Tree a -> [a]
postfix Null = []
postfix (Node x lt rt) = postfix lt ++ postfix rt ++ [x]


find :: (Ord a) => Tree a -> a -> Tree a
find Null _ = error "empty tree"
find t@(Node x Null Null) n
	| x == n = t 
	| otherwise = Null
find t@(Node x lt rt) n 
	| x == n = t
	| (find lt n) /= Null = (find lt n)
	| find rt n /=  Null = (find rt n)


height t n = treeHeight (find t n)  

treeHeight (Node x Null Null) = 0
treeHeight (Null) = 0
treeHeight (Node x lt rt) = (max (treeHeight lt) (treeHeight rt)) + 1
-}


data BST = Node Int (BST) (BST )|Empty deriving (Show, Read, Eq)
bt = Node 10 (Node 5 (Node 2 Empty Empty) Empty) (Node 20 Empty Empty)


bstInsert::Int -> BST -> BST
bstInsert n Empty = (Node n Empty Empty)
bstInsert n (Node a t1 t2)
	|n <= a = Node a (bstInsert n t1) t2
	|n > a  = Node a t1 (bstInsert n t2)

bstFind :: BST  -> Int -> Bool 
bstFind Empty _ = False
bstFind t@(Node x t1 t2) n
	| x == n = True 
	| n<= x = bstFind t1 n
	| n>x = bstFind t2 n


bstRemove :: Int -> BST -> BST
bstRemove n Empty = (Node n Empty Empty)
bstRemove n t@(Node a t1 t2)
	| bstFind t n == False = t
	| n==a = 
