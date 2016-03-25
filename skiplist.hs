
data SkipList = Node Int [SkipList]| Null deriving (Ord, Show, Read, Eq)

initList :: SkipList
initList = Node 0 [Null]



node :: Int -> Int -> SkipList
node n h = (Node n (take h (repeat Null)))

val :: SkipList -> Int
val Null = -1
val (Node a _) =  a  

-- In lst the first node is next and the others are pointers above it
nodeHeight :: SkipList -> Int
nodeHeight (Node _ lst) = (length lst) - 1  


--Temp
listHeight :: SkipList -> Int
listHeight sl@(Node _ ls) = max (nodeHeight sl) (listHeight (head ls) )
listHeight Null = 0
--Temp end


numElements :: SkipList -> Int -> Int
numElements (Node v lst) l 
	|lst !! l /= Null = 1 + numElements (lst !! l) l
	|lst !! l == Null = 0 



contains :: SkipList -> Int -> Bool
contains sl@(Node v lst)  n
	|v == n =  True
	|v /= n = contains (head lst) n
	|sl == Null = False

	