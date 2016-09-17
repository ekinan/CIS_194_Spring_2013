-- Exercise 1 solution

fun1_original :: [Integer] -> Integer
fun1_original [] = 1
fun1_original (x:xs)
	| even x = (x - 2) * fun1_original xs
	| otherwise = fun1_original xs
	
fun1 :: [Integer] -> Integer
fun1 = foldr (\x y -> (x-2)*y) 1 . filter even

fun2_original :: Integer -> Integer
fun2_original 1 = 0
fun2_original n
	| even n = n + fun2_original (n `div` 2)
	| otherwise = fun2_original (3 * n + 1)
	
fun2 :: Integer -> Integer
fun2 n = foldr (+) 0 evenArgs
 where
  reduce n
	| even n = n `div` 2
	| otherwise = 3 * n + 1
  
  argSeq = takeWhile (/= 1) (iterate reduce n)
  evenArgs = filter even argSeq
  
-- Exercise 2 solution

data Tree a = Leaf 
			| Node Integer (Tree a) a (Tree a)
	deriving (Eq, Show)

showHelper :: Show a => Int -> Tree a -> String
showHelper _ Leaf = "\n"
showHelper n root@(Node rh left rtVal right)
	= (showHelper (n+4) right)
		++ (replicate n ' ')  
		++"(" ++ (show rtVal) ++ "," ++ (show rh) ++ ")\n"
		++ (showHelper (n+4) left) 

height :: Tree a -> Integer
height Leaf = -1
height (Node h _ _ _) = h
	
insert :: a -> Tree a -> Tree a
insert x Leaf = Node 0 Leaf x Leaf
insert x root@(Node rh left rtVal right)
	| (old_left_height) > (old_right_height) 
			= Node rh left rtVal new_right
	| (old_left_height) < (old_right_height)
			= Node rh new_left rtVal right
	| otherwise
			= Node new_height new_left rtVal right
  where
   old_left_height = height left
   old_right_height = height right
   
   new_left = insert x left
   new_right = insert x right   
  
   new_left_height = height new_left
   new_height = rh + (new_left_height - old_left_height)
   
	
foldTree :: [a] -> Tree a
foldTree = foldr insert Leaf

-- Exercise 3 solution

xor :: [Bool] -> Bool
xor = odd . foldr (\x y -> y + 1) 0 . filter (==True)

map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x ys -> [f x] ++ ys) []

myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl f z = foldr (\y x -> f x y) z . reverse

-- Exercise 4 solution

cartProd :: [a] -> [b] -> [(a, b)]
cartProd xs ys = [(x,y) | x <- xs, y <- ys]

sieveSundaram :: Integer -> [Integer]
sieveSundaram n = primes
 where
  isIandJPair = \(i, j) -> (i <= j) && ((i + j + 2*i*j) <= n)
  sumIandJPair = \(i, j) -> i + j + 2*i*j
  
  seqN = [1..n]
  allIandJPairs = cartProd seqN seqN
  validIandJPairs = filter isIandJPair allIandJPairs
  
  validIandJSums = map sumIandJPair validIandJPairs
  
  filteredList = filter (not . (`elem` validIandJSums)) seqN
  primes = map ((+1) . (*2)) filteredList
  
