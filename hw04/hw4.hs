import qualified Data.List as L

fun1' :: [Integer] -> Integer
fun1' = product . map (\x -> x - 2) . filter even
 
fun2' :: Integer -> Integer
fun2' = sum . filter even . takeWhile (>1) . iterate params

params :: Integer -> Integer
params n
    | even n = n `div` 2
    | otherwise = 3*n + 1


data Tree a = Leaf
            | Node Integer (Tree a) a (Tree a)
    deriving (Show, Eq)

foldTree :: [a] -> Tree a
foldTree = foldr insert Leaf

height :: Tree a -> Integer
height Leaf = -1
height (Node h _ _ _) = h

insert :: a -> Tree a -> Tree a
insert x Leaf = Node 0 Leaf x Leaf
insert x (Node h Leaf n Leaf)  = Node (h + 1) (insert x Leaf) n Leaf
insert x (Node h Leaf n right) = Node h (insert x Leaf) n right
insert x (Node h left n Leaf)  = Node h left n (insert x Leaf)
insert x (Node h left n right)
    | height left > height right = Node h left n (insert x right)
    | height left < height right = Node h (insert x left) n right
    | otherwise = Node (height right' + 1) left n right'
        where
            right' = insert x right

-- Exercise 3: More folds!
xor :: [Bool] -> Bool
xor = foldl xor' False

xor' :: Bool -> Bool -> Bool
xor' True False = True
xor' False True = True
xor' _ _ = False

map' :: (a -> b) -> [a] -> [b]
map' f = foldr ((:) . f) []

myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl f base = foldr (flip f) base . reverse

sieveSundaram :: Integer -> [Integer]
sieveSundaram n = map (\x -> 2*x + 1)
                $ diff [1..n]
                $ sundaramRemoves n

sundaramRemoves :: Integer -> [Integer]
sundaramRemoves = L.nub . L.sort . removes'

removes' :: Integer -> [Integer]
removes' n = [x | i <- [1..n], j <- [i..n], let x = i+j+2*i*j, x <= n]

diff :: Eq a => [a] -> [a] -> [a]
diff _ [] = []
diff [] _ = []
diff (x:xs) (y:ys)
    | x == y = diff xs ys
    | otherwise = x : diff xs (y:ys)

cartProd :: [a] -> [b] -> [(a, b)]
cartProd xs ys = [(x, y) | x <- xs, y <- ys]

isPrime :: Integer -> Bool
isPrime x = not $ null [i | i <- [2..x], x `mod` i == 0]
