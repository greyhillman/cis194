import Data.List

skips :: [a] -> [[a]]
skips xs = [ [xs !! (i - 1) | i <- [j,j+j..length xs]] | j <- [1..length xs]]

groupN :: Int -> [a] -> [[a]]
groupN n = filter ((==n) . length) . map (take n) . tails

middle :: [a] -> a
middle xs = xs !! (length xs `div` 2)

localMaxima :: [Integer] -> [Integer]
localMaxima = map middle . filter isMiddleLocalMax . groupN 3
    where
        isMiddleLocalMax xs = middle xs == maximum xs

histogram :: [Integer] -> String
histogram xs = output (counts xs) ++ "==========\n" ++ "0123456789\n"

counts :: [Integer] -> [Int]
counts xs = map (count xs) [0..9]

output :: [Int] -> String
output = unlines . reverse . reversePrint

toStars :: [Int] -> String
toStars = map toStar

infiniteDec :: [Int] -> [[Int]]
infiniteDec = iterate (map pred)

isAllSpace :: String -> Bool
isAllSpace = (==[]) . words

reversePrint :: [Int] -> [String]
reversePrint = takeWhile (not . isAllSpace) . map toStars . infiniteDec

toStar :: Int -> Char
toStar x
    | x <= 0 = ' '
    | otherwise = '*'

count :: Eq a => [a] -> a -> Int
count xs x = length $ filter (==x) xs
