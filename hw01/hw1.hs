import Data.Char

toDigits :: Integer -> [Integer]
toDigits x
    | x <= 0    = []
    | otherwise = map (toInteger . digitToInt) . show $ x

toDigitsRev :: Integer -> [Integer]
toDigitsRev = reverse . toDigits

everyOther :: (a -> a) -> [a] -> [a]
everyOther _ []       = []
everyOther _ [x]      = [x]
everyOther f (x:y:xs) = x : f y : everyOther f xs

-- every other from the right
everyOtherRight f = reverse . (everyOther f) . reverse

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther = everyOtherRight (*2)

sumDigits :: [Integer] -> Integer
sumDigits = sum . concatMap toDigits

validate :: Integer -> Bool
validate x = sumDigits (doubleEveryOther (toDigits x)) `mod` 10 == 0


type Peg  = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _ = []
hanoi n a b c = hanoi (n - 1) a c b ++ [(a, b)] ++ hanoi (n - 1) c b a
