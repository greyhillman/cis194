{-# LANGUAGE FlexibleInstances #-}
fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

fibs1 :: [Integer]
fibs1 = map fib [0..]


fibs2 :: [Integer]
fibs2 = map fst $ iterate (\(a, b) -> (b, a+b)) (0, 1)


data Stream a = Stream a (Stream a)

instance Show a => Show (Stream a) where
    show = show . take 20 . streamToList

streamToList :: Stream a -> [a]
streamToList (Stream a s) = a : streamToList s

streamRepeat :: a -> Stream a
streamRepeat x = Stream x (streamRepeat x)

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Stream a s) = Stream (f a) (streamMap f s)

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f x = Stream x (streamFromSeed f (f x))

nats :: Stream Integer
nats = streamFromSeed (+1) 0

interleaveStreams :: Stream a -> Stream a -> Stream a
interleaveStreams (Stream a s) (Stream b s') = Stream a $ Stream b $ interleaveStreams s s'

zeroStream :: Num a => Stream a
zeroStream = streamRepeat 0

ruler :: Stream Integer
ruler = Stream 0 $ interleaveStreams (streamMap (+1) ruler) zeroStream


x :: Stream Integer
x = Stream 0 $ Stream 1 $ zeroStream

instance Num (Stream Integer) where
    fromInteger x = Stream x zeroStream
    negate = streamMap negate
    (Stream a s) + (Stream b s') = Stream (a + b) (s+s')
    (Stream a a') * (Stream b b') = Stream (a * b) $ streamMap (*a) b' + a'*(Stream b b')

instance Fractional (Stream Integer) where
    as@(Stream a a') / bs@(Stream b b') = Stream (a`div`b) $ streamMap (`div`b) (a' - (as/bs)*b')

fibs3 :: Stream Integer
fibs3 = x / (1 - x - x^2)


data Matrix a = Mat (a, a) (a, a)
    deriving Show

instance Num (Matrix Integer) where
    (Mat (a0, b0) (c0, d0)) * (Mat (a1, b1) (c1, d1)) = Mat (a', b') (c', d')
        where
            a' = a0*a1 + b0*c1
            b' = a0*b1 + b0*d1
            c' = c0*a1 + d0*c1
            d' = c0*b1 + d0*d1

fib4 :: Integer -> Integer
fib4 0 = 0
fib4 n = getFib $ fib4'^n

getFib :: Matrix Integer -> Integer
getFib (Mat (_, x) (_, _)) = x

fib4' :: Matrix Integer
fib4' = Mat (1, 1) (1, 0)
