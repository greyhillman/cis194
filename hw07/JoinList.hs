{-# LANGUAGE FlexibleInstances #-}
import Data.Monoid
import Sized
import Scrabble
import Buffer
import Editor

data JoinList m a = Empty
                  | Single m a
                  | Append m (JoinList m a) (JoinList m a)
    deriving (Eq, Show)

instance Buffer (JoinList (Score, Size) String) where
    toString Empty = ""
    toString (Single _ a) = a
    toString (Append _ ja jb) = toString ja ++ toString jb

    fromString s = foldr (+++) Empty $ map help $ lines s
        where
            help s = Single (scoreString s, Size 1) s

    line = indexJ
    replaceLine n l b = takeJ n b +++ fromString l +++ dropJ (n+1) b

    numLines = getSize . snd . tag
    value = getScore . fst . tag

tag :: Monoid m => JoinList m a -> m
tag Empty = mempty
tag (Single m _) = m
tag (Append m _ _) = m

(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
x +++ Empty = x
Empty +++ x = x
as +++ bs = Append (tag as <> tag bs) as bs

jlToList :: JoinList b a -> [a]
jlToList Empty = []
jlToList (Single _ a) = [a]
jlToList (Append _ ja jb) = jlToList ja ++ jlToList jb

indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ _ Empty = Nothing
indexJ 0 (Single _ x) = Just x
indexJ i (Single _ x) = Nothing
indexJ i (Append m ja jb)
    | i < 0 = Nothing
    | Size i >= leftSize = indexJ i' jb
    | Size i < leftSize = indexJ i ja
    where
        leftSize = size $ tag ja
        i' = getSize $ Size i - leftSize

dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
dropJ 0 s = s
dropJ _ Empty = Empty
dropJ 1 (Single _ _) = Empty
dropJ _ (Single _ _) = Empty
dropJ n (Append m ja jb)
    | Size n >= leftSize = dropJ n' jb
    | Size n <  leftSize = Append (tag ja' <> tag jb) ja' jb
    where
        leftSize = size $ tag ja
        n' = getSize $ Size n - leftSize
        ja' = dropJ n ja

takeJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
takeJ 0 s = Empty
takeJ _ Empty = Empty
takeJ n (Single m a) = (Single m a)
takeJ n (Append m ja jb)
    | Size n >= leftSize = Append (tag ja <> tag jb') ja jb'
    | Size n <  leftSize = takeJ n ja
    where
        leftSize = size $ tag ja
        jb' = takeJ n' jb
        n' = getSize $ leftSize - Size n

scoreLine :: String -> JoinList Score String
scoreLine s = Single (scoreString s) s

testList :: JoinList Size Char
testList = Append (Size 4)
    (Append (Size 3)
        (Single (Size 1) 'y')
        (Append (Size 2)
            (Single (Size 1) 'e')
            (Single (Size 1) 'a')))
    (Single (Size 1) 'h')

main = runEditor editor initialBuffer

initialBuffer :: JoinList (Score, Size) String
initialBuffer = fromString "Test"
