{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

import Log

parseMessage :: String -> LogMessage
parseMessage []     = Unknown ""
parseMessage (x:xs) = parseMessage' x xs

parseMessage' :: Char -> String -> LogMessage
parseMessage' 'I' = parseRegMessage Info
parseMessage' 'W' = parseRegMessage Warning
parseMessage' 'E' = parseErrorMessage
parseMessage' x   = Unknown . (:) x

parseRegMessage :: MessageType -> String -> LogMessage
parseRegMessage msgType s = LogMessage msgType ts' msg'
    where
        (ts:msg) = words s
        ts' = read ts
        msg' = unwords msg

parseErrorMessage :: String -> LogMessage
parseErrorMessage s = LogMessage (Error lvl') ts' msg'
    where
        (lvl:ts:msg) = words s
        lvl' = read lvl
        ts' = read ts
        msg' = unwords msg

parse :: String -> [LogMessage]
parse = map parseMessage . lines

timeStamp :: LogMessage -> TimeStamp
timeStamp (LogMessage _ ts _) = ts
timeStamp (Unknown _) = undefined

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) t = t
insert msg Leaf = Node Leaf msg Leaf
insert msg' (Node left msg right)
    | msgTS' > msgTS = Node left msg (insert msg' right)
    | otherwise      = Node (insert msg' left) msg right
    where
        msgTS' = timeStamp msg'
        msgTS  = timeStamp msg

build :: [LogMessage] -> MessageTree
build = foldl (flip insert) Leaf

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node left msg right) = inOrder left ++ [msg] ++ inOrder right

severity :: LogMessage -> Int
severity (LogMessage (Error x) _ _) = x
severity _ = 0

getMsg :: LogMessage -> String
getMsg (LogMessage _ _ s) = s
getMsg _ = ""

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong = map getMsg . filter (\x -> severity x < 20 && severity x > 0) . inOrder . build

msgs :: [LogMessage] -> [String]
msgs = map getMsg
