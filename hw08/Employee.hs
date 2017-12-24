module Employee where

import           Data.Tree
import Data.Monoid
import Data.List

-- Employee names are represented by Strings.
type Name = String

-- The amount of fun an employee would have at the party, represented
-- by an Integer
type Fun  = Integer

-- An Employee consists of a name and a fun score.
data Employee = Emp { empName :: Name, empFun :: Fun }
  deriving (Show, Read, Eq)

smallCompany :: Tree Employee
smallCompany = Node (Emp "A" 5)
    [ Node (Emp "B" 4) []
    , Node (Emp "C" 1) []
    ]

-- A small company hierarchy to use for testing purposes.
testCompany :: Tree Employee
testCompany
  = Node (Emp "Stan" 9)
    [ Node (Emp "Bob" 2) -- (
      [ Node (Emp "Joe" 5) -- (5, 6)
        [ Node (Emp "John" 1) [] -- (1, 0)
        , Node (Emp "Sue" 5) [] -- (5, 0)
        ]
      , Node (Emp "Fred" 3) [] -- (3, 0)
      ]
    , Node (Emp "Sarah" 17) -- (17, 4)
      [ Node (Emp "Sam" 4) [] -- (4, 0)
      ]
    ]

testCompany2 :: Tree Employee
testCompany2
  = Node (Emp "Stan" 9)
    [ Node (Emp "Bob" 3) -- (8, 8)
      [ Node (Emp "Joe" 5) -- (5, 6)
        [ Node (Emp "John" 1) [] -- (1, 0)
        , Node (Emp "Sue" 5) [] -- (5, 0)
        ]
      , Node (Emp "Fred" 3) [] -- (3, 0)
      ]
    , Node (Emp "Sarah" 17) -- (17, 4)
      [ Node (Emp "Sam" 4) [] -- (4, 0)
      ]
    ]

-- A type to store a list of guests and their total fun score.
data GuestList = GL [Employee] Fun
  deriving (Show, Eq)

instance Ord GuestList where
  compare (GL _ f1) (GL _ f2) = compare f1 f2

instance Monoid GuestList where
    mempty = GL [] 0
    mappend (GL es1 fun1) (GL es2 fun2) = GL (es1 ++ es2) (fun1 + fun2)

glFun :: GuestList -> Fun
glFun (GL _ fun) = fun

glCons :: Employee -> GuestList -> GuestList
glCons e (GL es fun) = GL (e:es) (fun + empFun e)

moreFun :: GuestList -> GuestList -> GuestList
moreFun = max

moreFunPair :: (GuestList, GuestList) -> (GuestList, GuestList) -> (GuestList, GuestList)
moreFunPair (a, b) (c, d)
    | a > c && a > d = (a, b)
    | b > c && b > d = (a, b)
    | c > a && c > b = (c, d)
    | d > a && d > b = (c, d)
    | otherwise = error $ show (a, b) ++ show (c, d)

treeFold :: (a -> [b] -> b) -> Tree a -> b
treeFold f (Node x ts) = f x (map (treeFold f) ts)

nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
nextLevel boss = addBossToPair boss . mconcat

addBossToPair :: Employee -> (GuestList, GuestList) -> (GuestList, GuestList)
addBossToPair boss ((GL gs fun), gs') = (bestWithBoss, bestWithoutBoss)
    where
        bestWithBoss = moreFun bossBoss bossNoBoss
        bestWithoutBoss = moreFun (GL gs fun) gs'
        bossBoss = GL (boss:gs) (empFun boss)
        bossNoBoss = glCons boss gs'

maxFun :: Tree Employee -> GuestList
maxFun tree = moreFun withBoss withoutBoss
    where
        (withBoss, withoutBoss) = treeFold nextLevel tree

main :: IO ()
main = do
    company <- readFile "company.txt"
    let (GL guests fun) = maxFun $ read company
    putStrLn $ "Total fun: " ++ show fun
    mapM_ putStrLn $ sort $ map empName guests
