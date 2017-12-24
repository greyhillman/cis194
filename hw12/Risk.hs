{-# LANGUAGE GeneralizedNewtypeDeriving, ScopedTypeVariables #-}
{-# OPTIONS -Wall #-}

module Risk where

import Control.Monad.Random
import Data.List

------------------------------------------------------------
-- Die values

newtype DieValue = DV { unDV :: Int } 
  deriving (Eq, Ord, Show, Num)

first :: (a -> b) -> (a, c) -> (b, c)
first f (a, c) = (f a, c)

instance Random DieValue where
  random           = first DV . randomR (1,6)
  randomR (low,hi) = first DV . randomR (max 1 (unDV low), min 6 (unDV hi))

die :: Rand StdGen DieValue
die = getRandom

------------------------------------------------------------
-- Risk

type Army = Int

data Battlefield = Battlefield { attackers :: Army, defenders :: Army }
    deriving Show

numAttackDie :: Army -> Int
numAttackDie x
    | x >= 4 = 3
    | x == 3 = 2
    | x == 2 = 1
    | otherwise = 0

numDefendDie :: Army -> Int
numDefendDie x
    | x >= 2 = 2
    | x == 1 = 1
    | otherwise = 0

dice :: Int -> Rand StdGen [DieValue]
dice n = sequence . replicate n $ die

attackOutcomes :: Int -> Int -> (Int, Int)
attackOutcomes a d
    | a >  d = (0, (-1))
    | otherwise = ((-1), 0)

riskDice :: Int -> Rand StdGen [Int]
riskDice n = decreasingSort . map unDV <$> dice n

decreasingSort :: Ord a => [a] -> [a]
decreasingSort = reverse . sort

fight :: [Int] -> [Int] -> (Int, Int)
fight as ds = foldr (\(x,y) (a,d) -> (a+x,d+y)) (0, 0) $ map (uncurry attackOutcomes) pairs
    where
        pairs = zip as ds

battle :: Battlefield -> Rand StdGen Battlefield
battle (Battlefield as ds) = do
    attackDies <- riskDice (numAttackDie as)
    defendDies <- riskDice (numDefendDie ds)
    let (attackLosses, defendLosses) = fight attackDies defendDies
    return $ Battlefield (as + attackLosses) (ds + defendLosses)

stopInvade :: Battlefield -> Bool
stopInvade (Battlefield as ds) = as == 1 || ds == 0

invade :: Battlefield -> Rand StdGen Battlefield
invade b
    | stopInvade b = return b
    | otherwise = battle b >>= invade

success :: Battlefield -> Bool
success (Battlefield _ ds) = ds == 0

successProb :: Battlefield -> Rand StdGen Double
successProb b = do
    let runs = 1000
    invasions <- replicateM runs $ invade b
    let successes = length $ filter success invasions
    return $ fromIntegral successes / fromIntegral runs
