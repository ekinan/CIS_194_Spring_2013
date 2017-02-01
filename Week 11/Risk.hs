{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Risk where

import Control.Monad.Random
import Data.List
import Control.Monad

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

dice :: Int -> Rand StdGen [DieValue]
dice n = sequence $ replicate n die

-- Exercise 2

battle :: Battlefield -> Rand StdGen Battlefield
battle btf@(Battlefield attks defs)
  | attks > 1 && defs > 0 = finalBtf -- Need at least two attacking units, one defending unit
  | otherwise = return btf -- Don't do anything in this case
  where
   -- Returns a randomly generated set of dice rolls, sorted from largest to smallest
   simRolls n
     = (dice $ n) >>= (return . sortBy (flip compare) . map unDV)

   -- As specified in the problem, if the defender rolls a > value than the attacker (or has the
   -- same value), attacker dies. Otherwise, defender dies
   fight (attkRoll, defRoll) (totDeadAtk, totDeadDef)
     | defRoll >= attkRoll = (totDeadAtk + 1, totDeadDef)
     | otherwise          = (totDeadAtk, totDeadDef + 1)

   -- Initially, nobody's dead. We just go through our rolls and fight each battle one by one
   doBattle attkRolls defRolls = foldr fight (0, 0) (zip attkRolls defRolls) 


   -- Number of remaining attackers should be initial attackers - the number of dead attackers,
   -- same with the defenders
   updateBattlefield (totDeadAtk, totDeadDef) 
     = Battlefield (attks - totDeadAtk) (defs - totDeadDef)

   -- Up to 3 units can attack
   attkRolls = simRolls (min 3 (attks - 1))

   -- Up to 2 defenders can attack
   defRolls = simRolls (max 1 (min 2 (defs - 1)))

   batRes = liftM2 doBattle attkRolls defRolls  

   finalBtf = liftM updateBattlefield batRes


-- Exercise 3

invade :: Battlefield -> Rand StdGen Battlefield
invade btf@(Battlefield attks defs)
  | attks < 2 || defs == 0 = return btf -- End the battle
  | otherwise = (battle btf) >>= invade

-- Exercise 4

attackWon :: Battlefield -> Bool
attackWon btf@(Battlefield attks defs)
  | defs  <= 0 = True
  | otherwise = False 

successProb :: Battlefield -> Rand StdGen Double
successProb btf = res
 where
  addToRes res (nDefs, tot)
   | res = (nDefs + 1.0, tot + 1.0)
   | otherwise = (nDefs, tot + 1.0)

  -- Given the battlefields generated from invade, find out the ones where the defense
  computeRes bfs = (\(n, t) -> n / t) $ foldr addToRes (0.0, 0.0) $ map (attackWon) bfs

  -- Run invade on the battlefield 1000 times, then figure out the probability
  res = liftM (computeRes) $ mapM (\x -> invade btf) [1..1000]

