{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}

module Resistance.Game
  ( initialGame
  , stepGame
  , gameEnd
  , mkConfig
  , missionSize
  , missionSuccess
  , missionSelection
  , missionSelection1
  , totalPlayers
  , validConfig
  , numSpies
  , nextPlayer
  , spyProbConfig
  , bayesGameUpdate
  ) where

import Resistance.Types

import qualified Data.Map.Strict as M
import Data.List hiding (group)
import MonadLib hiding (Id)
import Control.Exception.Base (assert)


------------------------------------------------------------
-- Misc functions

cntError :: String -> Cnt -> a
cntError nm n = error $ nm ++ " unimplemented for " ++ show n

------------------------------------------------------------
-- m combinations in n elements.
--
-- As an implementation detail, `combs` is in constant applicative form (CAF) to
-- get memoization for free. We memoize the typical calls to `comb'`, for
-- combinations drawn from `totalPlayers` and from `totalSpies`.

comb' :: (Ord a) => Int -> [a] -> [[a]]
comb' m ns
  = filter ((== fromIntegral m) . length)
  $ filterM (const [True, False]) ns

-- combs :: [[Combs Id]]
-- combs = map go [0 .. totalPlayers configFive]
--   where
--   go n = map (\i -> S.fromList $ map S.fromList $ comb' i [1..n])
--              [0.. n + 1]

-- External interface
-- comb :: Int -> Int -> Combs Id
-- comb m n
--   | m <= n && m >= 0 && n <=  (totalPlayers configFive)
--   = (combs !! n) !! m
--   | otherwise
--   = error $ "m: " ++ show m ++ " n: " ++ show n ++ " invalid args to comb"

------------------------------------------------------------

------------------------------------------------------------
-- Setup

totalPlayers :: Config -> Cnt
totalPlayers c = length (players c)

missionSize :: Cnt -> Rnd -> Cnt
missionSize 5  i | i >= 0 && i < 5 -- max rounds is 5
                 = [2,3,2,3,3] !! i
missionSize 6  i | i >= 0 && i < 5 -- max rounds is 5
                 = [2,3,3,3,4] !! i
missionSize 7  i | i >= 0 && i < 5 -- max rounds is 5
                 = [2,3,3,4,4] !! i
missionSize 8  i | i >= 0 && i < 5 -- max rounds is 5
                 = [3,4,4,5,5] !! i
missionSize  9  i | i >= 0 && i < 5 -- max rounds is 5
                 = [3,4,4,5,5] !! i
missionSize 10 i | i >= 0 && i < 5 -- max rounds is 5
                 = [3,4,4,5,5] !! i
                 | otherwise
                 = error $ "Out of bounds index: " ++ show i
missionSize n  _ = cntError "missionSize" n

validConfig :: Cnt -> Config
validConfig i =
  case i of
    5  -> Config [0..4] 2
    6  -> Config [0..5] 2
    7  -> Config [0..6] 3
    8  -> Config [0..7] 3
    9  -> Config [0..8] 3
    10 -> Config [0..9] 4
    _  -> error "invalid number of players"

numSpies :: [Id] -> Cnt
numSpies ids =
  case length ids of
    5  -> 2
    6  -> 2
    7  -> 3
    8  -> 3
    9  -> 3
    10 -> 4
    _  -> error "invalid number of players"

mkConfig :: Cnt -> Config
mkConfig i =
  let ps = [0 .. i-1] in
  Config { players    = ps
         , totalSpies = numSpies ps
         }

------------------------------------------------------------

------------------------------------------------------------
-- Helpers

probAssert :: Prob -> Bool
probAssert prob = prob >= 0 && prob <= 1

-- Get next player's turn.
nextPlayer :: Cnt -> Id -> Id
nextPlayer ps i = (i+1) `mod` ps

spyPoint :: Points -> Points
spyPoint p = p { spyPoints = spyPoints p + 1 }

resistancePoint :: Points -> Points
resistancePoint p = p { resistancePoints = resistancePoints p + 1 }

------------------------------------------------------------

------------------------------------------------------------
-- Missions

missionSuccess :: Config -> Rnd -> [MissionCard] -> Bool
missionSuccess c rnd cards =
  if | totalPlayers c >= 7 && rnd == 4
       -> length (filter (== Fail) cards) < 2
     | otherwise
       -> not (Fail `elem` cards)

failCards :: [MissionCard] -> Int
failCards cards = length (filter (== Fail) cards)

-- Select yourself and the remaining lowest probability players.
missionSelection :: MissionSelection
missionSelection idx sz spyProb =
  reverse $ fst $ foldl' go ([],1) sortedKeys
  where
  go (ls,cnt) i | i == idx  = (i:ls,cnt)
                | cnt < sz  = (i:ls,cnt+1)
                | otherwise = (ls,cnt)
  sorted     = sortBy (\a b -> compare (snd a) (snd b))
                      (M.assocs spyProb)
  sortedKeys = map fst sorted

-- Select the lowest probability players.
missionSelection1 :: MissionSelection
missionSelection1 _idx sz spyProb =
  reverse $ fst $ foldl' go ([],0) sortedKeys
  where
  go (ls,cnt) i | cnt < sz  = (i:ls,cnt+1)
                | otherwise = (ls,cnt)
  sorted     = sortBy (\a b -> compare (snd a) (snd b))
                      (M.assocs spyProb)
  sortedKeys = map fst sorted

------------------------------------------------------------

------------------------------------------------------------
-- Probability of being a spy

-- Division resulting in a probability distribution.
mkProb :: (Integral a, Integral b) => a -> b -> Prob
mkProb a b = assert (probAssert res) res
  where
  res = fromIntegral a / fromIntegral b

getSpyProb :: SpyProb -> Id -> Prob
getSpyProb spyProb idx =
  case M.lookup idx spyProb of
    Nothing -> error $ "Index " ++ show idx ++ " not in the spyProb map."
    Just p  -> p

updateSpyProb :: SpyProb -> (Id, Prob) -> SpyProb
updateSpyProb spyProb (idx,p) = M.insert idx p spyProb

initSpyProb :: Config -> Prob -> SpyProb
initSpyProb c p = M.fromList $ map ((,p)) (players c)

-- Bayes Theorem.
bayesThm :: Prob -> Prob -> Prob -> Prob
bayesThm pa pb pba
  | pb == 0.0
  = 0.0
  | otherwise
  = let pab = (pba * pa) / pb in
    assert (probAssert pab) pab

-- Given a list of players, the number of spies, and a spy probability map,
-- returns the probability that those players contain the number of spies
-- asserted.
spyProbConfig :: [Id] -> Int -> SpyProb -> Prob
spyProbConfig ls spyCnt spyProb = assert (spyCnt >= 0) res
  where
  spyCombs = comb' spyCnt ls
  res      = foldl' go 0 spyCombs
    where
    go p cs = p + p'
      where
      spyPs  = map (getSpyProb spyProb) cs
      goodPs = map (\i -> 1 - getSpyProb spyProb i)
                   (ls \\ cs)
      p' = product spyPs * product goodPs

-- A: idx is a spy.
-- B: The set of players has the number number of spies asserted.
-- Computes P(A|B) using Bayes Theorem.
bayesSpyProb :: Config -> SpyProb -> Id -> Prob
bayesSpyProb c spyProb idx = bayesThm pa pb pba
  where
  p   = players c
  s   = totalSpies c
  pa  = getSpyProb spyProb idx
  pb  = spyProbConfig p s spyProb
  pba | s == 0    = 0.0
      | otherwise = spyProbConfig (p \\ [idx]) (s-1) spyProb

-- Given a set of configuration (set of player ids and number of spies), update
-- their spy probabilities for the set number of spies.
bayesSpyUpdate :: Config -> SpyProb -> SpyProb
bayesSpyUpdate c spyProb = sp
  where
  ids = players c
  sp  = foldl' go spyProb ids
    where go p i = updateSpyProb p (i, bayesSpyProb c spyProb i)

bayesGameUpdate :: Config -> [Id] -> [MissionCard] -> SpyProb -> SpyProb
bayesGameUpdate c group cards spyProb =
  let res = sum $ map (getSpyProb spyProb') (players c) in
  let s   = fromIntegral (totalSpies c) in
  -- Sanity check, modulo floating point error
  assert (   res + 0.0001 >= s
          && res - 0.0001 <= s
         ) spyProb'
  where
  f        = failCards cards
  spGroup  = bayesSpyUpdate (Config group f) spyProb
  remSpies = totalSpies c - f
  spyProb' = bayesSpyUpdate (Config (players c \\ group) remSpies) spGroup

------------------------------------------------------------
-- Game setup

initialPoints :: Points
initialPoints = Points 0 0

initialGame :: Config -> Game
initialGame c =
  Game { points           = initialPoints
       , currentRnd       = 0
       , currentPlayer    = 0
       , spyProbability   = probs
       , missionPlayers   = mp
       }
  where
  -- Initially, spies/players probability of being a spy.
  probs = initSpyProb c (mkProb numS (totalPlayers c))
  numS  = totalSpies c
  mp    = take (missionSize (totalPlayers c) 0) (players c)

------------------------------------------------------------

------------------------------------------------------------
-- Running the game

-- Assumes a valid step can be taken.
stepGame :: MissionSelection -> Config -> Game -> [MissionCard] -> Game
stepGame ms c g cards = do
  let i        = currentPlayer g
  let t        = totalPlayers c
  let np       = nextPlayer t i
  let spyProb  = spyProbability g
  let rnd      = currentRnd g
  let rnd'     = rnd + 1
  let sz'      = missionSize t rnd'
  let spyProb' | failCards cards == 0 = spyProb
               | otherwise            =
                   bayesGameUpdate c (missionPlayers g) cards spyProb
  let g' = g { currentPlayer    = np
             , currentRnd       = rnd'
             , missionPlayers   = ms np sz' spyProb'
             }
  if missionSuccess c rnd cards
    then g' { points         = resistancePoint (points g)
            , spyProbability = spyProb'
            }
    else g' { points         = spyPoint (points g)
            , spyProbability = spyProb'
            }

-- Termination condition for the game.
gameEnd :: Game -> Winner
gameEnd game =
  let p = points game in
  if | resistancePoints p >= 3
       -> Resistance
     | spyPoints p >= 3
       -> Spies
     | otherwise
       -> None

------------------------------------------------------------

------------------------------------------------------------
-- Testing

_mkSP :: [Prob] -> SpyProb
_mkSP ls = M.fromList (zip [0..] ls)

-- test1 :: SpyProb
-- test1 = bayes (Config [0..4] 2) [0,1] sp cs
--   where
--   cs = [Fail,Fail]
--   sp = mkSP [0.5,0.25,0.25,0.25,0.5]

_sp0 :: SpyProb
_sp0 = _mkSP [0.5,0.25,1,0.25]

-- test2 = mapM_ print
--   [ conditionalSpyProb (Config 4 2) (0,True)  [0.5,1,0.1,0.4]
--   , conditionalSpyProb (Config 4 2) (0,True)  [0.5,0.25,0.1,0.4]
--   , conditionalSpyProb (Config 4 2) (0,False) [0.5,1,0.1,0.4]
--   , conditionalSpyProb (Config 4 2) (0,False) [0.5,0.25,0.1,0.4]
--   , conditionalSpyProb (Config 4 2) (0,True)  [0,0.25,1,0.75]
--   , conditionalSpyProb (Config 4 2) (0,False) [1,0.25,0.5,0.25]
--   ]

-- need 2:
-- 1/4x+0.1x+0.4x = 2

-- [0.25, 0.1, 0.4] = 0.75/3 = 0.25 average chance
-- now need to sum to a 2/3 average chance

-- P(A|B) = P(B|A) P(A) / P(B)

-- P(A|B): prob (A is faulty given that 2 are faulty in group)

-- P(A): prob of a

-- P(B): 0.25*0.1*0.6 + 0.25*0.9*0.4 + 0.75*0.1*0.4 = 0.135

-- P(A_a) = 1/4
-- P(B|A_a) = suppose a faulty. now compute 1/2 from b and c
--   = 0.1*0.6 + 0.9*0.4 = 0.42

-- P(A_a|B) = (1/4 * 0.42) / 0.135 = 0.777

-- P(B|A_b) = suppose b faulty. now compute 1/2 from b and c
--   = 0.25*0.6 + 0.75*0.4 = 0.45

-- P(A_b|B) = (0.1 * 0.45) / 0.135 = 0.333

-- P(B|A_c) = suppose c faulty. now compute 1/2 from a and b
--   = 0.25*0.9 + 0.75*0.1 = 0.3

-- P(A_c|B) = (0.4 * 0.3) / 0.135 = 0.888

_test3 :: IO ()
_test3 = do
  print $ bayesSpyUpdate c0 g0
  print $ bayesSpyUpdate c0 g1
  print $ bayesSpyUpdate c0 g2

  print $ bayesSpyUpdate c1 g0
  print $ bayesSpyUpdate c1 g1
  print $ bayesSpyUpdate c1 g2

  where
  c0 = Config [0..2] 2
  c1 = Config [0..2] 1
  g0 = _mkSP [0.25,0.1,0.4]
  g1 = _mkSP [1,0.4,0.4]
  g2 = _mkSP [0,1,0.5]

_test4 :: IO ()
_test4 = run (_mkSP [0.3,0.2,0.2,0.7])
  where
  run sp = do
    let sp' = bayesSpyUpdate (Config [0..3] 2) sp
    print sp'
    if sp == sp' then return () else run sp'

_test5 :: [Id]
_test5 = missionSelection1 2 3
          (M.fromList [(0,0.75),(1,0.25),(2,0.9),(3,0.01)])




-- cards are [T,F] played by 3,4
-- probs are [0.5,1,0.1,0.4]
-- calculate new spy probs based on outcome
-- P(A_a|B) = new spy prob of a given game output
-- P(A_a) = a's current spy prob
--   - 0.5
-- P(B) = prob of game outcome
--   - sum the ways those players can deliver those those cards
--   - 0.1  + 0.4 = 0.5: draw F first
--   - 0.9*0.4 + 0.1*0.6 = 0.42: draw T first

-- 0.1*0.4 = 0.004
-- 0.9*0.6 = 0.54

-- [T,T] = 0.9*0.6 = 0.54

-- [F,F] = impossible

-- P(B|A_a) = supposing A_a is a spy, what is the probability of the outcome?

-- [Tf] = 1/4*1/3 + 1/4*1/3 = 1/6
-- [TT] = 3/4 * 2/3 = 1/2
-- [FF] = 

-- need 2:
-- need 1: 0.1x+ 0.4x = 1 == 2
-- 1/2 1 0.2 0.8

--test3 =

-- [T,F] for 3 players, 1 spies
-- [0.5,0.25] for group members. what is prob. of outcome?
-- prob [T,F]:
--   - 2 assignments: 1/2(1/2) + 1/2(1) = 3/4
--   - or 1/4(1) + 3/4(0.6) = 0.7

--   - 1/2(1/4) + 1/2(3/4) = 1/2

-- possibile outcomes:
-- [T,T]
--   - 1 assignment: 1/2(1/2) = 1/4
--   - or 0.75(0.4) = .3


-- [T,F] for 3 players, 1 spies
-- [0.25,0.25] for group members. what is prob. of outcome?
-- prob [T,F]:
--   - 2 assignments: T first: 3/4(0.4) * 2 = 0.6
--   - or F first: 1/4(1) * 2 = 1/2

-- 3/4(1/4) * 2 = 3/4

-- possibile outcomes:
-- [T,T]
--   - 1 assignment: T first: 3/4(0.6) = 0.45

-- 3/4(3/4) = 0.5625

-- sim =
--   where
--   ls = [0.5,0.25]





------------------------------------------------------------
-- -- Given n spies in a mission, what is the probability that the mission had the outcome it
-- -- had?
-- probMissionSpies :: Int -> [MissionCard] -> Prob
-- probMissionSpies spies cards =
-- --  assert (probAssert res) res
--   trace ("pms : " ++ show spies ++ " " ++ show cards ++ " " ++ show res) res
--   where
--   res =
--     if | fc == 0 -- no fail cards
--          -> sucProb
--        | spySuccs == 0 -- no success cards from spies
--          -> failProb
--        | otherwise
--          -> sucProb * failProb
--   outcomes = 2^spies :: Integer -- resistance is fixed
--   fc       = let f = failCards cards in assert (spies >= f) f
--   spySuccs = spies - fc -- spies who voted for success
--   sucProb  = spyActsGoodProb ^ spySuccs
--   -- possible ways to fail
--   perms    = fromIntegral $ size $ comb fc spies
--   failProb = ((1-spyActsGoodProb) ^ fc) * perms

-- Given that a player 0 is assumed to be a spy (0,False) or resistance
-- (0,True), computes the updated spy probabilities for the remaining
-- players. An invariant is that the sum of all probabilities equals the total
-- number of spies before and after.
--
-- The algroithm is as follows. Suppose for `spyProb` assignments
-- [0.5,1,0.1,0.4], we assert that index 0 is resistance. So we need to increase
-- the probabilities of the last three by 0.5. Probabilities 0 and 1 are fixed,
-- so we ignore them. So we need to spread 0.5 over 2 players: 0.5/2 = 1/4
-- `avgSpyDelta`. We do this proportionally to how much much "room" there is to
-- increase the values (e.g., 1-0.1 and 1-0.4, respectively). We get the average delta: (1-0.1) + (1-0.4) / 2 = 1.5/2 = 3/4 `avgProbDelta`, and multiply that by the delta, to get increase of
--
-- p + ((1 - p) / avgProbDelta) * avgSpyDelta
--
-- For updating a spy, we do something similar, except decrease instead of
-- increase.
-- conditionalSpyProb :: Config -> (Id,Bool) -> SpyProb -> SpyProb
-- conditionalSpyProb c (idx,good) spyProb =
--   -- assert (let s     = sumSpyProb c sps in
--   --            s + 0.0001 >= spies
--   --         && s - 0.0001 <= spies
--   --         && and (map probAssert sps)
--   --        )
--   sps
--   where
--   spies = fromIntegral (totalSpies c)
--   s = if good then spies else spies - 1
--   -- r = sumSpyProb c spyProb - getSpyProb spyProb idx
--   -- spyDelta = s - r
-- --  isConst p = p == 0 || p == 1
--   -- t = let t' = foldl' go (-1) (players c) in
--   --     assert (t' > 0) t'
--   --   where
--   --   go cnt i = let p = getSpyProb spyProb i in
--   --              if isConst p then cnt else cnt+1

--   sumProbs = foldl' go (s,0) (players c)
--     where
--     go acc@(s',ps) i =
--       let p = getSpyProb spyProb i in
--       if | i == idx
--            -> acc
--          | p == 1
--            -> (s'-1,ps)
--          | otherwise
--            -> (s',ps+p)
--   delta = fst sumProbs / snd sumProbs

--   -- avgSpyDelta = spyDelta / t
--   -- avgProbDelta = sum remainingProbDeltas / t
--   --   where
--   --   remainingProbDeltas = foldl' go [] (players c)
--   --   go ls i = if | i == idx
--   --                  -> ls
--   --                | isConst (getSpyProb spyProb i)
--   --                  -> ls
--   --                | good
--   --                  -> 1 - getSpyProb spyProb i : ls
--   --                | not good
--   --                  -> getSpyProb spyProb i : ls

--   sps = foldl' go spyProb (players c)
--     where
--     go ps i = updateSpyProb ps (i,p')
--       where
--       p  = getSpyProb spyProb i
--       p' | p == 0 || p == 1
--          = p
--          | otherwise
--          = if i == idx then (if good then 0 else 1)
--              else p * delta

-- conditionalSpyProb :: Config -> (Id,Bool) -> SpyProb -> SpyProb
-- conditionalSpyProb c (idx,good) spyProb =
--   assert (let s     = sumSpyProb c sps in
--              s + 0.0001 >= spies
--           && s - 0.0001 <= spies
--           && and (map probAssert sps)
--          )
--   sps
--   where
--   spies = fromIntegral (totalSpies c)
--   s = if good then spies else spies - 1
--   r = sumSpyProb c spyProb - getSpyProb spyProb idx
--   spyDelta = s - r
--   isConst p = p == 0 || p == 1
--   t = let t' = foldl' go (-1) (players c) in
--       assert (t' > 0) t'
--     where
--     go cnt i = let p = getSpyProb spyProb i in
--                if isConst p then cnt else cnt+1
--   avgSpyDelta = spyDelta / t
--   avgProbDelta = sum remainingProbDeltas / t
--     where
--     remainingProbDeltas = foldl' go [] (players c)
--     go ls i = if | i == idx
--                    -> ls
--                  | isConst (getSpyProb spyProb i)
--                    -> ls
--                  | good
--                    -> 1 - getSpyProb spyProb i : ls
--                  | not good
--                    -> getSpyProb spyProb i : ls

--   sps = foldl' go spyProb (players c)
--     where
--     go ps i = updateSpyProb ps (i,p')
--       where
--       p  = getSpyProb spyProb i
--       p' | isConst p
--          = p
--          | otherwise
--          = if good
--              then if i == idx then 0
--                   else p + ((1 - p) / avgProbDelta) * avgSpyDelta
--              else if i == idx then 1
--                     else p + (p / avgProbDelta) * avgSpyDelta

-- Given a fixed number of spies and a particular outcome of the mission,
-- what is the probability of that outcome?
-- probMission :: Int -> [MissionCard] -> Prob
-- probMission spies cards =
--   if | spies < fs -- Impossible
--        -> 0
--      | fs == 0 -- No failures
--        -> goods
--      | l - fs == 0 -- No goods
--        -> bads
--      | otherwise
--        -> goods * bads * fromIntegral spies
--   where
--   l = length cards
--   fs = failCards cards
--   goods = spyActsGoodProb ^ (l - fs)
--   bads = (1.0 - spyActsGoodProb) ^ fs
