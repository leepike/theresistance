{-
  Monte Carlo simulations of the Resistance
-}

module Main ( main ) where

import Resistance.Types
import Resistance.Game

import Data.List (foldl', intersect)
import qualified Control.Monad.MC as MC
import qualified Control.Monad.Primitive as MC
import qualified Text.Printf as T

------------------------------------------------------------

initSpies :: MC.PrimMonad m => Config -> MC.MC m Spies
initSpies c = do
  ls <- MC.shuffle (players c)
  return $ take ns ls
  where
  ns = totalSpies c

-- Repeat steps until termination.
runGame :: MissionSelection -> CardSelect -> Config -> Spies -> R Winner
runGame ms cs c s = do
  g <- getGame
  let group = missionPlayers g
  let cards = cs c (currentRnd g) s group
  let g' = stepGame ms c g cards
  case gameEnd g' of
    None       -> setGame g' >> runGame ms cs c s
    Resistance -> return Resistance
    Spies      -> return Spies

mkGame :: MC.PrimMonad m => MissionSelection -> CardSelect -> Config -> MC.MC m Winner
mkGame ms cs c = do
  s <- initSpies c
  let (w, _) = runR (initialGame c) (runGame ms cs c s)
  return w

updateWinMap :: Wins -> Winner -> Wins
updateWinMap ws w =
  case w of
    None       -> ws
    Resistance -> ws { resistanceWins = resistanceWins ws + 1 }
    Spies      -> ws { spyWins = spyWins ws + 1 }

runMonteCarlo :: MissionSelection -> CardSelect -> Int -> MC.Seed -> Config -> Wins
runMonteCarlo ms cs reps seed config =
    foldl' updateWinMap (Wins 0 0)
  $ MC.replicateMC reps (mkGame ms cs config) (MC.mt19937 seed)

overConfigs :: MissionSelection -> CardSelect -> Int -> MC.Seed -> [Wins]
overConfigs ms cs reps seed =
  map (\i -> runMonteCarlo ms cs reps seed (validConfig i)) [5..10]

type ResistanceStrat = (String, MissionSelection)

resistanceStrats :: [ResistanceStrat]
resistanceStrats =
  [ ("select self"  , missionSelection)
  , ("select lowest", missionSelection1)
  , ("select logic" , missionSelection2)
  ]

type SpyStrat = (String, CardSelect)

spyStrats :: [SpyStrat]
spyStrats =
  [ ("selectAll" , cardSelectAll)
  , ("hide 1st"  , cardSelectHide1st)
  , ("select opt", cardSelectOpt)
  ]

allCombs :: [(ResistanceStrat, SpyStrat)]
allCombs = do
  ms  <- resistanceStrats
  cs  <- spyStrats
  return (ms, cs)

prettyShowWins :: Wins -> String
prettyShowWins (Wins r s) =
  let total :: Double
      total = fromInteger (r+s) in
  T.printf "R: %-4d (%.4f) S: %-4d (%.4f)" r (fromInteger r/total) s (fromInteger s/total)

printEach :: Int -> MC.Seed -> IO ()
printEach reps seed = mapM_ go allCombs
  where
  go :: (ResistanceStrat, SpyStrat) -> IO ()
  go (rs,ss) = do
    let res = overConfigs (snd rs) (snd ss) reps seed
    T.printf "Strategies: %s (R) %s (S)\n" (fst rs) (fst ss)
    T.printf "-------------------------------------\n"
    mapM_ printSizes (zip [5..10] res)
    T.printf "\n"

  printSizes :: (Int, Wins) -> IO ()
  printSizes (i,wins) =
    T.printf "%-2d %s\n" i (prettyShowWins wins)

main :: IO ()
main = do
  putStrLn "Enter seed for random number generator"
  seed <- readLn
  putStrLn "Enter number of reps"
  reps <- readLn
  printEach reps seed

  -- print $ runMonteCarlo missionSelection1 cardSelectAll reps seed (validConfig 9)

------------------------------------------------------------
-- Card strategies

type CardSelect = Config -> Rnd -> Spies -> [Id] -> [MissionCard]

-- Spies always choose fail cards
cardSelectAll :: CardSelect
cardSelectAll _ _ spies group =
  map go group
  where
  go i | i `elem` spies
       = Fail
       | otherwise
       = Success

cardSelectHide1st :: CardSelect
cardSelectHide1st _ r spies group =
  if r == 0
    then map (const Success) group
    else map go group
  where
  go i | i `elem` spies
       = Fail
       | otherwise
       = Success

-- Spies never reveal more than one card
cardSelectOpt :: CardSelect
cardSelectOpt _ _ spies group =
   let ls = map (const Success) group in
   if null (intersect spies group)
     then ls
     else Fail : tail ls


