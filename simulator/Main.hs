{-
  Monte Carlo simulations of the Resistance
-}

module Main where

import Resistance.Types
import Resistance.Game

import Data.List (foldl')
import qualified Data.Map as M
import Control.Monad.MC as MC
import Control.Monad.Primitive as MC

------------------------------------------------------------

playMission :: Config -> Rnd -> [Id] -> Spies -> Bool
playMission c rnd group spies = missionSuccess c rnd cards
  where
  cards = map (missionCard spies) group

-- What mission card do you produce for a mission?
-- XXX add smarts
missionCard :: Spies -> Id -> MissionCard
missionCard spies i
  | i `elem` spies
  = Fail
  | otherwise
  = Success

initSpies :: PrimMonad m => Config -> MC.MC m Spies
initSpies c = do
  ls <- MC.shuffle (players c)
  return $ take numSpies ls
  where
  numSpies = totalSpies c

-- Repeat steps until termination.
runGame :: Config -> Spies -> R Winner
runGame c s = runGame' c
  where
  runGame' c = do
    g <- getGame
    let group = missionPlayers g
    let cards = map (missionCard s) group
    let g' = stepGame c g cards
    case gameEnd g' of
      None       -> runGame' c
      Resistance -> return Resistance
      Spies      -> return Spies

mkGame :: PrimMonad m => Config -> MC.MC m Winner
mkGame c = do
  s <- initSpies c
  let (w, _) = runR (initialGame c) (runGame c s)
  return w

updateWinMap :: Wins -> Winner -> Wins
updateWinMap ws w = M.insertWith (+) w 1 ws

main :: IO ()
main = do
  putStrLn "Enter seed for random number generator"
  seed <- readLn
  let reps = 5000
  let counts = foldl' updateWinMap M.empty
             $ MC.replicateMC reps (mkGame (validConfig 9)) (MC.mt19937 seed)

  print counts
