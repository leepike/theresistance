{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Resistance.Types where

import Control.Applicative
import qualified MonadLib as L
--import qualified Data.Array.Unboxed as A
import qualified Data.Set as S
--import Data.Coerce
import qualified Data.Map.Strict as M

------------------------------------------------------------

type Combs a = S.Set (S.Set a)

type Cnt = Int

type Id = Cnt

type Rnd = Int

data MissionCard = Success | Fail
  deriving (Show, Read, Eq, Ord)

data Points = Points
  { resistancePoints :: !Int
  , spyPoints        :: !Int
  } deriving (Show, Read, Eq)

-- Spy IDs
type Spies = [Id]

type Prob = Double

-- type SpyProb = A.UArray Id Double
type SpyProb = M.Map Id Prob

data Config = Config
  { players    :: [Id]
  , totalSpies :: !Cnt
  }
  deriving (Show, Read, Eq)

type MissionSelection = Config -> Game -> [Id]

type SpyGroups = [[Id]]

data Game = Game
  { points           :: Points
  , currentRnd       :: !Rnd
  , currentPlayer    :: !Id
  , spyProbability   :: SpyProb
  , spyGroups        :: SpyGroups
  , missionPlayers   :: [Id]
  } deriving (Show, Eq)

data Winner = None | Resistance | Spies
  deriving (Show, Read, Eq, Ord)

-- (Resistance, Spies)
data Wins = Wins
  { resistanceWins :: Integer
  , spyWins        :: Integer
  } deriving (Show, Read, Eq)

-- Resistance monad
newtype R a = R { unR :: L.StateT Game L.Id a }
  deriving (Functor, Applicative, Monad)

getGame :: R Game
getGame = R L.get

setGame :: Game -> R ()
setGame = R . L.set

runR :: Game -> R a -> (a, Game)
runR game r = L.runId $ L.runStateT game (unR r)
