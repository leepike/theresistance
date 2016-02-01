{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Resistance.Types where

import Control.Applicative
import MonadLib hiding (Id)
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
  } deriving (Show, Read, Eq)

data Game = Game
  { points           :: Points
  , currentRnd       :: !Rnd
  , currentPlayer    :: !Id
  , spyProbability   :: SpyProb
  , missionPlayers   :: [Id]
  } deriving (Show, Eq)

data Winner = None | Resistance | Spies
  deriving (Show, Read, Eq, Ord)

type Wins = M.Map Winner Integer

-- Resistance monad
newtype R a = R { unR :: StateT Game L.Id a }
  deriving (Functor, Applicative, Monad)

getGame :: R Game
getGame = R get

setGame :: Game -> R ()
setGame = R . set

runR :: Game -> R a -> (a, Game)
runR game r = runId $ runStateT game (unR r)
