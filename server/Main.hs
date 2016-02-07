{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ExtendedDefaultRules #-}

module Main (main) where

import Resistance.Types
import Resistance.Game

import qualified Lucid.Base as L
import qualified Lucid.Html5 as L
import qualified Web.Scotty as S

import qualified Text.Printf as T
import Control.Monad (void)
import Data.Monoid (mempty, mconcat, (<>))
import qualified Data.Text as TT
import qualified Data.Text.Lazy as T
--import qualified Data.Text.Lazy.IO as T
import qualified Data.Map.Strict as M

--import Control.Monad.IO.Class (liftIO)
--import Debug.Trace

------------------------------------------------------------

main :: IO ()
main = S.scotty 3000 routes

routes :: S.ScottyM ()
routes = do
  S.get "/" $ do
    ls <- S.params
    -- liftIO (print ls)
    case parsePlayers ls of
      Nothing
        -> S.html indexHtml
      Just p
        -> do let c = validConfig p
              let g = initialGame c
              redirectGameRoute (c,g)
              -- S.html (gameHtml (c,g))

-- Route: totalplayers/points/currentRnd/currentPlayer/spyProbability
-- E.g.: 5/(1,2)/3/4/[(0,0.5),(1,0.5),(2,0.5),(3,0.5),(4,0.5)]/[2,3]
  S.get "/:totalplayers/:points/:currentrnd/:currentplayer/:spyprobability/:missionplayers/" $ do
    ls <- S.params
    tp   <- S.param "totalplayers"
    ps   <- S.param "points"
    r    <- S.param "currentrnd"
    p    <- S.param "currentplayer"
    prob <- S.param "spyprobability"
    mp   <- S.param "missionplayers"
    let ps'   = parsePoints ps
    let prob' = parseSpyProbability prob
    let mp'   = parseMissionPlayers mp
    let (c,g) = fromRoute tp ps' r p prob' mp'
    case parseNext ls of
      Just ()
        -> redirectGameRoute (c,failedVote (c,g))
      Nothing
        ->
        case parseFailCards ls of
          Nothing
            -> S.html $ gameHtml $ (c,g)
          Just f
            -> do let g' = stepGame missionSelection1 c g (toCards c g f)
                  S.html $ gameHtml $ (c,g')
                  redirectGameRoute (c,g')

failedVote :: (Config, Game) -> Game
failedVote (c,g) =
  g { currentPlayer  = np
    , spyProbability = sp'
    }
  where
  cp  = currentPlayer g
  np  = nextPlayer (totalPlayers c) cp
  -- Update the current player who proposed a bad group to have 100%
  -- probability.
  sp' = bayesGameUpdate c [cp] [Fail] (spyProbability g)

------------------------------------------------------------
-- Parsers for game data

eitherToMaybe :: Either a b -> Maybe b
eitherToMaybe e =
  case e of
    Left _  -> Nothing
    Right i -> Just i

parseParam :: S.Parsable a => T.Text -> Maybe a
parseParam = eitherToMaybe . S.parseParam

parsePlayers :: [S.Param] -> Maybe Cnt
parsePlayers params = do
  i <- lookup "players" params
  parseParam i

parsePoints :: T.Text -> Points
parsePoints p =
  let (rp,sp) = read (T.unpack p) in
  Points { resistancePoints = rp
         , spyPoints        = sp
         }

parseSpyProbability :: T.Text -> SpyProb
parseSpyProbability p =
  M.fromList $ read (T.unpack p)

parseMissionPlayers :: T.Text -> [Id]
parseMissionPlayers = read . T.unpack

parseFailCards :: [S.Param] -> Maybe Int
parseFailCards params = do
  i <- parseParam =<< lookup "fails" params
  return i

-- vote failed?
parseNext :: [S.Param] -> Maybe ()
parseNext params = do
  void (lookup "next" params)
  return ()

------------------------------------------------------------

------------------------------------------------------------
-- To routes

pointsRoute :: Points -> T.Text
pointsRoute Points { resistancePoints, spyPoints } =
  toText (resistancePoints, spyPoints)

spyProbRoute :: SpyProb -> T.Text
spyProbRoute spyProb =
  toText (M.toList spyProb)

gameRoute :: (Config, Game) -> T.Text
gameRoute (c,g) =
  T.intercalate "/"
    [ toText (totalPlayers c)
    , pointsRoute (points g)
    , toText (currentRnd g)
    , toText (currentPlayer g)
    , spyProbRoute (spyProbability g)
    , toText (missionPlayers g)
    ]

redirectGameRoute :: (Config, Game) -> S.ActionM ()
redirectGameRoute cg = S.redirect $ "/" `T.append` gameRoute cg

------------------------------------------------------------

------------------------------------------------------------
-- From routes

fromRoute :: Cnt
          -> Points
          -> Int
          -> Id
          -> SpyProb
          -> [Id]
          -> (Config, Game)
fromRoute tp ps r p prob mp =
  let c = validConfig tp in
  let g = Game { points         = ps
               , currentRnd     = r
               , currentPlayer  = p
               , spyProbability = prob
               , missionPlayers = mp
               }
  in (c,g)

------------------------------------------------------------
-- HTML for game body

toText :: Show a => a -> T.Text
toText = T.pack . show

pointsToText :: Points -> [T.Text]
pointsToText (Points resistancePoints spyPoints) =
  [ "Resistance points:" <> toText resistancePoints
  , "Spy points:"        <> toText spyPoints
  ]

prettySpyProb :: SpyProb -> [T.Text]
prettySpyProb spyProb =
  map go (M.toList spyProb)
  where
  go (i,p) = T.pack $ "Player " ++ show i ++ ": " ++ T.printf "%.4f\n" p

gameToHtml :: Game -> [L.Html ()]
gameToHtml
  Game { points
       , currentRnd
       , currentPlayer
       , spyProbability
       , missionPlayers
       }
  =  map L.toHtml (pointsToText points)
  ++ [ L.toHtml $ "Current round: "  <> toText currentRnd
     , L.toHtml $ "Current player: " <> toText currentPlayer
     , "Spy probabilities: "         <> mkSpyProbs
     , "Mission players: "           <> L.toHtml (toText missionPlayers)
     ]
  where
  mkSpyProbs = ul (map L.toHtml (prettySpyProb spyProbability))

winnerToHtml :: Game -> L.Html ()
winnerToHtml g =
  L.h2_ [] $ L.toHtml $ "Winner: " `T.append` toText (gameEnd g)

------------------------------------------------------------
-- Intro HTML

indexHtml :: T.Text
indexHtml = L.renderText
   $ L.doctypehtml_
   $ L.html_ $ do
       L.head_ $ do
         styles
         title
       L.body_ $ do
         L.h1_ "Resistance Solver"
         L.section_ $ do
           "Please select the number of players:"
           L.form_ $ do
             mconcat (map inp [5..10])
             submit
         L.aside_ $ do
           introText
           L.br_ [] <> L.br_ []
           creditText
  where
  inp i = L.input_ [L.type_ "radio", L.name_ "players", L.value_ (textNum i)]
       <> L.toHtml (textNum i) <> L.br_ []

creditText :: L.Html ()
creditText =
  L.p_ [L.style_ "text-align:right"] $ do
    "Sources (including Monte Carlo simulator):"
    L.br_ []
    let src = "https://github.com/leepike/theresistance"
    L.a_ [L.href_ src] (L.toHtml src)
    L.br_ []
    "Author: (c) Lee Pike, 2016"

introText :: L.Html ()
introText = do
  "This is a solver for the game "
  (L.a_ [L.href_ "https://en.wikipedia.org/wiki/The_Resistance_(game)"]
    "The Resistance")
  ". It applies Bayes Theorem interatively to discover an optimal solution for the resistance to win. Select the number of players to begin."

gameHtml :: (Config,Game) -> T.Text
gameHtml (c,g) = L.renderText
   $ L.doctypehtml_
   $ L.html_ $ do
       L.head_ $ do
         styles
         title
       L.body_$ do
         L.h1_ "Resistance Solver"
         L.section_ $ do
           L.a_ [L.href_ "/"]
             (L.p_ [L.style_ "text-align:center"] (L.b_ "reset"))
           mkWinner
           ul (gameToHtml g)
           L.br_ []
           mkNext
           L.br_ [] <> L.br_ []
           mkMission
         L.aside_ $ do
           gameText
           creditText
  where
  mkMission =
    if gameEnd g == None
      then missionForm (missionSize (totalPlayers c) (currentRnd g))
      else mempty
  -- Vote fails
  mkNext   =
    if gameEnd g == None
      then do
        "Vote failed?"
        L.form_ $ do
          L.input_ [L.type_ "radio", L.name_ "next", L.value_ (textNum 0)]
          submit
      else mempty
  mkWinner = if gameEnd g == None
               then mempty
               else winnerToHtml g

gameText :: L.Html ()
gameText = do
  L.p_ "This solver is from the perspective of the resistance team. We assume that all Resistance will behave according to the guidelines below."

  L.p_ (L.b_ "Players:") <> "Players are labeled 0 through n-1 players, where n is the total number of players. The first player to take a turn is player 0. If play continues clockwise, then the player to the right of 0 is player 1, and so on."

  L.p_ (L.b_ "Spy probabilities:") <> "The currently computed probability that a player is a spy, given the results of the mission the player is in, are listed. Initially, all players have a s/n probability of being a spy, where s is the total number of spies and n is the total number of players. The stategy for the Resistance is to always select the least probable players to be in missions (these players are shown as \"Mission Players\"."

  L.p_ (L.b_ "Voting:") <> "Only spies should propose a group that is not the set shown in \"Mission Players\". If a player does so, the majority (Resistance) must vote against the proposal, and the current player is known to be a spy. Record the failed vote. (Note that it is not possible to lose the game by failed votes since any Resistance player chooses the correct group for a mission."

  L.p_ (L.b_ "Missions:") <> "Assuming a mission is run, record the number of failed mission cards. The spy probabilities for the members of that group will be updated, as well as the others not in the group---we have new information about non-group members, since if fail cards were present, then a non-group member is less likely to be a spy)."

  L.p_ (L.b_ "Winning:") <> "Monte Carlo simulations suggest that the Resistance has at best around a 40% chance of winning. The chance of winning is similar regardless of the game size."

  L.h2_ [L.style_ "text-align:center"] "Viva la Resistance!"

missionForm :: Int -> L.Html ()
missionForm cards = do
  "Number select the number of *Fail* cards in mission:"
  L.form_ $ do
    mconcat (map cardButton [0..cards-1])
    submit
  where
  cardButton i = do
    L.input_ [L.type_ "radio", L.name_ "fails", L.value_ (textNum i)]
    L.toHtml (textNum i)

ul :: [L.Html ()] -> L.Html ()
ul ts = L.ul_ (mconcat (map L.li_ ts))

textNum :: Int -> TT.Text
textNum = TT.pack . show

submit :: L.Html ()
submit = L.input_ [L.type_ "submit", L.value_ "Submit"]

title :: L.Html ()
title = L.title_ "Resistance Game"

------------------------------------------------------------

------------------------------------------------------------
-- CSS

styles :: L.Html ()
styles = L.style_
 "h1 { \
 \background-color:black;\n\
 \    color:white;\n\
 \    text-align:center;\n\
 \    padding:5px;\n\
 \ }\n\
 \ section {\n\
 \   width:20%;\n\
 \   float:left;\n\
 \   border-style:solid;\n\
 \ }\n\
 \ aside {\n\
 \   width:70%;\n\
 \   float:right;\n\
 \   background-color:#eeeeee;\n\
 \   padding:20px;\n\
 \ }"

------------------------------------------------------------
-- Helpers

-- From number of fail cards to the set of mission cards.
toCards :: Config -> Game -> Int -> [MissionCard]
toCards c g f =
  replicate f Fail ++ replicate rst Success
  where
  rst = missionSize (totalPlayers c) (currentRnd g) - f
