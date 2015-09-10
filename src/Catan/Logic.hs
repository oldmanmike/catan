-------------------------------------------------------------------------------
-- |
-- Copyright        : (c) 2015 Michael Carpenter
-- License          : GPL3
-- Maintainer       : Michael Carpenter <oldmanmike.dev@gmail.com>
-- Stability        : provisional
-- Portability      : portable
--
-------------------------------------------------------------------------------
module Catan.Logic
( Biome(..)
, Board(..)
, Color(..)
, Hex(..)
, Player(..)
, ResourcePool(..)
, Settlement(..)
, coordList
, tokenList
, colorList
, biomeList
, testMap
, testSettlements
, devCardList
, rollDice
, isToken
, groupByFst
, groupBySnd
, zip2Hex
, biomeToResource
, settlementToHex
, produceResources
) where

import Data.List
import System.Random

-------------------------------------------------------------------------------
-- Core Data Structures
-------------------------------------------------------------------------------


data Player = Player
  { turnOrder       :: Int -- ^ When this players turn occurs relative to other players
  , name            :: String -- ^ Player nick used ingame
  , color           :: Color -- ^ Player's color
  , hand            :: [DevCard] -- ^ Development cards player has in hand
  , coffers         :: ResourcePool -- ^ Resource cards player has in hand
  , activeCards     :: [DevCard] -- ^ Development cards that are in-play
  , victoryPoints   :: Int -- ^ Player's calculated victory points
  } deriving (Show,Eq,Read)


data Board = Board
  { hexs        :: [Hex]
  , settlements :: [Settlement]
  , roads       :: [Road]
  } deriving (Show,Eq,Read)


-- | The deck of development cards can be acurately represented by a linked list.
type DevDeck = [DevCard]


data ResourcePool = ResourcePool
  { bricks     :: Int
  , grain      :: Int
  , lumber     :: Int
  , ore        :: Int
  , wool       :: Int
  } deriving (Show,Eq,Read)


data Settlement = Settlement
  { vertex        :: (Coord,Coord,Coord)
  , adjacentHexs  :: [Hex]
  , faction       :: Color
  , city          :: Bool
  } deriving (Show,Eq,Read)


data Hex = Hex
  { coord     :: Coord
  , token     :: Token
  , biome     :: Biome
  , robber    :: Bool
  } deriving (Show,Eq,Ord,Read)


-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------


data ResourceCard = Brick | Grain | Lumber | Ore | Wool
  deriving (Show,Read,Eq)


data DevCard = Soldier | VictoryPoints | Monopoly | RoadBuilding | YearOfPlenty
  deriving (Show,Eq,Read)


data Color = Blue | Red | Orange | White
  deriving (Show,Eq,Read,Ord)


type Coord = (Int,Int)


data Biome = Desert
           | Fields
           | Forest
           | Hills
           | Mountains
           | Pasture
           deriving (Show,Eq,Read,Ord)


type Road = (Coord,Coord)


type Token = Int


-------------------------------------------------------------------------------
-- Consts
-------------------------------------------------------------------------------

coordList :: [Coord]
coordList = [          ( 0,-2),( 1,-2),( 2,-2)
            ,      (-1,-1),( 0,-1),( 1,-1),( 2,-1)
            , (-2, 0),(-1, 0),( 0, 0),( 1, 0),( 2, 0)
            ,      (-2, 1),(-1, 1),( 0, 1),( 1, 1)
            ,          (-2, 2),(-1, 2),( 0, 2)         ]


tokenList :: [Token]
tokenList = [2,3,3,4,4,5,5,6,6,8,8,9,9,10,10,11,11,12]


colorList :: [Color]
colorList = [Blue,Red,Orange,White]


biomeList :: [Biome]
biomeList = replicate 4 Fields
        ++ replicate 4 Forest
        ++ replicate 3 Hills
        ++ replicate 3 Mountains
        ++ replicate 4 Pasture


devCardList :: DevDeck
devCardList = replicate 14 Soldier
            ++ replicate 5 VictoryPoints
            ++ replicate 2 Monopoly
            ++ replicate 2 RoadBuilding
            ++ replicate 2 YearOfPlenty


testMap :: [Hex]
testMap = [ (Hex ( 0,-2) 6 Forest False)
          , (Hex ( 1,-2) 5 Fields False)
          , (Hex ( 2,-2) 9 Mountains False)
          , (Hex (-1,-1) 4 Hills False)
          , (Hex ( 0,-1) 3 Pasture False)
          , (Hex ( 1,-1) 8 Hills False)
          , (Hex ( 2,-1) 10 Mountains False)
          , (Hex (-2, 0) 6 Mountains False)
          , (Hex (-1, 0) 5 Pasture False)
          , (Hex ( 0, 0) 7 Desert True)
          , (Hex ( 1, 0) 9 Fields False)
          , (Hex ( 2, 0) 12 Pasture False)
          , (Hex (-2, 1) 3 Hills False)
          , (Hex (-1, 1) 2 Forest False)
          , (Hex ( 0, 1) 10 Fields False)
          , (Hex ( 1, 1) 11 Forest False)
          , (Hex (-2, 2) 11 Fields False)
          , (Hex (-1, 2) 4 Forest False)
          , (Hex ( 0, 2) 8 Pasture False)]


testSettlements :: [Settlement]
testSettlements = [ (Settlement
                      ((0,-2),(1,-2),(0,-1))
                      [ (Hex ( 0,-2) 6 Forest False)
                      , (Hex ( 1,-2) 5 Fields False)
                      , (Hex ( 0,-1) 3 Pasture False)]
                      Blue
                      False)
                  , (Settlement
                      ((1,-2),(0,-1),(1,-1))
                      [ (Hex (1,-2) 5 Fields False)
                      , (Hex (0,-1) 3 Pasture False)
                      , (Hex (1,-1) 8 Hills False)]
                      Blue
                      False)
                  , (Settlement
                      ((1,-2),(1,-1),(2,-2))
                      [ (Hex (1,-2) 5 Fields False)
                      , (Hex (1,-1) 8 Hills False)
                      , (Hex (2,-2) 9 Mountains False)]
                      Blue
                      False)
                  , (Settlement
                      ((-1,2),(0,2),(0,1))
                      [ (Hex (-1,2) 4 Forest False)
                      , (Hex (0,2) 8 Pasture False)
                      , (Hex (0,1) 10 Fields False)]
                      Red
                      False)
                  ]


-------------------------------------------------------------------------------
-- Functions
-------------------------------------------------------------------------------


rollDice :: StdGen -> Int
rollDice = sum . take 2 . randomRs (1,6)


isToken :: Eq b => b -> (a,b,c) -> Bool
isToken t (_,x,_) = t == x


groupByFst :: Eq a => (a,b) -> (a,b) -> Bool
groupByFst (i,_) (j,_) = i == j


groupBySnd :: Eq b => (a,b) -> (a,b) -> Bool
groupBySnd (_,i) (_,j) = i == j


zip2Hex :: [Coord] -> [Token] -> [Biome] -> [Bool] -> [Hex]
zip2Hex [] [] [] [] = []
zip2Hex [] _ _ _ = []
zip2Hex _ [] _ _ = []
zip2Hex _ _ [] _ = []
zip2Hex _ _ _ [] = []
zip2Hex (c:cs) (t:ts) (b:bs) (r:rs) = (Hex c t b r):(zip2Hex cs ts bs rs)


biomeToResource :: Biome -> ResourceCard
biomeToResource Fields    = Grain
biomeToResource Forest    = Lumber
biomeToResource Hills     = Brick
biomeToResource Mountains = Ore
biomeToResource Pasture   = Wool
biomeToResource Desert    = undefined


settlementToHex :: [Settlement] -> [(Color,[Hex])]
settlementToHex [] = []
settlementToHex (x:xs) = do
  if city x
    then (faction x,adjacentHexs x):((faction x,adjacentHexs x):settlementToHex xs)
    else (faction x,adjacentHexs x):(settlementToHex xs)


--produceResources :: [Settlement] -> [[
produceResources settlementList = do
    let colorAndHex = settlementToHex settlementList
    let groupedByColor = groupBy groupByFst colorAndHex
    let joinedGroupedHexs = f groupedByColor
    let earnings = map.map
    return $ joinedGroupedHexs
  where f = map (map (\x -> (token x,biome x)).concat.(map snd))

