-------------------------------------------------------------------------------
-- |
-- Copyright    : (C) 2015 Michael Carpenter
-- License      : GPL3
-- Maintainer   : Michael Carpenter <oldmanmike.dev@gmail.com>
-- Stability    : experimental
-- Portability  : GHC
--
-------------------------------------------------------------------------------
module Catan.Types
( Biome (..)
, Board (..)
, Catan (..)
, CatanInstance (..)
, Color (..)
, Coord
, DevCard (..)
, DevDeck
, Edge
, Hex(..)
, Player (..)
, Port
, Resource (..)
, Resources (..)
, Road
, Settlement (..)
, Token
, Vertex
) where

import Control.Monad
import Control.Monad.State
import Test.QuickCheck

data Biome = Desert
           | Fields
           | Forest
           | Hills
           | Mountains
           | Pasture
           deriving (Show,Eq,Read,Ord)

instance Arbitrary Biome where
  arbitrary = do
    n <- choose (0,4) :: Gen Int
    return $ case n of
      0 -> Fields
      1 -> Forest
      2 -> Hills
      3 -> Mountains
      4 -> Pasture

data Board = Board
  { hexs        :: [Hex]
  , settlements :: [Settlement]
  , roads       :: [Road]
  } deriving (Show,Eq,Read)


newtype Catan s a = C (State CatanInstance a)


data CatanInstance = CatanInstance
  { players       :: [Player]
  , resourceBank  :: Resources
  , deck          :: DevDeck
  , board         :: Board
  } deriving (Show,Eq,Read)


data Color = Blue | Red | Orange | White
  deriving (Show,Eq,Read,Ord)


type Coord = (Int,Int)


data DevCard = Soldier | VictoryPoints | Monopoly | RoadBuilding | YearOfPlenty
  deriving (Show,Eq,Read)


-- | The deck of development cards can be acurately represented by a linked list.
type DevDeck = [DevCard]


type Edge = (Coord,Coord)


data Hex = Hex
  { coord     :: Coord
  , token     :: Token
  , biome     :: Biome
  , robber    :: Bool
  } deriving (Show,Eq,Ord,Read)

instance Arbitrary Hex where
  arbitrary = do
    let c = (liftM2 (,) (choose (-2,2) :: Gen Int) (choose (-2,2) :: Gen Int)) :: Gen Coord
    let t = choose (1,11) :: Gen Token
    let b = arbitrary :: Gen Biome
    let r = arbitrary :: Gen Bool
    liftM4 (Hex) c t b r

data Player = Player
  { turnOrder       :: Int -- ^ When this players turn occurs relative to other players
  , name            :: String -- ^ Player nick used ingame
  , color           :: Color -- ^ Player's color
  , hand            :: [DevCard] -- ^ Development cards player has in hand
  , coffers         :: Resources -- ^ Resource cards player has in hand
  , activeCards     :: [DevCard] -- ^ Development cards that are in-play
  , victoryPoints   :: Int -- ^ Player's calculated victory points
  } deriving (Show,Eq,Read)


type Port = Vertex

data Resource = Brick | Grain | Lumber | Ore | Wool | NoResource
  deriving (Show,Read,Eq,Ord)


data Resources = Resources
  { bricks     :: Int
  , grain      :: Int
  , lumber     :: Int
  , ore        :: Int
  , wool       :: Int
  } deriving (Show,Eq,Read,Ord)


type Road = (Color,Edge)


data Settlement = Settlement
  { getVert           :: Vertex
  , localResources    :: [(Token,Resource)]
  , owner             :: Color
  , city              :: Bool
  } deriving (Eq,Ord,Read,Show)


type Token = Int


type Vertex = (Coord,Coord,Coord)
