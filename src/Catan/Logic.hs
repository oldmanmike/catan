-------------------------------------------------------------------------------
-- |
-- Copyright        : (c) 2015 Michael Carpenter
-- License          : GPL3
-- Maintainer       : Michael Carpenter <oldmanmike.dev@gmail.com>
-- Stability        : provisional
-- Portability      : portable
--
-------------------------------------------------------------------------------
module Catan.Logic (
    Hex(..),
    Resources(..),
    coords,
    tokens,
    hexs,
    rollDice,
    buildSettlement
) where

import Data.List
import System.Random

-------------------------------------------------------------------------------
-- Nouns
-------------------------------------------------------------------------------

data Hex = Desert
         | Fields
         | Forest
         | Hills
         | Mountains
         | Pasture 
         deriving (Show,Eq,Read,Ord)

data Resources = Resources { bricks     :: Int
                           , grain      :: Int
                           , lumber     :: Int
                           , ore        :: Int
                           , wool       :: Int
                           } deriving (Show,Eq,Read)

type Seed = Int

coords :: [Coord]
coords = [          ( 0,-2),( 1,-2),( 2,-2)
         ,      (-1,-1),( 0,-1),( 1,-1),( 2,-1)
         , (-2, 0),(-1, 0),( 0, 0),( 1, 0),( 2, 0)
         ,      (-2, 1),(-1, 1),( 0, 1),( 1, 1)
         ,          (-2, 2),(-1, 2),( 0, 2)         ]

tokens :: [Int]
tokens = [2,3,3,4,4,5,5,6,6,8,8,9,9,10,10,11,11,12]

hexs :: [Hex]
hexs = [ Fields,Fields,Fields,Fields
       , Forest,Forest,Forest,Forest
       , Hills,Hills,Hills
       , Mountains,Mountains,Mountains
       , Pasture,Pasture,Pasture,Pasture]

type Coord = (Int,Int)

type Board = [(Coord,Int,Hex)]

-------------------------------------------------------------------------------
-- Verbs
-------------------------------------------------------------------------------



rollDice :: StdGen -> Int
rollDice = sum . take 2 . randomRs (1,6)

buildSettlement :: Resources -> Either String Resources
buildSettlement r = do
    if ((bricks r > 0) && (grain r > 0) && (lumber r > 0) && (wool r > 0))
        then Right Resources { bricks = (bricks r - 1)
                             , grain = (grain r - 1)
                             , lumber = (lumber r - 1)
                             , wool = (wool r - 1)
                             , ore = ore r}
        else Left "Error: Can't afford settlement!"
