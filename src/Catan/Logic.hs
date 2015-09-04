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
    Color(..),
    Hex(..),
    Resources(..),
    Settlement(..),
    coords,
    tokens,
    colors,
    hexs,
    rollDice,
    isToken
    --buildSettlement,
    --setRoad,
    --isAdjacent
) where

import System.Random

-------------------------------------------------------------------------------
-- Nouns
-------------------------------------------------------------------------------


data Color = Blue
           | Red
           | Orange
           | White
           deriving (Show,Eq,Read)


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


data Settlement = Settlement
    { coord       :: (Int,Int)
    , adjacentHex :: (Hex,Hex,Hex)
    , color       :: Color
    , city        :: Bool
    } deriving (Show,Eq,Read)
                    

coords :: [(Int,Int)]
coords = [          ( 0,-2),( 1,-2),( 2,-2)
         ,      (-1,-1),( 0,-1),( 1,-1),( 2,-1)
         , (-2, 0),(-1, 0),( 0, 0),( 1, 0),( 2, 0)
         ,      (-2, 1),(-1, 1),( 0, 1),( 1, 1)
         ,          (-2, 2),(-1, 2),( 0, 2)         ]


tokens :: [Int]
tokens = [2,3,3,4,4,5,5,6,6,8,8,9,9,10,10,11,11,12]

colors :: [Color]
colors = [Blue,Red,Orange,White]

hexs :: [Hex]
hexs = [ Fields,Fields,Fields,Fields
       , Forest,Forest,Forest,Forest
       , Hills,Hills,Hills
       , Mountains,Mountains,Mountains
       , Pasture,Pasture,Pasture,Pasture]


-------------------------------------------------------------------------------
-- Verbs
-------------------------------------------------------------------------------


rollDice :: StdGen -> Int
rollDice = sum . take 2 . randomRs (1,6)


isToken :: Eq b => b -> (a,b,c) -> Bool
isToken t (_,x,_) = t == x


-------------------------------------------------------------------------------
-- Helpers
-------------------------------------------------------------------------------

{-
setSettlement :: VertexMap -> Player -> (Coord,Coord,Coord) -> Either String VertexMap
setSettlement m p (a,b,c) = if x && y && z
                               then if oa && ob && oc
                                       then if M.notMember
                               else Left $ "Error: Not a valid vetex!"
    where x = isAdjacent (a,b)
          y = isAdjacent (b,c)
          z = isAdjacent (a,c)
          oa = M.notMember a m
          ob = M.notMember b m
          oc = M.notMember c m

setRoad :: EdgeMap -> Player -> (Coord,Coord) -> Either String EdgeMap
setRoad m p k = if isAdjacent k
                    then if M.notMember k m
                            then Right $ M.insert k p m
                            else Left $ "Error: Edge already occupied!"
                    else Left "Error: Not a valid edge!"

isAdjacent :: (Coord,Coord) -> Bool
isAdjacent ((x1,y1),(x2,y2)) = if ((abs (x1 - x2)) + (abs (y1 - y2))) == 1
                                    then True
                                    else False

-}
