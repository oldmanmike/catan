-------------------------------------------------------------------------------
-- |
-- Copyright    : (C) 2015 Michael Carpenter
-- License      : GPL3
-- Maintainer   : Michael Carpenter <oldmanmike.dev@gmail.com>
-- Stability    : experimental
-- Portability  : GHC
--
-------------------------------------------------------------------------------

module Catan.Common
( Biome (..)
, Color (..)
, Coord
, Edge
, Hex(..)
, Token
, Vertex
, getAdjacentHexs
) where


data Biome = Desert
           | Fields
           | Forest
           | Hills
           | Mountains
           | Pasture
           deriving (Show,Eq,Read,Ord)


data Color = Blue | Red | Orange | White
  deriving (Show,Eq,Read,Ord)

type Coord = (Int,Int)
type Edge = (Coord,Coord)
type Vertex = (Coord,Coord,Coord)


data Hex = Hex
  { coord     :: Coord
  , token     :: Token
  , biome     :: Biome
  , robber    :: Bool
  } deriving (Show,Eq,Ord,Read)


type Token = Int


getAdjacentHexs :: Vertex -> [Hex] -> [Hex]
getAdjacentHexs (x,y,z) hexs = filter (\i -> (((coord i)==x)||((coord i)==y)||((coord i)==z))) hexs
