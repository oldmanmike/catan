module Catan.Common
( Color (..)
, Coord
, Edge
, Vertex
, Token
) where

data Color = Blue | Red | Orange | White
  deriving (Show,Eq,Read,Ord)

type Coord = (Int,Int)
type Edge = (Coord,Coord)
type Vertex = (Coord,Coord,Coord)

type Token = Int
