-------------------------------------------------------------------------------
-- |
-- Copyright    : (C) 2015 Michael Carpenter
-- License      : GPL3
-- Maintainer   : Michael Carpenter <oldmanmike.dev@gmail.com>
-- Stability    : experimental
-- Portability  : GHC
--
-------------------------------------------------------------------------------
module Catan.Resource
( Resource(..)
, Resources(..)
, biomeToResource
) where

import Catan.Common

data Resource = Brick | Grain | Lumber | Ore | Wool
  deriving (Show,Read,Eq,Ord)


data Resources = Resources
  { bricks     :: Int
  , grain      :: Int
  , lumber     :: Int
  , ore        :: Int
  , wool       :: Int
  } deriving (Show,Eq,Read,Ord)


biomeToResource :: Biome -> Resource
biomeToResource Fields    = Grain
biomeToResource Forest    = Lumber
biomeToResource Hills     = Brick
biomeToResource Mountains = Ore
biomeToResource Pasture   = Wool
biomeToResource Desert    = undefined

