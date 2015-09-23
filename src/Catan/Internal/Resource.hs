-------------------------------------------------------------------------------
-- |
-- Copyright    : (C) 2015 Michael Carpenter
-- License      : GPL3
-- Maintainer   : Michael Carpenter <oldmanmike.dev@gmail.com>
-- Stability    : experimental
-- Portability  : GHC
--
-------------------------------------------------------------------------------
module Catan.Internal.Resource (biomeToResource) where

import Catan.Types

biomeToResource :: Biome -> Resource
biomeToResource Fields    = Grain
biomeToResource Forest    = Lumber
biomeToResource Hills     = Brick
biomeToResource Mountains = Ore
biomeToResource Pasture   = Wool
biomeToResource Desert    = NoResource
