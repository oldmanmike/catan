-------------------------------------------------------------------------------
-- |
-- Copyright    : (C) 2015 Michael Carpenter
-- License      : GPL3
-- Maintainer   : Michael Carpenter <oldmanmike.dev@gmail.com>
-- Stability    : experimental
-- Portability  : GHC
--
-------------------------------------------------------------------------------
module Catan.Build
( buildRoad
, buildSettlement
, buildCity
) where

import Catan.Types
import Catan.Internal.Resource
import Catan.Internal.Board

buildRoad :: Board -> Player -> Either String (Board,Player)
buildRoad b p = undefined

buildSettlement :: [Hex] -> [Settlement] -> Color -> Vertex -> Either String [Settlement]
buildSettlement hexs s color vert = if notElem (Settlement vert adjResources color False) s
                            then Right ((Settlement vert adjResources color False):s)
                            else Left "Error: Settlement already exists!"
  where adjResources = map (\x -> ((token x),(biomeToResource $ biome x))) $ getAdjacentHexs vert hexs


buildCity :: Board -> Player -> Vertex -> Either String (Board,Player)
buildCity b p v = undefined
