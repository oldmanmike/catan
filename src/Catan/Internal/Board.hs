-------------------------------------------------------------------------------
-- |
-- Copyright    : (C) 2015 Michael Carpenter
-- License      : GPL3
-- Maintainer   : Michael Carpenter <oldmanmike.dev@gmail.com>
-- Stability    : experimental
-- Portability  : GHC
--
-------------------------------------------------------------------------------
module Catan.Internal.Board (getAdjacentHexs) where

import Catan.Types

getAdjacentHexs :: Vertex -> [Hex] -> [Hex]
getAdjacentHexs (x,y,z) hexs = filter (\i -> (((coord i)==x)||((coord i)==y)||((coord i)==z))) hexs
