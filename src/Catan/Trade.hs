-------------------------------------------------------------------------------
-- |
-- Copyright    : (C) 2015 Michael Carpenter
-- License      : GPL3
-- Maintainer   : Michael Carpenter <oldmanmike.dev@gmail.com>
-- Stability    : experimental
-- Portability  : GHC
--
-------------------------------------------------------------------------------
module Catan.Trade
( tradeDomestic
, tradeMaritime
) where

import Catan.Types

tradeDomestic :: Player -> Resources -> Player -> Resources -> Player
tradeDomestic player request trader response = undefined

tradeMaritime :: Player -> Port -> Player
tradeMaritime player port = undefined
