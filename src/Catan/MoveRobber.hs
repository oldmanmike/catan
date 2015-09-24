{-# LANGUAGE PartialTypeSignatures #-}
-------------------------------------------------------------------------------
-- |
-- Copyright    : (C) 2015 Michael Carpenter
-- License      : GPL3
-- Maintainer   : Michael Carpenter <oldmanmike.dev@gmail.com>
-- Stability    : experimental
-- Portability  : GHC
--
-------------------------------------------------------------------------------
module Catan.MoveRobber (moveRobber) where

import Data.List
import Catan.Types

moveRobber :: [Hex] -> Hex -> [Hex]
moveRobber [] targetHex = []
moveRobber h targetHex = sort $ (newRobberHex:(nowSafeHex:otherHexs))
  where oldRobberHex  = head $ filter robber h
        newRobberHex  = Hex (coord targetHex)
                            (token targetHex)
                            (biome targetHex)
                            (True)
        nowSafeHex    = Hex (coord oldRobberHex)
                            (token oldRobberHex)
                            (biome oldRobberHex)
                            (False)
        otherHexs = filter (\x -> (x/=targetHex)&&(x/=oldRobberHex)) h



