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

{-
moveRobber :: [Hex] -> Hex -> Either String [Hex]
moveRobber [] targetHex = Left "Error: Empty hex list"
moveRobber h targetHex
  | length h < 19 = Left "Error: Illegally small hex number!"
  | length h > 19 = Left "Error: Illegally large hex number!"
  | notElem targetHex h = Left "Error: Target hex does not exist!"
  | length (filter ((==True).robber) h) /= 1 = Left "Error: More than one robber!"
  | (length h == 19) && (elem targetHex h) = Right $ sort $ (newRobberHex:(nowSafeHex:otherHexs))
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
-}
