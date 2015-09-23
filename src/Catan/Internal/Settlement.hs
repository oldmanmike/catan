-------------------------------------------------------------------------------
-- |
-- Copyright    : (C) 2015 Michael Carpenter
-- License      : GPL3
-- Maintainer   : Michael Carpenter <oldmanmike.dev@gmail.com>
-- Stability    : experimental
-- Portability  : GHC
--
-------------------------------------------------------------------------------
module Catan.Internal.Settlement
( upgradeToCity
--, testSettlements
) where

import Catan.Internal.Resource
import Catan.Types

upgradeToCity :: [Settlement] -> Settlement -> Either String [Settlement]
upgradeToCity slst s = do
  if elem s slst
    then Right ((s {city = True}):(filter (\i -> (getVert i) /= (getVert s)) slst))
    else Left "Error: Settlement does not exist!"

{-
testSettlements :: Settlements
testSettlements = V.fromList [ (Settlement
                      ((0,-2),(1,-2),(0,-1))
                      {-
                      [ (Hex ( 0,-2) 6 Forest False)
                      , (Hex ( 1,-2) 5 Fields False)
                      , (Hex ( 0,-1) 3 Pasture False)]
                      -}
                      Blue
                      False)
                  , (Settlement
                      ((1,-2),(0,-1),(1,-1))
                      {-
                      [ (Hex (1,-2) 5 Fields False)
                      , (Hex (0,-1) 3 Pasture False)
                      , (Hex (1,-1) 8 Hills False)]
                      -}
                      Blue
                      False)
                  , (Settlement
                      ((1,-2),(1,-1),(2,-2))
                      {-
                      [ (Hex (1,-2) 5 Fields False)
                      , (Hex (1,-1) 8 Hills False)
                      , (Hex (2,-2) 9 Mountains False)]
                      -}
                      Blue
                      False)
                  , (Settlement
                      ((-1,2),(0,2),(0,1))
                      {-
                      [ (Hex (-1,2) 4 Forest False)
                      , (Hex (0,2) 8 Pasture False)
                      , (Hex (0,1) 10 Fields False)]
                      -}
                      Red
                      False)
                  ]
-}
