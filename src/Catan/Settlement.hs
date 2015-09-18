-------------------------------------------------------------------------------
-- |
-- Copyright    : (C) 2015 Michael Carpenter
-- License      : GPL3
-- Maintainer   : Michael Carpenter <oldmanmike.dev@gmail.com>
-- Stability    : experimental
-- Portability  : GHC
--
-------------------------------------------------------------------------------
module Catan.Settlement
( Settlement(..)
, Settlements
, build
, upgradeToCity
--, testSettlements
) where

import qualified Data.Vector as V

import Catan.Common
import Catan.Resource


data Settlement = Settlement
  { getVert           :: Vertex
  , localResources    :: [(Token,Resource)]
  , owner             :: Color
  , city              :: Bool
  } deriving (Eq,Ord,Read,Show)


type Settlements = V.Vector Settlement


build :: [Hex] -> Settlements -> Color -> Vertex -> Either String Settlements
build hexs s color vert = if V.notElem (Settlement vert adjResources color False) s
                            then Right $ V.cons (Settlement vert adjResources color False) s
                            else Left "Error: Settlement already exists!"
  where adjResources = map (\x -> ((token x),(biomeToResource $ biome x))) $ getAdjacentHexs vert hexs


upgradeToCity :: Settlements -> Settlement -> Either String Settlements
upgradeToCity slst s = do
  if V.elem s slst
    then Right (V.cons (s {city = True})
                       (V.filter (\i -> (getVert i) /= (getVert s)) slst))
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
