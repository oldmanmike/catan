module Catan.Settlement
( Settlement(..)
, Settlements
, buildSettlement
, testSettlements
) where

import qualified Data.Vector as V

import Catan.Common

data Settlement = Settlement
  { coords  :: Vertex
  , owner   :: Color
  , city    :: Bool
  } deriving (Eq,Ord,Read,Show)

type Settlements = V.Vector Settlement

buildSettlement :: Settlements
                -> Color
                -> Vertex
                -> Settlements
buildSettlement s c v = V.cons (Settlement v c False) s

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

