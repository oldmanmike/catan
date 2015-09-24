import Catan

import Test.QuickCheck

prop_moveRobber :: [Hex] -> Hex -> Property
prop_moveRobber h targetHex = True ==>
    h == moveRobber (moveRobber h targetHex) oldRobberHex
  where oldRobberHex = head $ filter robber h

main :: IO ()
main = quickCheck prop_moveRobber
