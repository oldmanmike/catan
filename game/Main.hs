module Main where

import Catan
import Data.List
import System.Random
import System.Random.Shuffle

main :: IO ()
main = do
    dice <- getStdGen
    let roll = rollDice dice
    gen1 <- newStdGen
    gen2 <- newStdGen
    gen3 <- newStdGen
    let tiles = sort $ zip3 clst tlst hlst
            where clst = shuffle' coords 19 gen1
                  tlst = (7:) $ shuffle' tokens 18 gen2
                  hlst = (Desert:) $ shuffle' hexs 18 gen3
    putStrLn $ show $ sort tiles
