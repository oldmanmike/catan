module Main where

import Catan
import Data.Char
import Data.List
import Control.Monad
import System.IO
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
    hSetBuffering stdin NoBuffering
    putStrLn "How many players?"
    numPlayers <- liftM digitToInt getChar
    putStr "\n"
    putStrLn "Please enter player's names:"
    players <- replicateM numPlayers getLine
    let playerColors = if (numPlayers <= 4) && (numPlayers > 0)
                          then Right $ take numPlayers colors
                          else Left "Error: Can only have 1-4 players!"
    print $ liftM (zip players) playerColors
    print (sort tiles)
