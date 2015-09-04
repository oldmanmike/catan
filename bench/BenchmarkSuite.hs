import Catan
import Criterion.Main
import Data.List
import System.Random
import System.Random.Shuffle

isToken :: Eq b => b -> (a,b,c) -> Bool
isToken t (_,x,_) = t == x

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
    print (sort tiles)
    defaultMain [
        bgroup "catan" [ bench "2"  $ whnf (filter (isToken 2)) tiles
                       , bench "3"  $ whnf (filter (isToken 3)) tiles
                       , bench "4"  $ whnf (filter (isToken 4)) tiles
                       , bench "5"  $ whnf (filter (isToken 5)) tiles
                       , bench "6"  $ whnf (filter (isToken 6)) tiles
                       , bench "7"  $ whnf (filter (isToken 7)) tiles
                       , bench "8"  $ whnf (filter (isToken 8)) tiles
                       , bench "9"  $ whnf (filter (isToken 9)) tiles
                       , bench "10" $ whnf (filter (isToken 10)) tiles
                       , bench "11" $ whnf (filter (isToken 11)) tiles
                       ]
        ]
