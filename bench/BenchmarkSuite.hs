import Catan
import Data.List
import System.Random
import System.Random.Shuffle

main :: IO ()
main = do
    gen1 <- newStdGen
    gen2 <- newStdGen
    gen3 <- newStdGen
    let testHexs = sort $ zip4 clst tlst blst rlst
            where clst = shuffle' coordList 19 gen1
                  tlst = (7:) $ shuffle' tokenList 18 gen2
                  blst = (Desert:) $ shuffle' biomeList 18 gen3
                  rlst = True:(take 18 $ repeat False)
    print testHexs
