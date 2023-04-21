import Control.Monad.ST ( ST, runST )
import Data.STRef ( STRef, newSTRef, readSTRef )

comp1 :: ST s (STRef s Int)
comp1 = newSTRef 42

comp2 :: STRef s Int -> ST s Int
comp2 = readSTRef

main :: IO ()
main = print $ runST (comp1 >>= comp2)
