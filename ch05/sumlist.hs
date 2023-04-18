import Data.Foldable ( traverse_ )
import Control.Monad.State ( modify', MonadState(put, get), execState, State )

addItem :: Integer -> State Integer ()
addItem n = do
  s <- get
  put (s + n)

addItem' :: Integer -> State Integer ()
addItem' n = modify' (+n)

sumList :: [Integer] -> State Integer ()
sumList = traverse_ addItem

answer :: Integer
answer = execState (sumList [1..100]) 0

main :: IO ()
main = print answer
