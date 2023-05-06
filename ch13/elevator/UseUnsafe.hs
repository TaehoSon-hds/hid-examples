import Control.Monad ( foldM_ )
import System.Environment ( getArgs )

import Elevator.Unsafe ( Elevator(Elevator), Floor(Floor), DoorState(Closed), call )

gfElevator :: Elevator
gfElevator = Elevator (Floor 0) Closed

main :: IO ()
main = do
    floors <- map read <$> getArgs
    foldM_ traceTo gfElevator (map Floor floors)
  where
    prt el = print el >> pure el
    traceTo el fl = call fl el >>= prt
