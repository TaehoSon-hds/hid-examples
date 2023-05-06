{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

import GHC.Generics ( Generic )
import Data.Aeson ( encode, ToJSON )

newtype Age = Age {age :: Int}
--  deriving (Show, Generic, Num, ToJSON)
  deriving stock (Show, Generic)
  deriving newtype (Num)
  deriving anyclass (ToJSON)

theAge :: Age
theAge = 33

main :: IO ()
main = do
  print theAge
  print $ encode theAge
