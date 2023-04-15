{-# LANGUAGE OverloadedStrings #-}

import TextShow ( fromString, printT, TextShow(showb) )
import Person ( Person(..), homer, spj )

instance TextShow Person where
  showb (Person name Nothing) = fromString name
  showb (Person name (Just age)) =
    fromString name <> " (" <> showb age <> ")"

main :: IO ()
main = do
  printT homer
  printT spj
