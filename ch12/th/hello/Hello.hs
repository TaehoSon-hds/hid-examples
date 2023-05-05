{-# LANGUAGE TemplateHaskellQuotes #-}

module Hello where
import Language.Haskell.TH ( Exp, Q )

hello :: Q Exp
hello = [| putStrLn "Hello world" |]
