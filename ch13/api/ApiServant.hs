{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}

import GHC.Generics ( Generic )
import Data.Aeson ( ToJSON )
import Servant ( Proxy(..), serve, type (:<|>)(..), Capture, JSON, type (:>), Get, Server, Application )
import Servant.HTML.Blaze ( HTML )
import qualified Text.Blaze.Html5 as H
import Network.Wai.Handler.Warp ( run )

data Rating = Bad | Good | Great
  deriving (Show, Generic, ToJSON)

data ServiceStatus = Ok | Down
  deriving (Show, Generic, ToJSON)

type BookID = Int

type BookInfoAPI = Get '[JSON] ServiceStatus
                   :<|> "title" :> Capture "id" BookID :> Get '[HTML] H.Html
                   :<|> "year" :> Capture "id" BookID :> Get '[JSON] Int
                   :<|> "rating" :> Capture "id" BookID :> Get '[JSON] Rating

impl :: Server BookInfoAPI
impl = pure Ok
       :<|> title
       :<|> year
       :<|> rating
  where
    title _ = pure $ H.toHtml $ H.b "Haskell in Depth"
    year _ = pure 2020
    rating _ = pure Great

app :: Application
app = serve (Proxy :: Proxy BookInfoAPI) impl

main :: IO ()
main = run 8081 app
