{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE StandaloneDeriving  #-}

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.ToField

import GHC.Generics (Generic)
import Data.Int (Int64)
import Data.Maybe (catMaybes)
import qualified Data.ByteString.Char8 as B
import Data.Text (Text)
import Data.Text.IO

import Prelude hiding (putStr, putStrLn)

import TextShow
import FilmInfo

deriving instance Generic FilmId
deriving newtype instance FromField FilmId
deriving newtype instance ToField FilmId

deriving instance Generic FilmLength
deriving newtype instance FromField FilmLength

deriving instance Generic CatId
deriving newtype instance FromField CatId
deriving newtype instance ToField CatId

instance FromField Rating where
  fromField f Nothing = returnError UnexpectedNull f ""
  fromField f (Just bs) =
    case toMaybeRating bs of
      Nothing -> returnError ConversionFailed f (B.unpack bs)
      Just r -> pure r

deriving instance Generic FilmInfo
deriving instance FromRow FilmInfo

allFilms :: Connection -> IO [FilmInfo]
allFilms conn = query_ conn select
  where
    select = "SELECT film_id, title, description, length, rating FROM film"

totalFilmsNumber :: Connection -> IO Int64
totalFilmsNumber conn = do
  [Only cnt] <- query_ conn "SELECT count(*) FROM film"
  pure cnt

findFilm :: Connection -> Text -> IO (Maybe FilmInfo)
findFilm conn filmTitle = do
  res <- query conn select (Only filmTitle)
  case res of
    [film] -> pure $ Just film
    _ -> pure $ Nothing
  where
    select = "SELECT film_id, title, description, length, rating"
             <> " FROM film"
             <> " WHERE title=?"

filmsLonger :: Connection -> FilmLength -> IO [FilmInfo]
filmsLonger conn (FilmLength len) = query conn select (Only len)
  where
    select = "SELECT film_id, title, description, length, rating"
             <> " FROM film"
             <> " WHERE length >= ?"

filmsCategories :: Connection -> [Text] -> IO [FilmCategories]
filmsCategories conn films = catMaybes <$> mapM runSingle films
  where
    select = "SELECT category.name"
             <> " FROM film"
             <> " JOIN film_category USING (film_id)"
             <> " JOIN category USING (category_id)"
             <> " WHERE title = ?"
    runSingle filmTitle = do
      mfilm <- findFilm conn filmTitle
      case mfilm of
        Nothing -> pure Nothing
        Just film -> do
          cats <- query conn select (Only filmTitle)
          pure $ Just $ FilmCategories film (map head cats)

setRating :: Connection -> Rating -> Text -> IO Int64
setRating conn fRating filmTitle =
  execute conn "UPDATE film SET rating = ? WHERE title = ?"
          (fromRating fRating :: Text, filmTitle)

newCategory :: Connection -> Text -> IO CatId
newCategory conn catName = do
    [Only r] <- query conn insert (Only catName)
    pure r
  where
    insert = "INSERT INTO category (name) VALUES (?) RETURNING category_id"

catIdByName :: Connection -> Text -> IO (Maybe CatId)
catIdByName conn catName = do
  res <- query conn "SELECT category_id FROM category WHERE name=?" (Only catName)
  case res of
    [] -> pure Nothing
    ([cid]:_) -> pure $ Just cid
    _ -> error "should not happen"

findOrAddCategory :: Connection -> Text -> IO CatId
findOrAddCategory conn catName = do
  cats <- catIdByName conn catName
  case cats of
    Nothing -> newCategory conn catName
    Just cid -> pure cid

filmIdByTitle :: Connection -> Text -> IO (Maybe FilmId)
filmIdByTitle conn filmTitle = do
  res <- query conn "SELECT film_id FROM film WHERE title=?" (Only filmTitle)
  case res of
    [] -> pure Nothing
    ([fid]:_) -> pure $ Just fid
    _ -> error "should not happen"

isAssigned :: Connection -> CatId -> FilmId -> IO Bool
isAssigned conn cid fid = do
  [Only res] <- query conn ("SELECT count(category_id) FROM film_category"
                            <> " WHERE category_id = ? AND film_id= ?")
                     (cid, fid)
  pure $ res > (0 :: Int64)

assignCategory' :: Connection -> CatId -> FilmId -> IO Int64
assignCategory' conn cid fid =
  execute conn "INSERT INTO film_category (category_id, film_id) VALUES (?, ?)"
          (cid, fid)

assignCategory :: Connection -> Text -> Text -> IO Int64
assignCategory conn catName filmTitle = do
  cid <- findOrAddCategory conn catName
  mFilmId <- filmIdByTitle conn filmTitle
  case mFilmId of
    Nothing -> pure 0
    Just fid -> go cid fid
 where
   go cid fid = do
     b <- isAssigned conn cid fid
     case b of
       True -> pure 0
       False -> assignCategory' conn cid fid

unassignCategory :: Connection -> Text -> Text -> IO Int64
unassignCategory conn catName filmTitle =
  execute conn
     ("DELETE FROM film_category"
      <> " USING film, category"
      <> " WHERE category.name = ? AND film.title = ?"
      <> "       AND film_category.film_id=film.film_id"
      <> "       AND film_category.category_id=category.category_id")
     (catName, filmTitle)

demo :: Connection -> IO ()
demo conn = do
  allFilms conn >>= mapM_ printFilm . take 5

  putStr "\nTotal number of films: "
  totalFilmsNumber conn >>= printT

  let film = "MODERN DORADO"
  putStrLn "\nFilm information:"
  findFilm conn film >>= printT

  let len = FilmLength 185
  putStrLn $ "\nFilms of " <> showt len <> " and longer:"
  filmsLonger conn len >>= mapM_ printT

  let films = ["KISSING DOLLS", "ALABAMA DEVIL", film]
  putStrLn "\nFilms categories:"
  filmsCategories conn films >>= mapM_ printT

  let newRating = NC17
  putStr $ "\nSetting rating " <> fromRating newRating
              <>  " for a film (" <> film <> "): "
  setRating conn newRating film >>= printT
  findFilm conn film >>= printT

  let newCat = "Art"
  putStr "\nAssign category to a film: "
  assignCategory conn newCat film >>= print
  filmsCategories conn [film] >>= mapM_ printT

  putStr "\nUnassign category from a film: "
  unassignCategory conn newCat film >>= print
  filmsCategories conn [film] >>= mapM_ printT

main :: IO ()
main = do
  conn <- connectPostgreSQL connString
  demo conn
 where
   connString = "host=localhost dbname=sakila_films"
