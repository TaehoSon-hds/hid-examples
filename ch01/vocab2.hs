{-# LANGUAGE ImportQualifiedPost #-}

import Data.Char (isLetter)
import Data.List (group, sort)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import System.Environment (getArgs)

type Entry = (T.Text, Int)

type Vocabulary = [Entry]

extractVocab :: T.Text -> Vocabulary
extractVocab t = map buildEntry $ group $ sort ws
  where
    ws = map T.toCaseFold $ filter (not . T.null) $ map cleanWord $ T.words t
    buildEntry xs@(x : _) = (x, length xs)
    buildEntry [] = error "unexpected"
    cleanWord = T.dropAround (not . isLetter)

printAllWords :: Vocabulary -> IO ()
printAllWords vocab = do
  putStrLn "All words: "
  TIO.putStrLn $ T.unlines $ map fst vocab

processTextFile :: FilePath -> IO ()
processTextFile fname = do
  text <- TIO.readFile fname
  let vocab = extractVocab text
  printAllWords vocab

main :: IO ()
main = do
  args <- getArgs
  case args of
    [fname] -> processTextFile fname
    _ -> putStrLn "Usage: vocab2 filename"
