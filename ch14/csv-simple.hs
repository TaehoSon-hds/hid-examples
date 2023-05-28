import Control.Applicative ( Alternative((<|>)) )
import Control.Monad.Trans.Resource ( runResourceT )
import Data.Attoparsec.ByteString.Char8 as A ( char, takeWhile, endOfLine, manyTill, sepBy1, endOfInput, Parser )
import Data.Attoparsec.ByteString.Streaming ( parsed )
import Data.ByteString (ByteString)
--import qualified Data.ByteString as B
import Data.Function ((&))
import Data.Functor (void)
import Data.Text (Text)

import qualified Data.Text.Encoding as T
import qualified Streaming.ByteString as BS
import qualified Streaming.Prelude as S


field :: Parser ByteString
field = A.takeWhile (\c -> c /= ',' && c /= '\r' && c /= '\n')

textField :: Parser Text
textField = T.decodeUtf8 <$> field

record :: Parser [Text]
record = textField `sepBy1` char ','

endOfFile :: Parser ()
endOfFile = endOfInput <|> endOfLine *> endOfInput

file :: Parser [[Text]]
file =
  (:) <$> record
      <*> manyTill (endOfLine *> record)
                   endOfFile

main :: IO ()
-- main = B.readFile "data/quotes.csv" >>= print . parseOnly file
main = runResourceT $
           BS.readFile "data/quotes.csv"
         & parsed file
         & void
         & S.print
