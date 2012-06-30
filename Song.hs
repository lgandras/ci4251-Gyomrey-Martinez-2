
--module Song (Song, fromFile) where

import Genres
import qualified Data.ByteString as BS
import qualified Codec.Binary.UTF8.String as US
import Text.Parsec.String
import Text.Parsec.Combinator
import Text.Parsec.Char
import Text.Parsec.Prim
import Text.Parsec.Error
import Control.Monad
import Data.Char
import qualified Data.Map as DM

data Song = Song{
    title :: String,
    artist :: String,
    album :: String,
    year :: Int,
    comment :: String,
    track :: Int,
    genre :: String
} deriving Show


main = fromFile "2 Unlimited - Get Ready for This.mp3"

fromFile :: String -> IO Song
fromFile f = do
    metadata <- liftM (\x -> BS.drop (BS.length x - 128) x) $ BS.readFile f
    let result = parse songMeta "" $ US.decode $ BS.unpack metadata
    case result of
        Left e -> error $ foldr1 (++) $ map messageString $ errorMessages e
        Right s -> return s

songMeta :: GenParser Char st Song
songMeta = do
    string "TAG"
    title <- takeString 30
    artist <- takeString 30
    album <- takeString 30
    year <- takeString 4
    comment <- takeString 28
    hasTrack <- anyChar
    track <- anyChar
    genre <- anyChar
    return Song {
        title = title,
        artist = artist,
        album = album,
        year = read year,
        genre = case DM.lookup (fromIntegral $ ord genre) genres of
            Just c -> c
            Nothing -> "Unknown",

        -- http://en.wikipedia.org/wiki/ID3#cite_note-storage-2
        comment = comment ++ if hasTrack /= '\0' then (takeWhile (\x -> x /= '\0') [hasTrack, track]) else "",
        track = if hasTrack == '\0' then ord track else 0
    }

-- |Interpretar los siguientes @maxLength@ bytes como un string terminado en \0
takeString :: Int -> GenParser Char st String
takeString maxLength = liftM (takeWhile (\x -> x /= '\0')) $ sequence $ take maxLength $ repeat anyChar