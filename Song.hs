{-# LANGUAGE RecordWildCards #-}
module Song (Song, fromFile) where

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
--    path :: String,
    title :: String,
    artist :: String,
    album :: String,
    year :: Int,
    comment :: String,
    track :: Int,
    genre :: String
} deriving Show

fromFile :: String -> IO (Maybe Song)
fromFile f = do
    putStrLn $ "Cargando " ++ f ++ "... \n"
    metadata <- liftM truncate $ BS.readFile f `catch` (\_ -> return BS.empty)
    let result = parse (songMeta f) "" $ US.decode $ BS.unpack metadata
    case result of
        Left e -> return Nothing
        Right s -> return $ Just s
        where
            truncate x = BS.drop (BS.length x - 128) x

songMeta :: String -> GenParser Char st Song
songMeta path = do
    string "TAG"
    title <- takeString 30
    artist <- takeString 30
    album <- takeString 30
    year <- read `fmap` takeString 4
    c <- takeString 28
    hasTrack <- anyChar
    t <- anyChar
    g <- anyChar
    let genre = case DM.lookup (fromIntegral $ ord g) genres of
            Just c -> c
            Nothing -> "Unknown"
        -- http://en.wikipedia.org/wiki/ID3#cite_note-storage-2
        comment = c ++ if hasTrack /= '\0' then (takeWhile (\x -> x /= '\0') [hasTrack, t]) else ""
        track = if hasTrack == '\0' then ord t else 0
    return Song { .. }

-- |Interpretar los siguientes @maxLength@ bytes como un string terminado en \0
takeString :: Int -> GenParser Char st String
takeString maxLength = liftM (takeWhile (\x -> x /= '\0')) $ sequence $ take maxLength $ repeat anyChar