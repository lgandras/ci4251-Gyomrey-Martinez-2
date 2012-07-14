module Transport(Request(..), Response(..), writeData, readData) where

import qualified Data.Sequence as DS
import qualified Codec.Binary.UTF8.String as US
import qualified Data.ByteString.Lazy as DBL
import qualified Data.Binary as DB
import qualified System.IO as SI
import Control.Applicative
import Data.Int
import Data.Set
import Song

data Request = ReqReach (Set String) --Nodos alcanzables.
			  | ReqDownload (Set String) Int --Descargar cancion indexada.
			  | ReqConsult (Set String) (Maybe (Char, String)) --Consulta: Nothing -> Consultar toda la red. Just ([a|t], pattern)
			  | ReqLog --Obtener el log del nodo.
              deriving (Show)

instance DB.Binary Request where
     put (ReqReach x)      = DB.put (0 :: DB.Word8) >> DB.put x
     put (ReqDownload x y) = DB.put (1 :: DB.Word8) >> DB.put x >> DB.put y
     put (ReqConsult x y)  = DB.put (2 :: DB.Word8) >> DB.put x >> DB.put y
     put ReqLog          = DB.put (3 :: DB.Word8)
     get = do tag <- DB.getWord8
              case tag of
                  0 -> ReqReach <$> DB.get
                  1 -> ReqDownload <$> DB.get <*> DB.get
                  2 -> ReqConsult <$> DB.get <*> DB.get
                  3 -> return ReqLog

data Response =   RepReach [String] --Lista de nodos alcanzables (ip o hostname)
			    | RepConsult [Song] --Lista de canciones correspondientes a la consulta.
				| RepLog (DS.Seq String) --Seq con el log de mensajes del nodo.
              deriving (Show)

instance DB.Binary Response where
     put (RepReach x)   = DB.put (0 :: DB.Word8) >> DB.put x
     put (RepConsult x) = DB.put (1 :: DB.Word8) >> DB.put x
     put (RepLog x)     = DB.put (2 :: DB.Word8) >> DB.put x
     get = do tag <- DB.getWord8
              case tag of
                  0 -> RepReach <$> DB.get
                  1 -> RepConsult <$> DB.get
                  2 -> RepLog <$> DB.get

--writeRequest :: t -> SI.Handle -> IO ()
writeData dat handle = do
    let bs = DB.encode dat
    let i = DB.encode $ ((fromIntegral (DBL.length bs)) :: Int32) 
    DBL.hPut handle $ DBL.concat [i, bs]
    putStrLn $ show $ DBL.concat [i, bs]

--readRequest :: SI.Handle -> IO t
readData handle = do
    bi <- sequence $ take 4 $ repeat (SI.hGetChar handle)
    let i = fromIntegral $ ((DB.decode . DBL.pack . US.encode) bi :: Int32)
    bs <- sequence $ take i $ repeat (SI.hGetChar handle)
    putStrLn $ (show i) ++ (show bs)
    return $ (DB.decode . DBL.pack . US.encode) bs
