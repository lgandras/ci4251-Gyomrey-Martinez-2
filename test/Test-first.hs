
import System.FilePath.Find
import Control.Monad
import Control.Arrow
import Control.Monad.Trans
import Data.Maybe
import Network
import Network.Socket.Internal
import Network.Transport
import Network.Transport.TCP
import Control.Distributed.Process.Internal.Types
import Control.Distributed.Process.Node
import Control.Monad.Reader
import Control.Concurrent hiding (newChan)
import qualified Control.Distributed.Process as DP
import qualified Data.Binary as DB
import qualified Data.ByteString.Lazy as DBL
import qualified Codec.Binary.UTF8.String as US
import qualified System.IO as SI

cIP = "10.0.2.15"

main = withSocketsDo $ do
    let p = PortNumber (PortNum 9000)
    socket <- listenOn p
    putStrLn "preparando para recibir conexión"
    (handle, _host, _portNumber) <- accept socket
    putStrLn "conexión recibida"
    s <- DBL.hGetContents handle -- fmap (DBL.copy . (\x -> DBL.concat [DBL.take 16 x, DBL.pack $ US.encode "2", DBL.drop 17 x])) $ 
    putStrLn $ show s
    let sp = DB.decode s :: ProcessId
    putStrLn "encontrado"
    Right transport <- createTransport cIP "9001" defaultTCPParameters
    let rtable = initRemoteTable 
    node <- newLocalNode transport rtable
    runProcess node $ do
        DP.link sp
        DP.send sp (12345 :: Int) >> DP.send sp (12345 :: Int)
    putStrLn "enviado"
    _ <- accept socket
    return ()
    --SI.hClose handle
    {-
    Right transport <- createTransport "127.0.0.1" "9010" defaultTCPParameters
    let rtable = initRemoteTable 
    node <- newLocalNode transport rtable
    ports <- newEmptyMVar :: IO (MVar (SendPort Int, ReceivePort Int))
    runProcess node $ do
        c <- newChan
        liftIO $ putMVar ports c
    return () -}

