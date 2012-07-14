
import System.FilePath.Find
import Control.Monad
import Control.Arrow
import Control.Monad.Trans
import Data.Maybe
import Network
import Network.Socket.Internal
import Network.Transport
import Network.Transport.TCP
import qualified Control.Distributed.Process as DP
import Control.Distributed.Process.Internal.Types
import Control.Distributed.Process.Node
import Control.Concurrent hiding (newChan)
import qualified Data.Binary as DB
import qualified Data.ByteString.Lazy as DBL
import qualified Codec.Binary.UTF8.String as US
import qualified System.IO as SI

cIP = "10.0.2.15"

main = withSocketsDo $ do
    Right transport <- createTransport cIP "9002" defaultTCPParameters
    let rtable = initRemoteTable 
    node <- newLocalNode transport rtable
    pid <- newEmptyMVar
    runProcess node $ do
        c <- DP.getSelfPid
        liftIO $ putMVar pid c
    --let c = newChan :: Process (SendPort Int, ReceivePort Int)
    let p = PortNumber (PortNum 9000)
    handle <- connectTo cIP p
    pts <- readMVar pid
    let s = DB.encode pts
    putStrLn $ show s
    DBL.hPut handle s
    SI.hClose handle
    i <- newEmptyMVar :: IO (MVar Int)
    runProcess node $ do
        liftIO $ putStrLn "antes de recibir"
        c <- DP.expect 
        liftIO $ putStrLn "después de recibir"
        liftIO $ putMVar i c
    c <- takeMVar i
    
    --socket <- listenOn p
    --forever $ acceptClient socket
    {-
    x <- createTransport "192.168.1.6" "9000" defaultTCPParameters
    y <- newEndPoint $ c x
    let a = address $ c y
    let b = NodeId { nodeAddress = a }
    let p = linkNode b-}
    putStrLn $ "recibido " ++ (show c){-
        where c (Left e) = error "no se pudo abrir el puerto 9000"
              c (Right t) = t-}

--acceptClient socket = do
--    (handle, host, portNumber) <- accept socket
--    putStrLn $ "conexión desde " ++ host ++ "\n"