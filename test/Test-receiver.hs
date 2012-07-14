import Transport
import qualified Network as N
import qualified Network.Socket as NS


import qualified Data.ByteString.Lazy as DBL
import qualified Data.Binary as DB
import qualified Network.Socket.ByteString.Lazy as NSBL
import Data.Int

cIP = "10.0.2.15"
cPORT = 9000

main = NS.withSocketsDo $ do
    let p = N.PortNumber (NS.PortNum cPORT)
    socket <- N.listenOn p
    putStrLn "esperando conexión.."
    (handle, _host, _portNumber) <- N.accept socket
    putStrLn "conexión recibida"
    r <- readData handle :: IO Response
    putStrLn $ show r
    