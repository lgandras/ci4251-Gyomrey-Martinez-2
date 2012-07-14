import Transport
import qualified Network as N
import qualified Network.Socket as NS

cIP = "10.0.2.15"
cPORT = 9000

main = NS.withSocketsDo $ do
    let p = N.PortNumber (NS.PortNum cPORT)
    handle <- N.connectTo cIP p
    writeData (RepReach ["hola", "chao"]) handle