import System.Environment(getArgs)
import Control.Monad.Writer
import qualified Data.Sequence as DS
import Control.Monad
--import Node
--import Transport
import Data.Char
import Data.Foldable (toList)

type Client = WriterT (DS.Seq String) IO ()

main :: IO ()
main = do 
	   args <- getArgs --Obtengo los argumentos
	   if not (checkArgs args) then putStrLn $ "Error introduciendo los argumentos. A continuacion, las instrucciones: " ++ format
		else do log <- execWriterT prompt;
				putStrLn "EVENTOS DE LA SESION: ";
				summaryLog log;

	   
-- |Mensaje con el formato de la llamada correcta del programa.
format :: String
format = "\n\ncliente -p <puerto> -n <nodo> [-d <directorio de descargas>] [-l], donde: \n\n* puerto numero de puerto del servicio de la biblioteca musical\n* nodo hostname o direccion IP del nodo al cual se conectara el cliente\n* directorio de descargas trayectoria absoluta o relativa de un directorio donde seran almacenados los archivos de musica descargados. Si no se especifica, estos archivos se guardaran en el directorio actual.\n* Si la opcion -l esta activada significa que se desea habilitar la opcion de solicitar el registro de operaciones hechas (log)."

-- |Se verifica que los argumentos sean validos.
checkArgs :: [String] -> Bool
checkArgs [] = False
checkArgs args = case length args of
					4 -> "-p" `elem` args && "-n" `elem` args --5 -> "-p" `elem` args && "-n" `elem` args && "-l" `elem` args
					6 -> "-p" `elem` args && "-n" `elem` args && "-d" `elem` args
					--7 -> "-p" `elem` args && "-n" `elem` args && "-d" `elem` args && "-l" `elem` args
					_ -> False
					
-- |Escribe una nueva entrada en el log.
logIt :: String -> Client
logIt str = tell $ DS.singleton str

-- |Prompt de la aplicacion cliente
prompt :: Client
prompt = do
		   liftIO $ putStrLn ("Introduzca opcion: ");
		   option <- liftIO $ getLine;
		   let (op:args) = words option in 
			when (toUpper (op !! 0) /= 'Q') $ do
		     case toUpper (op !! 0) of
			 'C' -> case length args of
					0 -> do logIt "Consulta de todos los archivos de la red"; prompt;
									--handleC
					_ -> if head args == "-a" then 
							do logIt $ "Consulta de todas las canciones de \"" ++ (unwords $ tail args) ++ "\""; prompt;
										--handleC
						   else do logIt $ "Consulta de todas las canciones cuyo nombre contenga \"" ++ (unwords $ tail args) ++ "\""; prompt;
			 'A' -> do logIt "Consulta de lista de nodos alcanzables."; prompt;
								   --handleA
								    
						 --'L' -> do logIt "Log de operaciones" 
								    --handleL
			 'D' -> do logIt "Descarga"; prompt;
			 --handleD
			 _ -> do liftIO $ putStrLn $ "Operacion desconocida: " ++ op ++ ". Intente de nuevo."; logIt $ "Operacion desconocida: " ++ op; prompt;

summaryLog :: DS.Seq String -> IO ()
summaryLog seq = mapM_ (putStrLn . ("* " ++)) (toList seq)

--handleA :: ?? -> Maybe Response
--handleA = --do st <- get;
		  --send $ ReqReach _
		  --r <- receive
		  --case r of
		  --RepReach str -> Just str
		  --_ -> Nothing

--handleC = --do 
