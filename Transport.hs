import qualified Data.Sequence as DS

data Request = ReqReach --Nodos alcanzables.
			  | ReqDownload Int --Descargar cancion indexada.
			  | ReqConsult (Maybe (Char, String)) --Consulta: Nothing -> Consultar toda la red. Just ([a|t], pattern)
			  | ReqLog --Obtener el log del nodo.

data Response =   RepReach [String] --Lista de nodos alcanzables (ip o hostname)
			    | RepConsult [String] --Lista de canciones correspondientes a la consulta.
				| RepLog (DS.Seq String) --Seq con el log de mensajes del nodo.
