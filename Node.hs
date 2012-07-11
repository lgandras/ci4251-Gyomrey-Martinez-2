
import Song
import System.FilePath.Find
import Control.Monad
import Control.Arrow
import Control.Monad.Trans
import Data.Maybe
import qualified Data.Vector as DV


main :: IO ()
main = do
    fileNames <- fmap (take 10) $ find always (extension ==? ".mp3") "/media/sf_Andras/Music"
    songs <- makeSongs fileNames
    putStrLn $ concat $ liftM show songs

makeSongs :: [FilePath] -> IO [(Song, FilePath)]
makeSongs [] = do return []
makeSongs fileNames = do
    let (fileName:fileNames') = fileNames
    mSong <- fromFile fileName
    if isJust mSong
        then return . ((fromJust mSong, fileName):) =<< makeSongs fileNames'
        else makeSongs fileNames'
{-
makeSongs :: [FilePath] -> IO [(Song, FilePath)]
makeSongs fileNames = do
    let (fileName:fileNames') = fileNames
    song <- fromFile fileName
    if isJust song
        then return . ((fromJust song, fileName):) =<< makeSongs fileNames'
        else makeSongs fileNames'




makeSongs :: IO [FilePath] -> IO [(Song, FilePath)]
makeSongs fileNames = do
    return liftIO $ do
        songsM <- sequence $ map fromFile fileNames
        return [(fromJust sm, fp) | (sm, fp) <- zip songsM fileNames, isJust sm ]

loadDir :: FilePath -> IO [[(Song, FilePath)]]
loadDir dir = do
    a <- doesDirectoryExist dir
    p <- getPermissions dir
    -- directorio inválido
    guard(drop (length x) - 2 x /= "/." && drop (length x) - 3 x /= "/." && a && readable p)
    -- archivos del directorio
    fileNamesList <- liftM (fmap (dir++)) $ getDirectoryContents dir
    -- maybe canciones   fromFile :: FilePath -> IO (Maybe Song)
    songsM <- sequence $ fmap fromFile fileNamesList
    -- generar lista de subdirectorios válidos
    dirs <- sequence $ fmap validDir fileNamesList
    -- leer canciones de subdirectorios
    subDirs <- sequence $ fmap loadDir $ catMaybes dirs
    -- lista de canciones asociadas al directorio
    return [(fromJust sm, fp) | (sm, fp) <- zip songsM fileNamesList, isJust sm ] ++ subDirs

getFiles :: FilePath -> IO [FilePath]
getFiles path = do
    -- se lanza excepcion si no se tiene permiso para leer los permisos
    p <- getPermissions path `catch` (\_ -> return  Permissions {
                                                        readable   = False,
                                                        writable   = False,
                                                        executable = False,
                                                        searchable = False
                                                    })
    -- dirección inválido
    if not ((drop ((length path) - 2) path) /= pathSeparator:"."
       && (drop ((length path) - 3) path) /= pathSeparator:".."
       && readable p) then return [] else do
        isFile <- doesFileExist path
        -- caso base
        if isFile then return (return path) else do
            isDir <- doesDirectoryExist path
            if not isDir then return [] else do
                -- archivos del directorio
                fileNamesList <- liftM concatPaths $ getDirectoryContents path
                --liftM concat $ sequence [ getFiles x | x <- fileNamesList ]
                readFiles fileNamesList
        where
            concatPaths = fmap $ (path++).if last path /= pathSeparator then (pathSeparator:) else id

readFiles c = do
    x <- c
    return getFiles x
-}