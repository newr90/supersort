import System.Directory (createDirectoryIfMissing, doesDirectoryExist, listDirectory, renameFile)
import System.FilePath.Posix ((</>), takeExtension)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.IO (readFile, writeFile)
import Control.Monad (forM_, when)

-- Definiere einen Alias für die Konfigurationsdaten
type Config = Map.Map Text Text

-- Lädt die Konfigurationsdaten aus der angegebenen Datei
loadConfig :: FilePath -> IO Config
loadConfig configFile = do
    -- Lies den Inhalt der Datei
    contents <- T.lines <$> Data.Text.IO.readFile configFile
    -- Teile den Inhalt an den Gleichheitszeichen auf und erstelle Paare
    let pairs = map (T.breakOn (T.pack "=")) contents
    -- Erstelle eine Map aus den Paaren, wobei Schlüssel und Werte bereinigt werden
    return $ Map.fromList $ map (\(k,v) -> (T.strip k, T.strip $ T.drop 1 v)) pairs

-- Lädt die Hauptkonfiguration aus der Datei "suso.conf"
loadMainConfig :: IO (FilePath, FilePath)
loadMainConfig = do
    -- Lade die Konfiguration
    config <- loadConfig "suso.conf"
    -- Extrahiere die Dateiregeln und das Suchverzeichnis aus der Konfiguration
    let configFile = T.unpack $ Map.findWithDefault (T.pack "config.txt") (T.pack "configFile") config
    let dir = T.unpack $ Map.findWithDefault (T.pack ".") (T.pack "directory") config
    return (configFile, dir)

-- Sortiert die Dateien im angegebenen Verzeichnis gemäß der Konfiguration
sortFiles :: Config -> FilePath -> IO ()
sortFiles config dir