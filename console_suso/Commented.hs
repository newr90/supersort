-- Imports für die Verwendung von Datei- und Verzeichnisoperationen sowie für den Umgang mit Text und Maps.
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, listDirectory, renameFile)
import System.FilePath.Posix ((</>), takeExtension)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.IO (readFile, writeFile)
import Control.Monad (forM_, when)

-- Eine Map, die die Konfiguration enthält, wobei Schlüssel und Werte beide Text sind.
type Config = Map.Map Text Text

-- Lädt die Konfiguration aus einer Datei.
loadConfig :: FilePath -> IO Config
loadConfig configFile = do
    -- Die Dateiinhalte werden Zeile für Zeile gelesen und in Text umgewandelt.
    contents <- T.lines <$> Data.Text.IO.readFile configFile
    -- Die Inhalte werden in Schlüssel-Wert-Paare aufgeteilt.
    let pairs = map (T.breakOn (T.pack "=")) contents
    -- Die Schlüssel-Wert-Paare werden in eine Map umgewandelt, wobei Leerzeichen entfernt werden.
    return $ Map.fromList $ map (\(k,v) -> (T.strip k, T.strip $ T.drop 1 v)) pairs

-- Lädt die Hauptkonfiguration, die den Dateinamen der Regeln und das Stammverzeichnis enthält.
loadMainConfig :: IO (FilePath, FilePath)
loadMainConfig = do
    -- Die Hauptkonfiguration wird geladen.
    config <- loadConfig "suso.conf"
    -- Der Dateiname der Regeln wird aus der Konfiguration geholt.
    let configFile = T.unpack $ Map.findWithDefault (T.pack "config.txt") (T.pack "configFile") config
    -- Das Stammverzeichnis wird aus der Konfiguration geholt.
    let dir = T.unpack $ Map.findWithDefault (T.pack ".") (T.pack "directory") config
    return (configFile, dir)

-- Sortiert die Dateien im angegebenen Verzeichnis gemäß der Konfiguration.
sortFiles :: Config -> FilePath -> IO ()
sortFiles config dir = do
    -- Überprüft, ob das angegebene Verzeichnis existiert.
    isDir <- doesDirectoryExist dir
    if isDir
        then sortDirectory config dir dir
        else putStrLn "[Error] no valid search directory!"

-- Sortiert die Dateien im angegebenen Verzeichnis und seinen Unterverzeichnissen.
sortDirectory :: Config -> FilePath -> FilePath -> IO ()
sortDirectory config rootDir dir = do
    -- Listet alle Dateien im Verzeichnis auf.
    files <- listDirectory dir
    -- Verarbeitet jede Datei im Verzeichnis.
    forM_ files $ \file -> do
        let filePath = dir </> file
        -- Überprüft, ob es sich um ein Unterverzeichnis handelt.
        isDir <- doesDirectoryExist filePath
        if isDir
            -- Wenn es sich um ein Unterverzeichnis handelt, wird die Funktion rekursiv aufgerufen.
            then sortDirectory config rootDir filePath
            -- Wenn es sich nicht um ein Unterverzeichnis handelt, wird die Datei sortiert.
            else sortFile config rootDir dir file

-- Sortiert eine einzelne Datei gemäß der Konfiguration.
sortFile :: Config -> FilePath -> FilePath -> FilePath -> IO ()
sortFile config rootDir dir file = do
    -- Extrahiert die Dateierweiterung.
    let ext = T.pack $ takeExtension file
    -- Überprüft, ob eine Regel für die Dateierweiterung vorhanden ist.
    case Map.lookup ext config of
        Just subdir -> do
            -- Holt das Zielverzeichnis aus der Konfiguration.
            let destDir = rootDir </> T.unpack subdir
            -- Erstellt das Zielverzeichnis, falls es nicht existiert.
            createDirectoryIfMissing True destDir
            -- Erstellt den Ziel-Pfad für die Datei.
            let destFile = destDir </> file
            -- Verschiebt die Datei in das Zielverzeichnis.
            renameFile (dir </> file) destFile
        Nothing -> return ()

-- Hauptfunktion, die den Ablauf steuert.
main :: IO ()
main = do
    putStrLn "Loading main configuration from suso.conf..."
    -- Lädt die Hauptkonfiguration.
    (fileRulesConfig, rootSearchDirectory) <- loadMainConfig
    putStrLn $ "Using file rules: " ++ fileRulesConfig
    putStrLn $ "Using root search directory: " ++ rootSearchDirectory
    putStrLn "Loading additional configuration..."
    -- Lädt die zusätzliche Konfiguration aus der Datei der Regeln.
    config <- loadConfig fileRulesConfig
    putStrLn "Sorting files..."
    -- Sortiert die Dateien gemäß der Konfiguration.
    sortFiles config rootSearchDirectory
    putStrLn "Sorting complete."
