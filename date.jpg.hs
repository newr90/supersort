import Data.Maybe (fromMaybe)
import Graphics.Exif

-- Funktion zum Extrahieren des Aufnahmedatums aus den EXIF-Metadaten einer JPG-Datei.
-- Der Dateipfad der JPG-Datei wird als Eingabe erwartet.
-- Die Funktion gibt eine IO-Aktion zurück, die einen Maybe-String enthält.
getJpgCreationDate :: FilePath -> IO (Maybe String)
getJpgCreationDate filePath = do
    -- EXIF-Metadaten aus der JPG-Datei lesen.
    exifData <- parseFileExif filePath
    -- Fallunterscheidung basierend auf dem Ergebnis des EXIF-Metadaten-Lesesvorgangs.
    case exifData of
        -- Wenn ein Fehler auftritt, wird 'Nothing' zurückgegeben.
        Left _ -> return Nothing
        -- Wenn die Metadaten erfolgreich gelesen wurden:
        Right tags -> do
            -- Das Aufnahmedatum extrahieren.
            -- Zuerst wird nach dem Originalaufnahmedatum gesucht ('exifDateTimeOriginal').
            -- Falls nicht verfügbar, wird nach dem allgemeinen Aufnahmedatum gesucht ('exifDateTime').
            let maybeDateTime = lookup exifDateTimeOriginal tags <|> lookup exifDateTime tags
            -- Den Maybe-Wert in einen Maybe-String umwandeln, indem 'show' darauf angewendet wird.
            return $ fmap show maybeDateTime

main :: IO ()
main = do
    -- Das Aufnahmedatum aus der JPG-Datei 'example.jpg' extrahieren.
    maybeDate <- getJpgCreationDate "example.jpg"
    -- Das extrahierte Aufnahmedatum ausgeben oder "Keine Daten vorhanden" anzeigen,
    -- falls keine EXIF-Metadaten oder kein Aufnahmedatum vorhanden sind.
    putStrLn $ "Aufnahmedatum: " ++ fromMaybe "Keine Daten vorhanden" maybeDate
