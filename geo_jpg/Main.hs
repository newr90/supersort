import Codec.Picture.Extra
import Control.Monad (forM_)
import System.Directory (createDirectoryIfMissing, renameFile)

-- Pseudocode-Funktion zur Extraktion des Geostandorts aus Metadaten
extractGeoLocation :: FilePath -> IO (Maybe GeoLocation)
extractGeoLocation filePath = undefined

-- Funktion zum Sortieren der Bilder
sortImagesByGeoLocation :: [FilePath] -> IO ()
sortImagesByGeoLocation imagePaths = do
    createDirectoryIfMissing True "GeoLocation1"
    createDirectoryIfMissing True "GeoLocation2"
    forM_ imagePaths $ \filePath -> do
        maybeGeoLocation <- extractGeoLocation filePath
        case maybeGeoLocation of
            Just (GeoLocation1) -> renameFile filePath ("GeoLocation1/" ++ fileName filePath)
            Just (GeoLocation2) -> renameFile filePath ("GeoLocation2/" ++ fileName filePath)
            Nothing -> return ()

-- Funktion zum Ausführen des Sortiervorgangs
main :: IO ()
main = do
    -- Annahme: Die Liste imagePaths enthält die Pfade zu den Bilddateien
    let imagePaths = ["image1.jpg", "image2.jpg", "image3.jpg"]
    sortImagesByGeoLocation imagePaths