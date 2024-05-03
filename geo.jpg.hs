import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BS
import Data.Maybe (fromMaybe)
import Control.Monad (forM_)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import System.Environment (getArgs)
import Graphics.Exif (ExifValue(..), exifData)
import Codec.Picture.Metadata (Metadatas, lookup)
import Codec.Picture.Types (DynamicImage(..))
import Codec.Picture.Extra (decodeJpeg)

-- | Extract GPS coordinates from EXIF metadata
extractGPS :: Metadatas -> Maybe (Double, Double)
extractGPS metadata = do
    latRef <- getRef "GPSLatitudeRef"
    lonRef <- getRef "GPSLongitudeRef"
    latVals <- getVals "GPSLatitude"
    lonVals <- getVals "GPSLongitude"
    let lat = convertDMS latVals latRef
        lon = convertDMS lonVals lonRef
    return (lat, lon)
  where
    getRef key = lookup key metadata >>= \(ExifText t) -> Just (T.unpack t)
    getVals key = lookup key metadata >>= \(ExifList vals) -> Just (map (\(ExifRational val) -> val) vals)
    convertDMS [deg, min, sec] ref = (sign * (fromRational deg + (fromRational min / 60) + (fromRational sec / 3600)))
      where
        sign = if ref == "S" || ref == "W" then -1 else 1
    convertDMS _ _ = 0

-- | Read GPS coordinates from a JPG file
readGPSFromJPG :: FilePath -> IO ()
readGPSFromJPG filePath = do
    imgData <- BS.readFile filePath
    case decodeJpeg imgData of
        Left err -> putStrLn $ "Error decoding JPEG: " ++ err
        Right (ImageYCbCr8 img) -> do
            let metadata = exifData img
                gps = extractGPS metadata
            case gps of
                Just (lat, lon) -> putStrLn $ "GPS Coordinates: " ++ show lat ++ ", " ++ show lon
                Nothing -> putStrLn "No GPS coordinates found in the image."

main :: IO ()
main = do
    args <- getArgs
    case args of
        [filePath] -> readGPSFromJPG filePath
        _ -> putStrLn "Usage: extract-gps <image.jpg>"