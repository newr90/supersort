import System.Directory (createDirectoryIfMissing, doesDirectoryExist, listDirectory, renameFile)
import System.FilePath.Posix ((</>), takeExtension)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.IO (readFile, writeFile)
import Control.Monad (forM_, when)

type Config = Map.Map Text Text

loadConfig :: FilePath -> IO Config
loadConfig configFile = do
    contents <- T.lines <$> Data.Text.IO.readFile configFile
    let pairs = map (T.breakOn (T.pack "=")) contents
    return $ Map.fromList $ map (\(k,v) -> (T.strip k, T.strip $ T.drop 1 v)) pairs

sortFiles :: Config -> FilePath -> IO ()
sortFiles config dir = do
    isDir <- doesDirectoryExist dir
    if isDir
        then sortDirectory config dir dir -- Pass the root directory path
        else putStrLn "Please enter a valid directory."

sortDirectory :: Config -> FilePath -> FilePath -> IO ()
sortDirectory config rootDir dir = do
    files <- listDirectory dir
    forM_ files $ \file -> do
        let filePath = dir </> file
        isDir <- doesDirectoryExist filePath
        if isDir
            then sortDirectory config rootDir filePath
            else sortFile config rootDir dir file

sortFile :: Config -> FilePath -> FilePath -> FilePath -> IO ()
sortFile config rootDir dir file = do
    let ext = T.pack $ takeExtension file
    case Map.lookup ext config of
        Just subdir -> do
            let destDir = rootDir </> T.unpack subdir
            createDirectoryIfMissing True destDir
            let destFile = destDir </> file
            renameFile (dir </> file) destFile
        Nothing -> return ()

main :: IO ()
main = do
    putStrLn "Enter configuration file path:"
    configFile <- getLine
    config <- loadConfig configFile
    putStrLn "Enter directory to sort:"
    dir <- getLine
    sortFiles config dir
    putStrLn "Sorting complete."
