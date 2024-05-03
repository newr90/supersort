import System.Directory (createDirectoryIfMissing, doesFileExist, listDirectory, renameFile)
import System.FilePath.Posix (takeExtension, (</>))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.IO (readFile, writeFile)

type Config = Map Text Text

loadConfig :: FilePath -> IO Config
loadConfig configFile = do
    contents <- T.lines <$> Data.Text.IO.readFile configFile
    let pairs = map (T.breakOn (T.pack "=")) contents
    return $ Map.fromList $ map (\(k,v) -> (T.strip k, T.strip $ T.drop 1 v)) pairs

sortFiles :: Config -> FilePath -> IO ()
sortFiles config dir = do
    files <- listDirectory dir
    mapM_ (sortFile config dir) files

sortFile :: Config -> FilePath -> FilePath -> IO ()
sortFile config dir file = do
    let ext = T.pack $ takeExtension file
    case Map.lookup ext config of
        Just subdir -> do
            let destDir = dir </> T.unpack subdir
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
    exists <- doesFileExist dir
    if exists
        then putStrLn "Please enter a directory, not a file."
        else sortFiles config dir
    putStrLn "Sorting complete."