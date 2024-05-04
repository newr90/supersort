import System.Directory (createDirectoryIfMissing, doesDirectoryExist, listDirectory, renameFile)
import System.FilePath ((</>))
import Control.Monad (forM_)

-- | Verschiebt sortierte Dateien aus den Unterordnern zurück in den Hauptordner
moveFilesToMainFolder :: FilePath -> IO ()
moveFilesToMainFolder mainFolder = do
    subfolders <- filter (\f -> f /= "." && f /= "..") <$> listDirectory mainFolder
    forM_ subfolders $ \subfolder -> do
        let subfolderPath = mainFolder </> subfolder
        isDir <- doesDirectoryExist subfolderPath
        if isDir
            then moveFilesFromSubfolder mainFolder subfolderPath
            else putStrLn $ "Skipping non-directory file: " ++ subfolder

-- | Verschiebt Dateien aus einem Unterordner zurück in den Hauptordner
moveFilesFromSubfolder :: FilePath -> FilePath -> IO ()
moveFilesFromSubfolder mainFolder subfolder = do
    files <- listDirectory subfolder
    forM_ files $ \file -> do
        let src = subfolder </> file
            dest = mainFolder </> file
        renameFile src dest
        putStrLn $ "Moved file: " ++ file ++ " from " ++ subfolder ++ " to " ++ mainFolder

main :: IO ()
main = do
    putStrLn "Enter main folder path:"
    mainFolder <- getLine
    moveFilesToMainFolder mainFolder
    putStrLn "Files moved successfully."