{-# LANGUAGE MonoLocalBinds, TypeApplications #-}

import Data.Text (Text)
import qualified Data.Text as Text
import Data.Fixed (Centi)
import Control.Concurrent (threadDelay, forkIO)
import System.Random (randomRIO)
import Text.Read (readMaybe)
import Control.Monad (unless, forM_)

import qualified GI.Gtk as Gtk
import qualified GI.Gio as Gio
import qualified GI.GLib as GLib
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, listDirectory, renameFile)
import System.FilePath.Posix ((</>), takeExtension)
import qualified Data.Map.Strict as Map
import Data.Text.IO (readFile, writeFile)

-- define a type synonym for conf as a map of text keys to text values
type Config = Map.Map Text Text

-- load conf from file
loadConfig :: FilePath -> IO Config
loadConfig configFile = do
    -- Read contents of the config file and split into lines
    contents <- Text.lines <$> Data.Text.IO.readFile configFile
    -- Split each line into a key-value pair separated by '=' and strip whitespace
    let pairs = map (Text.breakOn (Text.pack "=")) contents
    -- Construct a map from the list of key-value pairs
    return $ Map.fromList $ map (\(k,v) -> (Text.strip k, Text.strip $ Text.drop 1 v)) pairs

-- load main configuration file and extract file paths
loadMainConfig :: IO (FilePath, FilePath)
loadMainConfig = do
    config <- loadConfig "suso.conf"
    let configFile = Text.unpack $ Map.findWithDefault (Text.pack "config.txt") (Text.pack "configFile") config
    let dir = Text.unpack $ Map.findWithDefault (Text.pack ".") (Text.pack "directory") config
    return (configFile, dir)

-- recursively sort files in a directory based on configuration
sortFiles :: Config -> FilePath -> IO ()
sortFiles config dir = do
    isDir <- doesDirectoryExist dir
    if isDir
        then sortDirectory config dir dir
        else putStrLn "[Error] no valid search directory!"

-- recursively sort files in a directory
sortDirectory :: Config -> FilePath -> FilePath -> IO ()
sortDirectory config rootDir dir = do
    files <- listDirectory dir
    forM_ files $ \file -> do
        let filePath = dir </> file
        isDir <- doesDirectoryExist filePath
        if isDir
            then sortDirectory config rootDir filePath
            else sortFile config rootDir dir file

-- sort a single file based on its extension
sortFile :: Config -> FilePath -> FilePath -> FilePath -> IO ()
sortFile config rootDir dir file = do
    let ext = Text.pack $ takeExtension file
    case Map.lookup ext config of
        Just subdir -> do
            let destDir = rootDir </> Text.unpack subdir
            createDirectoryIfMissing True destDir
            let destFile = destDir </> file
            renameFile (dir </> file) destFile
        Nothing -> return ()

-- process main process entry
main :: IO ()
main = do
  _ <- GLib.setenv "GDK_BACKEND" "x11" True  -- Set GDK_BACKEND environment variable because idk
  Just app <- Gtk.applicationNew (Just appId) []
  _ <- Gio.onApplicationActivate app (appActivate app)
  _ <- Gio.applicationRun app Nothing
  return ()

appId :: Text
appId = Text.pack "io.gbs.gui-suso-app"

-- gtk form paint main entry point
appActivate :: Gtk.Application -> IO ()
appActivate app = do
  window <- Gtk.applicationWindowNew app
  Gtk.setWindowTitle window (Text.pack "[SuSo] main ^v.0.0.3_build83459")
  Gtk.setWindowResizable window False
  Gtk.setWindowDefaultWidth window 300
  vbox <- Gtk.boxNew Gtk.OrientationVertical 10
  Gtk.setWidgetMargin vbox 10
  Gtk.containerAdd window vbox
  Gtk.widgetShow vbox
  (fileRulesConfig, rootSearchDirectory) <- loadMainConfig
  configFileEntry <- addEntry (Text.pack fileRulesConfig) vbox
  directoryEntry <- addEntry (Text.pack rootSearchDirectory) vbox
  btnTestRun <- Gtk.buttonNew
  Gtk.setButtonLabel btnTestRun (Text.pack "test sort")
  Gtk.setWidgetHalign btnTestRun Gtk.AlignCenter
  Gtk.containerAdd vbox btnTestRun
  _ <- Gtk.onButtonClicked btnTestRun $
    do Gtk.widgetSetSensitive btnTestRun False
       _ <- forkIO $ do
         c <- -- TODO: set handling function after btn click
           Gtk.widgetSetSensitive btnTestRun True
           return False
         return ()
       return ()
  btnRealRun <- Gtk.buttonNew
  Gtk.setButtonLabel btnRealRun (Text.pack "start sort")
  Gtk.setWidgetHalign btnRealRun Gtk.AlignCenter
  Gtk.containerAdd vbox btnRealRun
  _ <- Gtk.onButtonClicked btnRealRun $ callProvidedMain
  Gtk.widgetShow btnTestRun
  Gtk.widgetShow btnRealRun
  Gtk.widgetShow window

-- set relation between two input fields
setEntryRelation :: Gtk.Entry -> (Double -> Double) -> Gtk.Entry -> IO ()
setEntryRelation entrySource conv entryTarget = do
  _ <- Gtk.onEditableChanged entrySource $
    do target_focused <- Gtk.widgetHasFocus entryTarget
       unless target_focused $ do
         s <- Gtk.entryGetText entrySource
         case parseDouble s of
           Nothing -> return ()
           Just v ->
             let s' = renderDouble (conv v)
             in Gtk.entrySetText entryTarget s'
  return ()

-- add string input combined with a label to our gtk "form"
addEntry :: Gtk.IsContainer a => Text -> a -> IO Gtk.Entry
addEntry labelStr container = do
  hbox <- Gtk.boxNew Gtk.OrientationHorizontal 5
  entry <- Gtk.entryNew
  Gtk.setWidgetExpand entry True
  Gtk.setEntryXalign entry 1
  label <- Gtk.labelNew (Just labelStr)
  Gtk.containerAdd hbox entry
  Gtk.containerAdd hbox label
  Gtk.containerAdd container hbox
  Gtk.widgetShow entry
  Gtk.widgetShow label
  Gtk.widgetShow hbox
  return entry

-- zeh sorting magic happens in here
callProvidedMain :: IO ()
callProvidedMain = do
  putStrLn "Loading main configuration from suso.conf..."
  -- load main config containing file rules and root sort path
  (fileRulesConfig, rootSearchDirectory) <- loadMainConfig
  putStrLn $ "Using file rules: " ++ fileRulesConfig
  putStrLn $ "Using root search directory: " ++ rootSearchDirectory
  putStrLn "Loading additional configuration..."
  -- Load specific file to directory config
  config <- loadConfig fileRulesConfig
  putStrLn "Sorting files..."
  -- finally.. sort files based on configuration
  sortFiles config rootSearchDirectory
  putStrLn "Sorting complete."
