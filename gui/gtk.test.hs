import Graphics.UI.Gtk

main :: IO ()
main = do
    _ <- initGUI
    window <- windowNew
    set window [windowTitle := "Hello World",
                windowDefaultWidth := 200,
                windowDefaultHeight := 200,
                containerBorderWidth := 10]
    button <- buttonNew
    set button [buttonLabel := "Click Me"]
    onClicked button (putStrLn "Hello, world!")
    containerAdd window button
    onDestroy window mainQuit
    widgetShowAll window
    mainGUI