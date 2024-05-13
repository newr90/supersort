-- Importieren der benötigten Module für die HTTP-Anfragen und die Verarbeitung von Byte-Strings.
import Network.HTTP.Simple
import qualified Data.ByteString.Lazy.Char8 as L8

-- Hauptfunktion, die die HTTP-Anfrage durchführt.
main :: IO ()
main = do
    -- Definieren der URL, an die die Anfrage gesendet werden soll.
    let url = "http://192.168.50.11:5000/webhook"
    -- Definieren des Anfragekörpers (hier leer, da es sich um einen POST-Request handelt).

    -- Erstellen der Anfrage mit der angegebenen URL und der leeren Anfrage.
    request <- parseRequest $ "POST " ++ url
    -- Festlegen des Anfragekörpers für die Anfrage.
    let request' = setRequestBodyLBS (L8.pack requestBody) request
    -- Durchführen der HTTP-Anfrage und Speichern der Antwort.
    response <- httpLBS request'

    -- Ausgabe des Statuscodes der Antwort.
    putStrLn $ "Response status code: " ++ show (getResponseStatusCode response)
    -- Ausgabe des Antwortkörpers (als Zeichenkette) der Antwort.
    putStrLn $ "Response body: " ++ L8.unpack (getResponseBody response) 