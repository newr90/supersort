import Pdf.Content
import Text.Regex.Posix

-- Funktion zum Extrahieren des Textinhalts aus einer PDF-Datei.
-- Die Funktion erwartet einen Dateipfad zur PDF-Datei und gibt einen IO-String zurück.
extractPDFContent :: FilePath -> IO String
extractPDFContent filePath = do
    -- Die PDF-Datei öffnen und deren Seiteninformationen erhalten.
    pdf <- either error id <$> pdfWithPageSizes filePath
    -- Eine Liste der Seitennummern erstellen.
    let pages = [1 .. pageCount pdf]
    -- Den Inhalt jeder Seite der PDF-Datei extrahieren.
    contents <- mapM (pageContent pdf) pages
    -- Den extrahierten Text von allen Seiten zu einem einzigen String zusammenfügen und zurückgeben.
    return (concatMap extractText contents)

-- Funktion zum Extrahieren des Textinhalts aus PDF-Seiten.
-- Diese Funktion wird von 'extractPDFContent' verwendet.
extractText :: PageContent -> String
extractText (PageContentText text) = text  -- Extrahiert den Textinhalt aus einer Textseite.
extractText _ = ""  -- Falls der Inhaltstyp nicht Text ist, wird ein leerer String zurückgegeben.

-- Funktion zum Suchen nach Schlüsselwörtern in einem Text.
-- Die Funktion verwendet reguläre Ausdrücke, um das Keyword im Text zu suchen.
searchKeywords :: String -> String -> Bool
searchKeywords text keyword = text =~ keyword  -- Überprüft, ob das Keyword im Text vorhanden ist.

-- Funktion zum Durchsuchen des PDF-Inhalts nach Schlüsselwörtern.
-- Diese Funktion erwartet einen Dateipfad zur PDF-Datei und ein Schlüsselwort,
-- und gibt zurück, ob das Schlüsselwort im PDF-Inhalt gefunden wurde.
searchKeywordsInPDF :: FilePath -> String -> IO Bool
searchKeywordsInPDF filePath keyword = do
    -- Den Textinhalt der PDF-Datei extrahieren.
    content <- extractPDFContent filePath
    -- Den Textinhalt in Zeilen aufteilen.
    let lines' = lines content
    -- Überprüfen, ob das Schlüsselwort in einer der Zeilen vorhanden ist.
    return (any (\line -> searchKeywords line keyword) lines')

-- Beispielaufruf:
-- Sucht nach dem Schlüsselwort "Haskell" in der PDF-Datei "example.pdf".
-- Das Ergebnis wird entweder True oder False sein.
-- searchKeywordsInPDF "example.pdf" "Haskell"
