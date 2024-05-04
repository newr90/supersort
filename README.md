# SuSo


## Erste Ausführung:

kopieren und umbenennen von:

bsp-suso.conf >> suso.conf

suso.conf entsprechend editieren:

configFile=files.conf

directory=home/gbs/Dokumente/supersort/t1


```bash
ghci> :l suso.hs
ghci> main
```
    
---

## Allgemeine Erklärungen:

Dateiregeln:

Jede Zeile ist eine Konfiguration für das verschieben einer spezifischen Dateiendung

Beispiel: .txt ist das Dateiformat und TextFiles der Ordner

```bash
.txt = TextFiles
.docx = WordDocuments
.pdf = PDFs
.jpg = ImageFiles
.mp3 = AudioFiles
.mp4 = VideoFiles
```

```
C:\Users\Robert\Documents\GitHub\supersort\test.conf

C:\Users\Robert\Documents\GitHub\supersort\Testdir
```
