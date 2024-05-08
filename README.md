# SuSo


## Erste Ausführung:

kopieren und umbenennen von:

bsp-suso.conf >> suso.conf

suso.conf entsprechend editieren:

configFile=files.conf

directory=home/gbs/Dokumente/supersort/t1


```bash
gbs@gbs-Haskell:~/Dokumente/supersort-main$ cd gui_weather/
gbs@gbs-Haskell:~/Dokumente/supersort-main/gui_weather$ nix-shell

[nix-shell:~/Dokumente/supersort-main/gui_weather]$ make build
ghc ./Main.hs -outputdir _build -o ./weather_gui -threaded -Wall -O2
Do you want to execute ./weather_gui? (y/n)
y

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
