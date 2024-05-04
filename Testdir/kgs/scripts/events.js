document.getElementById("weitereDatenForm").addEventListener("submit", function (event) {
    event.preventDefault();
    document.getElementById("weitereDaten").style.display = "none";
    document.getElementById("betriebsratForm").style.display = "block";
});

document.getElementById('betriebsratInfoForm').addEventListener('submit', function (event) {
    event.preventDefault();

    let betriebsratMitgliedJa = document.getElementById('betriebsratMitgliedJa').checked;
    let azubiJa = document.getElementById('azubiJa').checked;
    let behindertJa = document.getElementById('behindertJa').checked;
    let mutterschutzJa = document.getElementById('mutterschutzJa').checked;


    // wenn es ein schweres vergehen ist brauchen wir keinen fadenscheinigen Grund auswählen - raus mit der Person!
    if (document.getElementById('schweresVergehenJa').checked) {
        document.getElementById('statusPerson').style.display = 'block';
        calcKuendigungsGrund(document.getElementById("personalnummer").value);

        document.getElementById("kundigungsgrundForm").style.display = "none";
        document.getElementById("betriebsratForm").style.display = "none";
        document.getElementById("kundigungErgebnis").style.display = "block";
    }
    // wenn es ein betriebsratmitglied, azubi, schwer behindert, oder mutterschutz ist.. brauchen wir auch keinen Grund auswählen!
    else if (betriebsratMitgliedJa || azubiJa || behindertJa || mutterschutzJa) {
        document.getElementById('statusPerson').style.display = 'block';
        calcKuendigungsGrund(document.getElementById("personalnummer").value);

        document.getElementById("kundigungsgrundForm").style.display = "none";
        document.getElementById("betriebsratForm").style.display = "none";
        document.getElementById("kundigungErgebnis").style.display = "block";
    }
    else {
        document.getElementById("betriebsratForm").style.display = "none";
        document.getElementById("kundigungsgrundForm").style.display = "block";
    }
});

document.getElementById("kundigungsgrundForm").addEventListener("submit", function (event) {
    event.preventDefault();
    let kundigungsgrund = document.getElementById("kundigungsgrund").value
    const kundigungsgrundText = {
        A: "Mitarbeiter ist für die Stelle nicht (mehr) geeignet",
        B: "Mitarbeiter hat sich falsch verhalten",
        C: "Aufgrund der wirtschaftlichen Lage entfällt der Arbeitsplatz des Mitarbeiters"
    };

    switch (kundigungsgrund) {
        case 'A':
            document.getElementById("kundigungText").innerHTML = `<p>Grund: ${kundigungsgrundText[kundigungsgrund]}.</p><p>Resultat: Wenn kein anderer Arbeitsplatz vorhanden ist, kann gekündigt werden.`;
            break;
        case 'B':
            document.getElementById("kundigungText").innerHTML = `<p>Grund:\r\n ${kundigungsgrundText[kundigungsgrund]}.</p><p>Resultat: Bevor gekündigt werden kann, muss eine Abmahnung erteilt werden.`;
            break;
        case 'C':
            document.getElementById("kundigungText").innerHTML = `<p>Grund:\r\n ${kundigungsgrundText[kundigungsgrund]}.</p><p>Resultat: Es muss ein Sozialplan erstellt werden und eine Abfindung angeboten werden.`;
            break;
        default:
            document.getElementById("kundigungText").innerHTML = `Grund:\r\n Der Kündigungsgrund ist nicht A, B oder C`;
    }

    document.getElementById('statusPerson').style.display = 'block';
    calcKuendigungsGrund(document.getElementById("personalnummer").value);

    document.getElementById("kundigungsgrundForm").style.display = "none";
    document.getElementById("kundigungErgebnis").style.display = "block";
});