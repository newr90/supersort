// Funktion zur Berechnung der Kündigungsfrist
function calculateKuendigungsfrist(months) {
    let kundigungsfrist;

    switch (true) {
        case months >= 24 && months < 60:
            kundigungsfrist = 1; // 1 Monat Kündigungsfrist
            break;
        case months >= 60 && months < 120:
            kundigungsfrist = 2; // 2 Monate Kündigungsfrist
            break;
        case months >= 120 && months < 192:
            kundigungsfrist = 3; // 3 Monate Kündigungsfrist
            break;
        case months >= 192 && months < 240:
            kundigungsfrist = 4; // 4 Monate Kündigungsfrist
            break;
        case months >= 240 && months < 288:
            kundigungsfrist = 5; // 5 Monate Kündigungsfrist
            break;
        case months >= 288 && months < 360:
            kundigungsfrist = 6; // 6 Monate Kündigungsfrist
            break;
        case months >= 360:
            kundigungsfrist = 7; // 7 Monate Kündigungsfrist
            break;
        default:
            kundigungsfrist = 1; // 1 Monat Kündigungsfrist wenn sonst nichts zutrifft
            break;
    }


    return kundigungsfrist;
}

// Funktion für zufallsgehalt
function calcRandomGehalt(minGehalt = 1000, maxGehalt = 6000) {
    const randomGehalt = Math.floor(Math.random() * (maxGehalt - minGehalt + 1)) + minGehalt;
    return randomGehalt;
}

// Funktion zur Berechnung der Regelabfindung
function calculateAbfindung(monate, gehalt) {
    return Math.floor((monate / 12) * 0.5 * gehalt); // 0.5 Monatsgehalt pro Jahr der Betriebszugehörigkeit
}

function calcKuendigungsGrund(personalnummer) {
    let message;
    let generateFristloseKuendigung = false;
    let generateBetriebsratBenachrichtigung = false;
    let generateAbmahnung = false;
    let generateKuendigung = false;
    let generateAbfindungsangebot = false;

    // status aus html page auslesen
    let kundigungsgrund = document.getElementById("kundigungsgrund").value
    let schweresVergehenJa = document.getElementById('schweresVergehenJa').checked;
    let betriebsratBenachrichtigtJa = document.getElementById('betriebsratJa').checked;

    let betriebsratMitgliedJa = document.getElementById('betriebsratMitgliedJa').checked;
    let azubiJa = document.getElementById('azubiJa').checked;
    let behindertJa = document.getElementById('behindertJa').checked;
    let mutterschutzJa = document.getElementById('mutterschutzJa').checked;

    switch (true) {
        case betriebsratBenachrichtigtJa && schweresVergehenJa:
            message = 'Fristlose Kündigung wurde generiert! :) \r\n Betriebsrat wurde bereits informiert.';
            generateFristloseKuendigung = true;
            break;
        case !betriebsratBenachrichtigtJa && schweresVergehenJa:
            message = 'Fristlose Kündigung & Betriebsrat Benachrichtigung wurden generiert! :)';
            generateFristloseKuendigung = true;
            generateBetriebsratBenachrichtigung = true;
            break;
        case betriebsratMitgliedJa || azubiJa || behindertJa || mutterschutzJa:
            let extraReason = "";
            if (betriebsratMitgliedJa)
                extraReason += "\r\nDer Angestellte ist im Betriebsrat.."
            if (azubiJa)
                extraReason += "\r\nDer Angestellte ist ein Azubi.."
            if (behindertJa)
                extraReason += "\r\nDer Angestellte ist schwer behindert.."
            if (mutterschutzJa)
                extraReason += "\r\nDer Angestellte ist im Mutterschutz.."

            message = 'Eine Kündigung ist nicht möglich. :( ' + extraReason + '\r\n\r\n Aber wir haben eine fadenscheinige Abmahnung generiert!';
            generateAbmahnung = true;
            if (!betriebsratBenachrichtigtJa) {
                generateBetriebsratBenachrichtigung = true;
                message = message.replace(new RegExp("generiert.", "g"), "& Betriebsrat Benachrichtigung generiert!");
            }
            break;
        case kundigungsgrund === "A":
            message = 'Eine Kündigung ist möglich. :) \r\n Wir haben außerdem eine fadenscheinige Abmahnung generiert!';
            generateAbmahnung = true;
            generateKuendigung = true;
            if (!betriebsratBenachrichtigtJa) {
                generateBetriebsratBenachrichtigung = true;
                message = message.replace(new RegExp("generiert.", "g"), "& Betriebsrat Benachrichtigung generiert!");
            }
            break;
        case kundigungsgrund === "B":
            message = 'Eine Kündigung ist möglich. :) \r\n Wir haben außerdem eine fadenscheinige Abmahnung generiert!';
            generateKuendigung = true;
            generateAbmahnung = true;
            if (!betriebsratBenachrichtigtJa) {
                generateBetriebsratBenachrichtigung = true;
                message = message.replace(new RegExp("generiert.", "g"), "& Betriebsrat Benachrichtigung generiert!");
            }
            break;
        case kundigungsgrund === "C":
            message = 'Eine Kündigung ist möglich. :) Leider müssen wir eine Abfindung anbieten.. :( \r\n Wir haben außerdem eine fadenscheinige Abmahnung generiert!';
            generateKuendigung = true;
            generateAbfindungsangebot = true;
            generateAbmahnung = true;
            if (!betriebsratBenachrichtigtJa) {
                generateBetriebsratBenachrichtigung = true;
                message = message.replace(new RegExp("generiert.", "g"), "& Betriebsrat Benachrichtigung generiert!");
            }
            break;
    }

    console.log('Status:', message);
    document.getElementById('statusPerson').innerText = message;

    let tmpFilesText = "";

    if (generateFristloseKuendigung) {
        console.log('Generiere fristlose Kündigung...');
        let tmpFileName = personalnummer + "_fristlosekuendigung.pdf";
        tmpFilesText += "<li>Fristlose Kündigung: <a href='./dl/pdf/" + tmpFileName + "' target='_blank'>" + tmpFileName + "</a></li>";
    }
    if (generateAbmahnung) {
        console.log('Generiere Abmahnung...');
        let tmpFileName = personalnummer + "_abmahnung.pdf";
        tmpFilesText += "<li>Abmahnung: <a href='./dl/pdf/" + tmpFileName + "' target='_blank'>" + tmpFileName + "</a></li>";
    }
    if (generateKuendigung) {
        console.log('Generiere Kündigung...');
        let tmpFileName = personalnummer + "_.pdf";
        if (kundigungsgrund === "A") {
            tmpFileName = personalnummer + "_personenbedingte_kündigung.pdf";
        }
        if (kundigungsgrund === "B") {
            tmpFileName = personalnummer + "_verhaltungsbedingte_kündigung.pdf";
        }
        if (kundigungsgrund === "C") {
            tmpFileName = personalnummer + "_betriebsbedingte_kündigung.pdf";
        }
        tmpFilesText += "<li>Kündigung: <a href='./dl/pdf/" + tmpFileName + "' target='_blank'>" + tmpFileName + "</a></li>";
    }
    if (generateBetriebsratBenachrichtigung) {
        console.log('Generiere Betriebsrat Benachrichtigung...');
        let tmpFileName = personalnummer + "_schreiben_betriebsrat.pdf";
        tmpFilesText += "<li>Betriebsrat Benachrichtigung: <a href='./dl/pdf/" + tmpFileName + "' target='_blank'>" + tmpFileName + "</a></li>";
    }
    if (generateAbfindungsangebot) {
        console.log('Generiere Abfindungsangebot...');
        let tmpFileName = personalnummer + "_abfindungangsangebot.pdf";
        let tmpFileNameTwo = personalnummer + "_bestätungsformular.pdf";
        tmpFilesText += "<li>Abfindung Angebot: <a href='./dl/pdf/" + tmpFileName + "' target='_blank'>" + tmpFileName + "</a></li>";
        tmpFilesText += "<li>Abfindung Bestätigungsformular: <a href='./dl/pdf/" + tmpFileNameTwo + "' target='_blank'>" + tmpFileNameTwo + "</a></li>";
    }
    generateFiles()

    document.getElementById('files').innerHTML = tmpFilesText;
}
