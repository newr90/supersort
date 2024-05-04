const personalnummerForm = document.getElementById("personalnummerForm");
const personalnummerSelect = document.getElementById("personalnummer");
let letzteFrist = 0;

function generateFiles() {
    const selectedPersonalnummer = personalnummerSelect.value;
    const selectedKundigungsgrund = document.getElementById("kundigungsgrund").value;
    let kundigungsgrund = document.getElementById("kundigungsgrund").value
    const kundigungsgrundText = {
        A: "Mitarbeiter ist für die Stelle nicht (mehr) geeignet",
        B: "Mitarbeiter hat sich falsch verhalten",
        C: "Aufgrund der wirtschaftlichen Lage entfällt der Arbeitsplatz des Mitarbeiters"
    };
    // API-Anfrage, um den "create_file"-Endpunkt aufzurufen
    const requestData = {
        unternehmen: "Sample Company",
        absender: "Sample Company, Sample Address",
        empfaenger: {
            name: document.getElementById("name").value,
            straße_hausnummer: document.getElementById("adresse").value,
            plz_ort: document.getElementById("plz").value,
        },
        grund: kundigungsgrundText[kundigungsgrund],
        personalnummer: selectedPersonalnummer,
        name: document.getElementById("name").value,
        ort: "Sample City",
        texttype: "verhaltungsbedingte_kündigung, abfindungangsangebot, abmahnung, bestätungsformular, betriebsbedingte_kündigung, fristlosekuendigung, personenbedingte_kündigung, schreiben_betriebsrat, verhaltungsbedingte_kündigung",
        ihr_Name: "Samuel Adams",
        Ihre_Position: "Personalabteilung",
        e_mail: "your.email@example.com",
        unser_zeichen: "ABC-123",
        telefon: "1234567890",
        kündigungsfrist: letzteFrist,
        ausfürliche_beschreibung: "Sample description",
        abfindung: "2000€",
    };

    fetch(`https://lolex.info:3500/create_file`, {
        method: 'POST',
        headers: {
            'Content-Type': 'application/json',
        },
        body: JSON.stringify(requestData),
    })
        .then(response => response.json())
        .then(result => {
            console.log(result);
        })
        .catch(error => {
            console.error('Error:', error);
        });
}

// API-Anfrage, um Personalnummern zu laden
fetch('https://lolex.info:3500/mitarbeiter')
    .then(response => response.json())
    .then(data => {
        // Erstelle eine leere Option
        const emptyOption = document.createElement('option');
        emptyOption.value = '';
        emptyOption.textContent = 'Bitte auswählen';

        // Füge die leere Option zuerst hinzu
        personalnummerSelect.appendChild(emptyOption);

        // Füge die Mitarbeiter-Optionen hinzu
        data.forEach(mitarbeiter => {
            const option = document.createElement('option');
            option.value = mitarbeiter.Personalnummer;
            option.textContent = `${mitarbeiter.Personalnummer} - ${mitarbeiter.Vorname} ${mitarbeiter.Nachname}`;
            personalnummerSelect.appendChild(option);
        });
    });

// In der Funktion, die aufgerufen wird, wenn eine Personalnummer ausgewählt wird
personalnummerSelect.addEventListener("change", function () {
    const selectedPersonalnummer = personalnummerSelect.value;

    // API-Anfrage, um die Daten des ausgewählten Mitarbeiters und seine Beschäftigungen zu laden
    fetch(`https://lolex.info:3500/mitarbeiter/${selectedPersonalnummer}`)
        .then(response => response.json())
        .then(data => {
            // Daten in die entsprechenden Formularfelder einfügen
            document.getElementById("name").value = data[0].Vorname + ' ' + data[0].Nachname;
            document.getElementById("geburtstag").value = data[0].Geburtstag;
            document.getElementById("plz").value = data[0].Postleitzahl;
            document.getElementById("adresse").value = data[0].Adresse;
            // Das Geburtsdatum im gewünschten Format anzeigen
            const geburtstag = new Date(data[0].Geburtstag);
            const geburtstagFormatted = `${geburtstag.getDate()}.${geburtstag.getMonth() + 1}.${geburtstag.getFullYear()}`;
            document.getElementById("geburtstag").value = geburtstagFormatted;
            // Alter berechnen und anzeigen
            const currentDate = new Date();
            const age = currentDate.getFullYear() - geburtstag.getFullYear();
            document.getElementById("alter").value = age;
            //checkboxes                    
            document.getElementById("betriebsratMitgliedJa").checked = data[0].IstBetriebsratMitglied === 1;
            document.getElementById("azubiJa").checked = data[0].IstAuszubildender === 1;
            document.getElementById("behindertJa").checked = data[0].IstSchwerbehindert === 1;
            document.getElementById("mutterschutzJa").checked = data[0].IstImMutterschutz === 1;

            // Sortiere die Beschäftigungen nach dem Startdatum
            data.sort((a, b) => new Date(a.start) - new Date(b.start));

            // Anzeige der Beschäftigungen
            const beschaeftigungenList = document.getElementById("beschaeftigungenList");
            beschaeftigungenList.innerHTML = ""; // Löschen der vorherigen Einträge

            // Iteration durch alle bekannten Beschäftigungen
            data.forEach(beschaeftigung => {
                const startDate = new Date(beschaeftigung.start);
                const endDate = beschaeftigung.end ? new Date(beschaeftigung.end) : new Date(); // Wenn end null ist, verwenden Sie das aktuelle Datum
                const months = (endDate.getFullYear() - startDate.getFullYear()) * 12 + endDate.getMonth() - startDate.getMonth();

                // Berechne die Kündigungsfrist zur Betriebszugehörigkeit
                const betriebszugehoerigkeitFrist = calculateKuendigungsfrist(months);
                letzteFrist = calculateKuendigungsfrist(months);

                // Berechnung der Regelabfindung
                const gehalt = calcRandomGehalt();
                const abfindung = calculateAbfindung(months, gehalt);

                const beschaeftigungInfo = [
                    `<p><strong>Abteilung: ${beschaeftigung.abteilung}</strong>`,
                    `<li>Dauer: ${months} Monate (Einstand: ${startDate.toLocaleDateString()}, Austand: ${endDate.toLocaleDateString()})</li>`,
                    `<li>Kündigungsfrist: ${betriebszugehoerigkeitFrist} Monate</li>`,
                    `<li>Abfindung: ${abfindung} EUR bei ${gehalt} € Monatsgehalt</li></p></br></br>`
                ];
                // Ausgabe
                for (let i = 0; i < beschaeftigungInfo.length; i++) {
                    beschaeftigungenList.innerHTML += `${beschaeftigungInfo[i]}`;
                }
            });
        });

    // weitere Daten Formular anzeigen
    document.getElementById("weitereDaten").style.display = "block";
    // evtl. vorherige Kündigungs Formularergebnisse verbergen
    document.getElementById("kundigungErgebnis").style.display = "none";
    // reset button anzeigen
    const resetbutton = document.getElementById("resetbutton").style.display = "block";
});
