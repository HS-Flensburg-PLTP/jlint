# jlint

## Setup

1. [GHCup](https://www.haskell.org/ghcup/) installieren

2. [VSCode](https://code.visualstudio.com) installieren

3. VSCode Extension `Haskell` installieren

In den Extension Settings sollten noch folgende Einstellungen überprüft werden:
- Formatting Provider: `ormolu`
- Manage HLS: `GHCup`
- Ghcup Executable Path: expl. Angabe, wenn `ghcup` nicht im PATH vorhanden

Die Extension übernimmt dann automatisch die notwendigen Installationen (GHC, HLS).

Bei korrektem Setup sollten z.B folgende Features von HLS in VScode funktionieren:
- Linter (als Anmerkungen im Code)
- Formatter `Shift-Alt-F`
- Typinformationen `Hover`
- Sprung zur Deklaration `Ctrl-Click`

### Empfehlungen für VSCode

- `Insert Final Newline` aktivieren
  - der Formatchecker über github erwartet diese, lokal fügt Ormolu diese nicht automatisch hinzu
- `Auto Save` und `Format on Save` aktivieren

## Entwicklung

### Branching

Der Branch `main` stellt den Entwicklungsstand dar.  
Der Zustand auf `release` wird auf den GitHub Runner ausgeliefert und ist somit der Production-Branch.  

#### Feature Branch

Beim Entwickeln eines neuen Features ist stets ein eigener Branch anzulegen.  
Dieser zweigt von `main` ab und folgt der Namenskonvention `feature-<issue>`, wobei `<issue>` für die ID des Tickets steht.

### Schreiben neuer Regeln

Zu neuen Regeln sollten immer entsprechende Testfälle mitentwickelt werden (Ordner: `test`). Dazu gehören Java-Beispieldateien, die eigentlichen Tests und der jeweilige Eintrag in `test/Spec.hs`.

Um neue Regeln in der Anwendung zu integrieren, müssen diese in `src/Language/Java/Rules.hs` angegeben werden.

## Lokale Verwendung

- `stack build` - Laden der Dependencies und Builden des Projekts
- `stack test` - Führt die Test-Suite aus
- `stack exec jlint <srcpath>` - Ausführen der Executable
  - nützliche Entwickler-Optionen: (zusätzlich `--` zum Escapen der Optionen von `stack`)
    - `-t/--show-ast` - nur Anzeige von AST, keine Analyse
    - `--pretty` - printet formatierten Java-Code
  - Beispiel 1: `stack exec jlint -- /test/java` führt die Analyse aller .java Files im angegebenen Ordner durch
  - Beispiel 2: `stack exec jlint -- /test/java/UseElse.java -t` zeigt den AST der Datei