# jlint

## Setup

1. [GHCup](https://www.haskell.org/ghcup/) installieren

2. [VSCode](https://code.visualstudio.com) installieren

3. VSCode Extension `Haskell` installieren

In den Extension Settings sollten noch folgende Einstellungen überprüft werden:
- Formatting Provider: ormolu
- Manage HLS: GHCup

Die Extension übernimmt dann automatisch die notwendigen Installationen (GHC, HLS).

### Trojanerwarnung unter Windows

Der Versuch, die HLS-Version 1.9.1.0 zu installieren über GHCup scheitert aktuell (diese Version wird aber von der Extension verwendet).
Für die Installation (auch automatisch ausgelöst durch die Extension) sollte temporär eine Ausnahme hinzugefügt werden:
- In der `Protection history` unter `Virus & threat protection` kann der flaschgemeldete Trojaner erlaubt werden vor dem erneuten Versuch der Installation
- In `Allowed threats` kann nach erfolgter Installation die Ausnahme wieder entfernt werden

## Entwicklung

Bei korrektem Setup sollten z.B folgende Features von HLS in VScode funktionieren:
- Linter (als Anmerkungen im Code)
- Formatter `Shift-Alt-F`
- Typinformationen `Hover`
- Sprung zur Deklaration `Ctrl-Click`

## Verwendung

- `stack build` - Laden der Dependencies und Builden des Projekts
- `stack exec jlint -- --path <srcpath>` ausführen der Executable

## Branching

Der Branch `main` stellt den Entwicklungsstand dar.  
Der Zustand auf `release` wird auf den GitHub Runner ausgeliefert und ist somit der Production-Branch.  

#### Feature Branch

Beim Entwickeln eines neuen Features ist stets ein eigener Branch anzulegen.  
Dieser zweigt von `main` ab und folgt der Namenskonvention `feature-<issue>`, wobei `issue` für die ID des Tickets steht.
