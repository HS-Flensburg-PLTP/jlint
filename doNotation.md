# do Notation (oder: Monaden sind überall)
Do Notationen sind an imperative Sprachen erinnernde Konstrukte um Funktionen (in diesem Fall Monaden) aneinander zu reihen.
```haskell
check :: CompilationUnit -> FilePath -> [Diagnostic]
check cUnit path = do
methods <- extractMethods cUnit
checkStatements methods path
```
Die do-Notation ist “nur” syntaktischer Zucker für Monadenverkettung:
```haskell
check :: CompilationUnit -> FilePath -> [Diagnostic]
check cUnit path = extractMethods cUnit >>= (\methods -> checkStatements methods path)
```
```haskell
check cUnit path = do
```
In diesem Beispiel wird die Funktion “check” eingeführt die eine CompilationUnit (cUnit) und einen FilePath (path) bekommt.
Das Wort “do” leitet den Funktionskörper ein.
```haskell
methods <- extractMethods cUnit
```
Hier wird die Funktion “extractMethods” aufgerufen mit der CompilationUnit als Argument. Der Rückgabewert wird hier in der lokalen Variable “methods”
zwischengespeichert
```haskell
checkStatements methods path
```
und als Argument für den Aufruf der nächsten Funktion ```checkStatements``` genutzt. Diese Funktion beendet die do-Notation und gibt ihren Rückgabewert zurück.
Die Funktionsdeklaration:
```haskell
checkStatements :: (String, MethodBody) -> FilePath -> [Diagnostic]
```
verrät uns, dass hier die benötigte Liste von Diagnostic zurückgegeben wird, die als Rückgabe von ```check``` deklariert wurde.
Eine Funktion die in einer do-Notation aufgerufen wird ohne Ihren Wert zwischenzuspeichern oder dass sie der letzte Aufruf wäre, wird nur ausgeführt, ohne dass Ihre Rückgabe benutzt wird.
```haskell
putStrLn “String”
```
wäre eine solche “leere” Funktion, die eigentlich keinen Wert zurückgibt. Tatsächlich aber gibt sie eine “leere” IO Action zurück (```IO ()```).
Innerhalb eines do-Blocks sind alle bekannten Sprachkonstrukte in Haskell nutzbar (z. B. ```if-then-else, guards, let, where```, etc.).