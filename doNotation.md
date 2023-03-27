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

und als Argument für den Aufruf der nächsten Funktion `checkStatements` genutzt. Diese Funktion beendet die do-Notation und gibt ihren Rückgabewert zurück.
Die Funktionsdeklaration:

```haskell
checkStatements :: (String, MethodBody) -> FilePath -> [Diagnostic]
```

verrät uns, dass hier die benötigte Liste von Diagnostic zurückgegeben wird, die als Rückgabe von `check` deklariert wurde.

Eine Funktion die in einer do-Notation aufgerufen wird ohne Ihren Wert zwischenzuspeichern oder dass sie der letzte Aufruf wäre, wird nur ausgeführt, ohne dass Ihre Rückgabe benutzt wird.

```haskell
putStrLn “String”
```

wäre eine solche “leere” Funktion, die eigentlich keinen Wert zurückgibt. Tatsächlich aber gibt sie eine “leere” IO Action zurück (`IO ()`).

Innerhalb eines do-Blocks sind alle bekannten Sprachkonstrukte in Haskell nutzbar (z. B. `if-then-else`, `guards`, `let`, `where`, etc.).

## Einführung Monaden

Monaden sind eine Typklasse die Container-Datenstrukturen beschreiben, die einen puren Datentyp umschließen.

Folgende Funktionen sind für Monaden definiert:

```haskell
TypKlasse Monad
(>>=) :: Monad m => m a -> (a -> m b) -> m b
(>>) :: Monad m => m a -> m b -> m b
return :: Monad m => a -> m a
```

(>>=) : Der Bind Operator
Verknüpft eine Monade auf der linken Seite mit einer Funktion auf der rechten Seite die den Urtyp von der Monade der linken Seite als Eingabe benötigt.

(>>) :
Verwirft die Rückgabe der ersten Monade. Nützlich für Operationen mit unerwünschten Seiteneffekten (z.B. IO)

return :
Macht aus einem puren Datentyp eine Monade. (Im Falle von Maybe würde hier "Just a" erzeugt werden)

```haskell
TypKlasse MonadPlus
mzero :: Monad m => m a
```

mzero:
Gibt das "Nichts" der Monade zurück.
Im Falle einer Liste wäre das die leere Liste [], im Falle vom Maybe wäre es `Nothing`.

Eine Anwendung für `mzero` findet man in jeder Regel, falls keines der Kriterien zutrifft:

```haskell
checkStatements :: (String, MethodBody) -> FilePath -> [Diagnostic]
checkStatements (methodName, methodBody) path = do
  stmt <- universeBi methodBody
  checkStatement stmt
  where
    checkStatement (Do Empty _) = return (methodDiagnostic methodName "A Do-Loop has a empty loop body." path)
    [...]
    checkStatement (EnhancedFor _ _ _ _ (StmtBlock (Block []))) = return (methodDiagnostic methodName "A ForEach-Loop has a empty loop body." path)
    checkStatement _ = mzero
```
