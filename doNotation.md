# do-Notation

## Einführung Monaden
Monaden sind eine Typklasse die Container-Datenstrukturen beschreiben, die einen puren Datentyp umschließen.
Da im Projekt `jlint` überwiegend die Listenmonade Einsatz findet, wird diese eingehender betrachtet.

Folgende Funktionen sind für die Typklasse `Monad` definiert:

```haskell
(>>=) :: Monad m => m a -> (a -> m b) -> m b
(>>) :: Monad m => m a -> m b -> m b
return :: Monad m => a -> m a
```

(`>>=`) : bind-Operator\
Verknüpft eine Monade auf der linken Seite mit einer Funktion auf der rechten Seite die den Urtyp von der Monade der linken Seite als Eingabe benötigt.\
Eine Implementierung von `>>=` für `[]` könnte folgendermaßen aussehen:
```haskell
x >>= f = concat (map f x)
```

(`>>`) :
Verwirft die Rückgabe des ersten bzw. linken Arguments. Nützlich für Operationen mit unerwünschten Seiteneffekten.

`return` :
Macht aus einem puren Datentyp eine Monade. (Im Falle von `[]` würde hier `[a]` erzeugt werden)

Typklasse MonadPlus
```haskell
mzero :: Monad m => m a
```

`mzero`\
Gibt das "Nichts" der Monade zurück.
Im Falle von `[]` ist das die leere Liste `[]`.

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

Die `do`-Notation ist ein an imperative Sprachen erinnerndes Konstrukt, um Funktionen nacheinander auszuführen.

```haskell
extractMethods :: CompilationUnit -> [(String, MethodBody)]
extractMethods cUnit = do
  membDecl <- universeBi cUnit
  extractBody membDecl
  where
    extractBody (MethodDecl _ _ _ _ (Ident n) _ _ _ b) = return (n, b)
    extractBody _ = mzero
```

Bei der `do`-Notation handelt es sich um syntaktischen Zucker (eine Sprach-Erweiterung, ohne eine neue Funktion einzuführen) - der Compiler wandelt den aufgeführten `do`-Block folgendermaßen um:

```haskell
extractMethods cUnit = do
  membDecl <- universeBi cUnit
  extractBody membDecl
```
```haskell
extractMethods cUnit = universeBi cUnit >>= (\membDecl -> extractBody membDecl)
```
Man kann es sich so vorstellen, dass `universeBi cUnit` eine Liste zurückgibt, von der jedes Element genommen und als Parameter an `extractBody` übergeben wird.

Innerhalb eines do-Blocks sind alle bekannten Sprachkonstrukte in Haskell nutzbar (z. B. `if-then-else`, `guards`, `let`, `where`, etc.).

