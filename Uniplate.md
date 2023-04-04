# Uniplate
>Die Grundidee hinter Uniplate ist, dass viele Operationen auf Datenstrukturen ähnliche Muster aufweisen, wie z.B. das Durchlaufen von Kindknoten oder das Ersetzen von Teilbäumen. Uniplate bietet eine generische Möglichkeit, solche Muster auf Datenstrukturen anzuwenden, wodurch Code wiederverwendet werden kann und die Lesbarkeit verbessert wird.

### Funktionen der Uniplate Bibliothek

### universe
Die Funktion `universe` nimmt eine Datenstruktur vom Typ `a` und gibt eine Liste aller darin enthaltenen Strukturen desselben Typs zurück.

```haskell
universe :: Uniplate a => a -> [a]
```

### Verwendung von universe in jlint
Im folgenden Fall werden mit `universe` alle Strukturen vom Typ `Stmt` aus einem `Stmt` in eine Liste generiert. 
Und mit list comprehension werden dann alle `Stmt` aus der Liste, die mit dem Konstruktor `Empty` erzeugt wurden, in eine neue Liste getan.

```haskell
getEmptysFromStmt :: Stmt -> [Stmt]
getEmptysFromStmt stmt = [Empty| Empty <- universe stmt]
```


### universeBi
Die Funktion `universeBi` nimmt eine Datenstruktur vom Typ `from`, die Datenstrukturen vom Typ `to` enthält,  und gibt alle enthaltenen `to`-Strukturen in einer Liste aus. 
Wird `universeBi` auf eine Datenstruktur vom Typ `from` aufgerufen, die keine Strukturen vom Typ `to` enthält, wird eine leere Liste zurückgegeben.

```haskell
universeBi :: Biplate from to => from -> [to]
universeBi = universeOn biplate 
```

### Verwendung von universeBi in jlint
In jlint wird `universeBi` verwendet um Strukturen eines bestimmten Types aus dem AST heraus zu filtern.

In der folgenden Funktion liefert `universeBi` bei Anwendung auf eine `CompilationUnit` eine Liste aller darin enthaltenen `Stmt`.
```haskell
getStatements :: CompilationUnit -> [Stmt]
getStatements cUnit = universeBi cUnit
```

Die folgenden Beispiele zeigen, wie `universeBy` in jlint unterschiedlich verwendet wird.

``` haskell
--Ausschnitt aus Language.Java.Rules.AvoidNegations
checkStatements :: (String, MethodBody) -> FilePath -> [RDF.Diagnostic]
checkStatements (methodName, methodBody) path = do
  stmt <- universeBi methodBody
  checkStatement stmt
  where
  ...

--Ausschnitt aus Language.Java.Rules.DefaultComesLast
checkDefaultComesLast :: (String, MethodBody) -> FilePath -> [RDF.Diagnostic]
checkDefaultComesLast (methodName, methodBody) path = do
  (Switch _ _ blocks) <- universeBi methodBody
  checkSwitch blocks mzero
  ...

--Ausschnitt aus Language.Java.Rules.UseElse
checkIfWithoutElse :: CompilationUnit -> FilePath -> [RDF.Diagnostic]
checkIfWithoutElse cUnit path = do
  blocks <- universeBi cUnit
  checkBlocks blocks
  where
  ...
``` 


