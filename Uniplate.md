# Uniplate
> Die Grundidee hinter Uniplate ist, dass viele Operationen auf Datenstrukturen ähnliche Muster aufweisen, wie z.B. das Durchlaufen von Kindknoten oder das Ersetzen von Teilbäumen. Uniplate bietet eine generische Möglichkeit, solche Muster auf Datenstrukturen anzuwenden, wodurch Code wiederverwendet werden kann und die Lesbarkeit verbessert wird.

Um die Beispiele in diesem Text zu verstehen, sollte zuvor der Text zur [`do`-Notation](doNotation.md) gelesen werden.

### Funktionen der Uniplate-Bibliothek

### universe
Die Funktion `universe` nimmt eine Datenstruktur vom Typ `a` und gibt eine Liste aller darin enthaltenen Strukturen desselben Typs zurück.

```haskell
universe :: Uniplate a => a -> [a]
```

### Verwendung von universe in jlint
Im folgenden Fall werden in einem `do`-Block mit `universe` nacheinander alle Strukturen vom Typ `Stmt` aus einem `Stmt` gelesen. Diese werden einzeln mit der Hilfsmethode überprüft, ob sie eine Schleife darstellen. Wenn ja, wird die Abbruchbedingung der Schleife zurückgegeben.

```haskell
extractLoopCondiditions :: Stmt -> [Exp]
extractLoopCondiditions parentStmt = do 
    stmt <- universe parentStmt
    checkLoop stmt
    where
        checkLoop (While exp _) = return exp
        checkLoop (BasicFor _ (Just exp) _ _) = return exp
        checkLoop (EnhancedFor _ _ _ exp _) = return exp
        checkLoop _ = mzero
```


### universeBi
Die Funktion `universeBi` nimmt eine Datenstruktur vom Typ `from`, die Datenstrukturen vom Typ `to` enthält,  und gibt alle enthaltenen `to`-Strukturen in einer Liste aus. 
Wird `universeBi` auf eine Datenstruktur vom Typ `from` aufgerufen, die keine Strukturen vom Typ `to` enthält, wird eine leere Liste zurückgegeben.

```haskell
universeBi :: Biplate from to => from -> [to]
universeBi = universeOn biplate 
```

### Verwendung von universeBi in jlint
In jlint wird `universeBi` verwendet um Strukturen eines bestimmten Typs aus dem AST herauszufiltern.

In der folgenden Funktion liefert `universeBi` bei Anwendung auf eine `CompilationUnit` eine Liste aller darin enthaltenen `Stmt`.
```haskell
statements :: CompilationUnit -> [Stmt]
statements cUnit = universeBi cUnit
```

Die folgenden Beispiele zeigen, wie `universeBy` in jlint unterschiedlich verwendet wird.

``` haskell
--Ausschnitt aus Language.Java.Rules.AvoidNegations checkStatements
checkStatements :: CompilationUnit -> FilePath -> [RDF.Diagnostic]
checkStatements cUnit path = do
  stmt <- universeBi cUnit
  checkIfThenElse stmt path
  where
    checkIfThenElse :: Stmt -> FilePath -> [RDF.Diagnostic]
    ...

--Ausschnitt aus Language.Java.Rules.AvoidNegations checkExpressions
checkExpression :: CompilationUnit -> FilePath -> [RDF.Diagnostic]
checkExpression cUnit path = do
  exp <- universeBi cUnit
  checkCond stmt path
  where
    checkCond :: Exp -> FilePath -> [RDF.Diagnostic]
    ...

--Ausschnitt aus Language.Java.Rules.AST
extractMethods :: CompilationUnit -> [(String, MethodBody)]
extractMethods cUnit = do
  membDecl <- universeBi cUnit
  extractBody membDecl
  where
    extractBody :: MemberDecl -> [(String, MethodBody)]
    extractBody (MethodDecl _ _ _ _ (Ident n) _ _ _ b) = return (n, b)
    extractBody _ = mzero
``` 
