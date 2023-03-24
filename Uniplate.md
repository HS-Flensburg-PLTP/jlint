# Uniplate
>Die grundlegende Idee hinter Uniplate ist, dass viele Operationen auf Datenstrukturen ähnliche Muster aufweisen, wie zum Beispiel das Durchlaufen von Kindknoten oder das Ersetzen von Teilbäumen. Uniplate stellt eine generische Möglichkeit bereit, solche Muster auf Datenstrukturen anzuwenden, wodurch Code wieder verwendet werden kann und sich die Lesbarkeit verbessert.

### Funktionen der Uniplate Bibliothek

Die universe Funktion nimmt eine Datenstruktur und gibt eine Liste an alle in ihr liegenden Strukturen desselben Typs zurück.
```
universe :: Uniplate α ⇒ α → [α] 
universe x = x : concatMap universe (children x )
```

Die universeBi Funktion nimmt eine Datenstruktur vom Typ from die Datenstrukturen von Typ to enthält und gibt alle enthaltenen to Strukturen in einer List aus. 
```
universeBi :: Biplate from to ⇒ from → [to] 
universeBi = universeOn biplate 
```

#### Funktionsweise in JLint
```
stmt <- universeBi cUnit
```
universeBi auf eine CompilationUnit angewendet, liefert eine Liste an allen Statements, die sie enthält. 
Der zurückgelieferte Typ wird bestimmt durch Typinferenz, ausgehend davon, wie die Liste danach verwendet wird. 


