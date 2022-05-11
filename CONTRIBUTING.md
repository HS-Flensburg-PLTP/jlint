# Namenskonvention für Branches

Es gibt die Branches `main`, `dev` und `qa`. `qa` ist der Branch für Testing
Alle weiteren Branches sind nach folgendem Schema definiert.
Feature Branches zweigen vom `dev` ab und beginnen mit dem Schlüsselwort `feature`, gefolgt 
von der zugehörigen Nummer des Issues und einem kurzen Titel.
Bug Branches zweigen vom jeweiligen Branch ab, in dem der Bug gefunden wurde und 
werden mit dem Keyword Bug eigeleitet und es folgt ebenfalls die Issue Nummer und der Titel.
Die Wörter des Namens eines Branches werden mit Bindestrich getrennt und klein geschrieben.
__Beispiel:__
`feature-11-first-linting-rule`
`bug-11-first-linting-rule`
