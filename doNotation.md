check :: CompilationUnit -> FilePath -> [Diagnostic]
check cUnit path = do
methods <- extractMethods cUnit
checkStatements methods path

funktion arg1 arg2 = do
zwischenvar <- funktion arg
funktion2 zwischenvar arg

Do Notationen sind an imperative Sprachen erinnernde Konstrukte um Funktionen aneinander zu reihen.

check :: CompilationUnit -> FilePath -> [Diagnostic]
check cUnit path = do
methods <- extractMethods cUnit
checkStatements methods path

check cUnit path = do

In diesem Beispiel wird die Funktion "check" eingeführt die eine CompilationUnit (cUnit) und einen FilePath (path) bekommt.
Das Wort "do" leitet den Funktionskörper ein.

methods <- extractMethods cUnit

Hier wird die Funktion "extractMethods" aufgerufen mit der CompilationUnit als Argument. Der Rückgabewert wird hier in der lokalen Variable "methods"
zwischengespeichert,

checkStatements methods path

und als Argument für den Aufruf der nächsten Funktion "checkStatements" genutzt. Diese Funktion beendet die do-Notation und gibt ihren Rückgabewert zurück.
Die Funktionsdeklaration:

checkStatements :: (String, MethodBody) -> FilePath -> [Diagnostic]

verrät uns das hier die benötigte Liste von Diagnostic zurückgegeben wird, die als Rückgabe von "check" deklariert wurde.

Eine Funktion die in einer Do-Notation aufgerufen wird ohne Ihren Wert zwischenzuspeichern oder das sie der letzte Aufruf wäre, wird nur ausgeführt, ohne das Ihre Rückgabe benutzt wird.
