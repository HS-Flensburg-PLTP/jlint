-------------------------
NoBracesIfThenElseFirst
+++++++++++++++++++++++++
class Test {
    public void testFunc() {
        if(true) 
            i++;
        else {
            i--;
        }
    }
}
-------------------------
NoBracesIfThenElseSecond
+++++++++++++++++++++++++
class Test {
    public void testFunc() {
        if(true) {
            i++;
        } else 
            i--;
    }
}
-------------------------
NoBracesIfThenElseBoth
+++++++++++++++++++++++++
class Test {
    public void testFunc() {
        if(true)
            i++;
        else 
            i--;
    }
}