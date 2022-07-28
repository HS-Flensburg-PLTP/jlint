-------------------------
NoBracesIfThenMultiLine
+++++++++++++++++++++++++
class Test {
    public void testFunc() {
        if (true)
            i++;       
    }
}
-------------------------
NoBracesIfThenOneLine
+++++++++++++++++++++++++
class Test {
    public void testFunc() {
        if(true) i++;
    }
}