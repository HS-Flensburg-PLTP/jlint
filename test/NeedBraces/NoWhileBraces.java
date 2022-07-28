-------------------------
NoBracesWhileMultiLine
+++++++++++++++++++++++++
class Test {
    public void testFunc() {
        while (true)
            i++;
    }
}
-------------------------
NoBracesWhileOneLine
+++++++++++++++++++++++++
class Test {
    public void testFunc() {
        while (true) i++;
    }
}