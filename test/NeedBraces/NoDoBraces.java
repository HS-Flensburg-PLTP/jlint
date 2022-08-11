-------------------------
NoBracesDoMultiLine
+++++++++++++++++++++++++
class Test {
    public void testFunc() {
        do
            i++;
        while (true);
    }
}
-------------------------
NoBracesDoOneLine
+++++++++++++++++++++++++
class Test {
    public void testFunc() {
        do i++; while (true);
    }
}