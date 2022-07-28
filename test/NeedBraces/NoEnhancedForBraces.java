-------------------------
NoBracesEnhancedForMultiLine
+++++++++++++++++++++++++
class Test {
    public void testFunc() {
        for (int i : arr) 
            System.out.println(i);
    }
}
-------------------------
NoBracesEnhancedForSingleLine
+++++++++++++++++++++++++
class Test {
    public void testFunc() {
        for (int i : arr) System.out.println(i);
    }
}