-------------------------
NoBracesBasicForMultiLine
+++++++++++++++++++++++++
class Test {
    public void testFunc() {
        for(int i = 0; i < 10; i++) 
            System.out.println(i);        
    }
}
-------------------------
NoBracesBasicForOneLine
+++++++++++++++++++++++++
class Test {
    public void testFunc() {
        for(int i = 0; i < 10; i++) System.out.println(i); 
    }
}