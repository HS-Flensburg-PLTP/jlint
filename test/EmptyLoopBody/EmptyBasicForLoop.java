-------------------------
EmptyBody
+++++++++++++++++++++++++
package de.fuas.algorithms;

public class Strings2 {

    public void testFunc(String name, final int age) {
        int i = 50;
        int j = 150;
        for(int i = 0; i <= 3; i ++){};
    }
}
-------------------------
WithoutBody
+++++++++++++++++++++++++
package de.fuas.algorithms;

public class Strings2 {

    public void testFunc(String name, final int age) {
        int i = 50;
        int j = 150;
        for(int i = 0; i <= 3; i ++);
    }
}