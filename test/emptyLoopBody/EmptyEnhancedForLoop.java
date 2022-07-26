package de.fuas.algorithms;

public class Strings2 {

    public void testFunc(String name, final int age) {
        String[] arr = {"a", "b", "c"};
        for(String elem : arr);
        // TODO: do {} while(true); IS NOT SEEN AS EMPTY FOR LOOP  
    }
}
-------------------------
EmptyBody
+++++++++++++++++++++++++
package de.fuas.algorithms;

public class Strings2 {

    public void testFunc(String name, final int age) {
        do; while(true);
    }
}

-------------------------
WithoutBody
+++++++++++++++++++++++++
package de.fuas.algorithms;

public class Strings2 {

    public void testFunc(String name, final int age) {
        do {} while(true);
    }
}