-------------------------
ReturnDoWhileLoop
+++++++++++++++++++++++++
package test.NoLoopBreak.JavaFiles;

public class ReturnDoWhileLoop {

    public static int testFunc() {
        int i = 0;
        do {
            i += 1;
            return i;
        } while (i < 10);
    }

}
-------------------------
ReturnEnhancedForLoop
+++++++++++++++++++++++++
package test.NoLoopBreak.JavaFiles;

public class ReturnEnhancedForLoop {

    public static String testFunc() {
        String[] arr = { "a", "b", "c" };
        for (String elem : arr) {
            elem = elem + "i";
            return elem;
        }
        return "test";
    }

}
-------------------------
ReturnForLoop
+++++++++++++++++++++++++
package test.NoLoopBreak.JavaFiles;

public class ReturnForLoop {

    public static int testFunc() {
        int a = 100;
        for (int i = 0; i < a; i++) {
            System.out.println(i);
            return i;
        }
        return a;
    }
}
-------------------------
ReturnNestedForLoop
+++++++++++++++++++++++++
package test.NoLoopBreak.JavaFiles;

public class ReturnNestedForLoop {

    static boolean testFunc(final String str) {
        boolean dupFound = false;
        for (int i = 0; i < str.length() - 1; i++) {
            char charAtI = str.charAt(i);
            if (charAtI == str.charAt(i + 1)) {
                return true;

            }
        }
        return false;
    }
}
-------------------------
ReturnNestedWhileLoop
+++++++++++++++++++++++++
package test.NoLoopBreak.JavaFiles;

public class ReturnNestedWhileLoop {
    public static int testFunc() {
        int[] arr = { 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 };
        int i = 0;
        while (i < arr.length) {
            if (i == 7) {
                System.out.println(i);
                return i;
            } else {
                i++;
            }
        }
        return i;
    }
}
-------------------------
ReturnWhileLoop
+++++++++++++++++++++++++
package test.NoLoopBreak.JavaFiles;

public class ReturnWhileLoop {
    static boolean testFunc(final String str) {
        boolean found = false;
        int i = 0;
        while (i < str.length()) {
            i++;
            return true;
        }
        return found;
    }
}
