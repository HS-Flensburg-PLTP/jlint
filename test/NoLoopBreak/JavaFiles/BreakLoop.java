-------------------------
BreakDoWhileLoop
+++++++++++++++++++++++++
package test.NoLoopBreak.JavaFiles;

public class BreakDoWhileLoop {

    public static void testFunc() {
        int i = 0;
        do {
            i += 1;
            break;
        } while (i < 10);
    }
}

-------------------------
BreakEnhancedForLoop
+++++++++++++++++++++++++
package test.NoLoopBreak.JavaFiles;

public class BreakEnhancedForLoop {

    public static void testFunc() {
        String[] arr = { "a", "b", "c" };
        for (String elem : arr) {
            elem = elem + "i";
            break;
        }
    }
}
-------------------------
BreakForLoop
+++++++++++++++++++++++++
package test.NoLoopBreak.JavaFiles;

public class BreakForLoop {

    public static void testFunc() {
        int a = 100;
        for (int i = 0; i < a; i++) {
            System.out.println(i);
            break;
        }
    }
}
-------------------------
BreakNestedForLoop
+++++++++++++++++++++++++
package test.NoLoopBreak.JavaFiles;

public class BreakNestedForLoop {

    public static void testFunc() {
        int[] arr = { 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 };
        for (int i = 0; i < arr.length; i++) {
            if (i == 7) {
                System.out.println(i);
                break;
            }
        }
    }
}
-------------------------
BreakNestedWhileLoop
+++++++++++++++++++++++++
package test.NoLoopBreak.JavaFiles;

public class BreakNestedWhileLoop {

    public static void testFunc() {
        int[] arr = { 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 };
        int i = 0;
        while (i < arr.length) {
            if (i == 7) {
                System.out.println(i);
                break;
            } else {
                i++;
            }
        }
    }

}
-------------------------
BreakWhileLoop
+++++++++++++++++++++++++
package test.NoLoopBreak.JavaFiles;

public class BreakWhileLoop {

    public static void testFunc() {
        int i = 0;
        while (i < 10) {
            i++;
            break;
        }
    }
}
