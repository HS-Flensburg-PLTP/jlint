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
