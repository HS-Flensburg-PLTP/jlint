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
