package test.noLoopBreak.JavaFiles;

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
