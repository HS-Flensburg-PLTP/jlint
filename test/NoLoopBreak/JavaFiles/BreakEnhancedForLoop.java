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
