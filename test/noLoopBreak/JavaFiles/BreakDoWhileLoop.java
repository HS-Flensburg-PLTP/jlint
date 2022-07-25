package test.noLoopBreak.JavaFiles;

public class BreakDoWhileLoop {

    public static void testFunc() {
        int i = 0;
        do {
            i += 1;
            break;
        } while (i < 10);
    }
}
