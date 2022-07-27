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
