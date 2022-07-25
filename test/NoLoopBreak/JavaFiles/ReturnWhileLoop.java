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
