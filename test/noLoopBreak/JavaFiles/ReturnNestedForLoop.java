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
