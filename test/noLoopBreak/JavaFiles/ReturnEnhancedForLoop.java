package test.noLoopBreak.JavaFiles;

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
