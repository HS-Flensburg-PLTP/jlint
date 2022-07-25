package test.noLoopBreak.JavaFiles;

public class BreakForLoop {

    public static void testFunc() {
        int a = 100;
        for (int i = 0; i < a; i++) {
            System.out.println(i);
            break;
        }
    }
}