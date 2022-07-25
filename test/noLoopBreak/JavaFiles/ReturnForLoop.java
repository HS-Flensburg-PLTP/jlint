package test.NoLoopBreak.JavaFiles;

public class ReturnForLoop {

    public static int testFunc() {
        int a = 100;
        for (int i = 0; i < a; i++) {
            System.out.println(i);
            return i;
        }
        return a;
    }
}