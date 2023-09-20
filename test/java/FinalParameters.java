import java.util.LinkedList;
import java.util.List;

class FinalParameters {

    public FinalParameters(final String name) {
    }

    public FinalParameters(String name, final int number) {
    }

    public void foo(final String name, int age) {
    }

    public void boo(String test, final int color) {
    }

    public void bar(String text1, String text2) {
    }

    public final void finalbar(final String text1, final String text2) {
    }

    public void test() {
    }

    public void baa() {
        final String word = "test";
    }

    public void catchTest() {
        int a = 5;
        int b = 4;

        try {
            a = 6;
        } catch (final Exception ex) {
            throw ex;
        }
    }

    public void catchTest2() {
        int a = 5;
        int b = 4;

        try {
            a = 6;
        } catch (Exception ex) {
            throw ex;
        }
    }

    public void constTest() {
        final List<String> list = new LinkedList<>();
    }
}
