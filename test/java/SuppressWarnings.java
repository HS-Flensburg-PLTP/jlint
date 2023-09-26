@SuppressWarnings
public class SuppressWarnings {

    @SuppressWarnings
    void method1() {
        @SuppressWarnings("unchecked") var x = 1;
    }

    @SuppressWarnings(value = "unchecked")
    void method2() {
        @SuppressWarnings("unchecked") var x = 1;
    }
}
