import java.util.LinkedList;
import java.util.List;

class NoExtraDataStructures {
    public void set() {
        String b = "testString";
        Integer a = 5;
        String[] test = new String[5];
        String[] test2 = new String[5];
    }

    public void set(String param) {
        String b = "testString";
        Integer a = 5;
    }

    public void get() {
        String b = "testString";
        Integer a = 5;
    }

    public void get(String param) {
        String b = "testString";
        Integer a = 5;
        String[] test = new String[5];
    }

    public void push() {
        String b = "testString";
        Integer a = 5;
    }

    public void pull() {
        String b = "testString";
        Integer a = 5;
        String[] test = new String[5];
    }

    public void pop() {
        List<String> list = new LinkedList<>();
        String b = "testString";
        Integer a = 5;
        String test = pop(new String("test"));
    }

    public String pop(String param) {
        String b = "testString";
        Integer a = 5;
        return param;
    }

    static void shiftElements(final Object[] array, final int start, final int end) {
        var clone = array.clone();
        for (int i = start; i <= end; i++) {
            array[i + 1] = clone[i];
        }
    }

     static void shiftElements(Object[] array, int start, int end) {
        if (array == null || start < 1 || end > array.length - 1 || start > end) {
            throw new IllegalArgumentException("Invalid Input for shiftElementsBack");
        } else {
            for (int i = end; i >= start; i--) {
                array[i - 1] = array[i];
            }
        }
    }
}
