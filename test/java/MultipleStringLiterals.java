public class MultipleStringLiterals {
    private String a;
    private String b;
    private String c;

    public MultipleStringLiterals() {
        a = "Foo";
        b = "Bar";
        c = "foobar";

        this.aMethod("Foo");
    }

    public void aMethod(String string) {
        String d = "Foo";

        c = string + "Bar";
    }

    String a1 = "unchecked";
    @SuppressWarnings("unchecked") 
    public void myTest() {
        String a2 = "SingleString";
    }
}
