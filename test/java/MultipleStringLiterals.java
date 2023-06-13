public class MultipleStringLiterals {
    private String a = "Foo";
    private String b = "Bar";
    private String c = "Bar";

    public MultipleStringLiterals() {
        a = "Foo";
        b = "Bar";
        c = "foobar";
    }

    public void aMethod() {
        c = "Foo" + "Bar";
    }
}
