public class MultipleStringLiterals {
    private String a;
    private String a1 = "unchecked";

    public MultipleStringLiterals() {
        a = "Foo";

        this.a.split("Foo");
    }

    @SuppressWarnings("unchecked") 
    public void testMethod() {
        String a2 = "SingleString";
    }
}
