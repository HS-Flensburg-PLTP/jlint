class CheckNonPrivateAttributes {

    private int age = 20;
    String address = "Zu Hause";
    public String myname = "Philipp";
    protected int count;
    int one, two;
    int three, four, five;

    public void foo() {}
    private void bar() {}
    protected void baz() {}
    void foobar() {}
}
