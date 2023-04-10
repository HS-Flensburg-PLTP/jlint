public class DeclarationOrder {

    public int a;
    protected int b;
    public int c;

    Test() {
          this.a = 0;
        }

    public void foo() {
    }

    Test(int a) {
          this.a = a;
        }

    private String name;
}
