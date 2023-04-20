public class WrongDeclarationOrder {

    public int a;
    protected int b;
    public int c;

    DeclarationOrder() {
          this.a = 0;
        }

    public void foo() {
    }

    DeclarationOrder(int a) {
          this.a = a;
        }

    private String name;
}