// Foo implements @FooBar Bar1, @FooBar(foo = "bar") Bar2, @FooBar("foo") Bar3,
// @BarFoo Bar4, @BarFoo(bar = "foo") Bar5, @BarFoo("bar") Bar6
public class Foo {

    /*
    String foo1 = (@FooBar String) "bar";
    String foo2 = (@BarFoo String) "bar";
    String foo3 = (@FooBar(foo = "bar") String) "bar";
    String foo4 = (@BarFoo(bar = "foo") String) "bar";
    String foo5 = (@FooBar("foo") String) "bar";
    String foo6 = (@BarFoo("bar") String) "bar";

    String foo7 = new @FooBar String();
    String foo8 = new @BarFoo String();
    String foo9 = new @FooBar(foo = "bar") String();
    String foo10 = new @BarFoo(bar = "foo") String();
    String foo11 = new @FooBar("foo") String();
    String foo12 = new @BarFoo("bar") String();
    

    void foofunc1() throws @FooBar BarException {
    }

    void foofunc2() throws @BarFoo BarException {
    }

    void foofunc3() throws @FooBar(foo = "bar") BarException {
    }

    void foofunc4() throws @BarFoo(bar = "foo") BarException {
    }

    void foofunc5() throws @FooBar("foo") BarException {
    }

    void foofunc6() throws @BarFoo("bar") BarException {
    }
    */
    
    @FooBar
    @BarFoo
    @FooBar(foo = "bar")
    @BarFoo(bar = "foo")
    @FooBar("foo")
    @BarFoo("bar")
    @SuppressWarnings
    @SuppressWarnings("unchecked")
    @SuppressWarnings(value = "unchecked")
    @Override
    void foofunc7() {
       
    }

}

