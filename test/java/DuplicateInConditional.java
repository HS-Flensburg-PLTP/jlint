class DuplicateInConditional {

    private int a;

    public foo(int index) {

        var b = "string";

        if (index > 0) {
            a = 5;
            b = "";
        } else {
            a = 5;
            b = "bar";
        }

        if (index > 0) {
            a = 5;
        } else {
            a = 5;
        }

        if (index > 1) {
            b = "string2";
            a = 4;
        } else
            a = 4;

        if (index > 2) {
            var x = 5;
            System.out.println("");
            a = x;
        } else {
            var x = 5;
            a = x;
        }

        if (index > 2) {
            var x = 5;
            System.out.println("");
            a = 4;
        } else {
            var x = 5;
            a = 4;
        }
    }

    public int bar(int index) {
        if (index < 1) {
            a = 10;
            return index;
        } else {
            a = index;
            return index;
        }
    }

    public int baz(int index) {
        if (index < 1) {
            var v = a + 10;
            a = 10;
            return v;
        } else {
            var v = a - 10;
            a = 10;
            return v;
        }
    }

    public int both(int index) {
        if (index < 1) {
            a = 10;
            b = "str";
            return index;
        } else {
            a = 10;
            return index;
        }
    }

    public int innerVar(int index) {
        if (index < 1) {
            a = 1;
            for (int i = 0; i < 10; i++) {
                var a = i + 3;
                System.out.println(a);
            }
            return a;
        } else {
            a = 10;
            return a;
        }
    }

    public int shadowed(int index) {
        if (index < 1) {
            var a = 10;
            return a;
        } else {
            a = 5;
            return a;
        }
    }

    private DLNode<T> nodeAt(int index) {
        if (index < 0 || index >= this.size) {
            throw new IndexOutOfBoundsException();
        } else if (index <= this.size / 2) {
            var current = this.first;
            for (int i = 0; i < index; i++) {
                current = current.next();
            }
            return current;
        } else {
            var current = this.last;
            for (int i = 0; i < this.size - index; i++) {
                current = current.prev();
            }
            return current;
        }
    }

    public func(boolean b) {
        var x = 5;
        if (b) {
            System.out.println("Ok");
            this.shadowed(x);
        } else {
            System.out.println("Failed");
            this.shadowed(x);
        }
    }
}
