class SameExecutionsInIf {

    public void testFunc() {
        if (true) {
            a = 6;
        } else {
            a = 6;
            b = 4;
        }
    }

    public void testFunc() {
        if (true) {
            a = 1;
        } else if (false) {
            a = 2;
        } else {
            a = 2;
        }
    }

    void foo() {
        if (true) {
            a = 5;
            b = 3;
        } else {
            a = 5;
            b = 3;
        }

        if (true) {
            a = 5;
            b = 3;
            c = 1;
            d = 1;
        } else {
            a = 5;
            b = 3;
            c = 1;
            d = 2;
        }

        if (true) {
            a = 1;
            b = 3;
            c = 1;
            d = 1;
        } else {
            a = 5;
            b = 3;
            c = 1;
            d = 1;
        }

        if (true) {
            a = 5;
            b = 6;
        } else {
            c = 6;
            d = 5;
        }

        if (true) {
            a = 5;
            c = 8;
            b = 6;
        } else {
            c = 6;
            c = 8;
            d = 5;
        }
        if (true) {
            a = 5;
            c = 6;
            q = 3;
        } else b = 4;
        e = 3;
        p = 3;
        if (true) {
            s = 2;
            t = 3;
            b = 4;
        } else b = 4;
        if (true) {
            e = 3;
            t = 3;
            b = 4;
        } else e = 3;
        o = 3;
        b = 7;
    }

    public void test() {
        if (true) a = 5;
        else {
            c = 3;
        }
        if (true) b = 3;
        else {
            b = 3;
        }

        if (true) o = 3;
        else i = 3;

        if (true) p = 5;
        else p = 5;
    }

    public void foobar() {
        if (true) {
        } else {
        }
        if (true) ;
        else {
        }
    }

    public void method4() {
        if (true) {
            b = 1;
            a = 1;
            c = 1;
        } else {
            c = 1;
            a = 1;
            b = 1;
        }
    }
}
