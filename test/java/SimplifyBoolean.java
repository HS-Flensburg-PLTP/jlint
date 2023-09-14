class SimplifyBoolean {

    private boolean cond;
    private Foo a;
    private Foo b;

    public boolean method1() {
        if (cond) {
            return true;
        } else {
            return false;
        }
    }

    public boolean method2() {
        if (cond) {
            return false;
        } else {
            return true;
        }
    }

    public boolean method3() {
        return cond;
    }

    public boolean method4() {
        if (cond == true) {
            return 1;
        } else {
            return 2;
       }
    }

    public boolean method5() {
        if (false == cond) {
            return 1;
        } else {
            return 2;
       }
    }

    public Foo method6() {
        if (cond) {
            return a;
        } else {
            return b;
        }
    }

    public Foo method7() {
        return cond ? a: b;
    }
}
