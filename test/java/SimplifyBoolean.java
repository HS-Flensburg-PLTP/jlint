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
        if (cond != true) {
            return 1;
        } else {
            return 2;
       }
    }

    public boolean method6() {
        if (false == cond) {
            return 1;
        } else {
            return 2;
       }
    }

    public Foo method7() {
        if (cond) {
            return a;
        } else {
            return b;
        }
    }

    public boolean method8() {
        return cond ? true : false;
    }

    public boolean method9() {
        return cond ? false : true;
    }

    public Foo method10() {
        return cond ? a: b;
    }
}
