class MethodInvNumber {


    public int bar(int a, int b) {
        return a + b;
    }

    public int foo() {
        return bar(4, 5) + bar(6, 7) + bar(1, 2);
    }

    public int foo() {
        return bar(4, 5);
    }

    public int test() {
        return bar(4, 5) + bar(6, 7) + bar(1, 2) + bar(4, 5) + bar(6, 7) + bar(1, 2);
    }

    public int test2() {
        return bar(4, 5);
    }
}
