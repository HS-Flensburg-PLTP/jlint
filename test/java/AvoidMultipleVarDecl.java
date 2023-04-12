class AvoidMultipleVarDecl {

    public void foo() {
        int j = 3;

        int i, k;

        int m,
            n;

        int foo = 1, bar = 2;
    }
}
