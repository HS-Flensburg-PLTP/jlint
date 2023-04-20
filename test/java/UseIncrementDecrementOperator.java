class UseIncrementDecrementOperator {
    
    static int foo(int i, int j, int k, int l) {
        i = i + 1;
        j = j - 1;
        i = 1 + i;
        k += 1;

        for (int i = 0 ; i < 10 ; i = i + 1) {
        }

        for (int i = 0 ; i < 10 ; i += 1) {
        }

        return i = i + 1;
    }

    static int bar(int i, int j) {
        return i = j + 1;
    }

    static void foobar(int l) {
        l = l * 1;
        l = l / 1;
    }
}
