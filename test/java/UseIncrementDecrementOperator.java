class UseIncrementDecrementOperator {
    
    static int func1(int i) {
        i = i + 1;
        return i;
    }

    static int func2(int i) {
        return i = i + 1;
    }

    static int func3(int i) {
        i = i - 1;
        return i;
    }

    static int func4(int i) {
        return i = i - 1;
    }

    static int func5(int i) {
        i += 1;
        return i;
    }

    static int func6(int i) {
        return i += 1;
    }

    static int func7(int i) {
        i -= 1;
        return i;
    }

    static int func8(int i) {
        return i -= 1;
    }

    static void foo() {
        for (int i = 0 ; i < 10 ; i = i + 1) {
        }
    }

    static void bar() {
        for (int i = 0 ; i < 10 ; i += 1) {
        }
    }
}