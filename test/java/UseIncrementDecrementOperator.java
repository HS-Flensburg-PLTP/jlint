class UseIncrementDecrementOperator {

    
    static int func1(int i) {
        i = i + 1;
        return i;
    }

    static int func2(int i) {
        return i = i - 1;
    }

    static int func3(int i) {
        return i += 1;
    }

    static int func4(int i) {
        return i -= 1;
    }
}