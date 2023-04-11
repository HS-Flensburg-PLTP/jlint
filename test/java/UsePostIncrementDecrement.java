class UsePostIncrementDecrement {
    static int PreIncrementOperator(int i) {
        int q = ++i;
        int j = q++;
        ++j;
        for(int k = 0; k < 2; ++k) {
            j++;
        }
        return q;
    }
}
