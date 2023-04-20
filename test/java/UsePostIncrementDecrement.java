class UsePostIncrementDecrement {
    static int preIncrementOperator(int i) {
        int q = ++i;
        int j = q++;
        ++j;
        for(int k = 0; k < 2; ++k) {
            j++;
        }
        return q;
    }
}
