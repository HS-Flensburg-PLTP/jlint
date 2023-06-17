class FooBar {
    private int a = 2;
    private Int i = a++;

    public FooBar () {
        int y = (i--)++;
        int[] array = new int[4];

        array[a--] = 4;
        Bool c = false;
        for (int j = i++; j < 10; j++) {
            for (int k; j < 10; k++, j++, a = k++, c = k++ == i--) {

            }
        }

        if (i++ == a--) {
           Funct(a--);
            a++; 
        }
    }

    private void Funct (int b) {
        b = a++;
    }
}