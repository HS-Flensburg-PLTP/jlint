class UseJavaArrayTypeStyle {
    int[] nums;
    String string[];

    void vars() {
        int number[] = nums;
        String[] strings = new String[] {"1", "2", "3"};
    }

    void multiDim(int arr[][]) {
        int number[][] = nums;
    }

    void params(double[] arr1, double arr2[]) {}
}
