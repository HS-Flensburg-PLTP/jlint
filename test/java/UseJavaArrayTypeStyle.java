class UseJavaArrayTypeStyle {
    int[] nums;
    String string[];

    void vars() {
        int number[] = nums;
        String[] strings = new String[] {"1", "2", "3"};
        int number[][] = nums;
    }

    void multiDim(int arr[][]) {}

    void params(double[] arr1, double arr2[]) {}
}
