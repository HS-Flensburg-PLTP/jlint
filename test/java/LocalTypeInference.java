class LocalTypeInference {

    void foo() {
        int a = 5;
        int b;

        int c = 1, d, e = 6;

        List<String> list1 = new List<String>();
        var list2 = new List<Integer>();
        var list3 = new List<>();
        List<String> list4 = new List<>();

        var[] arr = new int[5];
    }
}