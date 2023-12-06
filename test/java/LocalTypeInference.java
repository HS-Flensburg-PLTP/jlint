class LocalTypeInference {

    void foo() {
        int a = 5;
        int b = someMethod(new XType<>(), 10);
        int noInit;
        int c = 1, d, e = 6;

        List<String> list1 = new List<String>();
        var list2 = new List<Integer>();
        var list3 = new List<>();
        List<String> list4 = new List<>();

        var[] arr = new int[5];

        for (int i = 0; i < a; i++) {
            a++;
        }

        XType xType = null;

        var result = someMethod(new List<>());
        var x = new XType<>(10, "foo");
    }
}
