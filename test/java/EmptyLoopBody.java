class EmptyLoopBody {

    void foo() {

        for(int i = 0; i <= 3; i ++){};
        for(int i = 0; i <= 3; i ++);

        do {} while(true);
        do; while(true);

        String[] arr = {"a", "b", "c"};
        for(String elem : arr){};
        for(String elem : arr);

        while(true){};
        while(true);
    }
}
