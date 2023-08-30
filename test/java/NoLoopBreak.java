class NoLoopBreak {
    


    public static void testFunc() {
        int i = 0;
        while (i < 10) {
            i++;
            label: break;
        }
    }

    // Liefert 2 Hinweise obwohl nur einer bei der inneren geliefert werden soll !! TICKET FEHLER !!
    public static void testFunc2() {
        int i = 0;
        while (i < 10) {
            i++;
            int j = 0;
            while (j < 10) {
                break;
            }
        }
    }

    void foo() {

        String[] arr = { "a", "b", "c" };
        int i = 0;

        do {
            i += 1;
            break;
        } while (i < 10);

        while (i < 10) {
            i++;
            break;
        }

        for (String elem : arr) {
            elem = elem + "i";
            break;
        }

        for (int i = 0; i < 20; i++) {
            System.out.println(i);
            break;
        }

        for (int i = 0; i < arr.length; i++) {
            if (i == 7) {
                System.out.println(i);
                break;
            }
        }

        while (i < arr.length) {
            if (i == 7) {
                System.out.println(i);
                break;
            } else {
                i++;
            }
        }

        do {
            i += 1;
            return;
        } while (i < 10);

        while (i < 10) {
            i++;
            return;
        }

        for (String elem : arr) {
            elem = elem + "i";
            return;
        }

        for (int i = 0; i < 20; i++) {
            System.out.println(i);
            return;
        }

        for (int i = 0; i < arr.length; i++) {
            if (i == 7) {
                System.out.println(i);
                return;
            }
        }

        while (i < arr.length) {
            if (i == 7) {
                System.out.println(i);
                return;
            } else {
                i++;
            }
        }
        for(int o = 0; o < 5; o++) {
            switch (i) {
            case 5:
                System.out.println(i);
                break;
            case 6: 
                System.out.println(i);
        }
        }

    }

}
