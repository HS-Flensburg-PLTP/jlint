class NeedBraces {

    void foo() {
        for(int i = 0; i < 10; i++) System.out.println(i);

        for (int i : arr);

        do i++; while (true);

        while (true)
            if (true)
                i++;
        
        if(true);

        if(true) 
            i++;
        else {
            i--;
        }

        if(true) {
            i++;
        } else
            ;

        if(true)
            while(true) { i++; }
        else
            i--;

        for (int i : arr) {}
        
        for(int i = 0; i < 10; i++) {
            System.out.println(i);
        }

        while (true) {
            i++;
            i--;
        }

        if (true) {
            a = 1;
        } else if (false) {
            a = 2;
        } else {
            a = 3;
        }

        if (true) {
            a = 1;
        } else if (false)
            a = 2;

        if (true) {
            a = 1;
        } else if (false) {
            a = 2;
        } else a = 3;
    }
}
