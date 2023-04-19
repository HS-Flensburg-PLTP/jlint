class SameExecutionsInIf {

    void foo() {

        if(true) {
            a = 5;
            b = 3;
        } else {
            a = 5;
            b = 3;
        }

        if(true) {
            a = 5;
            b = 3;
            c = 1;
            d = 1;
        } else {
            a = 5;
            b = 3; 
            c = 1;
            d = 2;
        }

        if(true) {
            a = 1;
            b = 3;
            c = 1;
            d = 1;
        } else {
            a = 5;
            b = 3; 
            c = 1;
            d = 1;
        }

        if(true) {
            a = 1;
            b = 3;
            c = 1;
            d = 1;
        } else {
            a = 1;
            b = 3; 
            c = 2;
            d = 1;
        }
    }


}
