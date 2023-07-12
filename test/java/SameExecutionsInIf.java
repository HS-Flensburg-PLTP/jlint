class SameExecutionsInIf {

    public void testFunc() {
        if (true) {
            a = 6;
        } else {
            a = 6;
            b = 4;
        }
    }

    public void testFunc() {
        if (true) {
            a = 1;
        } else if (false) {
            a = 2;
        } else {
            a = 2;
        }
    }

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
            a = 5;
            b = 6;
        } else {
            c = 6;
            d = 5;
        }
        
         if(true) {
            a = 5;
            c = 8;
            b = 6;
        } else {
            c = 6;
            c = 8;
            d = 5;
        }
    }


}
