class SimplifyBooleanReturn {

    boolean mem = true;
    int n = 5;

    boolean foo() {

        boolean loc = true;
        boolean n = true;

        if (true) {
            return false;
        }

        if (false) {
            return false;
        }
        
        if (false) {
            return loc;
        }
        
        if (false) {
            return mem;
        }

        if (false) {
            return n;
        }

        if (true) {
            return false;
        } else {
            return true;
        }

        if (false) {
            return false;
        } else {
            return false;
        }

        if (false) {
            return loc;
        } else {
            return false;
        }

        if (false) {
            return mem;
        } else {
            return true;
        }

        if (false) {
            return n;
        } else {
            return true;
        }
    }
}
