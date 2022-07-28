-------------------------
SimplifyIfThenElseFirst
+++++++++++++++++++++++++
class Test {

    public static void testFunc() {
        if (true) {
            return false;
        } else {
            return true;
        }
    }
}
-------------------------
SimplifyIfThenElseSecond
+++++++++++++++++++++++++
class Test {
    public static void testFunc() {
        if (false) {
            return false;
        } else {
            return false;
        }
    }
}
-------------------------
SimplifyIfThenElseMethodVar
+++++++++++++++++++++++++
class Test {
    public static void testFunc() {
        boolean a = true;
        if (false) {
            return a;
        } else {
            return false;
        }
    }
}
-------------------------
SimplifyIfThenElseClassVar
+++++++++++++++++++++++++
class Test {
    boolean a = true;
    public static void testFunc() { 
        if (false) {
            return a;
        } else {
            return true;
        }
    }
}
-------------------------
SimplifyIfThenElseClassAndMethodVar
+++++++++++++++++++++++++
class Test {
    int a = 3;
    public static void testFunc() { 
        boolean a = true;
        if (false) {
            return a;
        } else {
            return true;
        }
    }
}