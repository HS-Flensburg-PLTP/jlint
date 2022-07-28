-------------------------
SimplifyIfThenFirst
+++++++++++++++++++++++++
class Test {

    public static void testFunc() {
        if (true) {
            return false;
        }
    }
}
-------------------------
SimplifyIfThenSecond
+++++++++++++++++++++++++
class Test {
    public static void testFunc() {
        if (false) {
            return false;
        }
    }
}
-------------------------
SimplifyIfThenMethodVar
+++++++++++++++++++++++++
class Test {
    public static void testFunc() {
        boolean a = true;
        if (false) {
            return a;
        }
    }
}
-------------------------
SimplifyIfThenClassVar
+++++++++++++++++++++++++
class Test {
    boolean a = true;
    public static void testFunc() { 
        if (false) {
            return a;
        }
    }
}
-------------------------
SimplifyIfThenClassAndMethodVar
+++++++++++++++++++++++++
class Test {
    int a = 3;
    public static void testFunc() { 
        boolean a = true;
        if (false) {
            return a;
        }
    }
}