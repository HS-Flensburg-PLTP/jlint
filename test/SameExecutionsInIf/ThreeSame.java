-------------------------
ThreeSameTop
+++++++++++++++++++++++++
public class Test {

    public void testFunc() {
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
    }
}
-------------------------
ThreeSameBottom
+++++++++++++++++++++++++
public class Test {

    public void testFunc() {
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
    }
}
-------------------------
ThreeSameMixed
+++++++++++++++++++++++++
public class Test {

    public void testFunc() {
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
