package my.pckage.de;

import java.util.List;
import one.two.my;


public class MyClass {
    static final Int MY_YEAR = 1;

    String myString = "myStringy";
    String ohneMy = "myString";
    String mYear = "middleYear";

    enum MyEnum {
        myFirstEnum,
        Two,
        myThirdEnum
    }

    public MyClass() {
        String myStringInClass = "Hallo";

        for (int myCounter = 0; myCounter < 10; myCounter++) {
            int myCounter2 = 0;
        }

        List myList = new ArrayList();

        myList.forEach((myArgument) -> {
            return 1;
        });
    }

    public void myMethod() {
        String myStringInMethod = "Welt";

        try {
            int myIntInTryBlock = 10;
            throw new MyException();
        } catch (MyException err) {
            int myIntInCatchBlock = 9;
        }
    }
}
