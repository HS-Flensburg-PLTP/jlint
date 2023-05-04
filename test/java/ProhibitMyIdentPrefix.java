package my.huge.pckage.de;

import java.util.List;

import one.two.my;

interface MyInterfaceToMark {

}

public class MyClass extends MyOtherClassWhichIsNotInThisFile implements MyInterfaceNotToMark {
    static final Int MY_YEAR = 1;

    String myString = "myStringy";
    String ohneMy = "myString";
    String mYear = "middleYear";

    enum MyEnum {
        myFirstEnum, Two, myThirdEnum
    }

    @MyOverriddenMethodConstructor
    public MyClass() {
        String myStringInClass = "Joscha";

        for (int myCounter = 0; myCounter < 10; myCounter++) {
            System.out.println(i);
            int myCounter2 = 0;
        }

        List myList = new ArrayList();

        numbers.forEach((myMoin) -> {
            return 1;
        });
    }

    public void myMethod() {
        String myStringInMethod = "Joscha";

        try {
            int myIntInTryBlock = 10;
            throw new MyException();
        } catch (MyException err) {
            int myIntInCatchBlock = 9;
        }
    }
}
