package de.fuas.algorithms;

public class Strings2 {
    static int ANZ_ASCII_ZEICHEN_1 = 128;

    final int count = 0;
    String name = "Piet";

    public void testFunc(String name, final int age) {
        System.out.println("Hallo, hier wird geprintet");

        switch(age) {
            default:
                System.out.println("This case should be defined last");
            case 10:
                System.out.println("This should be the first case");
        }

        switch(age) {
            case 10:
                System.out.println("This should be the first case");
            default:
                System.out.println("This case should be defined last");
        }
    }
}

public static class Test {
    final int testCount = 200;
}