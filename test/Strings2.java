package de.fuas.algorithms;

public class Strings2 {

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
        int i = 50;
        int j = 150;
         while(++i < --j);
    }
}
