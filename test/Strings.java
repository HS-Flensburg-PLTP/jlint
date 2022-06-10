package de.fuas.algorithms;

public class Strings {
    public static final int ANZ_ASCII_ZEICHEN = 128;
    public String hello = "Hello";

    static boolean containsDuplicates(String str) {
        boolean dupFound = false;
        for (int i = 0; i < str.length() - 1; i++) {
            char charAtI = str.charAt(i);

            for (int j = i + 1; j < str.length() && !dupFound; j++) {
                if (charAtI == str.charAt(j)) {
                    dupFound = true;
                }
            }
        }
        return dupFound;
    }

    static boolean containsDupOptWhile(final String str) {
        List<Boolean> charList = new ArrayList<Boolean>(ANZ_ASCII_ZEICHEN);
        boolean found = false;
        int i = 0;
        while (i < str.length() && !found) {
            int charAsInt = str.charAt(i);
            // could be done more intuitive by initilizing array with false for every index.
            // Only did this for training purpose though.
            found = charList.get(charAsInt) != null;
            charList.add(charAsInt, true);
            i++;
        }
        return found;
    }

    static boolean containsDupOptFor(final String str) {
        List<Boolean> asciiList = new ArrayList<Boolean>(ANZ_ASCII_ZEICHEN);
        boolean found = false;
        for (int i = 0; i < str.length() && !found; i++) {
            int charAsInt = str.charAt(i);
            found = asciiList.get(charAsInt);
        }
        return found;
    }

    static boolean containsDuplicatesOptRecur(final String str) {
        return containsDupOptRecur(new ArrayList<Boolean>(ANZ_ASCII_ZEICHEN), str, 0);
    }

    private static boolean containsDupOptRecur(final List<Boolean> asciiList, final String str, final int index) {
        if (str.length() <= index) {
            return false;
        } else {
            int charAsInt = str.charAt(index);
            // again could be done more intuitive if asciiList was initilized with all
            // indices being false
            if (asciiList.get(charAsInt) != null) {
                return true;
            } else {
                asciiList.add(charAsInt, true);
                return containsDupOptRecur(asciiList, str, index + 1);
            }
        }
    }
}