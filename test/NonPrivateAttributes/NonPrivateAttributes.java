package test.NonPrivateAttributes;

public class NonPrivateAttributes {
    private static int age = 20;
    public static String myname = "Philipp";
    private static String address = "Zu Hause";

    public static void testFunc() {
        System.out.println(myname + " wohnt in " + address + " und ist " + String.valueOf(age) + " alt");
    }

}
