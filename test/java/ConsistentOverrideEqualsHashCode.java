public static class Example1 {
    public int hashCode() { // violation, no 'equals'
        // code
    }
    public boolean equals(String o) {
        // code
    }
}
public static class Example2 {
    public boolean equals(Object o) { // violation, no 'hashCode'
        // code
    }
    public boolean equals(String o) {
        // code
    }
}
public static class Example3 {
    public int hashCode() {
        // code
    }
    public boolean equals(Object o) { // OK
        // code
    }
    public boolean equals(String o) {
        // code
    }
}
public static class Example4 {
    public int hashCode() {
        // code
    }
    public boolean equals(java.lang.Object o) { // OK
        // code
   }
}
public static class Example5 {
    public static int hashCode(int i) {
        // code
    }
    public boolean equals(Object o) { // violation, no 'hashCode'
        // code
    }
}
public static class Example6 {
    public int hashCode() { // violation, no 'equals'
        // code
    }
    public static boolean equals(Object o, Object o2) {
        // code
    }
}