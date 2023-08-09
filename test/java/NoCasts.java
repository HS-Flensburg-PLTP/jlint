class NoCasts {

    public void foo() {
        double num = 9.78d;
        int casted = (int) num;
    }

    public void bar() {
        double num = 9.78d;
        int casted = (int) num;
        int cast = (int) (num + 1);
    }

    public double bar() {
        double num = 9.78d;
        return num;
    }

}
