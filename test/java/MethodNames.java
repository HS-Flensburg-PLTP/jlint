public class Foo {
    int calculatingFaculty(int n) {
        if (n == 0) {
            return 1;
        } else {
            return n * f(n - 1);
        }
    }

    int f(int n) {
        if (n == 0) {
            return 1;
        } else {
            return n * f(n - 1);
        }
    }

    int powerOf(int n) {
        if (n == 0) {
            return 1;
        } else {
            return n * f(n - 1);
        }
    }

    int faculty(int n) {
        if (n == 0) {
            return 1;
        } else {
            return n * f(n - 1);
        }
    }
}
