public class RedundantLocalVariable {
    private int x = 5;
    private int y = 3;
    private int sum = 0;

    void method1() {
        var localSum = x + y;
        sum += localSum;
        var stub = x;
        foobar(stub);
    }

    int method2() {
        var n = x;
        return n;
    }

    void method3(int number) {
        var localSum = x * number;
        System.out.println(localSum);
        var str = "expensive operation";
        for (var i = 0; i < localSum; i++) {
            System.out.println(str);
        }
    }

    void method4(int number) {
        var localSum = x * number;
        for (var i = 0; i < localSum; i++) {
            System.out.println("a");
        }
    }

    // The rule should not fire if the rhs is a side effect
    long method5() {
        var start = System.nanoTime();
        return start;
    }

    String method6() {
        var dice = new Random();
        var diceRoll = dice.nextInt();
        return (diceRoll % 2 == 0) ? "Git ist super!" : "Java ist super!";
    }

    String method7() {
        var dice = new Random();
        var diceRoll = dice.nextInt();
        if (diceRoll % 2 == 0) {
            return "Git ist super!";
        } else {
            return "Java ist super!";
        }
    }

    // The rule should not fire because the code becomes less readable otherwise
    int method8() {
        int[] array = {1, 2, 3};
        System.out.println(array[0]);
        return array[0];
    }
}
