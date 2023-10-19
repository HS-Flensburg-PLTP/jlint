class MethodInvocations {


    public int bar(int a, int b) {
        return a + b;
    }

    public int foo() {
        return bar(4, 5) + bar(6, 7) + bar(1, 2);
    }

    public int foo(int a) {
        return bar(a, 5);
    }

    public int test() {
        return bar(4, 5) + bar(6, 7) + bar(1, 2) + bar(4, 5) + bar(6, 7) + bar(1, 2);
    }

    public int test2() {
        return bar(4, 5);
    }

    public T remove(int index) {
        T valueToRemove;

        if (index == 0) {
            valueToRemove = this.first.value();
            this.first = this.first.next();
        } else {
            var current = nodeAt(index);
            valueToRemove = current.value();
            nodeAt(index - 1).setNext(current.next());
        }
        this.nodeCount--;

        return valueToRemove;
    }
}
