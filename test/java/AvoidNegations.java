class AvoidNegations {

    public void test1() {
        boolean b1 = false;
        boolean b2 = false;
        if (!(b1 && b2)) {
            System.out.println("A");
        } else {
            System.out.println("B");
        }
    }

    public void test2() {
        boolean b = false;
        if (!b)
            System.out.println("A");
        else
            System.out.println("B");
    }

    public int sum() {
        if (this.root != null) {
            BinTree leftCheck = new BinTree(this.root.left);
            BinTree rightCheck = new BinTree(this.root.right);
            return this.root.value + leftCheck.sum() + rightCheck.sum();
        } else {
            return 0;
        }
    }

    // The rule should not fire for this method
    public void entries(final BinTreeNode helperNode, final List<Integer> returnList) {
        if (helperNode != null) {
            entries(helperNode.left, returnList);
            returnList.add(returnList.size(), helperNode.value);
            entries(helperNode.right, returnList);
        }
    }

    public boolean contains(Object object) {
        int key = index(object, this.table.length);
        if (this.table[key] != null) {
            return this.table[key].contains(object);
        } else {
            return false;
        }
    }

    public static <T> boolean contains2(final T firstValue, final T secondValue) {
        if (firstValue != null) {
            return firstValue.equals(secondValue);
        } else {
            return firstValue == secondValue;
        }
    }

    boolean contains3(final T value) {
        boolean contains = false;
        for (int i = 0; i < this.sizeCounter && !contains; i++) {
            if (array[i] != null) {
                contains = get(i).equals(value);
            } else {
                contains = get(i) == value;
            }
        }
        return contains;
    }

    boolean contains4(final T value) {
        boolean contains = false;
        for (int i = 0; i < this.sizeCounter && !contains; i++) {
            contains = array[i] != null ? get(i).equals(value) : get(i) == value;
        }
        return contains;
    }
}
