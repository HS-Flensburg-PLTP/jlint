class PreferExpressions {

    static int nodes(BinTreeNode node) {
        if (node == null) {
            return 0;
        } else {
            int result = 0;
            result++;
            result = result + nodes(node.left);
            result = result + nodes(node.right);
            return result;
        }
    }

    static int nodes2(BinTreeNode node) {
        if (node == null) {
            return 0;
        } else {
            int result2 = 0;
            result2 = result2 + nodes(node.left);
            result2 = result2 + nodes(node.right);
            result2++;
            return result2;
        }
    }

    static int nodes3(BinTreeNode node) {
        if (node == null) {
            return 0;
        } else {
            int result3;
            result3 = result3 + nodes(node.left);
            result3 = result3 + nodes(node.right);
            result3++;
            return result3;
        }
    }

    public int countNodes(final BinTreeNode<T> currentNode) {
        if (currentNode == null) {
            return 0;
        } else {
            int counter = 1;
            counter += countNodes(currentNode.left);
            counter += countNodes(currentNode.right);
            return counter;
        }
    }

    public int countNodesRecursive(final BinTreeNode<T> knots) {
        int entries = 0;
        if (knots == null) {
            return entries;
        } else {
            entries = countNodesRecursive(knots.left) + countNodesRecursive(knots.right);
            entries++;
        }
        return entries;
    }

    static <T> boolean contains(final List<T> list, final T value) {
         var index = 0;
         var foundItem = false;
         foundItem = isEqual(list.get(index), value);
         while (index < list.size() && !foundItem) {
             foundItem = isEqual(list.get(index), value);
             index++;
         }
         return foundItem;
     }

    public boolean helpContains(final T value, final int index) {
        final var currentIndex = this.array[index];
        var found = false;
        if (index == this.elementsArray) {
            return found;
        } else if (currentIndex == value || currentIndex != null && currentIndex.equals(value)) {
            found = true;
            return found;
        } else {
            return helpContains(value, index + 1);
        }
    }

    boolean contains(final int index, final T value) {
        final var current = this.array[index];
        var checker = false;
        if (index == this.size) {
            return checker;
        } else if (current == value || current != null && current.equals(value)) {
            return !checker;
        } else {
            return contains(index + 1, value);
        }
    }

    Integer max(final BinTreeNode node, final Integer result) {
        var currentMax = result;
        if (node == null) {
            return currentMax;
        } else {
            currentMax = Integers.max(node.value, currentMax);
            return max(node.left, max(node.right, currentMax));
        }
    }

    public T remove(int index) {
        var removed = this.get(index);

        for (int i = index; i < this.size - 1; i++) {
            this.array[i] = this.array[i + 1];
        }
        this.size--;

        return removed;
    }

    static int test() {
        int i;
        i = 23;
        return i + i;
    }
}