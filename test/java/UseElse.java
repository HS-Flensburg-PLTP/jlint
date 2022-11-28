class UseElse {

    public boolean isMagicNumber(int i) {
        if (i == 42) {
            return true;
        }
        return false;
    }

    private DLNode<T> nodeAt(int index) {
         if (index < 0 || index >= size) {
             throw new IndexOutOfBoundsException();
         }
         if (index <= size / 2) {
             var tempNode = head;
             for (int i = 0; i < index; i++) {
                 tempNode = tempNode.next;
             }
             return tempNode;

         }
         var tempNode = last;
         for (int i = size - 1; i > index; i--) {
             tempNode = tempNode.prev;
         }
         return tempNode;
     }

    public List<Integer> entriesHelper(final BinTreeNode current, final List<Integer> list) {
        if (current == null) {
            return list;
        }

        List<Integer> list2 = entriesHelper(current.rightNode, list);
        list2.add(0, current.value);
        list2 = entriesHelper(current.leftNode, list2);
        return list;
    }

    public List<Integer> entriesHelper2(final BinTreeNode current, final List<Integer> list) {
        if (current != null) {
            List<Integer> list2 = entriesHelper(current.rightNode, list);
            list2.add(0, current.value);
            list2 = entriesHelper(current.leftNode, list2);
            return list;
        }
        return list;
    }

    private Node<T> nodeAt2(final int index) {
        var current = this.first;
        if (index < 0 || index > this.size - 1) {
            throw new IndexOutOfBoundsException();
        } else {
            for (int i = 0; i < index; i++) {
                current = current.next;
            }
        }
        return current;
    }

    private Node<T> nodeAt3(final int index) {
        var current = this.first;
        if (index < 0 || index > this.size - 1) {
            throw new IndexOutOfBoundsException();
        } else {
            for (int i = 0; i < index; i++) {
                current = current.next;
            }
            return current;
        }
    }

    private void checkBounds(final int index, final int min, final int max) {
        if (index < min || max < index) {
            throw new IndexOutOfBoundsException();
        }
    }

    public static boolean isSubset(final int[] array1, final int[] array2) {
        var isInTwo = true;

        if (array1.length <= array2.length && array2.length > 0) {
            for (int i = 0; isInTwo && i < array1.length; i++) {
                var isIn = false;
                for (int j = 0; j < array2.length; j++) {
                    if (array1[i] == array2[j]) {
                        isIn = true;
                    }
                }
                isInTwo = isIn;
                isIn = false;
            }
        } else {
            return false;
        }
        return isInTwo;
    }
}