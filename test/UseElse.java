class UseElse {

    public boolean isMagicNumber(int i) {
        if (i == 42) {
            return true;
        }
        return false;
    }

    private Node<T> nodeAt(final int index) {
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

    private Node<T> nodeAt2(final int index) {
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

}