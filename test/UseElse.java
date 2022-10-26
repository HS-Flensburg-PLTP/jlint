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
}