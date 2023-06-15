class ConstantPropagation {

    public void add(int index, T value) {
        if (index == 0 && this.size == 0) {
            this.first = new DLNode<T>(null, value, null);
            this.last = this.first;

        } else if (index == 0 && this.size > 0) {
            this.first = new DLNode<T>(null, value, nodeAt(index));

        } else if (index > 0 && index == this.size) {
            this.last = new DLNode<T>(nodeAt(index - 1), value, null);
            this.last.getPrev().setNext(this.last);

        } else {
            nodeAt(index - 1).setNext(new DLNode<T>(nodeAt(index - 1), value, null));
            nodeAt(index).setPrev(nodeAt(index - 1).getNext());
        }
        this.size++;
    }

}
