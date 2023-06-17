class ConstantPropagation {

    public void add(int index, T value) {
        if (0 == index) {
            this.first = new DLNode<T>(null, value, nodeAt(index));
            this.last = this.first;
        }
        
        if (index == 0) {
            this.first = new DLNode<T>(null, value, nodeAt(index));

        }

        if (0 == index) {
            this.first = new DLNode<T>(null, value, null);
            this.last = this.first;
        }
        
        if (index > 0 && index == this.size) {
            this.last = new DLNode<T>(nodeAt(index - 1), value, null);
            this.last.getPrev().setNext(this.last);

        }
    }

}
