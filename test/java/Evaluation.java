class Evaluation {

    private Node<T> nodeAt(int index) {
        if (index < 0 || index > this.size - 1) {
            throw new IndexOutOfBoundsException();
        } else {
            var current = this.first;
            for (int i = 0; i < index; i++) {
                current = current.next();
            }
            return current;
        }
    }

    @Override
    public T remove(int index) {
        T value;
        if (index <= 0) {
            value = nodeAt(0).value();
            this.first = this.first.next();
        } else {
            var pred = nodeAt(index - 1);
            value = pred.next().value();
            pred.setNext(pred.next().next());
        }
        this.size--;
        return value;
    }
}