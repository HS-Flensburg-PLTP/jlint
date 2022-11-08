class InitializeVariables {

    static int nodes3(BinTreeNode node) {
        if (node == null) {
            return 0;
        } else {
            int result3;
            result3 = nodes(node.left) + 1 + nodes(node.right);
            return result3;
        }
    }

    public T remove(int index) {
        T value;
        if (index <= 0) {
            value = this.first.value;
            this.first = this.first.next;
        } else {
            var pred = nodeAt(index - 1);
            value = pred.next.value;
            pred.next = pred.next.next;
        }
        this.size--;
        return value;
    }
}