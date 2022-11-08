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
}
