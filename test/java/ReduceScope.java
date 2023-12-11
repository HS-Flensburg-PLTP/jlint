class ReduceScoope<T> {
    private Node<T> first;

    public T remove(int index) {
        var removed = this.get(index);

        for (int i = index; i < this.size - 1; i++) {
            this.array[i] = this.array[i + 1];
        }
        this.size--;

        return removed;
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
        var current2 = this.first;
        if (index < 0 || index > this.size - 1) {
            throw new IndexOutOfBoundsException();
        } else {
            for (int i = 0; i < index; i++) {
                current2 = current2.next;
            }
            return current2;
        }
    }

    private Node<T> nodeAt3(final int index) {
        var current3 = this.first;
        if (index < 0 || index > this.size - 1) {
            for (int i = 0; i < index; i++) {
                current3 = current3.next;
            }
            return current3;
        } else {
            throw new IndexOutOfBoundsException();
        }
    }

    public void add(int index, T e) {
         var prevNode = this.first;
         var nextNode = this.last;
         var newNode = this.first;

         if (index == 0 && this.size == 0) {
             this.first = new DLNode<T>(null, e, null);
             this.last = this.first;
         } else if (index == 0 && this.size > 0) {
             nextNode = this.first;
             newNode = new DLNode<T>(this.first, e, nextNode);
             this.first = newNode;
             nextNode.prev = newNode;
         } else if (index > 0 && index == this.size) {
             prevNode = this.last;
             newNode = new DLNode<T>(prevNode, e, null);
             prevNode.next = newNode;
             this.last = newNode;
         } else {
             prevNode = this.nodeAt(index - 1);
             nextNode = prevNode.next;
             newNode = new DLNode<T>(prevNode, e, nextNode);
             prevNode.next = newNode;
             nextNode.prev = newNode;
         }
         this.size++;
     }

    public T pop() {
        var value = this.first.value;
        this.first = this.first.next;
        return value;
    }

    public T remove(int index) {
        Node<T> holder;
        if (index == 0) {
            holder = this.first;
            this.first = this.first.next;
            this.size--;
            return holder.value;
        } else {
            var pred = nodeAt(index - 1);
            var rmvItem = pred.next;
            pred.next = rmvItem.next;
            this.size--;
            return rmvItem.value;
        }
    }

    public static void shiftElements(Object[] array, int index) {
        Object oldSavedElemement = null;
        Object newSavedElement = null;

        for (var i = index; i < array.length; i++) {
            newSavedElement = array[i];
            array[i] = oldSavedElemement;
            oldSavedElemement = newSavedElement;
        }

    }

    private static boolean binarySearchRec(final int[] array, final int value, final int low, final int up) {
        final int mid = (up - low) / 2 + low;
        if (low <= up) {
            if (value < array[mid]) {
                return binarySearchRec(array, value, low, mid - 1);
            } else if (value > array[mid]) {
                return binarySearchRec(array, value, mid + 1, up);
            } else {
                return true;
            }
        } else {
            return false;
        }
    }

    private static boolean binarySearchRec(final int[] array, final int value, final int start, final int end) {
        int mid2 = start + (end - start) / 2;
        if (start > end) {
            return false;
        } else {
            int midValue = array[mid2];
            if (value < midValue) {
                return binarySearchRec(array, value, start, mid2 - 1);
            } else if (value > midValue) {
                return binarySearchRec(array, value, mid2 + 1, end);
            } else {
                return true;
            }
        }
    }

    public static boolean isSubset(final int[] array1, final int[] array2) {
        var inBothCounter = 0;
        var in = false;
        var notIn = false;
        var arr1Index = 0;
        var arr2Index = 0;
        while (!notIn && arr1Index < array1.length) {
            while (!notIn && !in && arr2Index < array2.length) {
                if (array1[arr1Index] == array2[arr2Index]) {
                    inBothCounter++;
                    in = true;
                } else {
                    arr2Index++;
                }
                if (arr2Index == array2.length - 1 && array1[arr1Index] != array2[arr2Index]) {
                    notIn = true;
                }
            }
            in = false;
            arr1Index++;
            arr2Index = 0;
        }
        return inBothCounter == array1.length;
    }

    private static int sum(final int maxSpawn, final int[] array, final int start, final int end) {
        var mid = start + (end - start) / 2;
        var sum = 0;
        if (maxSpawn > 0) {
            sum = sum(maxSpawn - 1, array, start, mid - 1) + sum(maxSpawn - 1, array, mid, end);
        } else if (maxSpawn == 0) {
            for (int i = start; i <= end; i++) {
                sum += array[i];
            }
        }
        return sum;
    }

    // The rule should not fire for this example
     static int sum(final int cores, final int[] array, final int start, final int end) {
         if (cores <= 1 || end <= start) {
             var sum = 0;
             for (int i = start; i <= end; i++) {
                 sum += array[i];
             }
             return sum;
         } else {
             var mid = start + (end - start) / 2;
             return sum(cores / 2, array, start, mid) + sum(cores - cores / 2, array, mid + 1, end);
         }
     }

    // The rule should not fire for this example
    public T remove1(final int index) {
        var removed = nodeAt(index);
        if (index == 0) {
            this.first = removed.getNext();
        } else {
            var pred = nodeAt(index - 1);
            pred.setNext(removed.getNext());
        }
        this.elements--;
        return removed.getValue();
    }

    // The rule should not fire for this example
    public T remove2(final int index) {
        var removed = this.first;
        if (index == 0) {
            this.first = removed.getNext();
        } else {
            var pred = nodeAt(index - 1);
            pred.setNext(removed.getNext());
        }
        this.elements--;
        return removed.getValue();
    }

    private static boolean contains(final String value, final String[] array, final int start, final int end) {
        var found = false;
        var mid = start + (end - start) / 2;
        if (start <= end) {
            var compare = value.compareTo(array[mid]);
            if (compare > 0) {
                return contains(value, array, mid + 1, end);
            } else if (compare < 0) {
                return contains(value, array, start, mid - 1);
            } else {
                found = true;
            }
        }
        return found;
    }
}
