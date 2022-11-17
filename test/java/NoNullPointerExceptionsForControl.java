class NoNullPointerExceptionsForControl {

    private boolean contains(final T element, final Object[] array, final int current) {
        if (current == this.numberOfElements) {
            return false;
        } else {
            try {
                return (element.equals(this.array[current])) ? true : containsRecursive(element, array, current + 1);
            } catch (NullPointerException e) {
                return (element == this.array[current]) ? true : containsRecursive(element, array, current + 1);
            }
        }
    }

    private boolean contains2(final T element, final Object[] array, final int current) {
        if (current == this.numberOfElements) {
            return false;
        } else {
            try {
                return (element.equals(this.array[current])) ? true : containsRecursive(element, array, current + 1);
            } catch (java.lang.NullPointerException e) {
                return (element == this.array[current]) ? true : containsRecursive(element, array, current + 1);
            }
        }
    }
}
