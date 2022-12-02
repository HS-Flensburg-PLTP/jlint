class UseAssignOp {

    static int sum(final int currentSpawn, final int maxSpawn, final int[] array, final int start, final int end) {
        if (currentSpawn < maxSpawn && start < end) {
            var newEnd = start + (end - start) / 2;
            return sum(currentSpawn + 1, maxSpawn, array, start, newEnd)
                    + sum(currentSpawn + 1, maxSpawn, array, newEnd + 1, end);
        } else {
            var current = 0;
            for (int i = start; i <= end; i++) {
                current = current + array[i];
                System.out.println(array[i]);
            }
            return current;
        }
    }
}