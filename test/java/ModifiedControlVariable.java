package test.java;

public class ModifiedControlVariable {
    public void SingleForLoop() {
        for (int i = 0; i < 20; i++) {
            i = 12;
        }
    }

    public void NestedForLoop() {
        for (int i = 0; i < 20; i++) {
            i = i + 9;

            for (int j = 0; j < 20; j++) {
                j++;
            }
        }
    }

    public void NestedForLoop2() {
        for (int i = 0; i < 20; i++) {
            i--;
            for (int j = 0; j < 20; j++) {
                --j;
            }
        }
    }

    public void NestedForLoop3() {
        for (int i = 0; i < 20; i++) {
            i--;
            for (int j = 0; j < 20; j++, i++) {
                --j;
                for (int k = 0; k < 20; k++, ++j) {
                    --j;
                }
            }
        }
    }

    public void LoopWithIf() {
        for (int i = 0; i < 20; i++) {
            if (i == 3) {
                i++;
            }
        }
    }

}
