package de.pltp.mistakes;

import java.util.ArrayList;
import java.util.List;
import java.util.function.Consumer;
import java.util.function.Function;

public class UseLocalTypeInference {

    void foo() {

        // hit
        int integer = 5;
        String string = "a" + "b";
        List<Integer> list1 = new ArrayList<Integer>();

        // should hit and mention diamond operator
        List<Integer> list2 = new ArrayList<>();

        // declarations in for loops are currently not checked
        for (int i = 0; i < integer; i++) {
            for (int j : list1) {
                System.out.println(j);
            }
            System.out.println(i);
        }

        // ignore
        final var list3 = new ArrayList<Float>();
        var list4 = List.of();

        // var not applicable
        int[] array = {1, 2, 3};
        Consumer<Float> lambda = a -> System.out.println(a);
        Function<Integer, String> methodReference = Object::toString;
        List<String> list5 = List.of();
        List<String> list6 = Arrays.asList();

        // should raise warning about implicit Object
        var list7 = new ArrayList<>();
    }
}
